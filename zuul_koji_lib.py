#!/bin/env python
#
# Copyright 2017 Red Hat
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may
# not use this file except in compliance with the License. You may obtain
# a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations
# under the License.

import logging
import yaml
import subprocess
import sys
import os

import rpm  # type: ignore

from rpmUtils.miscutils import splitFilename  # type: ignore


def format_rst_header(header):
    return "\n".join([
        "",
        header,
        "~" * len(header)
    ])


def list_repos(distro_info):
    return list(map(lambda s: s['pkg_name'], distro_info["packages"]))


def prettify(lines):
    return map(lambda s: "* " + s, sorted(lines))


def diff_ensure(set1, set2, msg, log):
    diff = set1.difference(set2)
    if diff:
        log.error(msg + ":\n" + "\n".join(
            prettify(diff)))
        raise RuntimeError("abort")


def execute(argv, log=None, capture=False, cwd=None, test=False):
    if capture is True:
        stdout = subprocess.PIPE
    else:
        stdout = None
    if log:
        log.debug("Running %s" % argv)
    s = subprocess.Popen(argv, stdout=stdout, cwd=cwd)
    out, _ = s.communicate()
    code = s.wait()
    if log:
        log.debug("Command %s exited with code %s" % (argv, code))
    if code:
        if not test and log:
            log.error("Command %s failed" % argv)
        raise RuntimeError()
    return out


def most_recent(rpms):
    rpms.sort(cmp=rpm.labelCompare, key=lambda x: (x[1][3], x[1][1], x[1][2]))
    return rpms[-1]


def nvr_name(nvr_tuple):
    if nvr_tuple[3]:
        raise RuntimeError("Can't format %s" % str(nvr_tuple))
    return "%s-%s-%s.%s" % (nvr_tuple[0], nvr_tuple[1], nvr_tuple[2],
                            nvr_tuple[4])


def get_tag_content(tag, log=None):
    return list(map(lambda x: x.split()[0],
                    execute(["koji", "list-tagged", tag], log,
                            capture=True).splitlines()[2:]))


def list_tag(tag, log=None):
    return list_tag_content(get_tag_content(tag, log))


# import typing
# TagNvrs = typing.List[str]
# TagPkgs = typing.Dict[str, typing.Tuple[str, str, str, str, str]]
# TagRepo = typing.Tuple[TagNvrs, TagPkgs]
# def list_tag(tag: str, log=None) -> TagRepo:
def list_tag_content(tag_content, log=None):
    # Remove 9999 package
    rpms = map(lambda x: (x, splitFilename(x)),
               filter(lambda x: "9999" not in x, tag_content))
    pkgs = set()
    pkgs_list = {}
    for _, inf in rpms:
        pkgs.add(inf[0])
        pkgs_list[inf[0]] = inf
    nvrs = []
    for name in sorted(list(pkgs)):
        pkgs = [rpm for rpm in rpms if rpm[1][0] == name]
        if len(pkgs) > 1:
            pkg = most_recent(pkgs)[0]
            if log:
                log.info("Picked %s out of %s" % (pkg, pkgs))
        elif len(pkgs) == 1:
            pkg = pkgs[0][0]
        nvrs.append(pkg)
    return nvrs, pkgs_list


# def tag_to_packages(tagRepo: TagRepo) -> typing.List[str]:
def tag_to_packages(tagRepo):
    return list(tagRepo[1].keys())


class App:
    # App needs usage() and main() method

    @classmethod
    def _get_name(cls):
        return cls.__name__

    def execute(self, argv, capture=False, cwd=None, test=False):
        return execute(argv, self.log, capture, cwd, test)

    def get_repo_version(self, repo):
        ref = "HEAD"
        # Check if last change changed .gitreview
        view_ref = self.execute(["git", "log", "-n1", "--format='%H'",
                                 ".gitreview"], capture=True, cwd=repo).strip()
        head_ref = self.execute(["git", "log", "-n1", "--format='%H'"],
                                capture=True, cwd=repo).strip()
        if view_ref == head_ref:
            self.log.debug("Skipping .gitreview update")
            ref = "HEAD^"
        try:
            version = self.execute(["git", "describe", "--tags", ref],
                                   capture=True, cwd=repo, test=True).strip()
        except RuntimeError:
            version = self.execute(["git", "rev-list", "--count", ref],
                                   capture=True, cwd=repo, test=True).strip()
            # Make sure rev-list version is lower than first tag
            version = "0.0.0.0-dev%s" % version
        return version.replace('-', '.')

    def get_spec_field(self, specfile, field):
        spec = self.execute(["rpmspec", "-P", specfile], capture=True)
        for line in spec.split('\n'):
            if line.startswith("%s:" % field):
                return line.split()[1]
        raise RuntimeError("%s: couldn't find %s" % (specfile, field))

    def get_spec_version(self, specfile):
        return self.get_spec_field(specfile, "Version")

    def get_spec_release(self, specfile):
        return self.get_spec_field(specfile, "Release")

    def clone_package(self, package):
        # TODO: handle internal source
        if package["name"].startswith("rpms/"):
            repo = package["name"]
        else:
            repo = "%s-distgit" % package["name"]
        repo_url = os.path.join(self.args.source, repo)
        if not os.path.isdir(repo):
            os.makedirs(repo)
        if not os.path.isdir(os.path.join(repo, ".git")):
            self.execute(["git", "clone", repo_url, repo])
        else:
            # TODO: support branch
            self.execute(["git", "clean", "-xfd"], cwd=repo)
            self.execute(["git", "fetch"], cwd=repo)
            self.execute(["git", "reset", "--hard", "origin/master"], cwd=repo)
        return repo

    def _load_distro_info(self, path):
        if not os.path.isfile(path):
            raise RuntimeError()
        self.distro_info = yaml.safe_load(open(path))
        if "inherit" in self.distro_info:
            loc = os.path.dirname(path)
            ipath = os.path.join(loc, self.distro_info["inherit"])
            if not os.path.isfile(ipath):
                raise RuntimeError("Inherited file not found in %s" % ipath)
            base_distro_info = yaml.safe_load(open(ipath))
            base_distro_info.update(self.distro_info)
            self.distro_info = base_distro_info
            self.log.debug("Found inheritance from distro %s" % ipath)
        # Make sure branch name is str
        self.distro_info["branch"] = str(self.distro_info["branch"])
        # Update package infos
        for package in self.distro_info["packages"]:
            if package.get("spec") == "included" or \
               package["name"].startswith("rpms/"):
                package["distgit"] = package["name"]
            elif not package.get("distgit"):
                package["distgit"] = "%s-distgit" % package["name"]

            # Find the true koji package name
            try:
                if package["name"].startswith("rpms/python-"):
                    package["pkg_name"] = "python3-%s" % (
                        package["name"].split("rpms/python-")[1])
                elif "/" in package["name"]:
                    package["pkg_name"] = package["name"].split("/")[1]
                else:
                    package["pkg_name"] = package["name"]
            except Exception:
                print("Oops", package["name"])
                raise

        self.distro_info["repos"] = self.distro_info.get("baserepos", []) + \
            self.distro_info.get("extrarepos", [])

    def _parse_arguments(self, args):
        # Parse command line
        p = self.usage()
        p.add_argument('--verbose', action='store_true', help='verbose output')
        p.add_argument("--distro-info", default='distro.yaml',
                       help="The yaml distro info file")
        return p.parse_args(args)

    def _setup_logging(self, verbose=False):
        self.log = logging.getLogger('zuulkoji.%s' % self._get_name())
        fmt = '\033[1;33m%(levelname)-5.5s [%(name)s] %(message)s\033[1;0m'
        if verbose:
            logging.basicConfig(format=fmt, level=logging.DEBUG)
        else:
            logging.basicConfig(format=fmt, level=logging.INFO)

    def __init__(self, args=sys.argv[1:]):
        args = self._parse_arguments(args)
        self._setup_logging(args.verbose)
        self._load_distro_info(args.distro_info)
        self.main(args)
