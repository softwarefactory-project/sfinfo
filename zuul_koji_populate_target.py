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

import argparse
import re
import os

import rpm  # type: ignore
from rpmUtils.miscutils import splitFilename  # type: ignore

import zuul_koji_lib


def most_recent(rpms):
    rpms.sort(cmp=rpm.labelCompare, key=lambda x: (x[1][3], x[1][1], x[1][2]))
    return rpms[-1]


class ZuulKojiPopulateTarget(zuul_koji_lib.App):
    def usage(self):
        p = argparse.ArgumentParser(description='Zuul Koji Populate Target')
        p.add_argument("--git-server", default=os.environ.get("GIT_SERVER",
                       "https://softwarefactory-project.io/r"))
        p.add_argument("--dry-run", action='store_true')
        p.add_argument("--local-git-dir", default="~/koji-git")
        p.add_argument("--candidate", action='store_true')
        p.add_argument("--update", action='store_true', help='Update repo')
        p.add_argument("--project",
                       help="Restrict action to a single project")
        p.add_argument("--branch",
                       help="Override the branch to use")
        p.add_argument("--internal", action='store_true',
                       help="Restrict to internal project")
        return p

    def clone(self, repo, base_dir, git_server, update):
        dirname = "%s/%s" % (base_dir, repo)
        if not os.path.isdir(os.path.dirname(dirname)):
            os.mkdir(os.path.dirname(dirname), 0o700)
        repourl = "%s/%s" % (git_server, repo)
        branch = self.distro_info["branch"]
        if not os.path.isdir(dirname):
            self.log.info("Cloning %s to %s" % (repourl, dirname))
            self.execute(["git", "clone", repourl, dirname])
        elif update:
            self.log.info("Updating %s" % dirname)
            self.execute(["git", "fetch", "--all"], cwd=dirname)

        try:
            self.execute(["git", "checkout", branch], cwd=dirname)
            self.execute(["git", "pull"], capture=True, cwd=dirname)
        except RuntimeError:
            self.log.warning("Couldn't find branch %s in %s" % (
                             branch, repo))

    def clone_projects(self, packages, base_dir, git_server, update):
        for package in packages:
            self.clone(package["distgit"], base_dir, git_server, update)
            if package["source"] == "internal":
                self.clone(package["name"], base_dir, git_server, update)

    def discover_nvr_from_repo(self, packages, base_dir):
        self.log.info("===== Discovering package from repo")
        for package in packages:
            specfile = "%s/%s/%s.spec" % (base_dir, package["distgit"],
                                          os.path.basename(package["name"]))
            if package["source"] == "internal":
                self.log.debug("Getting nvr from internal sources")
                package["version"] = self.get_repo_version("%s/%s" % (
                    base_dir, package["name"]))
            else:
                self.log.debug("Getting nvr from distgit spec")
                package["version"] = self.get_spec_version(specfile)

            package["release"] = self.get_spec_release(specfile)
            name = os.path.basename(package["name"])
            if package["name"].startswith("rpms/python-"):
                name = name.replace("python-", "python3-")
            package["nvr"] = "%s-%s-%s" % (name,
                                           package["version"],
                                           package["release"])
            if package.get("scl"):
                package["nvr"] = "%s-%s" % (package["scl"], package["nvr"])
            self.log.info("%(name)s: %(nvr)s" % package)

    def discover_nvr_from_koji(self, packages, tag):
        self.log.info("===== Discovering package from koji tag %s", tag)
        tag_content = zuul_koji_lib.get_tag_content(tag, self.log)
        (_, koji_tag_pkgs) = zuul_koji_lib.list_tag_content(tag_content)
        koji_tag_pkgs_set = set(koji_tag_pkgs.keys())
        distro_pkgs_set = set()
        # Remove 9999 package
        rpms = map(lambda x: (x, splitFilename(x)),
                   filter(lambda x: "9999" not in x, tag_content))
        for package in packages:
            name = os.path.basename(package["name"])
            if package.get("scl"):
                name = "%s-%s" % (package["scl"], name)
            if package["name"].startswith("rpms/python-"):
                name = name.replace("python-", "python3-")
            pkgs = [rpm for rpm in rpms if rpm[1][0] == name]
            if len(pkgs) > 1:
                pkg = most_recent(pkgs)
                self.log.info("Picked %s out of %s" % (pkg, pkgs))
            elif len(pkgs) == 1:
                pkg = pkgs[0]
            else:
                package["valid_nvr"] = False
                self.log.warning("Package %s doesn't exists in %s" %
                                 (name, tag))
                continue
            distro_pkgs_set.add(name)
            package["nvr"] = pkg[0]
            v = pkg[1][1]

            def invalid_nvr():
                return (
                    name != "python3-devnest"  # It's ok, this one is on me
                    and (v.startswith("0.0.0.0")
                         or re.search("[a-zA-Z]", v))
                    )
            if invalid_nvr():
                self.log.warning("Package %s doesn't look like to be "
                                 "tagged: %s" % (name, package["nvr"]))
                package["valid_nvr"] = False
            else:
                package["valid_nvr"] = True

        zuul_koji_lib.diff_ensure(
            koji_tag_pkgs_set, distro_pkgs_set,
            "Package in koji not in distro file", self.log)
        zuul_koji_lib.diff_ensure(
            distro_pkgs_set, koji_tag_pkgs_set,
            "Package in distro not in koji", self.log)

    def add_packages_to_target(self, packages, tag, candidate, dry_run=False):
        self.log.info("===== Adding package to target")
        if candidate:
            tag += "-candidate"
        tag_content = self.execute(["koji", "list-tagged", tag], capture=True)
        to_tag = []
        missing_packages = []
        for package in packages:
            name = os.path.basename(package["name"])
            if package.get("scl"):
                name = "%s-%s" % (package["scl"], name)
            if package["name"].startswith("rpms/python-"):
                name = name.replace("python-", "python3-")
            if candidate and not package["valid_nvr"]:
                missing_packages.append(name)
                continue
            # Check if package in tag
            # Prefix with new line to prevent conflict when nvr exists in scl
            if "\n" + package["nvr"] in tag_content:
                # self.log.info("%s already in %s" % (package["nvr"], tag))
                package["populated"] = True
                continue
            self.log.info("Adding package %s to %s (because it is missing)" %
                          (name, tag))
            if not dry_run:
                self.execute(["koji", "add-pkg", tag, name, "--owner=sfci"])
            to_tag.append(package["nvr"])
        try:
            if to_tag:
                self.log.info("Tagging %s for %s", tag, to_tag)
                if not dry_run:
                    self.execute(["koji", "tag-build", tag] + to_tag)
            package["populated"] = True
        except RuntimeError:
            self.log.warning("Couldn't tag build for %s", to_tag)
            exit(1)
        if missing_packages:
            self.log.warning("Missing packages %s", missing_packages)
            exit(1)
        if dry_run:
            self.log.info("Remove dry-run to proceed")
        else:
            self.log.info("SUCCESS: %s is populated" % tag)

    def main(self, args):
        if self.distro_info["branch"] == "master":
            self.log.error("Couldn't populate master branch...")
            exit(1)
        if args.branch:
            self.distro_info["branch"] = args.branch
        base_dir = os.path.expanduser(args.local_git_dir)
        if not os.path.isdir(base_dir):
            os.mkdir(base_dir, 0o700)
        if args.project:
            pkgs = [pkg for pkg in self.distro_info["packages"] if
                    pkg['name'] == args.project]
        else:
            pkgs = self.distro_info["packages"]
        if args.internal:
            pkgs = filter(lambda pkg: pkg["source"] == "internal", pkgs)
        if args.candidate:
            self.discover_nvr_from_koji(pkgs, self.distro_info["koji-target"])
        else:
            self.clone_projects(pkgs, base_dir, args.git_server, args.update)
            self.discover_nvr_from_repo(pkgs, base_dir)
        self.add_packages_to_target(pkgs, self.distro_info["koji-target"],
                                    args.candidate, args.dry_run)


if __name__ == "__main__":
    ZuulKojiPopulateTarget()
