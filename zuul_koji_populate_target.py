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

import rpm
from rpmUtils.miscutils import splitFilename

import zuul_koji_lib


def most_recent(rpms):
    rpms.sort(cmp=rpm.labelCompare, key=lambda x: (x[1][3], x[1][1], x[1][2]))
    return rpms[-1]


class ZuulKojiPopulateTarget(zuul_koji_lib.App):
    def usage(self):
        p = argparse.ArgumentParser(description='Zuul Koji Populate Target')
        p.add_argument("--git-server", default=os.environ.get("GIT_SERVER",
                       "https://softwarefactory-project.io/r"))
        p.add_argument("--local-git-dir", default="~/koji-git")
        p.add_argument("--candidate", action='store_true')
        p.add_argument("--update", action='store_true', help='Update repo')
        p.add_argument("--project",
                       help="Restrict action to a single project")
        p.add_argument("--internal", action='store_true',
                       help="Restrict to internal project")
        return p

    def clone(self, repo, base_dir, git_server, update):
        dirname = "%s/%s" % (base_dir, repo)
        if not os.path.isdir(os.path.dirname(dirname)):
            os.mkdir(os.path.dirname(dirname), 0700)
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
            package["nvr"] = "%s-%s-%s" % (os.path.basename(package["name"]),
                                           package["version"],
                                           package["release"])
            if package.get("scl"):
                package["nvr"] = "%s-%s" % (package["scl"], package["nvr"])
            self.log.info("%(name)s: %(nvr)s" % package)

    def discover_nvr_from_koji(self, packages, tag):
        self.log.info("===== Discovering package from koji")
        tag_content = map(lambda x: x.split()[0],
                          self.execute(["koji", "list-tagged", tag],
                                       capture=True).splitlines()[2:])
        # Remove 9999 package
        rpms = map(lambda x: (x, splitFilename(x)),
                   filter(lambda x: "9999" not in x, tag_content))
        for package in packages:
            name = os.path.basename(package["name"])
            if package.get("scl"):
                name = "%s-%s" % (package["scl"], name)
            pkgs = [rpm for rpm in rpms if rpm[1][0] == name]
            if len(pkgs) > 1:
                pkg = most_recent(pkgs)
                self.log.info("Picked %s out of %s" % (pkg, pkgs))
            elif len(pkgs) == 1:
                pkg = pkgs[0]
            else:
                package["valid_nvr"] = False
                self.log.warning("Package %s doesn't exists in %s" % (name, tag))
                continue
            package["nvr"] = pkg[0]
            v = pkg[1][1]
            if v.startswith("0.0.0.0") or re.search("[a-zA-Z]", v):
                self.log.warning("Package %s doesn't look like to be "
                                 "tagged: %s" % (name, package["nvr"]))
                package["valid_nvr"] = False
            else:
                package["valid_nvr"] = True

    def add_packages_to_target(self, packages, tag, candidate):
        self.log.info("===== Adding package to target")
        if candidate:
            tag += "-candidate"
        missing_packages = []
        tag_content = self.execute(["koji", "list-tagged", tag], capture=True)
        for package in packages:
            name = os.path.basename(package["name"])
            if package.get("scl"):
                name = "%s-%s" % (package["scl"], name)
            if candidate and not package["valid_nvr"]:
                missing_packages.append(name)
                continue
            # Check if package in tag
            if package["nvr"] in tag_content:
                self.log.info("%s already in %s" % (package["nvr"], tag))
                package["populated"] = True
                continue
            self.log.info("Package %s not in %s" % (name, tag))
            self.execute(["koji", "add-pkg", tag, name, "--owner=sfci"])
            self.log.info("Adding %s to %s" % (package["nvr"], tag))
            try:
                self.execute(["koji", "tag-build", tag, package["nvr"]])
                package["populated"] = True
            except RuntimeError:
                self.log.warning("Couldn't tag build for %s" % package["nvr"])
                missing_packages.append(package["nvr"])
        if missing_packages:
            self.log.error("Failed to populate target, it's missing %s" %
                           " ".join(missing_packages))
            exit(1)
        else:
            self.log.info("SUCCESS: %s is populated" % tag)

    def main(self, args):
        if self.distro_info["branch"] == "master":
            self.log.error("Couldn't populate master branch...")
            exit(1)
        base_dir = os.path.expanduser(args.local_git_dir)
        if not os.path.isdir(base_dir):
            os.mkdir(base_dir, 0700)
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
                                    args.candidate)


if __name__ == "__main__":
    ZuulKojiPopulateTarget()
