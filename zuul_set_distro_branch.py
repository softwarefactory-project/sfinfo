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

import re
import os
import sys
import pwd
import argparse

import zuul_koji_lib


class ZuulSetDistroBranch(zuul_koji_lib.App):
    def usage(self):
        p = argparse.ArgumentParser(description='Zuul Set Distro Branch')
        p.add_argument("--git-server", default=os.environ.get("GIT_SERVER",
                       "https://softwarefactory-project.io/r"))
        p.add_argument("--local-git-dir", default="~/koji-git")
        p.add_argument("--username", default=pwd.getpwuid(os.getuid())[0],
                       help="Username used to git push")
        p.add_argument("--push-branch", action="store_true",
                       help="Push the branch")
        p.add_argument("--force-push-branch", action="store_true",
                       help="Force reset the branch")
        p.add_argument("--project",
                       help="Restrict action to a single project")
        return p

    def branch(self, repo, base_dir, git_server):
        dirname = "%s/%s" % (base_dir, repo)
        if not os.path.isdir(os.path.dirname(dirname)):
            os.mkdir(os.path.dirname(dirname), 0700)
        repourl = "%s/%s" % (git_server, repo)
        branch = self.distro_info["branch"]
        bfrom = self.distro_info.get("from", "master")
        if not os.path.isdir(dirname):
            self.log.info("Cloning %s to %s" % (repourl, dirname))
            self.execute(["git", "clone", repourl, dirname])
        else:
            self.log.info("Fetch all references of %s" % dirname)
            self.execute(["git", "fetch", "--all"], cwd=dirname)

        try:
            self.log.info("Checkout branch %s for repo %s" % (branch, repo))
            self.execute(["git", "checkout", branch], test=True, cwd=dirname)
            self.execute(["git", "pull"], capture=True, cwd=dirname)
        except RuntimeError:
            self.log.info("Failed to checkout branch %s for repo %s" % (
                          branch, repo))
            self.log.info("Creating branch %s for %s from %s" % (
                          branch, repo, bfrom))
            self.execute(["git", "branch", branch,
                          "origin/%s" % bfrom], cwd=dirname)
            self.execute(["git", "checkout", branch], cwd=dirname)

        # self.log.info("Last commit on %s for repo %s is:" % (branch, repo))
        # self.execute(["git", "log", "-n", "1"], cwd=dirname)

    def branch_project(self, base_dir, git_server, package):
        self.branch(package["distgit"], base_dir, git_server)
        if package["source"] == "internal":
            self.branch(package["name"], base_dir, git_server)

    def push(self, repo, base_dir, username, force):
        branch = self.distro_info["branch"]
        dirname = "%s/%s" % (base_dir, repo)
        gitreview = open("%s/.gitreview" % dirname).read()
        gitreview_updated = open("%s/.gitreview" % dirname).read()
        gitreview_updated = re.sub(
            "defaultbranch=.*", "defaultbranch=%s" % branch, gitreview)
        port = re.findall("port=.*", gitreview)[-1].replace("port=", "")
        host = re.findall("host=.*", gitreview)[-1].replace("host=", "")
        if gitreview_updated != gitreview:
            self.log.info("Update .gitreview file on %s for repo %s" % (
                          branch, repo))
            with open("%s/.gitreview" % dirname, "w") as of:
                of.write(gitreview_updated)
            self.execute(["git", "commit", "-a", "-m",
                          "Update .gitreview to %s" % branch], cwd=dirname)
        remote = "ssh://%s@%s:%s/%s" % (username, host, port, repo)
        if force:
            self.log.info("Force push branch %s on repo %s" % (branch, repo))
            self.execute(["git", "push", remote, "-f", branch], cwd=dirname)
        else:
            self.log.info("Push branch %s on repo %s" % (branch, repo))
            self.execute(["git", "push", remote, branch], cwd=dirname)

    def push_branch(self, base_dir, package, username, force=False):
        self.push(package["distgit"], base_dir, username, force)
        if package["source"] == "internal":
            self.push(package["name"], base_dir, username, force)

    def main(self, args):
        base_dir = os.path.expanduser(args.local_git_dir)
        if not os.path.isdir(base_dir):
            os.mkdir(base_dir, 0700)
        if args.project:
            packages = [pkg for pkg in self.distro_info["packages"] if
                        pkg['name'] == args.project]
        else:
            packages = self.distro_info["packages"]
        for package in packages:
            self.branch_project(base_dir, args.git_server, package)
            if args.push_branch:
                if not os.path.isfile(os.path.expanduser("~/.gitconfig")):
                    self.log.error("Please defines a ~/.gitconfig file")
                    sys.exit(1)
                self.push_branch(base_dir, package, args.username,
                                 args.force_push_branch)


if __name__ == "__main__":
    ZuulSetDistroBranch()
