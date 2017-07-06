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
import yaml
import argparse

import zuul_koji_lib


class ZuulSetDistroBranch(zuul_koji_lib.App):
    def usage(self):
        p = argparse.ArgumentParser(description='Zuul Set Distro Branch')
        p.add_argument("--git-server", default=os.environ.get("GIT_SERVER",
                       "https://softwarefactory-project.io/r"))
        p.add_argument("--local-git-dir", default="~/koji-git")
        p.add_argument("--project-resources", default="software-factory.yaml")
        p.add_argument("--username", default=pwd.getpwuid(os.getuid())[0],
                       help="Username used to git push")
        return p

    def clone_config_repo(self, base_dir, git_server):
        dirname = "%s/%s" % (base_dir, 'config')
        if not os.path.isdir(os.path.dirname(dirname)):
            os.mkdir(os.path.dirname(dirname), 0700)
        repourl = "%s/config" % git_server
        # bfrom = self.distro_info.get("from", "master")
        if not os.path.isdir(dirname):
            self.log.info("Cloning %s to %s" % (repourl, dirname))
            self.execute(["git", "clone", repourl, dirname])
        else:
            self.log.info("Fetch all references of %s" % dirname)
            self.execute(["git", "fetch", "--all"], cwd=dirname)
            self.log.info("Checkout branch master for repo config")
            self.execute(["git", "reset", "--hard", "origin/master"],
                         cwd=dirname)

    def push_config_repo_review(self, base_dir, username):
        remote = "ssh://%s@%s:%s/config" % (username, host, port)
        self.log.info("Push review on repo config")
        self.execute(["git", "push", remote, 'refs/for/master'],
                     cwd=dirname)

    def repo_get_offset(self, raw, name):
        offset = 0
        while True:
            if 'repos:' in raw[offset]:
                break
            offset += 1
        while True:
            if '%s:' % name in raw[offset]:
                break
            offset += 1
        return offset


    def main(self, args):
        base_dir = os.path.expanduser(args.local_git_dir)
        if not os.path.isdir(base_dir):
            os.mkdir(base_dir, 0700)
        packages = self.distro_info["packages"]
        self.clone_config_repo(base_dir, args.git_server)
        p_resources_path = os.path.join(base_dir,
            'config', 'resources', args.project_resources)
        raw_resources = file(p_resources_path).readlines()
        resources = yaml.load(file(p_resources_path))
        offset = self.repo_get_offset(
                raw_resources, "software-factory/sf-config")
        branches = resources['resources']['repos']['software-factory/sf-config'].get('branches', {})
        branches['2.6'] = 'HEAD'
        raw_resources.insert(offset, str(branches))
        file('out', 'w').writelines(raw_resources)
        #self.push(config, base_dir, args.username)


if __name__ == "__main__":
    ZuulSetDistroBranch()
