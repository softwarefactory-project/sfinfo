#!/bin/env python3
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

import os
import sys
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
        return p

    def clone_config_repo(self, base_dir, git_server):
        dirname = "%s/%s" % (base_dir, 'config')
        if not os.path.isdir(os.path.dirname(dirname)):
            os.mkdir(os.path.dirname(dirname), 0o700)
        repourl = "%s/config" % git_server
        if not os.path.isdir(dirname):
            self.log.info("Cloning %s to %s" % (repourl, dirname))
            self.execute(["git", "clone", repourl, dirname])
        else:
            self.log.info("Fetch all references of %s" % dirname)
            self.execute(["git", "fetch", "--all"], cwd=dirname)
            self.log.info("Checkout branch master for repo config")
            self.execute(["git", "reset", "--hard", "origin/master"],
                         cwd=dirname)

    def repo_get_offset(self, raw, name):
        offset = 0
        # Find repos sections
        while True:
            if 'repos:' in raw[offset]:
                break
            offset += 1
        # Find repo definition
        while True:
            if '%s:' % name in raw[offset]:
                break
            offset += 1
        start = offset
        # Find the end of the repo definition section
        # based on the indent
        # Get indent offset
        ioffset = raw[start].find('%s:' % name)
        while True:
            offset += 1
            if offset >= len(raw):
                # EOF
                return start, len(raw), ioffset
            # Remove indent
            nextl = raw[offset].strip()
            inextl = raw[offset].find(nextl)
            if inextl == ioffset:
                end = offset
                break
        return start, end, ioffset

    def update_repo_def(self, resources, raw_resources, name, branch, bfrom):
        orepo = resources['resources']['repos'].get(name)
        if not orepo:
            self.log.info(
                "Repo %s not found in the current resources file. Skip !" % (
                    name))
            return
        start, end, ioffset = self.repo_get_offset(
            raw_resources, name)
        # Remove old repo section
        raw_resources[start:end] = []
        repo = {}
        repo[name] = orepo
        repo[name].setdefault('branches', {})
        repo[name]['branches'][branch] = bfrom
        repo_raw = yaml.dump(repo, default_flow_style=False).split('\n')
        repo_raw = ['%s%s\n' % (" " * ioffset, l) for l in repo_raw]
        del repo_raw[-1]
        # Insert new repo section
        raw_resources[start:start] = repo_raw

    def main(self, args):
        base_dir = os.path.expanduser(args.local_git_dir)
        if not os.path.isdir(base_dir):
            os.mkdir(base_dir, 0o700)
        # Get repos list to act on
        packages = self.distro_info["packages"]
        bfrom = self.distro_info.get("from", "master")
        branch = self.distro_info.get("branch")
        if not branch:
            self.log.error("Branch is missing from the distro file")
            sys.exit(1)
        repos_names = []
        for package in packages:
            if package['source'] == 'internal':
                repos_names.append(package['name'])
                repos_names.append(package['distgit'])
            else:
                repos_names.append(package['distgit'])
        # Get and load the resources file
        self.clone_config_repo(base_dir, args.git_server)
        p_resources_path = os.path.join(
            base_dir, 'config', 'resources', args.project_resources)
        raw_resources = open(p_resources_path).readlines()
        resources = yaml.safe_load(open(p_resources_path))
        for name in repos_names:
            self.update_repo_def(
                resources,
                raw_resources,
                name,
                branch,
                bfrom)
        open(p_resources_path, 'w').write("\n".join(raw_resources))


if __name__ == "__main__":
    ZuulSetDistroBranch()
