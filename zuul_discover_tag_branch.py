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
import os

import zuul_koji_lib


class ZuulDiscoverTagBranch(zuul_koji_lib.App):
    def usage(self):
        p = argparse.ArgumentParser(description='Zuul Discover Tag Branch')
        p.add_argument("--git-server", default=os.environ.get("GIT_SERVER",
                       "https://softwarefactory-project.io/r"))
        p.add_argument("--project", default=os.environ.get("ZUUL_PROJECT"))
        p.add_argument("--ref", default=os.environ.get("ZUUL_REF"))
        return p

    def main(self, args):
        tag = args.ref.replace('refs/tags/', '')
        if not os.path.isdir(args.project):
            self.execute(["zuul-cloner", args.git_server, args.project])
        branches = self.execute(["git", "branch", "--contains", tag],
                                capture=True, cwd=args.project).split('\n')
        try:
            # Remove (no branch) and prefix space
            valid_branch = map(lambda x: x[2:],
                               filter(lambda x: "(" not in x, branches))
            self.log.info("Tag found on branches: %s" % ', '.join(valid_branch))
            if "master" in valid_branch:
                print("master")
            else:
                print(valid_branch[0])
        except:
            self.log.error("Couldn't find branch for tag %s" % tag)
            exit(1)

if __name__ == "__main__":
    ZuulDiscoverTagBranch()
