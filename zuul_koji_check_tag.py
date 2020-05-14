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
import difflib

import rpm
from rpmUtils.miscutils import splitFilename

import zuul_koji_lib


class ZuulKojiCheckTarget(zuul_koji_lib.App):
    def usage(self):
        p = argparse.ArgumentParser(description='Zuul Koji Check Target')
        return p

    def main(self, args):
        repos = set(zuul_koji_lib.list_repos(self.distro_info))
        tagged_repos = set(zuul_koji_lib.tag_to_packages(
            zuul_koji_lib.list_tag(self.distro_info["koji-target"])))
        pretty = lambda l: map(lambda s: "* " + s, sorted(l))
        print("To untag:\n" + "\n".join(pretty(tagged_repos.difference(repos))))
        print("")
        print("To tag:\n" + "\n".join(pretty(repos.difference(tagged_repos))))


if __name__ == "__main__":
    ZuulKojiCheckTarget()
