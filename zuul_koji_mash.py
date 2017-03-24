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

import zuul_koji_lib


class ZuulKojiMash(zuul_koji_lib.App):
    def usage(self):
        p = argparse.ArgumentParser(description='Zuul Koji Mash')
        p.add_argument("--release", action='store_true')
        p.add_argument("--build", action='store_true')
        return p

    def main(self, args):
        mash_file = "%s.mash" % self.distro_info["koji-target"]
        with open(mash_file, "w") as of:
            of.write("""[%(koji-target)s]
tag = %(koji-target)s
arches = %(arch)s
inherit = False
debuginfo = False

[%(koji-target)s-candidate]
tag = %(koji-target)s-candidate
arches = %(arch)s
inherit = False
debuginfo = False

[%(koji-target)s-release]
tag = %(koji-target)s-release
arches = %(arch)s
inherit = False
debuginfo = False
""" % self.distro_info)
        self.execute(["sudo", "mv", mash_file, "/etc/mash"])
        if args.build:
            tag = self.distro_info["koji-target"]
        elif args.release:
            tag = "%s-release" % self.distro_info["koji-target"]
        else:
            tag = "%s-candidate" % self.distro_info["koji-target"]
        self.execute(["sudo", "mash", "-o", "/mnt/koji/repos", tag])


if __name__ == "__main__":
    ZuulKojiMash()
