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


def most_recent(rpms):
    rpms.sort(cmp=rpm.labelCompare, key=lambda x: (x[1][3], x[1][1], x[1][2]))
    return rpms[-1]


class ZuulKojiPopulateTarget(zuul_koji_lib.App):
    def usage(self):
        p = argparse.ArgumentParser(description='Zuul Koji Populate Target')
        p.add_argument("--rst", action="store_true")
        p.add_argument("src_tag")
        p.add_argument("dst_tag")
        return p

    def list_tag(self, tag):
        self.log.info("===== Discovering package from koji")
        tag_content = map(lambda x: x.split()[0],
                          self.execute(["koji", "list-tagged", tag],
                                       capture=True).splitlines()[2:])
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
                self.log.info("Picked %s out of %s" % (pkg, pkgs))
            elif len(pkgs) == 1:
                pkg = pkgs[0][0]
            nvrs.append(pkg)
        return nvrs, pkgs_list

    def main(self, args):
        src, src_pkgs = self.list_tag(args.src_tag)
        dst, dst_pkgs = self.list_tag(args.dst_tag)
        if args.rst:
            new_pkgs = []
            updated_pkgs = []
            for pkg, inf in dst_pkgs.items():
                pkg_name = "%s-%s-%s.%s" % (inf[0], inf[1], inf[2], inf[4])
                if pkg not in src_pkgs:
                    new_pkgs.append(pkg_name)
                elif pkg_name not in src:
                    updated_pkgs.append(pkg_name)
            print("Updated Packages")
            print("~~~~~~~~~~~~~~~~\n")
            for pkg in sorted(updated_pkgs):
                print("- %s" % pkg)
            print("\n\nNew Packages")
            print("~~~~~~~~~~~~\n")
            for pkg in sorted(new_pkgs):
                print("- %s" % pkg)
        else:
            for diff in difflib.ndiff(src, dst):
                print(diff)


if __name__ == "__main__":
    ZuulKojiPopulateTarget()
