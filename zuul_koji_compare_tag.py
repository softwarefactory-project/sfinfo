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

import zuul_koji_lib

from rpmUtils.miscutils import splitFilename  # type: ignore


class ZuulKojiCompareTag(zuul_koji_lib.App):
    def usage(self):
        p = argparse.ArgumentParser(description='Zuul Koji Populate Target')
        p.add_argument("--rst", action="store_true")
        p.add_argument("src_tag")
        p.add_argument("dst_tag")
        return p

    def main(self, args):
        src_name, dst_name = "src: " + args.src_tag, "dst: " + args.dst_tag
        src, src_pkgs = zuul_koji_lib.list_tag(args.src_tag, self.log)
        dst, dst_pkgs = zuul_koji_lib.list_tag(args.dst_tag, self.log)
        if args.rst:
            new_pkgs = []
            updated_pkgs = []
            deleted_pkgs = []
            for nvr in dst:
                pkg_name = splitFilename(nvr)[0]
                if pkg_name not in src_pkgs:
                    new_pkgs.append(nvr)
                elif nvr not in src:
                    updated_pkgs.append(nvr)
            for nvr in src:
                pkg_name = splitFilename(nvr)[0]
                if pkg_name not in dst_pkgs:
                    deleted_pkgs.append(nvr)
            print(zuul_koji_lib.format_rst_header("Updated Packages different in " + dst_name + " (package name is in src)"))
            for pkg in sorted(updated_pkgs):
                print("- %s" % pkg)
            print("")
            print(zuul_koji_lib.format_rst_header("Not in " + src_name))
            for pkg in sorted(new_pkgs):
                print("- %s" % pkg)
            print("")
            print(zuul_koji_lib.format_rst_header("Not in " + dst_name))
            for pkg in sorted(deleted_pkgs):
                print("- %s" % pkg)
        else:
            for diff in difflib.ndiff(src, dst):
                print(diff)


if __name__ == "__main__":
    ZuulKojiCompareTag()
