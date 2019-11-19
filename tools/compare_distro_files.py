#!/bin/env python
#
# Copyright 2018 Red Hat
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
import yaml


def compare(src, dst):
    src_pkgs, dst_pkgs = set(), set()
    for src_pkg in src["packages"]:
        src_pkgs.add(src_pkg["name"])
    for dst_pkg in dst["packages"]:
        dst_pkgs.add(dst_pkg["name"])
    for pkg in src_pkgs:
        if pkg not in dst_pkgs:
            print("-", pkg)
    for pkg in dst_pkgs:
        if pkg not in src_pkgs:
            print("+", pkg)


def main():
    p = argparse.ArgumentParser(
        "Compare package difference between distro files")
    p.add_argument("src")
    p.add_argument("dst")
    args = p.parse_args()
    src = yaml.safe_load(open(args.src))
    dst = yaml.safe_load(open(args.dst))
    compare(src, dst)


if __name__ == "__main__":
    main()
