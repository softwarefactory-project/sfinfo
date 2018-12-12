#!/bin/env python3
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
import sys

# Global packages not referenced in distro infos
GL_PACKAGES = set((
    "software-factory/sfinfo",
    "www.softwarefactory-project.io",
    "software-factory/sf-desktop-notifications",
    "software-factory/sf-ci",
    "software-factory/sf-ui",
    "software-factory/sf-selinux",
    "software-factory/sf-ops",
    "software-factory/sf-specs",
    "software-factory/npmfed",
    "SF_devs_public_keys",
    "SF_password_store",
    "SF_talks",
    "sf-issues-migration",
    "software-factory/scrum-tools",
))


def clean(lines, packages):
    new_file = []
    dirty = False
    skip = False
    pos = 0
    while pos < len(lines):
        line = lines[pos]
        if line.startswith("- project:"):
            project_name = lines[pos + 1].split()[1]
            if project_name not in packages:
                skip = True
                dirty = True
        elif not line.startswith(" "):
            skip = False
        if not skip:
            new_file.append(line)
        pos += 1
    if dirty:
        print("Dirty!")
        return new_file


def main():
    p = argparse.ArgumentParser("Remove unused project definition")
    p.add_argument("zuul_files", nargs='+')
    args = p.parse_args()
    packages = set(map(lambda x: x[:-1], sys.stdin.readlines())).union(
        GL_PACKAGES)
    for zuul_file in args.zuul_files:
        updated_file = clean(open(zuul_file).readlines(), packages)
        if updated_file:
            with open(zuul_file, "w") as of:
                of.write("".join(updated_file))
                print("Updated", zuul_file)


if __name__ == "__main__":
    main()
