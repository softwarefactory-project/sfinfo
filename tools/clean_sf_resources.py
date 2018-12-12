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

from clean_zuul_config import GL_PACKAGES


def repodef(line):
    return line.startswith("    ") and line[4] not in (" ", "#")


def srdef(line):
    return line.startswith("      ") and line[6] == "-"


def clean(lines, packages):
    new_file = []
    dirty = False
    skip = False
    state = None
    pos = 0
    while pos < len(lines):
        line = lines[pos]
        if line.startswith("      source-repositories:"):
            state = "sr"
        elif line.startswith("  repos:"):
            state = "repos"
        else:
            if state == "sr" and line != "\n" and \
               not line.startswith("      -") and \
               not line.startswith("      #") and \
               not line.startswith("       "):
                state = None
            if state == "repos" and line != "\n" and \
               not line.startswith("    "):
                state = None
        if state == "sr" and srdef(line):
            p = line.split()[1].rstrip(':')
            if p not in packages:
                skip = True
                pos += 1
                while pos < len(lines):
                    if srdef(lines[pos]):
                        break
                    pos += 1
                pos -= 1
        if state == "repos" and repodef(line):
            p = line.split()[0].rstrip(':')
            if p not in packages:
                skip = True
                pos += 1
                while pos < len(lines):
                    if repodef(lines[pos]):
                        break
                    pos += 1
                pos -= 1
        if skip:
            dirty = True
        else:
            new_file.append(line)
        pos += 1
        skip = False
    if dirty:
        return new_file


def main():
    p = argparse.ArgumentParser("Remove unused project definition")
    p.add_argument("resources_files", nargs='+')
    args = p.parse_args()
    packages = set(map(lambda x: x[:-1], sys.stdin.readlines())).union(
        GL_PACKAGES)
    for resources_file in args.resources_files:
        updated_file = clean(open(resources_file).readlines(), packages)
        if updated_file:
            with open(resources_file, "w") as of:
                of.write("".join(updated_file))
                print("Updated", resources_file)


if __name__ == "__main__":
    main()
