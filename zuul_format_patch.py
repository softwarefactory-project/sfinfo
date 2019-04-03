#!/bin/env python
#
# Copyright 2019 Red Hat
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

"""
Load a patches.yaml and format the distgit patches:
  project: https://git.openstack.org/openstack-infra/zuul
  paths: zuul/ doc/ web/
  target: 2495512589c157862a9aaa45056568d04d445356
  changes:
    - url: https://review.openstack.org/568216
      cat: Tech previews
    - url: https://review.openstack.org/573473
      cat: Better UI
"""

import subprocess
import os
import yaml

os.environ["EDITOR"] = "/bin/true"


def execute(cmd, cwd=None):
    if subprocess.Popen(cmd.split(), cwd=cwd).wait():
        raise RuntimeError("%s failed" % cmd)


def pread(cmd, cwd=None):
    return subprocess.Popen(
        cmd.split(),
        cwd=cwd, stdout=subprocess.PIPE).communicate()[0].decode('utf-8')


def main():
    patches = yaml.load(open("patches.yaml"))
    project = os.path.basename(patches["project"])

    def git(cmd, read=False):
        cmd = "git " + cmd
        if read:
            return pread(cmd, cwd=project)
        return execute(cmd, cwd=project)

    def conflict_list():
        return list(map(lambda x: x.split()[1],
                        filter(lambda x: x.startswith("UU"),
                               git("status --short", read=True).split('\n'))))

    def solve_conflict(change):
        solved = True
        for conflict in conflict_list():
            if "tests/" in conflict:
                git("checkout --theirs %s" % conflict)
                git("add %s" % conflict)
            elif conflict in change.get("automerge", []):
                content = open(os.path.join(project, conflict)).readlines()
                with open(os.path.join(project, conflict), "w") as of:
                    for line in content:
                        if (line.startswith("<<<<<<<") or
                            line.startswith(">>>>>>>") or
                            line.startswith("=======")):
                            continue
                        of.write(line)
                git("add %s" % conflict)
            elif conflict in change.get("keep-ours", []):
                git("checkout --ours %s" % conflict)
                git("add %s" % conflict)
            elif conflict in change.get("autopatch", {}):
                p = subprocess.Popen(["patch", "-p0"], cwd=project, stdin=subprocess.PIPE)
                p.communicate(change["autopatch"][conflict])
                if p.wait():
                    raise RuntimeError("Couldn't apply patch")
                git("add %s" % conflict)
            else:
                solved = False
        if solved:
            git("cherry-pick --continue")
        return solved

    # Clone the project
    if not os.path.isdir(project):
        execute("git clone %s" % patches["project"])
        git("config user.name SF")
        git("config user.email zuul@sf")

    # Checkout target
    try:
        git("clean -f -d")
        git("reset --hard %s" % patches["target"])
    except RuntimeError:
        git("fetch origin")
        try:
            git("reset --hard %s" % patches["target"])
        except RuntimeError:
            print("Couldn't find target %s" % patches["target"])
            exit(1)

    if "not nodepool/tests" in patches["paths"]:
        # Special trick to get everything but nodepool/tests
        patches["paths"] = patches["paths"].replace(
            "not nodepool/tests", " ".join(list(
                map(lambda x: "nodepool/" + x,
                    filter(lambda x: x != "tests",
                           os.listdir("nodepool/nodepool"))))))

    # Get refs
    print("Fetching all refs...")
    changes = {}
    for ref in git("ls-remote", read=True).split('\n'):
        if "refs/changes" in ref:
            ref = ref.split()[1].split('/')
            cn, pn = ref[-2], ref[-1]
            if cn in changes and int(changes[cn]) >= int(pn):
                continue
            changes[ref[-2]] = ref[-1]

    # Generate patches
    for change in patches["changes"]:
        print("\n=> fetching %s" % change["url"])
        nr = os.path.basename(change["url"])
        ref = "refs/changes/%s/%s/%s" % (nr[-2:], nr, changes[nr])
        git("fetch %s %s" % (patches["project"], ref))
        if "master" in git("branch --contains FETCH_HEAD", read=True):
            change["merged"] = True
            continue
        change["merged"] = False
        try:
            git("cherry-pick FETCH_HEAD")
        except RuntimeError:
            if not solve_conflict(change):
                raw_input("Fix cherry-pick and press enter to continue...: ")
        change["filename"] = git(
            "format-patch -1 %s" % patches["paths"], read=True).strip()
        print("<= patch is %s" % change["filename"])
        os.rename(os.path.join(project, change["filename"]),
                  change["filename"])
        # Remove useless From line
        content = open(change["filename"]).readlines()
        with open(change["filename"], "w") as of:
            of.write("From 00000000000000000000000000000000000000000000000000 Mon Sep 17 00:00:00 2001\n")
            for line in content[1:]:
                of.write(line)
        

    # Generate spec file instruction
    idx = 10
    note = None
    for change in filter(lambda x: not x["merged"], patches["changes"]):
        if note != change.get("note", "Tech preview"):
            note = change.get("note", "Tech preview")
            print("\n# %s" % note)
        print("Patch%2d:        %s" % (idx, change["filename"]))
        idx += 1

    print("\n\n")
    for change in filter(lambda x: x["merged"], patches["changes"]):
        print("%s is already" % change)

if __name__ == "__main__":
    main()
