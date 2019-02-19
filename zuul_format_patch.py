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

    # Clone the project
    if not os.path.isdir(project):
        execute("git clone %s" % patches["project"])
        git("config --global user.name SF")
        git("config --global user.email zuul@sf")

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
        git("cherry-pick FETCH_HEAD")
        change["filename"] = git(
            "format-patch -1 %s" % patches["paths"], read=True).strip()
        print("<= patch is %s" % change["filename"])
        os.rename(os.path.join(project, change["filename"]),
                  change["filename"])

    # Generate spec file instruction
    idx = 10
    cat = None
    for change in patches["changes"]:
        if cat != change["cat"]:
            cat = change["cat"]
            print("\n# %s" % cat)
        print("Patch%2d:         %s" % (idx, change["filename"]))
        idx += 1


if __name__ == "__main__":
    main()
