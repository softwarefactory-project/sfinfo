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

import argparse
import os
import re
import sys
import shlex
import glob
import shutil
import time
import json

import requests
from semantic_version import Version, SpecItem

import zuul_koji_lib


class ZuulUpdateSpec(zuul_koji_lib.App):
    def usage(self):
        p = argparse.ArgumentParser(description='Zuul SPEC updater')
        p.add_argument("--source",
                       default="https://softwarefactory-project.io/r/",
                       help="The git server to fetch projects from")
        return p

    def get_github_versions(self, url):
        url = "https://api.github.com/repos/%s/tags" % (
            url.rstrip('/').replace("https://github.com/", ""))
        resp = requests.get(url)
        return map(lambda x: x["name"].replace("v", ""), resp.json())

    def get_upstream_version(self, package):
        if package["upstream"] in self.cache:
            versions_str = self.cache[package["upstream"]]
        if "github.com" in package["upstream"]:
            versions_str = self.get_github_versions(package["upstream"])
        else:
            raise RuntimeError("Unknown upstream: %s" % package["upstream"])
        self.cache[package["upstream"]] = versions_str
        versions = []
        for version in versions_str:
            if "rc" in version or re.match(".*[abc][0-9]+$", version):
                continue
            try:
                versions.append(Version(version))
            except Exception as e:
                print("Couldn't parse: %s" % version)
        if package.get("constrain"):
            spec = SpecItem(package["constrain"])
            versions = filter(lambda x: spec.match(x), versions)
        return str(list(sorted(versions))[-1])

    def get_changeid(self, repo, branch="master"):
        query = "status:open+owner:dlrn+project:%s+branch:%s" % (repo, branch)
        url = "%s/changes/?q=%s" % (self.args.source.rstrip('/'), query)
        changes = json.loads(requests.get(url).text[5:])
        if changes:
            return changes[0]["change_id"], changes[0]["subject"]
        return None, ""

    def update_spec(self, package):
        upstream_version = self.get_upstream_version(package)

        repo = self.clone_package(package)
        changeId, commit_message = self.get_changeid(repo)
        if upstream_version in commit_message:
            self.log.info("Bump already proposed")
            return
        specfile = "%s/%s.spec" % (repo, os.path.basename(package["name"]))
        local_version = self.get_spec_version(specfile)
        if upstream_version != local_version:
            self.log.debug("Updating local spec of %s from %s to %s",
                           package["name"], local_version, upstream_version)
            # TODO: make this a param
            author = "DLRN dlrn@softwarefactory-project.io"
            spec = open(specfile).readlines()
            with open(specfile, "w") as of:
                for line in spec:
                    if line.startswith("Version:"):
                        line = line.replace(local_version, upstream_version)
                    if line.startswith("Release:"):
                        line = re.sub("[0-9]+", "1", line)
                    of.write(line)
                    if line.startswith("%changelog"):
                        of.write("* %s %s <%s> - %s-1\n" % (
                            time.strftime("%a %b %d %Y"),
                            author.split()[0],
                            author.split()[1],
                            upstream_version))
                        of.write("- Automatic bump to %s\n" % upstream_version)
                        of.write("\n")

            def git(cmd):
                cmd = "git " + cmd
                self.execute(shlex.split(cmd), cwd=repo)
            git("config user.name %s" % author.split()[0])
            git("config user.email %s" % author.split()[1])
            git("config gitreview.username %s" % author.split()[0])
            msg = "Automatic bump to %s" % upstream_version
            if changeId:
                msg += "\n\nChange-Id: %s" % changeId
            git("commit -a -m '%s'" % msg)
            git("review")

    def main(self, args):
        self.args = args
        self.cache = {}
        for package in filter(lambda x: x.get('upstream'),
                              self.distro_info["packages"]):
            try:
                self.update_spec(package)
            except Exception as e:
                self.log.exception("Couldn't update %s:" % package["name"])


if __name__ == "__main__":
    ZuulUpdateSpec()
