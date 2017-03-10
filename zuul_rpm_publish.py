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
import glob
import requests
import re
import os

import zuul_koji_lib


class ZuulRpmPublish(zuul_koji_lib.App):
    def usage(self):
        p = argparse.ArgumentParser(description='Zuul RPM publish')
        p.add_argument("--project", default=os.environ.get("ZUUL_PROJECT"))
        p.add_argument("--pipeline", default=os.environ.get("ZUUL_PIPELINE"))
        p.add_argument("--testing-repo")
        return p

    def get_srpms(self, url):
        match = re.findall(r'<a href="([^"]*)">', requests.get(url).text)
        if not match:
            raise RuntimeError("Couldn't find srpms in %s..." % url)
        srpms = []
        for m in match:
            if m.endswith(".src.rpm"):
                srpms.append(m)
        return srpms

    def get_package_name(self, project):
        oproject = None
        if project.endswith("-distgit"):
            oproject = project
            project = project[:-8]
        for package in self.distro_info["packages"]:
            if project == package["name"]:
                return os.path.basename(project)
            if oproject == package.get("distgit", ""):
                return os.path.basename(project)
        raise RuntimeError("Couln't find project %s in distro info" % project)

    def download(self, url):
        local_filename = url.split('/')[-1]
        self.log.info("Downloading %s" % url)
        r = requests.get(url, stream=True)
        with open(local_filename, 'wb') as f:
            for chunk in r.iter_content(chunk_size=1024):
                if chunk:
                    f.write(chunk)

    def main(self, args):
        package_name = self.get_package_name(args.project)

        if args.pipeline == "tag":
            # srpm has been built locally
            srpms = glob.glob("zuul-rpm-build/*.src.rpm")
        else:
            srpms = self.get_srpms(args.testing_repo)
            srpms = filter(lambda x: re.match("^%s-\d.*" % package_name, x),
                           srpms)

        if len(srpms) != 1:
            raise RuntimeError("Multiple package available... %s" % srpms)
        srpm = srpms[0]

        if not os.path.isfile(srpm):
            srpm_url = "%s/%s" % (args.testing_repo, srpm)
            self.download(srpm_url)

        self.execute(["koji", "--authtype=ssl", "add-pkg", "--owner=sfci",
                      self.distro_info["koji-target"], package_name])
        self.execute(["koji", "--authtype=ssl", "build", "--noprogress",
                      "--wait",
                      self.distro_info["koji-target"], srpm])


if __name__ == "__main__":
    ZuulRpmPublish()
