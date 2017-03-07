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
import logging
import os
import re
import requests
import subprocess
import sys
import yaml


class ZuulRpmPublish:
    log = logging.getLogger('zuulrpm.publish')

    def __init__(self):
        self.distro_info = None

    def setup_logging(self, verbose=False):
        if verbose:
            logging.basicConfig(level=logging.DEBUG)
        else:
            logging.basicConfig(level=logging.INFO)

    def parse_arguments(self, args):
        p = argparse.ArgumentParser(description='Zuul RPM publish')
        p.add_argument("--project", default=os.environ.get("ZUUL_PROJECT"))
        p.add_argument("--testing-repo")
        p.add_argument("--distro-info", default="distro.yaml",
                       help="The yaml distro info file")
        p.add_argument('--verbose', action='store_true', help='verbose output')
        self.args = p.parse_args()

    def load_distro_info(self, path):
        if not os.path.isfile(path):
            raise RuntimeError()
        self.distro_info = yaml.safe_load(file(path))

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
        r = requests.get(url, stream=True)
        with open(local_filename, 'wb') as f:
            for chunk in r.iter_content(chunk_size=1024):
                if chunk:
                    f.write(chunk)

    def execute(self, argv):
        self.log.debug("Running %s" % argv)
        s = subprocess.Popen(argv)
        if s.wait():
            self.log.error("Command %s failed" % argv)
            raise RuntimeError()

    def main(self, args=sys.argv[1:]):
        self.parse_arguments(args)
        # Force debug during test phase
        self.args.verbose = True
        self.setup_logging(verbose=self.args.verbose)
        self.load_distro_info(self.args.distro_info)

        package_name = self.get_package_name(self.args.project)

        srpms = self.get_srpms(self.args.testing_repo)

        srpms = filter(lambda x: re.match("^%s-\d.*" % package_name, x), srpms)
        if len(srpms) != 1:
            raise RuntimeError("Multiple package available... %s" % srpms)
        srpm = srpms[0]
        srpm_url = "%s/%s" % (self.args.testing_repo, srpm)
        self.log.info("Going to submit %s" % srpm_url)
        self.download(srpm_url)

        self.execute(["koji", "--authtype=ssl", "add-pkg", "--owner=sfci",
                      self.distro_info["koji-target"], package_name])
        self.execute(["koji", "--authtype=ssl", "build", "--noprogress",
                      "--wait",
                      self.distro_info["koji-target"], srpm])


if __name__ == "__main__":
    ZuulRpmPublish().main()
