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
import urllib2
import yaml


class ZuulRpmSetup:
    log = logging.getLogger('zuulrpm.setup')

    def __init__(self):
        self.distro_info = None

    def setup_logging(self, verbose=False):
        if verbose:
            logging.basicConfig(level=logging.DEBUG)
        else:
            logging.basicConfig(level=logging.INFO)

    def parse_arguments(self):
        p = argparse.ArgumentParser(description='Zuul RPM setup')
        p.add_argument("--testing-repo", help="The previously built repo")
        p.add_argument("--distro-info", default="distro.yaml",
                       help="The yaml distro info file")
        p.add_argument('--verbose', action='store_true', help='verbose output')

        self.args = p.parse_args()

    def load_distro_info(self, path):
        if not os.path.isfile(path):
            raise RuntimeError()
        self.distro_info = yaml.safe_load(file(path))

    def write_repo(self, of, conf):
        self.log.info("Adding repo %s" % conf['name'])
        of.write("""[%(name)s]
name=%(name)s
baseurl=%(baseurl)s
gpgkey=%(gpgkey)s
gpgcheck=%(gpgcheck)s

""" % conf)

    def check_repo(self, url):
        try:
            request = urllib2.Request("%s/repodata/repomd.xml" % url)
            request.get_method = lambda: 'HEAD'
            response = urllib2.urlopen(request)
            return response.info().getheader('Content-Length') > 0
        except:
            return False

    def main(self):
        self.parse_arguments()
        # Force debug during test phase
        self.args.verbose = True
        self.setup_logging(verbose=self.args.verbose)
        self.load_distro_info(self.args.distro_info)

        repo_file_path = "/etc/yum.repos.d/zuul-built.repo"
        self.log.info("Writing repo to %s" % repo_file_path)
        with open(repo_file_path, "w") as of:
            conf = {"name": "release",
                    "baseurl": self.distro_info['koji-url'],
                    "gpgkey": '',
                    "gpgcheck": 0}
            self.write_repo(of, conf)
            for extrepo in self.distro_info['baserepos']:
                self.write_repo(of, extrepo)
            if self.args.testing_repo:
                conf = {"name": "testing",
                        "baseurl": self.args.testing_repo,
                        "gpgkey": '',
                        "gpgcheck": 0}
                if self.check_repo(conf["baseurl"]):
                    self.write_repo(of, conf)
                else:
                    self.log.warning("Testing repo unavailable...")


if __name__ == "__main__":
    ZuulRpmSetup().main()
