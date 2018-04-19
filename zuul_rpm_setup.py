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
import urllib2

import zuul_koji_lib


class ZuulRpmSetup(zuul_koji_lib.App):
    def usage(self):
        p = argparse.ArgumentParser(description='Zuul RPM setup')
        p.add_argument("--testing-repo", help="The previously built repo")
        return p

    def write_repo(self, of, conf):
        self.log.info("Adding repo %s" % conf['name'])
        if "mirror" in conf:
            conf["repourl"] = "mirrorlist=%s" % conf["mirror"]
        elif "baseurl" in conf:
            conf["repourl"] = "baseurl=%s" % conf["baseurl"]
        print conf
        of.write("""[%(name)s]
name=%(name)s
%(repourl)s
gpgkey=%(gpgkey)s
gpgcheck=%(gpgcheck)s
priority=%(kojipriority)s

""" % conf)

    def check_repo(self, url):
        try:
            request = urllib2.Request("%s/repodata/repomd.xml" % url)
            request.get_method = lambda: 'HEAD'
            response = urllib2.urlopen(request)
            return response.info().getheader('Content-Length') > 0
        except:
            return False

    def main(self, args):
        repo_file_path = "/etc/yum.repos.d/zuul-built.repo"
        self.log.info("Writing repo to %s" % repo_file_path)
        with open(repo_file_path, "w") as of:
            conf = {"name": "sfmaster",
                    "baseurl": self.distro_info['koji-url'],
                    "gpgkey": '',
                    "gpgcheck": 0,
                    "kojipriority": 110}
            self.write_repo(of, conf)
            if self.distro_info['extrarepos']:
                for extrepo in self.distro_info['extrarepos']:
                    self.write_repo(of, extrepo)
            if args.testing_repo:
                conf = {"name": "sftesting",
                        "baseurl": args.testing_repo,
                        "gpgkey": '',
                        "gpgcheck": 0,
                        "kojipriority": 2}
                if self.check_repo(conf["baseurl"]):
                    self.write_repo(of, conf)
                else:
                    self.log.warning("Testing repo unavailable...")


if __name__ == "__main__":
    ZuulRpmSetup()
