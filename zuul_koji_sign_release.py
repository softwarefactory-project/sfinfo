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

import os
import re
import shutil
import tempfile
import argparse
import zuul_koji_lib


class ZuulKojiSignRelease(zuul_koji_lib.App):
    def usage(self):
        return argparse.ArgumentParser(description='Zuul Koji Sign release')

    def discover_pkgs_to_sign(self):
        tag_content = self.execute(
            ["koji", "list-tagged", "--latest",
             "--quiet", "--inherit", "--sigs", self.release_tag], capture=True)
        pkgs = []
        already_sign = []
        for item in tag_content.splitlines():
            sitem = item.split()
            if len(sitem) == 2:
                # Already signed
                already_sign.append(sitem[1])
            elif len(sitem) == 1:
                # No signature
                pkgs.append(sitem[0])
            else:
                self.log.error("Not expected result for %s" % sitem)
                raise RuntimeError()
        self.to_sign = set(pkgs) - set(already_sign)
        self.to_sign = list(self.to_sign)
        self.log.info("%s packages will be signed" % len(self.to_sign))

    def retrieve_pkgs_to_sign(self):
        self.td = tempfile.mkdtemp()
        self.log.info("Temporary path is %s" % self.td)
        for f in self.to_sign:
            copied = False
            for bd in self.basedirs:
                path = bd + f + '.rpm'
                if os.path.isfile(path):
                    self.log.info("Copy %s in %s" % (path, self.td))
                    shutil.copy(path, self.td)
                    copied = True
            if not copied:
                self.log.error("Unable to copy %s in the temp directory" %
                               path)
                raise RuntimeError

    def write_nvr_list_to_sign(self):
        self.to_sign_txt = os.path.join(self.td, 'to_sign.txt')
        fd = file(self.to_sign_txt, 'w')
        for i in self.to_sign:
            pkg = re.sub('.el7.*$', '.el7', i)
            fd.write("%s\n" % pkg)
        fd.close()

    def main(self, args):
        self.release_tag = self.distro_info['koji-target'] + '-release'
        self.candidate_tag = self.distro_info['koji-target'] + '-candidate'
        self.basedirs = [
            '/mnt/koji/repos/%s/source/SRPMS/' % self.candidate_tag,
            '/mnt/koji/repos/%s/Mash/' % self.candidate_tag,
            '/mnt/koji/repos/%s/x86_64/debug/' % self.candidate_tag]
        self.log.info('Release tag to act on is %s' % self.release_tag)
        self.discover_pkgs_to_sign()
        if self.to_sign:
            self.retrieve_pkgs_to_sign()
            self.write_nvr_list_to_sign()
            print("Now run:")
            print("cd %s" % self.td)
            print("rpm --addsign *.rpm")
            print("koji import-sig *.rpm")
            print("""
for nvr in $(cat %s); do
    koji write-signed-rpm 1c3bae4b $nvr;
done""" % self.to_sign_txt)

if __name__ == "__main__":
    ZuulKojiSignRelease()
