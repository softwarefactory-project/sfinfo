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

import zuul_koji_lib


def prettify(lines):
    return map(lambda s: "* " + s, sorted(lines))


class ZuulKojiCheckTarget(zuul_koji_lib.App):
    def usage(self):
        p = argparse.ArgumentParser(description='Zuul Koji Check Target')
        return p

    def main(self, args):
        repos = set(zuul_koji_lib.list_repos(self.distro_info))
        list_tag = zuul_koji_lib.list_tag(self.distro_info["koji-target"])
        tagged_repos = set(zuul_koji_lib.tag_to_packages(list_tag))
        tagged_nvr = list_tag[1]
        argv = ["koji", "untag-build", self.distro_info["koji-target"]] + [
            zuul_koji_lib.nvr_name(tagged_nvr[to_untag])
            for to_untag in sorted(tagged_repos.difference(repos))
        ]
        if len(argv) > 3:
            print("To untag:\n" + "\n".join(
                prettify(tagged_repos)))
            print("")
            print(" ".join(argv))
            zuul_koji_lib.execute(argv, log=self.log)
        to_tag = repos.difference(tagged_repos)
        if to_tag:
            print("To tag:\n" + "\n".join(
                prettify(to_tag)))


if __name__ == "__main__":
    ZuulKojiCheckTarget()
