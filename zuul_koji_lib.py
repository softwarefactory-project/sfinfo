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

import logging
import yaml
import subprocess
import sys
import os


class App:
    # App needs usage() and main() method

    @classmethod
    def _get_name(cls):
        return cls.__name__

    def execute(self, argv, capture=False, cwd=None, test=False):
        if capture is True:
            stdout = subprocess.PIPE
        else:
            stdout = None
        self.log.debug("Running %s" % argv)
        s = subprocess.Popen(argv, stdout=stdout, cwd=cwd)
        out, _ = s.communicate()
        code = s.wait()
        self.log.debug("Command %s exited with code %s" % (argv, code))
        if code:
            if not test:
                self.log.error("Command %s failed" % argv)
            raise RuntimeError()
        return out

    def get_repo_version(self, repo):
        ref = "HEAD"
        # Check if last change changed .gitreview
        view_ref = self.execute(["git", "log", "-n1", "--format='%H'",
                                 ".gitreview"], capture=True, cwd=repo).strip()
        head_ref = self.execute(["git", "log", "-n1", "--format='%H'"],
                                capture=True, cwd=repo).strip()
        if view_ref == head_ref:
            self.log.debug("Skipping .gitreview update")
            ref = "HEAD^"
        try:
            version = self.execute(["git", "describe", "--tags", ref],
                                   capture=True, cwd=repo, test=True).strip()
        except RuntimeError:
            version = self.execute(["git", "rev-list", "--count", ref],
                                   capture=True, cwd=repo, test=True).strip()
            # Make sure rev-list version is lower than first tag
            version = "0.0.0.0-dev%s" % version
        return version.replace('-', '.')

    def get_spec_field(self, specfile, field):
        spec = self.execute(["rpmspec", "-P", specfile], capture=True)
        for line in spec.split('\n'):
            if line.startswith("%s:" % field):
                return line.split()[1]
        raise RuntimeError("%s: couldn't find %s" % (specfile, field))

    def get_spec_version(self, specfile):
        return self.get_spec_field(specfile, "Version")

    def get_spec_release(self, specfile):
        return self.get_spec_field(specfile, "Release")

    def _load_distro_info(self, path):
        if not os.path.isfile(path):
            raise RuntimeError()
        self.distro_info = yaml.safe_load(file(path))
        if "inherit" in self.distro_info:
            loc = os.path.dirname(path)
            ipath = os.path.join(loc, self.distro_info["inherit"])
            if not os.path.isfile(ipath):
                raise RuntimeError("Inherited file not found in %s" % ipath)
            base_distro_info = yaml.safe_load(file(ipath))
            base_distro_info.update(self.distro_info)
            self.distro_info = base_distro_info
            self.log.debug("Found inheritance from distro %s" % ipath)
        # Make sure branch name is str
        self.distro_info["branch"] = str(self.distro_info["branch"])
        # Update package infos
        for package in self.distro_info["packages"]:
            if package.get("spec") == "included":
                package["distgit"] = package["name"]
            elif not package.get("distgit"):
                package["distgit"] = "%s-distgit" % package["name"]

    def _parse_arguments(self, args):
        # Parse command line
        p = self.usage()
        p.add_argument('--verbose', action='store_true', help='verbose output')
        p.add_argument("--distro-info", default='distro.yaml',
                       help="The yaml distro info file")
        return p.parse_args(args)

    def _setup_logging(self, verbose=False):
        self.log = logging.getLogger('zuulkoji.%s' % self._get_name())
        fmt = '\033[1;33m%(levelname)-5.5s [%(name)s] %(message)s\033[1;0m'
        if verbose:
            logging.basicConfig(format=fmt, level=logging.DEBUG)
        else:
            logging.basicConfig(format=fmt, level=logging.INFO)

    def __init__(self, args=sys.argv[1:]):
        args = self._parse_arguments(args)
        self._setup_logging(args.verbose)
        self._load_distro_info(args.distro_info)
        self.main(args)
