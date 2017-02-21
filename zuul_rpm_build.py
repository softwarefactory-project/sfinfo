#!/bin/env python
#
# Copyright 2016 Red Hat
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
import subprocess
import sys
import yaml
import shlex
import glob
import shutil

from jinja2 import FileSystemLoader
from jinja2.environment import Environment


class ZuulRpmBuild:
    log = logging.getLogger('zuulrpm.builder')

    def __init__(self):
        # The list of package already built
        self.built_srpms = set()
        self.distro_info = None

    def execute(self, argv, capture=False, cwd=None):
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
            self.log.error("Command %s failed" % argv)
            raise RuntimeError()
        return out

    def parse_arguments(self, args=sys.argv[1:]):
        p = argparse.ArgumentParser(description='Zuul RPM builder')
        p.add_argument("--pipeline",
                       default=os.environ.get('ZUUL_PIPELINE', 'check'))
        p.add_argument("--changes",
                       default=os.environ.get('ZUUL_CHANGES', ''))
        p.add_argument("--project", action='append',
                       help="Specify project name when running outside"
                            " of Zuul context"),
        p.add_argument("--source", default=os.environ.get('GIT_SERVER'),
                       help="The git server to fetch projects from")
        p.add_argument("--output", default=os.environ.get('RPM_REPO'),
                       help="The repository to publish package")
        p.add_argument("--local_output", default="zuul-rpm-build",
                       help="The directory to locally store build")
        p.add_argument("--distro-info", default="distro.yaml",
                       help="The yaml distro info file")
        p.add_argument('--verbose', action='store_true', help='verbose output')
        p.add_argument('--noclean', dest='clean', default=True,
                       action='store_false', help="don't clean between build")

        self.args = p.parse_args()

    def setup_logging(self, verbose=False):
        if verbose:
            logging.basicConfig(level=logging.DEBUG)
        else:
            logging.basicConfig(level=logging.INFO)

    def render_mock_template(self):
        loader = FileSystemLoader(os.path.dirname(os.path.realpath(__file__)))
        env = Environment(trim_blocks=True, loader=loader)
        template = env.get_template("mock.cfg.j2")
        return template.render(self.distro_info)

    def load_distro_info(self, path):
        if not os.path.isfile(path):
            raise RuntimeError()
        self.distro_info = yaml.safe_load(file(path))

    def update_spec_file(self, specfile, source_version, release_version):
        self.log.info("Update specfile %s" % specfile)
        # Update spec file when source is internal
        spec = open(specfile).read()
        # Change Source0 with the pre-created local sdist HEAD.tgz
        spec = re.sub(r"Source0:.*", r"Source0: HEAD.tgz", spec)
        # Update version with local distgit and source version info
        if source_version:
            spec = re.sub(r"Version:.*", r"Version: %s" % source_version, spec)
        if release_version:
            spec = re.sub(r"Release:.*",
                          r"Release: %s%%{?dist}" % release_version, spec)
        open(specfile, "w").write(spec)

    def build_sdist(self, project, package_info, source_version):
        self.log.info("Generate sdist for %s" % project)
        if ("sdist-build" in package_info and
                "sdist-output-filter" in package_info):
            self.log.debug("%s: Building sdist using custom command" % project)
            sdist_cmd = package_info["sdist-build"]
            sdist_output_filter = package_info["sdist-output-filter"]
            self.execute(shlex.split(sdist_cmd), cwd=project)
            self.execute([
                "mv",
                glob.glob(os.path.join(project, sdist_output_filter))[0],
                'HEAD.tgz'])
        else:
            # When no sdist available, archive the whole project directory
            self.log.debug("%s: Building sdist with tar" % project)
            dir_name = "%s-%s" % (os.path.basename(project), source_version)
            if os.path.exists(dir_name):
                os.unlink(dir_name)
            os.symlink(project, dir_name)
            sdist_cmd = "tar -cz --exclude .git -f HEAD.tgz -h %s" % (
                dir_name)
            self.execute(shlex.split(sdist_cmd))

    def build_srpm(self, distgit, source_version=None, release_version=None):
        # Fetch external source using spectool and create src.rpm
        specs = filter(lambda x: x.endswith(".spec"), os.listdir(distgit))
        if len(specs) != 1:
            self.log.error("%s: Incorrect number of .spec files: %s" %
                           (distgit, specs))
            exit(1)
        specfile = "%s/%s" % (distgit, specs[0])

        if source_version and release_version:
            self.update_spec_file(specfile, source_version, release_version)

        self.log.info("%s: Fetching sources" % distgit)
        self.execute(["spectool", "-g", "-C", distgit, specfile])

        self.log.info("%s: Building SRPM" % distgit)
        self.create_mock_config()
        self.execute(["mock", "--buildsrpm",
                      "--resultdir", self.args.local_output,
                      "--spec", specfile, "--sources", distgit] +
                     self.mock_argument)

    def check_postinstall_failed(self, project):
        pattern = "^WARNING .* Failed install built packages$"
        rootlog = file(os.path.join(self.args.local_output, 'root.log')).read()
        if re.findall(pattern, rootlog, re.MULTILINE):
            raise RuntimeError("%s: built packages failed to be installed" %
                               project)

    def build_rpm(self):
        # Build all new .src.rpm files
        for srpm in filter(lambda x: x.endswith('.src.rpm'),
                           os.listdir(self.args.local_output)):
            if srpm in self.built_srpms:
                continue
            self.create_mock_config()
            if self.args.clean:
                self.execute(["mock", "--clean"])
            self.log.info("%s: Building RPM" % srpm)
            self.execute(["mock", "--rebuild", "--postinstall",
                          "--resultdir", self.args.local_output,
                          "%s/%s" % (self.args.local_output, srpm)] +
                         self.mock_argument)
            self.built_srpms.add(srpm)

    def _get_release_version(self, repo):
        try:
            describe = "git describe --tags"
            release_version = self.execute(
                shlex.split(describe), capture=True, cwd=repo).strip()
        except RuntimeError:
            describe = "git rev-list --count HEAD"
            release_version = self.execute(
                shlex.split(describe), capture=True, cwd=repo).strip()
        release_version = release_version.replace('-', '.')
        return release_version

    def get_release_version(self, project, distgit):
        project_v = self._get_release_version(project)
        distgit_v = self._get_release_version(distgit)

        release_version = "0.source.%s.distgit.%s" % (
            project_v, distgit_v)

        self.log.info("%s: Detected release version: %s" % (
            project, release_version))
        return release_version

    def get_source_version(self, project, package_info):
        if "source-version" in package_info:
            source_version = self.execute(
                shlex.split(package_info['source-version']),
                capture=True, cwd=project).strip()
            self.log.info("%s: Detected source version: %s" % (
                project, source_version))
        else:
            source_version = "0.0"
        return source_version

    def create_mock_config(self):
        mockconf = os.path.expanduser('~/.mock/user.cfg')
        self.log.debug("Installing mock configuration in %s" % mockconf)
        built_release = {
            "name": "built-release",
            "baseurl": self.distro_info["koji-url"],
            "enabled": 1,
            "gpgcheck": 0,
        }
        if not [r for r in self.distro_info['baserepos']
                if r['name'] == "built-release"]:
            self.distro_info['baserepos'].append(built_release)
        if os.path.isfile("%s/repodata/repomd.xml" % self.args.local_output):
            self.log.debug("Append local-build to mock configuration in %s" %
                           mockconf)
            local_build = {
                "name": "local-build",
                "baseurl": "file://%s/zuul-rpm-build/" % os.getcwd(),
                "enabled": 1,
                "gpgcheck": 0,
            }
            if not [r for r in self.distro_info['baserepos']
                    if r['name'] == "local-build"]:
                self.distro_info['baserepos'].append(local_build)
        rendered = self.render_mock_template()
        if not os.path.isdir(os.path.dirname(mockconf)):
            os.mkdir(os.path.dirname(mockconf))
        file(mockconf, 'w').write(rendered)

    def get_package_info(self, project):
        package_info = [pi for pi in self.distro_info['packages']
                        if pi['name'] == project][0]
        return package_info

    def build(self, project):
        if project.endswith("-distgit"):
            project = project.replace('-distgit', '')

        try:
            package_info = self.get_package_info(project)
        except IndexError:
            self.log.warning("%s: project not found in info yaml" %
                             project)
            return False

        if package_info.get("spec") == "included":
            # Spec file is part of project source
            distgit = project
        else:
            distgit = "%s-distgit" % project

        # Fetch the distgit repository
        if not os.path.isdir(distgit):
            self.execute(["zuul-cloner", self.args.source, distgit])

        if package_info.get("source") == "internal":
            # Fetch repository with zuul-cloner
            if not os.path.isdir(project):
                self.execute(["zuul-cloner", self.args.source, project])

            # Discover version's numbers
            source_version = self.get_source_version(project, package_info)
            release_version = self.get_release_version(project, distgit)
            self.log.info("%s: Detected source %s release %s" % (
                          project, source_version, release_version))

            # Generate a local source tarball
            self.build_sdist(project, package_info, source_version)

            # Move tarball to distgit directory
            self.execute(["mv", "HEAD.tgz", distgit])
        else:
            # When source is external, use version's numbers from spec file
            source_version = None
            release_version = None

        try:
            self.build_srpm(distgit, source_version, release_version)
            self.build_rpm()
            self.check_postinstall_failed(project)
        except RuntimeError:
            self.log.warning("Gathering logs...")
            for log in ["root.log", "build.log"]:
                # glob.glob("%s/*.log" % self.args.local_output):
                self.log.warning("\n\n===== %s =====\n%s" % (
                                 log, open("%s/%s" % (
                                     self.args.local_output, log)).read()))
            raise
        return True

    def main(self):
        self.parse_arguments()
        self.mock_argument = []
        if not self.args.verbose:
            self.mock_argument.append("-q")
        if not self.args.clean:
            self.mock_argument.extend(["--no-clean", "--no-cleanup-after"])
        # Force debug during test phase
        self.args.verbose = True
        self.setup_logging(verbose=self.args.verbose)
        self.load_distro_info(self.args.distro_info)

        if not self.args.changes and not self.args.project:
            self.log.error("No changes or project defined, stopping now")
            exit(1)
        if not self.args.changes:
            changes = []
            for project in self.args.project:
                changes.append("%s:master:refs/HEAD" % project)
            self.args.changes = "^".join(changes)
        self.log.debug("Computed or detected ZUUL_CHANGES is %s" %
                       self.args.changes)

        if os.path.isdir(self.args.local_output):
            if self.args.clean:
                shutil.rmtree(self.args.local_output)
                os.mkdir(self.args.local_output, 0700)
            else:
                self.log.info("Skipping cleaning of intermediary repo: %s" %
                              self.args.local_output)
        else:
            os.mkdir(self.args.local_output, 0700)

        # Clean logs
        for logfile in glob.glob("%s/*.log" % self.args.local_output):
            os.unlink(logfile)

        # For each change, build package and create intermediary repo
        for change in self.args.changes.split('^'):
            project, branch, ref = change.split(':')
            try:
                if self.build(project):
                    self.execute(["createrepo", "."],
                                 cwd=self.args.local_output)
            except RuntimeError, e:
                raise
                self.log.error(e)
                sys.exit(1)


if __name__ == "__main__":
    ZuulRpmBuild().main()
