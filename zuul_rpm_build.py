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
import os
import re
import sys
import shlex
import glob
import shutil

from jinja2 import FileSystemLoader
from jinja2.environment import Environment

import zuul_koji_lib


class ZuulRpmBuild(zuul_koji_lib.App):
    def usage(self):
        p = argparse.ArgumentParser(description='Zuul RPM builder')
        p.add_argument("--zuulv3", action="store_true",
                       help="temporary flag to ease transition")
        p.add_argument("--pipeline",
                       default=os.environ.get('ZUUL_PIPELINE', 'check'))
        p.add_argument("--project", action='append', default=[],
                       help="Specify project name when running outside"
                            " of Zuul context"),
        p.add_argument("--source",
                       default="https://softwarefactory-project.io/r/",
                       help="The git server to fetch projects from")
        p.add_argument("--output", default=os.environ.get('RPM_REPO'),
                       help="The repository to publish package")
        p.add_argument("--local_output", default="zuul-rpm-build",
                       help="The directory to locally store build")
        p.add_argument('--srpm', action='store_true', help='Only build srpm')
        p.add_argument('--noclean', dest='clean', default=True,
                       action='store_false', help="don't clean between build")
        return p

    def render_mock_template(self):
        loader = FileSystemLoader(os.path.dirname(os.path.realpath(__file__)))
        env = Environment(trim_blocks=True, loader=loader)
        template = env.get_template("mock.cfg.j2")
        return template.render(self.distro_info)

    def update_spec_file(self, specfile, source_version):
        self.log.info("Update specfile %s" % specfile)
        # Update spec file when source is internal
        spec = open(specfile).read()
        # Change Source0 with the pre-created local sdist HEAD.tgz
        spec = re.sub(r"Source0:.*", r"Source0: HEAD.tgz", spec)
        # Update version with source version info
        spec = re.sub(r"Version:.*", r"Version: %s" % source_version, spec)
        open(specfile, "w").write(spec)

    def build_sdist(self, project, source_version):
        self.log.info("Generate sdist for %s" % project)
        dir_name = "%s-%s" % (os.path.basename(project), source_version)
        if os.path.exists(dir_name):
            os.unlink(dir_name)
        os.symlink(project, dir_name)
        sdist_cmd = "tar -cz --exclude .git -f HEAD.tgz -h %s" % dir_name
        self.execute(shlex.split(sdist_cmd))

    def check_spec(self, specfile):
        # Check specfile name == project name
        project_name = os.path.basename(os.path.dirname(specfile)) \
            .replace('-distgit', '')
        spec_name = os.path.basename(specfile).replace('.spec', '')
        if project_name != spec_name:
            self.log.error("File doesn't match project's name: %s != %s" % (
                project_name, spec_name))

        # Check name == project_name
        spec = self.execute(["rpmspec", "-P", specfile], capture=True)
        for line in spec.split('\n'):
            if line.startswith("Name:"):
                name = line.split()[1]
                if specfile.startswith("rpms/python"):
                    name = name.replace("python3-", "python-")
                if project_name == name:
                    return True
                self.log.error("Spec Name doesn't match project's name: "
                               "%s != %s" % (project_name, name))
                return False

        self.log.error("Couldn't find Name in spec")
        return False

    def build_srpm(self, distgit, source_version=None):
        # Fetch external source using spectool and create src.rpm
        specs = filter(lambda x: x.endswith(".spec"), os.listdir(distgit))
        if len(specs) > 1:
            self.log.error("%s: Incorrect number of .spec files: %s" %
                           (distgit, specs))
            exit(1)
        if not specs:
            self.log.warning("%s: Unable to find a spec file" % distgit)
            return False
        specfile = "%s/%s" % (distgit, specs[0])

        if not self.check_spec(specfile):
            self.log.error("%s: Something wrong with specfile" % specfile)
            exit(1)

        if source_version is not None:
            self.update_spec_file(specfile, source_version)

        self.log.info("%s: Fetching sources" % distgit)
        self.execute(["spectool", "-g", "-C", distgit, specfile])

        self.log.info("%s: Building SRPM" % distgit)
        self.create_mock_config()

        self.execute(["mock", "--buildsrpm",
                      "--resultdir", self.args.local_output,
                      "--spec", specfile, "--sources", distgit] +
                     self.mock_macros + self.mock_argument)
        return True

    def check_postinstall_failed(self, project):
        pattern = "^WARNING .* Failed install built packages$"
        rootlog = open(os.path.join(self.args.local_output, 'root.log')).read()
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
                         self.mock_macros + self.mock_argument)
            self.built_srpms.add(srpm)

    def create_mock_config(self):
        mockconf = os.path.expanduser('~/.mock/user.cfg')
        self.log.debug("Installing mock configuration in %s" % mockconf)
        built_release = {
            "name": "built-release",
            "baseurl": self.distro_info["koji-url"],
            "enabled": 1,
            "gpgcheck": 0,
        }
        if not [r for r in self.distro_info['repos']
                if r['name'] == "built-release"]:
            self.distro_info['repos'].append(built_release)
        if os.path.isfile("%s/repodata/repomd.xml" % self.args.local_output):
            self.log.debug("Append local-build to mock configuration in %s" %
                           mockconf)
            local_build = {
                "name": "local-build",
                "baseurl": "file://%s/zuul-rpm-build/" % os.getcwd(),
                "enabled": 1,
                "gpgcheck": 0,
            }
            if not [r for r in self.distro_info['repos']
                    if r['name'] == "local-build"]:
                self.distro_info['repos'].append(local_build)
        rendered = self.render_mock_template()
        if not os.path.isdir(os.path.dirname(mockconf)):
            os.mkdir(os.path.dirname(mockconf))
        open(mockconf, 'w').write(rendered)

    def get_package_info(self, project, fallback=None):
        package_info = [pi for pi in self.distro_info['packages']
                        if pi['name'] == project]
        if not package_info and fallback:
            package_info = [pi for pi in self.distro_info['packages']
                            if pi.get('distgit', '') == fallback]
        if not package_info:
            raise IndexError
        return package_info[0]

    def build(self, project):
        o_project = None
        if project.endswith("-distgit"):
            o_project = project
            project = project.replace('-distgit', '')

        if project.startswith("rpms/"):
            project = project.strip('/')
            package_info = {
                'source': 'external',
                'distgit': project,
                'name': project
            }
        else:
            try:
                package_info = self.get_package_info(project,
                                                     fallback=o_project)
            except IndexError:
                self.log.warning("%s: project not found in info yaml" %
                                 project)
                return False

        project = package_info["name"]
        distgit = package_info["distgit"]

        # Fetch the distgit repository
        if not os.path.isdir(distgit):
            if not os.path.isdir(os.path.dirname(distgit)):
                os.makedirs(os.path.dirname(distgit))
            self.execute(["git", "clone",
                          os.path.join(self.args.source, distgit),
                          distgit])

        if package_info.get("source") == "internal":
            if not os.path.isdir(project):
                if not os.path.isdir(os.path.dirname(project)):
                    os.makedirs(os.path.dirname(project))
                self.execute(["git", "clone",
                              os.path.join(self.args.source, project),
                              project])

            # Discover version number
            version = self.get_repo_version(project)
            self.log.info("%s: Detected version %s" % (project, version))

            # Generate a local source tarball
            self.build_sdist(project, version)

            # Move tarball to distgit directory
            self.execute(["mv", "HEAD.tgz", distgit])
        else:
            # When source is external, use version's numbers from spec file
            version = None

        # TODO: use distro-info file for the dist value
        self.mock_macros = ["-D", "dist .el7"]
        if package_info.get("scl"):
            self.mock_macros.extend(["-D", "scl %s" % package_info["scl"]])

        try:
            if not self.build_srpm(distgit, version):
                return False
            if self.args.srpm:
                return True
            self.build_rpm()
            if self.args.clean:
                self.check_postinstall_failed(project)
        except RuntimeError:
            self.log.warning("Gathering logs...")
            for log in ["root.log", "build.log"]:
                logfile = "%s/%s" % (self.args.local_output, log)
                if not os.path.exists(logfile):
                    continue
                # glob.glob("%s/*.log" % self.args.local_output):
                self.log.warning("\n\n===== %s =====\n%s" % (
                                 log, open(logfile).read()))
            raise
        return True

    def main(self, args):
        self.args = args
        self.built_srpms = set()
        self.mock_argument = []
        if not self.args.verbose:
            self.mock_argument.append("-q")
        if not self.args.clean:
            self.mock_argument.extend(["--no-clean", "--no-cleanup-after"])

        if not self.args.project:
            self.log.error("No changes or project defined, stopping now")
            exit(1)

        if os.path.isdir(self.args.local_output):
            if self.args.clean:
                shutil.rmtree(self.args.local_output)
                os.mkdir(self.args.local_output, 0o755)
            else:
                self.log.info("Skipping cleaning of intermediary repo: %s" %
                              self.args.local_output)
        else:
            os.mkdir(self.args.local_output, 0o755)

        # Clean logs
        for logfile in glob.glob("%s/*.log" % self.args.local_output):
            os.unlink(logfile)

        # For each change, build package and create intermediary repo
        for change in self.args.project:
            project, change_branch, ref = change.split(':')
            if change_branch != self.distro_info["branch"]:
                self.log.warning("Skipping %s because not on branch %s" % (
                                 change, self.distro_info["branch"]))
                continue
            try:
                if project.endswith("patternfly-react-ui-deps-distgit"):
                    self.log.info(
                        "Injecting patternfly-react-ui depends workaound for"
                        "%s %s", project, ref)
                    # Special case for zuul ui to re-use the src.rpm:
                    buildset_url = zuul_koji_lib.get_buildset_url(project, ref)
                    if buildset_url is None:
                        self.log.warning("Buildset is missing")
                        continue
                    self.log.info("Using this buildset %s", buildset_url)
                    srpms = zuul_koji_lib.get_srpms(buildset_url)
                    self.log.info("And this srpm %s", str(srpms))
                    srpm = zuul_koji_lib.download(
                        self.log, buildset_url + srpms[0])
                    self.execute(["mv", srpm, self.args.local_output])
                    self.mock_macros = ["-D", "dist .el7"]
                    self.build_rpm()
                    self.execute(["createrepo", "."],
                                 cwd=self.args.local_output)
                elif self.build(project):
                    self.execute(["createrepo", "."],
                                 cwd=self.args.local_output)
            except RuntimeError:
                raise
                # self.log.error(e)
                sys.exit(1)


if __name__ == "__main__":
    ZuulRpmBuild()
