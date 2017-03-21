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


class ZuulKojiSetTarget(zuul_koji_lib.App):
    def usage(self):
        return argparse.ArgumentParser(description='Zuul Koji Set Target')

    def koji_list_external_repos(self):
        out = self.execute(['koji', 'list-external-repos'], True)
        self.defined_external_repos = [line.split() for line in
                                       out.splitlines()[2:]]
        self.log.debug("Already defined external repos: %s" % (
                       self.defined_external_repos))

    def koji_list_targets(self):
        out = self.execute(['koji', 'list-targets'], True)
        self.defined_targets = [line.split()[0] for line in
                                out.splitlines()[2:]]
        self.log.debug("Already defined targetss: %s" % (
                       self.defined_targets))

    def koji_list_tags(self):
        out = self.execute(['koji', 'list-tags'], True)
        self.defined_tags = out.splitlines()

    def koji_list_tag_groups(self, tag):
        out = self.execute(['koji', 'list-groups', tag], True)
        self.defined_tag_groups = [
            line.split()[0] for line in
            out.splitlines() if not line.startswith(' ')]
        self.log.debug("Already defined tag groups: %s" % (
                       self.defined_tag_groups))

    def define_external_repo(self):
        self.koji_list_external_repos()
        repos = self.distro_info.get('repos', [])
        for repo in repos:
            if repo['name'] in [defined[0] for defined in
                                self.defined_external_repos]:
                self.log.warning("External repo %s already defined" % (
                                 repo['name']))
            else:
                self.log.info("Define external repo %s to %s" % (
                              repo['name'], repo['baseurl']))
                self.execute(['koji', 'add-external-repo',
                             repo['name'], repo['baseurl']])

    def add_external_repos(self, tag):
        repos = self.distro_info.get('repos', [])
        for repo in repos:
            self.log.info("Set %s as external repo for %s" % (
                          repo['name'], tag))
            self.execute(['koji', 'remove-external-repo', repo['name'], tag])
            self.execute(['koji', 'add-external-repo', '-t', tag,
                          '-p', str(repo['kojipriority']), repo['name']])

    def create_target(self, build_target, target):
        self.koji_list_targets()
        if target in self.defined_targets:
            self.log.warning("Target %s already defined" % target)
        else:
            self.log.info("Create the target %s that build in %s" % (
                          build_target, target))
            self.execute(['koji', 'add-target', target, build_target])

    def create_pkg_groups(self, build_target):
        chrootpackages = self.distro_info.get('chrootpackages', "")
        self.koji_list_tag_groups(build_target)
        for group in ('build', 'srpm-build'):
            if group in self.defined_tag_groups:
                self.log.warning("Tag group %s already exists" % group)
            else:
                self.log.info("Create pkg group %s in tag %s" % (
                              group, build_target))
                self.execute(['koji', 'add-group', build_target, group])
                cmd = ['koji', 'add-group-pkg', build_target, group]
                cmd.extend(chrootpackages.split())
                self.execute(cmd)

    def regen_repo(self, build_target):
        self.log.info('Regenerating repo for %s ...' % build_target)
        self.execute(['koji', 'regen-repo', build_target])

    def create_distro_build_target(self):
        target = self.distro_info['koji-target']
        build_target = target + '-build'
        candidate = target + '-candidate'
        release = target + '-release'
        arch = self.distro_info['arch']
        self.koji_list_tags()
        self.log.info("Create tag %s" % target)
        for _target in (target, build_target, candidate, release):
            if _target in self.defined_tags:
                self.log.warning("Tag %s already defined" % _target)
            else:
                self.execute(['koji', 'add-tag', _target])
        # Set the -build tag parent
        self.log.info("Set %s as parent for %s" % (
                      target, build_target))
        self.execute(['koji', 'add-tag-inheritance', '--force=FORCE',
                      build_target, target])
        # Set arch to the -build tag
        self.execute(['koji', 'edit-tag', '--arches', arch, build_target])
        # Add external repos to the tag
        self.add_external_repos(build_target)
        # Define the build target
        self.create_target(build_target, target)
        # Create and set pkg groups
        self.create_pkg_groups(build_target)
        return build_target

    def main(self, args):
        self.define_external_repo()
        build_target = self.create_distro_build_target()
        self.regen_repo(build_target)


if __name__ == "__main__":
    ZuulKojiSetTarget()
