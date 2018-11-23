#!/bin/env python
#
# Copyright 2018 Red Hat
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

pip_to_rpm_map = {
    "ansible": "ansible",
    "Babel": "python-babel",
    "fb-re2": "python-re2",
    "gitdb2": "python-gitdb",
    "github3.py": "python-github3",
    "GitPython": "GitPython",
    "Jinja2": "python-jinja2",
    "Mako": "python-mako",
    "MakupSafe": "python-markupsafe",
    "msgpack-python": "python-msgpack",
    "pycrypto": "python-crypto",
    "PyJWT": "python-jwt",
    "PyNaCl": "python-pynacl",
    "PyYAML": "PyYAML",
    "smmap2": "python-smmap",
    "SQLAlchemy": "python-sqlalchemy",
    "diskimage-builder": "diskimage-builder",
    "Paste": "python-paste",
    "WebOb": "python-webob",
    "dogpile.cache": "python-dogpile-cache",
    "repoze.lru": "python-repoze-lru",
    "CherryPy": "python-cherrypy",
    "MarkupSafe": "python-markupsafe",
    "Routes": "python-routes",
    "backports.functools-lru-cache": "python-backports-functools-lru-cache",
}


def load_pip_file(f):
    pip_versions = {}
    for line in open(f).readlines():
        if '==' not in line:
            continue
        pip_name, pip_ver = line.split('==')
        pkg_name = pip_to_rpm_map.get(pip_name)
        if not pkg_name:
            if "python-" not in pip_name:
                pkg_name = "python-%s" % pip_name
            else:
                pkg_name = pip_name
        pip_versions[pkg_name] = pip_ver[:-1]
    return pip_versions


def get_rpm_version(pkg_name):
    import subprocess

    p = subprocess.Popen(["rpm", "-q", pkg_name], stdout=subprocess.PIPE)
    p.wait()
    rpmq = p.stdout.read().decode('utf-8')
    if "not installed" in rpmq:
        print("%s is not installed" % pkg_name)
        return
    try:
        return rpmq.split('-')[-2]
    except Exception:
        print("couldn't decode", rpmq)


def main():
    import argparse

    p = argparse.ArgumentParser()
    p.add_argument("--scl", default="rh-python35")
    p.add_argument("pip_freeze_file")
    args = p.parse_args()

    pip_versions = load_pip_file(args.pip_freeze_file)
    for name, ver in sorted(pip_versions.items()):
        pkg_name = "%s-%s" % (args.scl, name)
        pkg_ver = get_rpm_version(pkg_name)
        if pkg_ver and pkg_ver != ver:
            print("%s rpm version %s != %s (pip)" % (pkg_name, pkg_ver, ver))


if __name__ == "__main__":
    main()
