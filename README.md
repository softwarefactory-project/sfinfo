# zuul koji toolbox

## On koji, to enable sf-ci to create package:
Add to /etc/koji-hub/hub.conf, [policy] section
  package_list = all :: allow
Then restart httpd

## Prepare runtime for zuul_rpm_build.py
sudo yum install mock rpm-build rpmdevtools createrepo python-jinja2 python-semantic_version python-requests
sudo rsync -a rpm-gpg/ /etc/pki/rpm-gpg/
sudo usermod -a -G mock $USER
sudo mkdir /var/lib/sf
sudo ln -s /home/$SUDO_USER/zuul-rpm-build /var/lib/sf/zuul-rpm-build
sudo sed -i 's/\(%dist .el7\).*/\1/' /etc/rpm/macros.dist

## Release process

Check create-release.org from the www.softwarefactory-project.io repository.

## Cleanup zuul config and resources

./tools/get_package_lists.py sf-*.yaml | ./tools/clean_zuul_config.py ./zuul.d/*
./tools/get_package_lists.py sf-*.yaml | ./tools/clean_sf_resources.py ../config/resources/scl.yaml


## Using haskell stack to propose spec update:

```ShellSession
$ sudo dnf copr enable -y petersen/stack2 && sudo dnf install -y stack && sudo stack upgrade
$ git clone https://softwarefactory-project.io/r/software-factory/bugzilla-haskell ..
$ stack install
$ stack exec sfinfo -- --help
Propose spec files update

Usage: sfinfo OUTDATED

Available options:
  -h,--help                Show this help text
  OUTDATED                 Pkgtreediff outdated list
```

Using the output of pkgtreediff, such as: (TODO: document howto generate this)

```
python3-CacheControl: 0.12.5-1.el7 -> 0.12.6-1.el7
python3-GitPython: 2.1.11-1.el7 -> 3.1.7-1.el7
python3-alembic: 0.9.2-1.el7 -> 1.4.2-1.el7
...
```
