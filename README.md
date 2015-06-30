# fum2github

> Compare Futurice FUM and GitHub Organization users

[![Build Status](https://travis-ci.org/futurice/fum2github.svg?branch=master)](https://travis-ci.org/futurice/fum2github)

## Structure

The server setup uses Docker containers.

The `checker` cron job compares FUM and GitHub users.
Its output is in a `data/` directory.

The `web` container serves `data/` through port 3001.
Ansible configures the server's Apache to forward requests for
`[dev.]fum2github.futurice.com` to the `web` container.

## Setup

Multipackage setup is handler with [stack](https://github.com/commercialhaskell/stack)

```bash
stack build
stack test
```

(Create a personal GitHub access token](https://github.com/settings/tokens/new)

```bash
stack exec checker https://api.fum.futurice.com/users/ «fum-token» futurice «github-token»
```

## Deploy

This clones the repository from GitHub (it does not use your local repository)
and deploys the `master` branch.
You can select a different one by passing `-e DEPLOY_BRANCH=mybranch`
to the `ansible-playbook` command.

Checkout the same `DEPLOY_BRANCH` locally when deploying, because it will use
some of the local files in `ansible/`.

Install Ansible (e.g. in a Python virtual enviroment using `pip install`).

The machine we are deploying to only needs Docker installed
(because installation varies with Linux distribution and user preference),
everything else (e.g. creating the `fum2github` user if it's not present)
is done by Ansible.

Set the FUM and GitHub tokens in `ansible/secrets/yml`:
```bash
cp ansible/secrets.yml.example ansible/secrets.yml
```

### Production

```bash
ansible-playbook ansible/playbook.yml -i ansible/production --ask-become-pass -v -u «remote-username»
```

### Local (e.g. for testing)

You can use the `Vagrantfile` to create a VM and deploy to it.
It assumes your public SSH key is in `~/.ssh/id_rsa.pub`
(it's copied into the VM user's `authorized_keys` file),
sets the RAM size and number of CPUs
(the cabal build is faster with more CPUs).
You may want to tweak these settings.

```bash
vagrant up
ansible-playbook ansible/playbook.yml -i ansible/local --ask-become-pass -v -u vagrant
```

Ansible will ask for the SUDO password which is `vagrant`.

Make `dev.fum2github.futurice.com` point to `127.0.0.1` e.g. in `/etc/hosts`.
`Vagrantfile` forwards local port 3000 to its internal port 80:

http://dev.fum2github.futurice.com:3000/

## Copyright

Copyright © [Futurice](https://futurice/com),
published under the BSD 3-clause license.
The `LICENSE` file contains a copy of the license.
