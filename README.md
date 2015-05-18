[![Build Status](https://travis-ci.org/futurice/fum2github.svg?branch=master)](https://travis-ci.org/futurice/fum2github)

# Compare Futurice FUM and GitHub Organization users

## Setup
```bash
cabal update
cabal sandbox init
cabal install --only-dependencies --enable-tests
cabal configure --enable-tests
cabal build
cabal test
```

Create a personal GitHub access token: https://github.com/settings/tokens/new

```bash
cabal run -- https://api.fum.futurice.com/users/ «fum-token» futurice «github-token»
dist/build/fum2github/fum2github https://api.fum.futurice.com/users/ «fum-token» futurice «github-token»
```


## Deploy
Install Ansible (e.g. in a Python virtual enviroment using `pip install`).

Set the FUM and GitHub tokens in `ansible/secrets/yml`:
```bash
cp ansible/secrets.yml.example ansible/secrets.yml
```

Deploy to the machine in `ansible/hosts`, passing your remote username.
The machine only needs Docker installed (because installation varies with Linux
distribution and user preference), everything else (e.g. creating the
`fum2github` user if it's not present) is done by ansible.
```bash
ansible-playbook ansible/playbook.yml -i ansible/hosts --ask-become-pass -v -u «remote-user»
```

## Copyright

Copyright © [Futurice](https://futurice/com),
published under the BSD 3-clause license.
The `LICENSE` file contains a copy of the license.
