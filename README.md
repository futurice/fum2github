[![Build Status](https://travis-ci.org/futurice/fum2github.svg?branch=master)](https://travis-ci.org/futurice/fum2github)

# Compare Futurice FUM and GitHub Organization users

## Setup
```bash
cabal sandbox init
cabal update
cabal install --enable-tests
cabal test

.cabal-sandbox/bin/fum2github
```


## Deploy
Install Ansible (e.g. in a Python virtual enviroment using `pip install`).

Deploy to the machine in `ansible/hosts`, passing your remote username.
The machine needs no prior setup, ansible will create the `fum2github` user
and install the system packages it needs.
Tested on Ubuntu 14.04 64-bit.
```bash
ansible-playbook ansible/playbook.yml -i ansible/hosts --ask-become-pass -v -u «remote-user»
```

## Copyright

Copyright © [Futurice](https://futurice/com),
published under the BSD 3-clause license.
The `LICENSE` file contains a copy of the license.
