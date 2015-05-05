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

Create a personal GitHub [access token](https://github.com/settings/tokens/new).

cabal run -- http://api.fum.futurice.com/users/ «fum-token» futurice «github-token»
dist/build/fum2github/fum2github http://api.fum.futurice.com/users/ «fum-token» futurice «github-token»
```

## Copyright

Copyright © [Futurice](https://futurice/com),
published under the BSD 3-clause license.
The `LICENSE` file contains a copy of the license.
