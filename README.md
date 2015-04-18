[![Build Status](https://travis-ci.org/futurice/fum2github.svg?branch=master)](https://travis-ci.org/futurice/fum2github)

# Compare Futurice FUM and GitHub Organization users

## Setup
```bash
cabal sandbox init
cabal update
cabal install --enable-tests
cabal test

.cabal-sandbox/bin/fum2github http://api.fum.futurice.com/users/ «auth-token»
```

## Copyright

Copyright © [Futurice](https://futurice/com),
published under the BSD 3-clause license.
The `LICENSE` file contains a copy of the license.
