FROM phadej/ghc

COPY . /tmp/f2g-repo

WORKDIR /tmp/f2g-repo
RUN cabal update \
  && cabal sandbox init \
  && cabal install --only-dependencies --enable-tests \
  && cabal configure --enable-tests \
  && cabal build \
  && cabal test \
  && cp dist/build/fum2github/fum2github /bin/

WORKDIR /
RUN rm -rf /tmp/f2g-repo

ENTRYPOINT ["/bin/fum2github"]
