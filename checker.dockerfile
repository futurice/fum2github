FROM phadej/ghc

COPY . /tmp/f2g-repo

WORKDIR /tmp/f2g-repo
RUN rm -f cabal.config \
  && wget https://www.stackage.org/lts/cabal.config
  && cabal update \
  && cabal sandbox init \
  && cabal install --only-dependencies --enable-tests \
  && cabal configure --enable-tests \
  && cabal build \
  && cabal test \
  && mkdir /fum2github \
  && cp dist/build/checker/checker /fum2github/

WORKDIR /
RUN rm -rf /tmp/f2g-repo

ENTRYPOINT ["/fum2github/checker"]
