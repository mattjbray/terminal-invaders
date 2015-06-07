FROM haskell:7.10

RUN cabal update
RUN cabal install vty==5.2.9 lens
