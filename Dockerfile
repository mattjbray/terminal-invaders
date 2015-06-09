FROM haskell:7.10

WORKDIR /terminal-invaders
ADD ./terminal-invaders.cabal ./
RUN mkdir -p /root/.cabal/logs
RUN cabal update && cabal install -j --only-dependencies
ADD . ./
RUN cabal install -j
CMD ./dist/build/terminal-invaders/terminal-invaders
