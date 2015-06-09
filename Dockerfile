FROM haskell:7.10

RUN cabal update
RUN cabal install vty==5.2.9 lens random

ADD . /terminal-invaders
WORKDIR /terminal-invaders
CMD /bin/bash -c "cd src && runhaskell Main.hs"
