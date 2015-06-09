# terminal-invaders
A Haskell experiment with Vty, Lens and State.

## Usage

### Nix

```
nix-shell
cd src
runhaskell Main.hs
```

### Docker

```
docker build -t terminal-invaders .
docker run -it --rm -v $(pwd):/host_code --workdir /host_code terminal-invaders bash
cd src
runhaskell Main.hs
```

## TODO

cabal init
