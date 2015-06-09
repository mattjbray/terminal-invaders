# terminal-invaders
A Haskell experiment with Vty, Lens and State.

## Usage

### Nix

```
NIXPKGS_ALLOW_UNFREE=1 nix-shell
cabal run
```

### Docker

```
docker build -t terminal-invaders .
docker run -it --rm terminal-invaders
```
