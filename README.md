# Interaction nets based processor in Clash

## Required tools

### Pre-commit

We use [pre-commit](https://pre-commit.com/) for general tidy up of files.
To install pre-commit run:

```shell
pip install pre-commit # or install using your distro package manager
pre-commit install
```

To run pre-commit on all files run

```shell
pre-commit run --all-files
```

### Fourmolu

We use [Fourmolu](https://fourmolu.github.io/) as a formatter for Haskell source files with our custom config.
**Fourmolu must be explicitly enabled in VS Code!**

## Editor

Our editor of choice is [VS Code](https://code.visualstudio.com/) with following extensions:

- [Haskell](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)
