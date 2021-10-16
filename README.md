# Acinetobase
### Compendium of Experiments in the Lab

[![Built with Nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)
![Deploy](https://github.com/vibbits/acinetobase-static/actions/workflows/main.yml/badge.svg)

<img src="https://raw.githubusercontent.com/vibbits/acinetobase-static/master/static/logo.png" align="right" alt="Acinetobase logo" width="200" height="200" />

Static site for the Acinetobacter baumannii database. Data is sourced from a table of metadata and a directory of
images. Database includes microscope images, density plots, and GenBank links.

### Building
There is a single build requirement: [Nix](https://nixos.org/). After installing `nix` you can build the generator with:

```bash
nix-build release.nix
```

You can then build the static site itself with:

```bash
./result/bin/build-site
```
