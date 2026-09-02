# Notes

todo: comprehensive list of features and missing features or limitations

## Limitations

- No SHA-256 support
- No symlinks support
- No submodules support
- Doesn't support blobs larger than ram
- No gc, everything is written to the loose obj storage

## Supported features

### Commands

- init
- status
- add
- commit
- reset
- switch
- log

### Plumbing commands

### Object storage

- loose objects: read, write
- pack.pack (v2): read
- pack.idx (v2): read, ~~write (for `git index-pack` and `git fetch`)~~
