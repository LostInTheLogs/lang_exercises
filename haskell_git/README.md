# Notes

todo: comment all the functions with a -- |
todo: comprehensive list of features and missing features or limitations

## Limitations

- No reflog support
- No tags and packed-refs support
- No SHA-256 support
- No symlinks support
- No submodules support
- Doesn't support blobs larger than ram
- No gc

## Supported features

### Commands

- init
- fetch (smart git wire v1 via http, capabilities: multi_ack, multi_ack_detailed)
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
- pack.idx (v2): read, write (for `git fetch`)
