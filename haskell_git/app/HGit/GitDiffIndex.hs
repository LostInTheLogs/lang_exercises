module HGit.GitDiffIndex () where

{-
Case 1: git diff-index --cached <tree> (Tree vs. Index)
When --cached is passed, Git completely ignores all stat fields in both entries.
It only compares the two things that both Trees and Index entries share:
    File Path
    Blob SHA-1 Hash (and Mode)
    If Path A exists in Tree with Hash X, and in Index with Hash X → Unchanged
    If Path A exists in Tree with Hash X, and in Index with Hash Y → Modified (M)
    If Path A exists in Index but not in Tree → Added (A)
    If Path A exists in Tree but not in Index → Deleted (D)

Missing stat data in the Tree-derived entry is never checked or needed.
Case 2: git diff-index <tree> (Tree vs. Working Directory on disk)
When checking if a working directory file matches a Tree entry:
    Git looks at the real .git/index file on disk first to see if it has valid cached stat() data for that file path.
    If the file on disk matches the stat() metadata cached in .git/index, Git knows the disk file's hash equals the Index's hash without reading the disk file.
    Git then compares that known hash directly against the Tree entry's hash.
    If the disk stat() doesn't match .git/index (or the file isn't in .git/index), Git reads the physical file from disk, hashes it, and compares that computed hash to the Tree entry's SHA-1.
-}
