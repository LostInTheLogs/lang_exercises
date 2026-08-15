module HGit.GitDiffIndex () where

{-
doesn't compare files not in index

If Path exists in Index but not in Tree → Added (A)
If Path exists in Tree but not in Index → Deleted (D)
If Path exists in both (M):
    --cached : compare Hash from Index and Tree
    else: index entry unmodified do --cached else hash the file
-}

--
