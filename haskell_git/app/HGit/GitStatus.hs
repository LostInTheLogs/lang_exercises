module HGit.GitStatus () where

{-

Changes to be committed:
  (use "git restore --staged <file>..." to unstage)

  git diff-index --cached HEAD | diff between index and head tree

Changes not staged for commit:
  (use "git add <file>..." to update what will be committed)
  (use "git restore <file>..." to discard changes in working directory)

  git ls-files -m | modified index files

Untracked files:
  (use "git add <file>..." to include in what will be committed)

-}

--
