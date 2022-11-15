#!/bin/sh

# Script to facilitate migration to a monorepo while keeping the git history
# Cribbed from https://medium.com/@TimHolzherr/how-to-move-your-git-repository-into-a-monorepo-without-loosing-its-history-9b9d2da27155

repoToMove=$1
targetDir=$2

cd ~/git/$repoToMove
git format-patch --no-stat --stdout --root HEAD > ~/patch


# Adjust the folder structure so that all files are now under apps/myApp
sed -ri 's|^([-+]{3} [ab]\/)(\S+)$|\1'$targetDir'/\2|g' ~/patch
sed -ri 's|^diff --git a\/((\S)+) b\/((\S)+)$|diff --git a/'$targetDir'/\1 b/'$targetDir'/\3|g' ~/patch
sed -ri 's@^(\s*rename )((from)|(to))\s(\S+)$@\1\2 '$targetDir'/\5@gm' ~/patch

# Apply the patch in the monorepo
cd ~/git/go-recordkeeper
git am --committer-date-is-author-date ~/patch
