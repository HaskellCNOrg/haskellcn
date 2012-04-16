---
title: TEST BLOG: Objects created at one commit
author: Haisheng, Wu
tags: git
---

## Play it

### new two files
~~~~~~{.sh}
haiswu@haiswu-VirtualBox:~/github/haisheng-homepage/test$ echo "11111" > 1.txt
haiswu@haiswu-VirtualBox:~/github/haisheng-homepage/test$ echo "22222" > 2.txt
~~~~~~

### git-add

~~~~~~{.sh}
haiswu@haiswu-VirtualBox:~/github/haisheng-homepage/test$ git add .
haiswu@haiswu-VirtualBox:~/github/haisheng-homepage/test$ git st
# On branch master
#
# Initial commit
#
# Changes to be committed:
#   (use "git rm --cached <file>..." to unstage)
#
#	new file:   1.txt
#	new file:   2.txt
#
~~~~~~
  
### no objects created yet

~~~~~~{.sh}
haiswu@haiswu-VirtualBox:~/github/haisheng-homepage/test$ git rev-list --objects --all
~~~~~~

### git-commit

~~~~~~{.sh}
haiswu@haiswu-VirtualBox:~/github/haisheng-homepage/test$ git ci "first commit"
[master (root-commit) 8ef1919] first commit
 2 files changed, 2 insertions(+), 0 deletions(-)
 create mode 100644 1.txt
 create mode 100644 2.txt

~~~~~~

### git-obj-list

~~~~~~{.sh}

haiswu@haiswu-VirtualBox:~/github/haisheng-homepage/test$ git rev-list --objects --all
8ef1919e4ca33cf602488a27b5671a6b6acf754b
8ed6deeabbbb89cda775aafc10745ea40dcd8456 
f7c6dd0164fe0eb4fde767f9e731a6c8ade0b69f 1.txt
0ac1ae0ae201d8db7ac29015a6ba7494db37d59c 2.txt

~~~~~~

### what are they

~~~~~~{.sh}
## | Blob objects
~/github/haisheng-homepage/test$ git cat-file -p f7c6dd0164fe0eb4fde767f9e731a6c8ade0b69f
11111
~/github/haisheng-homepage/test$ git cat-file -p 0ac1ae0ae201d8db7ac29015a6ba7494db37d59c
22222

## | Commit objects
~/github/haisheng-homepage/test$ git cat-file -p 8ef1919e4ca33cf602488a27b5671a6b6acf754b
tree 8ed6deeabbbb89cda775aafc10745ea40dcd8456
author Haisheng.W.WU <freizl@gmail.com> 1334230087 +0800
committer Haisheng.W.WU <freizl@gmail.com> 1334230087 +0800

first commit

## | Tree objects
~/github/haisheng-homepage/test$ git cat-file -p 8ed6deeabbbb89cda775aafc10745ea40dcd8456
100644 blob f7c6dd0164fe0eb4fde767f9e731a6c8ade0b69f	1.txt
100644 blob 0ac1ae0ae201d8db7ac29015a6ba7494db37d59c	2.txt

~~~~~~

## Further
  - [Git object model]
  - [Git internal in one image]

[Git internal in one image]: (http://0.0.0.0:9900/posts/readings/2012-04-08-notes-progit.html#internal)
[Git object model]: (http://book.git-scm.com/)
