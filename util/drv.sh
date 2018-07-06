#!/bin/bash
#
# Usage: drv.sh <screen file> <scene.file> <output file>
#

if [ $# -ne 3 ]; then
  echo "Usage: drv.sh <screen file> <scene.file> <output file>" 1>&2
  exit 1
fi

dist/build/pm/pm $1 $2 | dist/build/rt/rt $1 $2 | convert - $3

