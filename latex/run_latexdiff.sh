#!/usr/bin/env bash
if [ $# -lt 1 ]; then
    error "Must provide revision SHA"
fi
orig_dir=`pwd`
cd `dirname $0`
mkdir -p changes
git latexdiff $1 --main NineMLSpec.tex --tmpdirprefix `pwd`/changes --exclude-textcmd="section,subsection,subsubsection,subsubsubsection,part" --exclude-safecmd="ref"
cd $orig_dir
