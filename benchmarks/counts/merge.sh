#!/bin/bash

rm -rf loc-results.tex
for file in *.tex; do cat $file; echo ""; done > loc-results.tmp
mv loc-results.tmp loc-results.tex
