#! /bin/bash

R CMD BATCH toKnitr.R
pdflatex note2.tex
pdflatex note2.tex

rm *log
rm *out
rm *aux
rm *bbl
rm *blg
rm *cb
rm *cb2
rm *fff
rm *synctex.gz*
rm *ttt
rm *snm
rm *toc
rm *nav
rm *vrb
