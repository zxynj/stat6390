#! /bin/bash

rm .RData
rm .Rhistory

echo 'knitr::knit("note3.Rnw")' > toKnitr.R

R CMD BATCH toKnitr.R
pdflatex note3.tex
bibtex note3.aux
pdflatex note3.tex
pdflatex note3.tex
rm note3.tex

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
rm *~
rm .RData
rm toKnitr.R
