#! /bin/bash

rm .RData
rm .Rhistory

echo 'knitr::knit("note2.Rnw")' > toKnitr.R

R CMD BATCH toKnitr.R
pdflatex note2.tex
bibtex note2.aux
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
rm .RData
rm toKnitr.R
