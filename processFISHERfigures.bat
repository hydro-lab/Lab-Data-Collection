#! /bin/bash

Rscript fisher.R
Rscript fisherPlots.R

git add Fisher711_FisherMet.dat
git add FisherHall_download.csv
git add FisherHall.csv
git add FISHERprecip.jpg
git add FISHERpres.jpg
git add FISHERrh.jpg
git add FISHERrad.jpg
git add FISHERtemp.jpg

git commit -m "data update"
git push

