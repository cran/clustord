## ----echo=FALSE,message=FALSE,warning=FALSE-----------------------------------
require(knitr)
require(formatR)
# Set so that long lines in R will be wrapped:
opts_chunk$set(tidy.opts=list(width.cutoff=40),tidy=TRUE)

## ----include=FALSE------------------------------------------------------------
library(clustord)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(clustord)
df <- read.table("eval_survey.txt")
colnames(df) <- paste0("Q",1:ncol(df))
rownames(df) <- paste0("ID",1:nrow(df))
long.df <- mat2df(df)
head(long.df)

## ----eval=FALSE---------------------------------------------------------------
# fit <- clustord(Y ~ ROWCLUST + COL, model="POM", nclus.row=2, long.df=long.df,
#                 EM.control=list(startEMcycles=2, EMcycles=100), nstarts=10)

