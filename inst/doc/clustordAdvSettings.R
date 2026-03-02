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
long_df <- mat_to_df(df)
head(long_df)

## ----eval=FALSE---------------------------------------------------------------
# fit <- clustord(Y ~ ROWCLUST + COL, model="POM", RG=2, long_df=long_df,
#                 control_EM=list(startmaxiter=2, maxiter=100), nstarts=10)

