## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(clustord)
set.seed(30)
long.df.sim <- data.frame(Y=factor(sample(1:3,5*30,replace=TRUE)),
                          ROW=factor(rep(1:30,times=5)),COL=rep(1:5,each=30))
fit <- clustord(Y ~ ROWCLUST, model="POM", nclus.row=2, long.df=long.df.sim, 
                nstarts=2, EM.control = list(EMcycles=2))

## -----------------------------------------------------------------------------
fit

## -----------------------------------------------------------------------------
summary(fit)

## -----------------------------------------------------------------------------
fit$EM.status$converged

## -----------------------------------------------------------------------------
fit_continued <- rerun(fit, long.df=long.df.sim, EM.control=list(EMcycles=20))
summary(fit_continued)

