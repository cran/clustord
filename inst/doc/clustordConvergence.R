## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
)

## -----------------------------------------------------------------------------
library(clustord)
set.seed(30)
long_df_sim <- data.frame(Y=factor(sample(1:3,5*30,replace=TRUE)),
                          ROW=factor(rep(1:30,times=5)),COL=rep(1:5,each=30))
fit <- clustord(Y ~ ROWCLUST, model="POM", RG=2, long_df=long_df_sim, 
                nstarts=2, control_EM = list(maxiter=2))

## -----------------------------------------------------------------------------
fit

## -----------------------------------------------------------------------------
summary(fit)

## -----------------------------------------------------------------------------
fit$EMstatus$converged

## -----------------------------------------------------------------------------
fit_continued <- rerun(fit, long_df=long_df_sim, control_EM=list(maxiter=20))
summary(fit_continued)

