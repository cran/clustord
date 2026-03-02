## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
)

## ----warning=FALSE, message=FALSE---------------------------------------------
library(clustord)
library(multgee)
head(arthritis)

## -----------------------------------------------------------------------------
arthritis$y <- factor(arthritis$y)

## -----------------------------------------------------------------------------
fit <- osm(y ~ baseline + sex + age, data=arthritis, subset = (time == 1))
fit

## -----------------------------------------------------------------------------
fit <- osm(y ~ baseline + sex + age, data=arthritis, 
           subset = (time == 1), control=list(maxit=5000))
fit

## -----------------------------------------------------------------------------
summary(fit)

## -----------------------------------------------------------------------------
arthritis$y_merged <- as.numeric(as.character(arthritis$y))
arthritis$y_merged[arthritis$y_merged %in% c(2,3)] <- 2.5
arthritis$y_merged <- factor(arthritis$y_merged)

fit_merged <- osm(y_merged ~ baseline + sex + age, data=arthritis, 
                  subset = (time == 1), control = list(maxit = 5000))
summary(fit_merged)

## -----------------------------------------------------------------------------
arthritis$y_merged2 <- as.numeric(as.character(arthritis$y_merged))
arthritis$y_merged2[arthritis$y_merged2 %in% c(4,5)] <- 4.5
arthritis$y_merged2 <- factor(arthritis$y_merged2)

fit_merged2 <- osm(y_merged2 ~ baseline + sex + age, data=arthritis, 
                   subset = (time == 1), control = list(maxit = 5000))
fit_merged2

## ----eval=FALSE---------------------------------------------------------------
# remotes::install_github("lfmcmillan/effects")

## ----eval=FALSE---------------------------------------------------------------
# fit1 <- osm(y ~ x + z, data=df)
# plot(Effect(focal.predictors = c("x"), fit1))

## ----eval=FALSE---------------------------------------------------------------
# detach("package:effects")
# remotes::install_github("lfmcmillan/effects")

## ----eval=FALSE---------------------------------------------------------------
# library(effects)
# plot(Effect(focal.predictors = c("baseline"), fit))

