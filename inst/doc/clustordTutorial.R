## ----echo=FALSE,message=FALSE,warning=FALSE-----------------------------------
require(knitr)
require(formatR)
# Set so that long lines in R will be wrapped:
opts_chunk$set(tidy.opts=list(width.cutoff=40),tidy=TRUE)

## ----echo=FALSE, out.width="70%", fig.align="center"--------------------------
knitr::include_graphics('vignette_fig/survey_table1.png')

## ----echo=FALSE, out.width="70%", fig.align="center"--------------------------
knitr::include_graphics('vignette_fig/survey_table2_rowclustering.png')

## ----echo=FALSE, out.width="70%", fig.align="center"--------------------------
knitr::include_graphics('vignette_fig/survey_table3_columnclustering.png')

## ----echo=FALSE, out.width="70%", fig.align="center"--------------------------
knitr::include_graphics('vignette_fig/survey_table4_biclustering.png')

## ----echo=FALSE, out.width="70%", fig.align="center"--------------------------
knitr::include_graphics('vignette_fig/model-based.png')

## ----echo=FALSE, out.width="70%", fig.align="center"--------------------------
knitr::include_graphics('vignette_fig/distance-based.png')

## ----echo=FALSE, out.width="70%", fig.align="center"--------------------------
knitr::include_graphics('vignette_fig/survey_table_categorical.png')

## ----echo=FALSE, out.width="70%", fig.align="center"--------------------------
knitr::include_graphics('vignette_fig/survey_table_categorical_numbered.png')

## ----echo=FALSE, out.width="70%", fig.align="center"--------------------------
knitr::include_graphics('vignette_fig/survey_table_numbered_only.png')

## ----echo=FALSE, out.width="70%", fig.align="center"--------------------------
knitr::include_graphics('vignette_fig/ordinal_scales.png')

## ----echo=FALSE, out.width="70%", fig.align="center"--------------------------
knitr::include_graphics('vignette_fig/ordinal_scale_phi.png')

## ----message=FALSE, warning=FALSE---------------------------------------------
library(clustord)

## -----------------------------------------------------------------------------
df <- read.table("eval_survey.txt")
colnames(df) <- paste0("Q",1:ncol(df))
rownames(df) <- paste0("ID",1:nrow(df))
head(df)
dim(df)

## ----echo=FALSE, out.width="70%", fig.align="center"--------------------------
knitr::include_graphics('vignette_fig/survey_table_single_cell.png')

## ----echo=FALSE, out.width="40%", fig.align="center"--------------------------
knitr::include_graphics('vignette_fig/long_form_data_frame.png')

## -----------------------------------------------------------------------------
long.df <- mat2df(df)
head(long.df)

## ----echo=FALSE, out.width="70%", fig.align="center"--------------------------
knitr::include_graphics('vignette_fig/survey_table2_rowclustering.png')

## ----ROWFIT, eval=FALSE-------------------------------------------------------
# set.seed(2)
# fit_rowclust_only <- clustord(Y ~ ROWCLUST, "POM", nclus.row=2,
#                               long.df=long.df, verbose=FALSE)

## ----echo=FALSE---------------------------------------------------------------
load("tutorial_fits.Rdata")

## -----------------------------------------------------------------------------
fit_rowclust_only$EM.status$converged

## -----------------------------------------------------------------------------
round(fit_rowclust_only$ppr,2)

## -----------------------------------------------------------------------------
fit_rowclust_only$RowClusterMembers

## -----------------------------------------------------------------------------
round(fit_rowclust_only$pi.out, 2)

## -----------------------------------------------------------------------------
fit_rowclust_only$parlist.out$rowc

## -----------------------------------------------------------------------------
boxplot(split(rowMeans(df), fit_rowclust_only$RowClusters), 
        "Mean response values across all questions for each individual", 
        names = c("Cluster 1", "Cluster 2"))

## ----echo=FALSE, out.width="70%", fig.align="center"--------------------------
knitr::include_graphics('vignette_fig/survey_table2c_rowclustering_columns.png')

## -----------------------------------------------------------------------------
head(df)

## ----ROWCOLFIT, eval=FALSE----------------------------------------------------
# set.seed(3)
# fit_rowclust_cols <- clustord(Y ~ ROWCLUST + COL, "POM",
#                               nclus.row=2, long.df=long.df, verbose=FALSE)

## -----------------------------------------------------------------------------
fit_rowclust_cols$EM.status$converged

## -----------------------------------------------------------------------------
round(fit_rowclust_cols$ppr, 2)

## -----------------------------------------------------------------------------
fit_rowclust_cols$RowClusterMembers

## -----------------------------------------------------------------------------
fit_rowclust_cols$parlist.out$rowc

## -----------------------------------------------------------------------------
boxplot(split(rowMeans(df), fit_rowclust_cols$RowClusters), "Mean response values across all questions for each individual", names = c("Cluster 1", "Cluster 2"))

## -----------------------------------------------------------------------------
round(fit_rowclust_cols$parlist.out$col, 2)

## -----------------------------------------------------------------------------
Y ~ ROWCLUST + COL + ROWCLUST:COL
Y ~ ROWCLUST * COL

## ----ROWCOLINTERACT, eval=FALSE-----------------------------------------------
# set.seed(1)
# fit_rowclust_cols_interact <- clustord(Y ~ ROWCLUST * COL, "POM",
#                                        nclus.row=2, long.df=long.df, verbose=FALSE)

## -----------------------------------------------------------------------------
fit_rowclust_cols_interact$EM.status$converged

## -----------------------------------------------------------------------------
round(fit_rowclust_cols_interact$ppr, 2)

## -----------------------------------------------------------------------------
fit_rowclust_cols_interact$RowClusterMembers

## -----------------------------------------------------------------------------
fit_rowclust_cols_interact$parlist.out$rowc

## -----------------------------------------------------------------------------
boxplot(split(rowMeans(df), fit_rowclust_cols_interact$RowClusters), "Mean response values across all questions for each individual", names = c("Cluster 1", "Cluster 2"))

## -----------------------------------------------------------------------------
round(fit_rowclust_cols_interact$parlist.out$col, 2)

## -----------------------------------------------------------------------------
round(fit_rowclust_cols_interact$parlist.out$rowc_col, 2)

## -----------------------------------------------------------------------------
rowc_col <- fit_rowclust_cols_interact$parlist.out$rowc_col
plot(rowc_col[1,], type="b", col="black", lwd=2, ylim=c(-1.3,1.3))
lines(rowc_col[2,], lty=2, col="blue", lwd=2)
points(rowc_col[2,], lty=2, col="blue", lwd=2)
legend("bottomright", legend=c("Cluster 1","Cluster 2"), col=c("black","blue"),
       lwd=c(2,2), lty=1:2)

## -----------------------------------------------------------------------------
fit_rowclust_only$criteria$AIC
fit_rowclust_cols$criteria$AIC
fit_rowclust_cols_interact$criteria$AIC

fit_rowclust_only$criteria$BIC
fit_rowclust_cols$criteria$BIC
fit_rowclust_cols_interact$criteria$BIC

## ----COLFIT, eval=FALSE-------------------------------------------------------
# set.seed(1)
# fit_colclust_only <- clustord(Y ~ COLCLUST, model = "POM", nclus.column = 2,
#                               long.df = long.df, verbose=FALSE)

## -----------------------------------------------------------------------------
# Convergence
fit_colclust_only$EM.status$converged

# Column cluster membership probabilities
round(fit_colclust_only$ppc,2)

# Members of each column cluster
fit_colclust_only$ColumnClusterMembers

# Mixing proportions
round(fit_colclust_only$kappa.out,2)

# Parameters
fit_colclust_only$parlist.out$colc

## ----COLROWFIT, eval=FALSE----------------------------------------------------
# set.seed(1)
# fit_colclust_rows <- clustord(Y ~ COLCLUST + ROW, model = "POM", nclus.column = 2,
#                               long.df = long.df, verbose=FALSE)

## -----------------------------------------------------------------------------
# Convergence
fit_colclust_rows$EM.status$converged

# Column cluster membership probabilities
round(fit_colclust_rows$ppc,2)

# Members of each column cluster
fit_colclust_rows$ColumnClusterMembers

# Mixing proportions
round(fit_colclust_rows$kappa.out,2)

# Parameters
fit_colclust_rows$parlist.out$colc
round(fit_colclust_rows$parlist.out$row, 2)

## ----COLROWINTERACT, eval=FALSE-----------------------------------------------
# set.seed(1)
# fit_colclust_rows_interact <- clustord(Y ~ COLCLUST * ROW, model = "POM", nclus.column = 2,
#                                        long.df = long.df, verbose=FALSE)

## -----------------------------------------------------------------------------
# Convergence
fit_colclust_rows_interact$EM.status$converged

# Column cluster membership probabilities
round(fit_colclust_rows_interact$ppc,2)

# Members of each column cluster
fit_colclust_rows_interact$ColumnClusterMembers

# Mixing proportions
round(fit_colclust_rows_interact$kappa.out,2)

# Parameters
fit_colclust_rows_interact$parlist.out$colc
round(fit_colclust_rows_interact$parlist.out$row, 2)
round(fit_colclust_rows_interact$parlist.out$colc_row, 2)

## -----------------------------------------------------------------------------
colc_row <- fit_colclust_rows_interact$parlist.out$colc_row
plot(colc_row[1,], type="b", col="black", lwd=2, ylim=c(-85,85),
     xlab="Subject", ylab="Cluster interaction effect")
lines(colc_row[2,], lty=2, col="blue", lwd=2)
points(colc_row[2,], lty=3, col="blue", lwd=2)
legend("topleft", legend=c("Cluster 1","Cluster 2"), col=c("black","blue"),
       lwd=c(2,2), lty=c(1,3))

## -----------------------------------------------------------------------------
colc_row <- fit_colclust_rows_interact$parlist.out$colc_row
plot(colc_row[1,], type="b", col="black", lwd=2, ylim=c(-10,10),
     xlab="Subject", ylab="Cluster interaction effect")
lines(colc_row[2,], lty=2, col="blue", lwd=2)
points(colc_row[2,], lty=3, col="blue", lwd=2)
legend("topleft", legend=c("Cluster 1","Cluster 2"), col=c("black","blue"),
       lwd=c(2,2), lty=c(1,3))

## -----------------------------------------------------------------------------
fit_colclust_only$criteria$AIC
fit_colclust_rows$criteria$AIC
fit_colclust_rows_interact$criteria$AIC

fit_colclust_only$criteria$BIC
fit_colclust_rows$criteria$BIC
fit_colclust_rows_interact$criteria$BIC

## ----BIFIT, eval=FALSE--------------------------------------------------------
# set.seed(4)
# fit_biclust <- clustord(Y ~ ROWCLUST + COLCLUST, model = "POM",
#                         nclus.row = 2, nclus.column = 2,
#                         long.df = long.df, verbose=FALSE)
# converged <- fit_biclust$EM.status$converged

## -----------------------------------------------------------------------------
# Convergence
fit_biclust$EM.status$converged

# Cluster membership probabilities
round(fit_biclust$ppr,2)
round(fit_biclust$ppc,2)

# Members of each cluster
fit_biclust$RowClusterMembers
fit_biclust$ColumnClusterMembers

# Mixing proportions
round(fit_biclust$pi.out,2)
round(fit_biclust$kappa.out,2)

# Parameters
fit_biclust$parlist.out$rowc
fit_biclust$parlist.out$colc

## ----BIINTERACT, eval=FALSE---------------------------------------------------
# set.seed(3)
# fit_biclust_interact <- clustord(Y ~ ROWCLUST * COLCLUST, model = "POM",
#                         nclus.row = 2, nclus.column = 2,
#                         long.df = long.df, verbose=FALSE)
# converged <- fit_biclust_interact$EM.status$converged

## -----------------------------------------------------------------------------
# Convergence
fit_biclust_interact$EM.status$converged

# Cluster membership probabilities
round(fit_biclust_interact$ppr,2)
round(fit_biclust_interact$ppc,2)

# Members of each cluster
fit_biclust_interact$RowClusterMembers
fit_biclust_interact$ColumnClusterMembers

# Mixing proportions
round(fit_biclust_interact$pi.out,2)
round(fit_biclust_interact$kappa.out,2)

# Parameters
fit_biclust_interact$parlist.out$rowc
fit_biclust_interact$parlist.out$colc
round(fit_biclust_interact$parlist.out$rowc_colc,2)

## -----------------------------------------------------------------------------
fit_rowclust_only$criteria$AIC
fit_rowclust_cols$criteria$AIC
fit_rowclust_cols_interact$criteria$AIC
fit_biclust$criteria$AIC
fit_biclust_interact$criteria$AIC

fit_rowclust_only$criteria$BIC
fit_rowclust_cols$criteria$BIC
fit_rowclust_cols_interact$criteria$BIC
fit_biclust$criteria$BIC
fit_biclust_interact$criteria$BIC

## -----------------------------------------------------------------------------
fit_colclust_only$criteria$AIC
fit_colclust_rows$criteria$AIC
fit_colclust_rows_interact$criteria$AIC
fit_biclust$criteria$AIC
fit_biclust_interact$criteria$AIC

fit_colclust_only$criteria$BIC
fit_colclust_rows$criteria$BIC
fit_colclust_rows_interact$criteria$BIC
fit_biclust$criteria$BIC
fit_biclust_interact$criteria$BIC

## ----eval=FALSE---------------------------------------------------------------
# fit <- clustord(Y ~ ROWCLUST + COL, model="POM", nclus.row=2, long.df=long.df,
#                 EM.control=list(startEMcycles=2, EMcycles=100), nstarts=10)

## ----echo=FALSE, out.width="70%", fig.align="center"--------------------------
knitr::include_graphics('vignette_fig/survey_table5_covariates.png')

## -----------------------------------------------------------------------------
age.df <- data.frame(age=round(runif(nrow(df), min=20, max=60)))

long.df <- mat2df(df, xr.df=age.df)

## -----------------------------------------------------------------------------
question.df <- data.frame(question=sample(c("Group A","Group B"), ncol(df), replace=TRUE))

long.df <- mat2df(df, xr.df=age.df, xc.df=question.df)

## ----eval=FALSE---------------------------------------------------------------
# fit_with_covariates <- clustord(Y ~ ROWCLUST + age + question, model="POM",
#                                 nclus.row=2, long.df=long.df, verbose=FALSE)

## -----------------------------------------------------------------------------
fit_with_covariates$EM.status$converged

fit_with_covariates$RowClusterMembers

fit_with_covariates$parlist.out

