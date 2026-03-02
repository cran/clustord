library(clustord)

# Lots of rows, not many columns
set.seed(2)
Y <- matrix(sample(1:4,100,replace=TRUE), nrow=20)
xr_df <- data.frame(xr1=rnorm(20),xr2=sample(c(TRUE,FALSE),20,replace=TRUE))
xc_df <- data.frame(xc1=runif(5))
long_df <- mat_to_df(Y, xr_df, xc_df)

results_original <- clustord(Y ~ ROWCLUST*COL+xr1+ROWCLUST:xr2+xc1, model="OSM", nstarts=1,
                             R=2, long_df=long_df, control_EM=list(maxiter=2,keep_all_params=TRUE))
results.reordered <- reorder.clustord(results_original, "row")

results_original$out_parlist
results.reordered$out_parlist

results_original$row_cluster_proportions
results.reordered$row_cluster_proportions

head(results_original$row_cluster_probs)
head(results.reordered$row_cluster_probs)

results_original$row_clusters
results.reordered$row_clusters

results_original$row_cluster_members
results.reordered$row_cluster_members

results_original$EMstatus$params_for_best_lli
results.reordered$EMstatus$params_for_best_lli

results_original$EMstatus$params_every_iteration
results.reordered$EMstatus$params_every_iteration

# Column clustering ============================================================

# Lots of columns, not many rows
set.seed(1)
Y <- matrix(sample(1:3,100,replace=TRUE), nrow=5)
xc_df <- data.frame(xc1=rnorm(20),xc2=sample(c(TRUE,FALSE),20,replace=TRUE))
xr_df <- data.frame(xr1=runif(5))
long_df <- mat_to_df(Y, xr_df, xc_df)

results_original <- clustord(Y ~ COLCLUST*ROW+xc1+COLCLUST:xc2+xr1, model="OSM", nstarts=1,
                             C=2, long_df=long_df, control_EM=list(maxiter=2,keep_all_params=TRUE))
results.reordered <- reorder.clustord(results_original, "column")

results_original$out_parlist
results.reordered$out_parlist

results_original$column_cluster_proportions
results.reordered$column_cluster_proportions

head(results_original$column_cluster_probs)
head(results.reordered$column_cluster_probs)

results_original$column_clusters
results.reordered$column_clusters

results_original$column_cluster_members
results.reordered$column_cluster_members

results_original$EMstatus$params_for_best_lli
results.reordered$EMstatus$params_for_best_lli

results_original$EMstatus$params_every_iteration
results.reordered$EMstatus$params_every_iteration

# Biclustering ============================================================

# Equal numbers of rows and columns
set.seed(6)
Y <- matrix(sample(1:3,100,replace=TRUE), nrow=10)
xc_df <- data.frame(xc1=rnorm(10),xc2=sample(c(TRUE,FALSE),10,replace=TRUE))
xr_df <- data.frame(xr1=runif(10))
long_df <- mat_to_df(Y, xr_df, xc_df)

results_original <- clustord(Y ~ ROWCLUST*COLCLUST+ROWCLUST:xr1+COLCLUST:xc2, model="POM", nstarts=1,
                             R=3, C=3, long_df=long_df, control_EM=list(maxiter=2,keep_all_params=TRUE))
results.reordered <- reorder.clustord(results_original, "row", decreasing=TRUE)

results_original$out_parlist
results.reordered$out_parlist

results_original$row_cluster_proportions
results.reordered$row_cluster_proportions

head(results_original$row_cluster_probs)
head(results.reordered$row_cluster_probs)

results_original$row_clusters
results.reordered$row_clusters

results_original$row_cluster_members
results.reordered$row_cluster_members

results_original$EMstatus$params_for_best_lli
results.reordered$EMstatus$params_for_best_lli

results_original$EMstatus$params_every_iteration
results.reordered$EMstatus$params_every_iteration



results.reordered <- reorder.clustord(results_original, "col")

results_original$out_parlist
results.reordered$out_parlist

results_original$column_cluster_proportions
results.reordered$column_cluster_proportions

head(results_original$column_cluster_probs)
head(results.reordered$column_cluster_probs)

results_original$ColClusters
results.reordered$ColClusters

results_original$ColClusterMembers
results.reordered$ColClusterMembers

results_original$EMstatus$params_for_best_lli
results.reordered$EMstatus$params_for_best_lli

results_original$EMstatus$params_every_iteration
results.reordered$EMstatus$params_every_iteration



results.reordered <- reorder.clustord(results_original, "both", decreasing=c(FALSE,TRUE))

results_original$out_parlist
results.reordered$out_parlist

results_original$row_cluster_proportions
results.reordered$row_cluster_proportions

results_original$column_cluster_proportions
results.reordered$column_cluster_proportions

head(results_original$row_cluster_probs)
head(results.reordered$row_cluster_probs)
head(results_original$column_cluster_probs)
head(results.reordered$column_cluster_probs)

results_original$row_clusters
results.reordered$row_clusters

results_original$row_cluster_members
results.reordered$row_cluster_members

results_original$ColClusters
results.reordered$ColClusters

results_original$ColClusterMembers
results.reordered$ColClusterMembers

results_original$EMstatus$params_for_best_lli
results.reordered$EMstatus$params_for_best_lli

results_original$EMstatus$params_every_iteration
results.reordered$EMstatus$params_every_iteration


### CONSTRAINT SUM ZERO FALSE ==================================================

# Lots of rows, not many columns
set.seed(1)
Y <- matrix(sample(1:3,100,replace=TRUE), nrow=20)
xr_df <- data.frame(xr1=rnorm(20),xr2=sample(c(TRUE,FALSE),20,replace=TRUE))
xc_df <- data.frame(xc1=runif(5))
long_df <- mat_to_df(Y, xr_df, xc_df)

results_original <- clustord(Y ~ ROWCLUST*COL+xr1+ROWCLUST:xr2+xc1, model="OSM", nstarts=1,
                             R=2, long_df=long_df, control_EM=list(maxiter=2,keep_all_params=TRUE),
                             constraint_sum_zero = FALSE)
results.reordered <- reorder.clustord(results_original, "row")

results_original$out_parlist
results.reordered$out_parlist

results_original$row_cluster_proportions
results.reordered$row_cluster_proportions

head(results_original$row_cluster_probs)
head(results.reordered$row_cluster_probs)

results_original$row_clusters
results.reordered$row_clusters

results_original$row_cluster_members
results.reordered$row_cluster_members

results_original$EMstatus$params_for_best_lli
results.reordered$EMstatus$params_for_best_lli

results_original$EMstatus$params_every_iteration
results.reordered$EMstatus$params_every_iteration

# Column clustering ============================================================

# Lots of columns, not many rows
n <- 5
set.seed(1)
Y <- matrix(sample(1:3,100,replace=TRUE), nrow=n)
xc_df <- data.frame(xc1=rnorm(100/n),xc2=sample(c(TRUE,FALSE),100/n,replace=TRUE))
xr_df <- data.frame(xr1=runif(n))
long_df <- mat_to_df(Y, xr_df, xc_df)

results_original <- clustord(Y ~ COLCLUST*ROW+xc1+COLCLUST:xc2+xr1, model="OSM", nstarts=1,
                             C=2, long_df=long_df, control_EM=list(maxiter=2,keep_all_params=TRUE),
                             constraint_sum_zero = FALSE)
results.reordered <- reorder.clustord(results_original, "column")

results_original$out_parlist
results.reordered$out_parlist

results_original$column_cluster_proportions
results.reordered$column_cluster_proportions

head(results_original$column_cluster_probs)
head(results.reordered$column_cluster_probs)

results_original$ColClusters
results.reordered$ColClusters

results_original$ColClusterMembers
results.reordered$ColClusterMembers

results_original$EMstatus$params_for_best_lli
results.reordered$EMstatus$params_for_best_lli

results_original$EMstatus$params_every_iteration
results.reordered$EMstatus$params_every_iteration

# Biclustering ============================================================

# Equal numbers of rows and columns
n <- 10
set.seed(1)
Y <- matrix(sample(1:3,100,replace=TRUE), nrow=n)
xc_df <- data.frame(xc1=rnorm(100/n),xc2=sample(c(TRUE,FALSE),100/n,replace=TRUE))
xr_df <- data.frame(xr1=runif(n))
long_df <- mat_to_df(Y, xr_df, xc_df)

results_original <- clustord(Y ~ ROWCLUST*COLCLUST+ROWCLUST:xr1+COLCLUST:xc2, model="POM", nstarts=1,
                             R=3, C=3, long_df=long_df, control_EM=list(maxiter=2,keep_all_params=TRUE),
                             constraint_sum_zero = FALSE)
results.reordered <- reorder.clustord(results_original, "row", decreasing=TRUE)

results_original$out_parlist
results.reordered$out_parlist

results_original$row_cluster_proportions
results.reordered$row_cluster_proportions

head(results_original$row_cluster_probs)
head(results.reordered$row_cluster_probs)

results_original$row_clusters
results.reordered$row_clusters

results_original$row_cluster_members
results.reordered$row_cluster_members

results_original$EMstatus$params_for_best_lli
results.reordered$EMstatus$params_for_best_lli

results_original$EMstatus$params_every_iteration
results.reordered$EMstatus$params_every_iteration



results.reordered <- reorder.clustord(results_original, "col")

results_original$out_parlist
results.reordered$out_parlist

results_original$column_cluster_proportions
results.reordered$column_cluster_proportions

head(results_original$column_cluster_probs)
head(results.reordered$column_cluster_probs)

results_original$ColClusters
results.reordered$ColClusters

results_original$ColClusterMembers
results.reordered$ColClusterMembers

results_original$EMstatus$params_for_best_lli
results.reordered$EMstatus$params_for_best_lli

results_original$EMstatus$params_every_iteration
results.reordered$EMstatus$params_every_iteration



results.reordered <- reorder.clustord(results_original, "both", decreasing=c(FALSE,TRUE))

results_original$out_parlist
results.reordered$out_parlist

results_original$row_cluster_proportions
results.reordered$row_cluster_proportions

results_original$column_cluster_proportions
results.reordered$column_cluster_proportions

head(results_original$row_cluster_probs)
head(results.reordered$row_cluster_probs)
head(results_original$column_cluster_probs)
head(results.reordered$column_cluster_probs)

results_original$row_clusters
results.reordered$row_clusters

results_original$row_cluster_members
results.reordered$row_cluster_members

results_original$ColClusters
results.reordered$ColClusters

results_original$ColClusterMembers
results.reordered$ColClusterMembers

results_original$EMstatus$params_for_best_lli
results.reordered$EMstatus$params_for_best_lli

results_original$EMstatus$params_every_iteration
results.reordered$EMstatus$params_every_iteration



# Biclustering ============================================================

# Equal numbers of rows and columns
n <- 10
set.seed(1)
Y <- matrix(sample(0:1,100,replace=TRUE), nrow=n)
xc_df <- data.frame(xc1=rnorm(100/n),xc2=sample(c(TRUE,FALSE),100/n,replace=TRUE))
xr_df <- data.frame(xr1=runif(n))
long_df <- mat_to_df(Y, xr_df, xc_df)

results_original <- clustord(Y ~ ROWCLUST*COLCLUST+ROWCLUST:xr1+COLCLUST:xc2, model="Binary", nstarts=1,
                             R=3, C=3, long_df=long_df, control_EM=list(maxiter=2,keep_all_params=TRUE),
                             constraint_sum_zero = FALSE)
results.reordered <- reorder.clustord(results_original, "row", decreasing=TRUE)

results_original$out_parlist
results.reordered$out_parlist

results_original$row_cluster_proportions
results.reordered$row_cluster_proportions

head(results_original$row_cluster_probs)
head(results.reordered$row_cluster_probs)

results_original$row_clusters
results.reordered$row_clusters

results_original$row_cluster_members
results.reordered$row_cluster_members

results_original$EMstatus$params_for_best_lli
results.reordered$EMstatus$params_for_best_lli

results_original$EMstatus$params_every_iteration
results.reordered$EMstatus$params_every_iteration



results.reordered <- reorder.clustord(results_original, "col")

results_original$out_parlist
results.reordered$out_parlist

results_original$column_cluster_proportions
results.reordered$column_cluster_proportions

head(results_original$column_cluster_probs)
head(results.reordered$column_cluster_probs)

results_original$ColClusters
results.reordered$ColClusters

results_original$ColClusterMembers
results.reordered$ColClusterMembers

results_original$EMstatus$params_for_best_lli
results.reordered$EMstatus$params_for_best_lli

results_original$EMstatus$params_every_iteration
results.reordered$EMstatus$params_every_iteration



results.reordered <- reorder.clustord(results_original, "both", decreasing=c(FALSE,TRUE))

results_original$out_parlist
results.reordered$out_parlist

results_original$row_cluster_proportions
results.reordered$row_cluster_proportions

results_original$column_cluster_proportions
results.reordered$column_cluster_proportions

head(results_original$row_cluster_probs)
head(results.reordered$row_cluster_probs)
head(results_original$column_cluster_probs)
head(results.reordered$column_cluster_probs)

results_original$row_clusters
results.reordered$row_clusters

results_original$row_cluster_members
results.reordered$row_cluster_members

results_original$ColClusters
results.reordered$ColClusters

results_original$ColClusterMembers
results.reordered$ColClusterMembers

results_original$EMstatus$params_for_best_lli
results.reordered$EMstatus$params_for_best_lli

results_original$EMstatus$params_every_iteration
results.reordered$EMstatus$params_every_iteration

