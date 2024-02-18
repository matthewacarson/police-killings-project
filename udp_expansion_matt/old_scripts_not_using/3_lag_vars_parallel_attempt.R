
# This begins at line 180 of 3_create_lag_vars_my_edits.r

library(parallel)

# Create a cluster with two cores
cl <- makeCluster(2)

# Set up parallel processing
clusterEvalQ(cl, {
  library(sp)
  library(spdep)
})

# Run computations in parallel
coords <- coordinates(stsp)
IDs <- row.names(as(stsp, "data.frame"))
stsp_nb <- poly2nb(stsp)
lw_bin <- nb2listw(stsp_nb, style = "W", zero.policy = TRUE)

# Divide the computations into two parts to run in parallel
clusterExport(cl, c("coords", "IDs", "stsp_nb", "lw_bin"))
result <- parLapply(cl, 1:2, function(i) {
  if (i == 1) {
    kern1 <- knn2nb(knearneigh(coords, k = 1), row.names=IDs)
    dist <- unlist(nbdists(kern1, coords))
    max_1nn <- max(dist)
    dist_nb <- dnearneigh(coords, d1=0, d2 = .1*max_1nn, row.names = IDs)
    set.ZeroPolicyOption(TRUE)
    dists <- nbdists(dist_nb, coordinates(stsp))
    idw <- lapply(dists, function(x) 1/(x^2))
    lw_dist_idwW <- nb2listw(dist_nb, glist = idw, style = "W")
    return(lw_dist_idwW)
  } else {
    stsp$tr_pchrent.lag <- lag.listw(lw_dist_idwW,stsp$tr_pchrent)
    stsp$tr_chrent.lag <- lag.listw(lw_dist_idwW,stsp$tr_chrent)
    stsp$tr_medrent18.lag <- lag.listw(lw_dist_idwW,stsp$tr_medrent18)
    return(stsp)
  }
})

# Merge the results
lw_dist_idwW <- result[[1]]
stsp <- result[[2]]

# Stop the cluster
stopCluster(cl)

# Continue with the rest of your code
