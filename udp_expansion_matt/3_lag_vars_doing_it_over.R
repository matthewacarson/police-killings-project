library(sp)
library(raster)
library(parallel)

# Create a cluster with 2 cores
cl <- makeCluster(2)

# Export necessary functions and libraries to the cluster
clusterEvalQ(cl, {
  library(sp)
  library(raster)
})

# Load the necessary package within the parallel workers
clusterExport(cl, c("tracts", "st"))

# Combine tracts for the first two states
# combined_tracts <- raster::union(tracts(st[1], cb = TRUE, class = 'sp'),
                                 # tracts(st[2], cb = TRUE, class = 'sp'))

# Function to combine tracts for a single state
combine_tracts <- function(i) {tracts(st[i], cb = TRUE, class = 'sp')}

# Export the combine_tracts function to the cluster
clusterExport(cl, c("combine_tracts"))

# Loop through st[3] to st[51] to combine tracts for the remaining states in parallel
combined_tracts <- parLapply(cl, 3:51, function(i) {
  combine_tracts(i)
}) %>% 
  reduce(raster::union)

# Stop the cluster
stopCluster(cl)

# Combine the resulting tracts
combined_tracts <- raster::union(combined_tracts)
