# =====================================================
# =====================================================
# DISPLACEMENT TYPOLOGY SET UP
# =====================================================
# =====================================================

if (!require(pacman)) install.packages("pacman"); pacman::p_load(googledrive, bit64, fs, data.table, tigris, tidycensus, tidyverse, spdep, raster, sp, parallel, sf)
# library(raster)
# library(sp)
# library(parallel)
# library(sf)

### Tract data extraction function: add your state here
st <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", 
        "DE", "DC", "FL", "GA", "HI", "ID", "IL", 
        "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
        "MA", "MI", "MN", "MS", "MO", "MT", "NE", 
        "NV", "NH", "NJ", "NM", "NY", "NC", "ND", 
        "OH", "OK", "OR", "PA", "RI", "SC", "SD", 
        "TN", "TX", "UT", "VT", "VA", "WA", "WV", 
        "WI", "WY")

# Combine tracts for the first two states
combined_tracts <- raster::union(tracts(st[1], cb = TRUE, class = 'sp'),
                                 tracts(st[2], cb = TRUE, class = 'sp'))

# Function to combine tracts for a single state
combine_tracts <- function(i) {tracts(st[i], cb = TRUE, class = 'sp')}

# Create a cluster with 2 cores
cl <- makeCluster(2)

# Export necessary functions and libraries to the cluster
clusterEvalQ(cl, {
  library(sp)
  library(raster)
})

# Load the necessary package within the parallel workers
clusterExport(cl, c(
  "tracts" 
  ,"st"
))
# Export the combine_tracts function to the cluster
clusterExport(cl, c("combine_tracts"))

# Loop through st[3] to st[51] to combine tracts for the remaining states in parallel
combined_tracts <- parLapply(cl, 3:51, function(i) {
  combine_tracts(i)
}) %>% 
  reduce(raster::union)

# Stop the cluster
stopCluster(cl)
save.image(file = paste0(data_dir, r_data_folder, 'doing_it_over_1.RData'))
# Combine the resulting tracts
combined_tracts <- raster::union(combined_tracts)
save.image(file = paste0(data_dir, r_data_folder, 'all_tracts_combined_doover.RData'))
