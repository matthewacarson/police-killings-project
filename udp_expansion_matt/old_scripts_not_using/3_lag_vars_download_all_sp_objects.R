if (!require(pacman)) install.packages("pacman")
pacman::p_load(googledrive, bit64, fs, data.table, tigris, tidycensus, tidyverse, spdep)

st <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", 
        "DE", "DC", "FL", "GA", "HI", "ID", "IL", 
        "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
        "MA", "MI", "MN", "MS", "MO", "MT", "NE", 
        "NV", "NH", "NJ", "NM", "NY", "NC", "ND", 
        "OH", "OK", "OR", "PA", "RI", "SC", "SD", 
        "TN", "TX", "UT", "VT", "VA", "WA", "WV", 
        "WI", "WY")

# Assuming `st` is a vector of objects containing state information

# Create an empty list to store the state objects
state_list <- list()

# Loop through indices 1 to 51 and create variables for each state
for (i in 1:51) {
  state_list[[i]] <- tracts(st[i], cb = TRUE, class = 'sp', year = 2018)
}

save.image(file = '3_lag_vars_all_sp_objects.RData')
# You can access the state objects from the list using state_list[[1]], state_list[[2]], etc.
# Or you can combine them into a single list or another suitable data structure for further processing.
