
# I decided to join some here because it was taking too long joining them all at once in another 

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
load(file = '3_lag_vars_all_sp_objects.RData')

st_50_51 <- raster::union(state_51, state_50)
st_48_49 <- raster::union(state_48, state_49)
st_46_47 <- raster::union(state_46, state_47)
st_44_45 <- raster::union(state_44, state_45)
st_42_43 <- raster::union(state_42, state_43)

st_48_49st_50_51 <- raster::union(st_50_51, st_48_49)
st_46_47st_44_45 <- raster::union(st_46_47,st_44_45)

st_48_49st_50_51st_46_47st_44_45 <- 
  raster::union(st_48_49st_50_51,st_46_47st_44_45)

st_43_thru_51 <- 
  raster::union(st_48_49st_50_51st_46_47st_44_45, state_43)

st_42_thru_51 <- 
  raster::union(state_42, st_43_thru_51)

rm(state_42, state_43, state_44, state_45, state_46, state_47,
   state_48, state_49, state_50, state_51)

save.image(file = "sp_sates.RData")
