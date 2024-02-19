# =====================================================
# =====================================================
# DISPLACEMENT TYPOLOGY SET UP
# =====================================================
# =====================================================

if (!require(pacman)) install.packages("pacman"); pacman::p_load(googledrive, bit64, fs, data.table, tigris, tidycensus, tidyverse, spdep, raster, sp, parallel, sf, foreach, doParallel)
# library(sf)
# 2/9/2024: I could not find this package.
# install.packages('colorout')

# options(width = Sys.getenv('COLUMNS'))

# =====================================================
# Pull in data
# =====================================================
# you will need to update the 'data_dir' variable to the directory
# you're using

data_dir <- paste0(getwd(), '/udp_expansion_matt')

r_data_folder <- '/data/R_data/'

# df <- read_csv(paste0(data_dir, '/data/outputs/databases/zillow_database_2018.csv'), col_types = cols(...1 = col_skip()))

# save(df, file = paste0(data_dir, '/data/outputs/databases/zillow_database_2018.RData'))
load(file = paste0(data_dir, '/data/outputs/databases/zillow_database_2018.RData'))



# Create rent gap and extra local change in rent
# =====================================================
#
# Tract data
# -----------------------------------------------------

### Tract data extraction function: add your state here
st <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", 
        "DE", "DC", "FL", "GA", "HI", "ID", "IL", 
        "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
        "MA", "MI", "MN", "MS", "MO", "MT", "NE", 
        "NV", "NH", "NJ", "NM", "NY", "NC", "ND", 
        "OH", "OK", "OR", "PA", "RI", "SC", "SD", 
        "TN", "TX", "UT", "VT", "VA", "WA", "WV", 
        "WI", "WY")

# tr_rent <- function(year, state){
#     get_acs(
#         geography = "tract",
#         variables = c('medrent' = 'B25064_001'),
#         state = state,
#         county = NULL,
#         geometry = FALSE,
#         cache_table = TRUE,
#         output = "tidy",
#         year = year,
#         keep_geo_vars = TRUE
#         ) %>%
#     select(-moe) %>% 
#     rename(medrent = estimate) %>% 
#     mutate(
#         county = str_sub(GEOID, 3,5), 
#         state = str_sub(GEOID, 1,2),
#         year = str_sub(year, 3,4) 
#     )
# }
# 
# ### Loop (map) across different states
# tr_rents18 <- 
#     map_dfr(st, function(state){
#         tr_rent(year = 2018, state) %>% 
#         mutate(COUNTY = substr(GEOID, 1, 5))
#     })
# 
# tr_rents12 <- 
#     map_dfr(st, function(state){
#         tr_rent(year = 2012, state) %>% 
#         mutate(
#             COUNTY = substr(GEOID, 1, 5),
#             medrent = medrent*1.07)
#     }); gc()
# 
# tr_rents <- 
#     bind_rows(tr_rents18, tr_rents12) %>% 
#     unite("variable", c(variable,year), sep = "") %>% 
#     group_by(variable) %>% 
#     spread(variable, medrent) %>% 
#     group_by(COUNTY) %>%
#     mutate(
#         tr_medrent18 = 
#             case_when(
#                 is.na(medrent18) ~ median(medrent18, na.rm = TRUE),
#                 TRUE ~ medrent18
#             ),
#         tr_medrent12 = 
#             case_when(
#                 is.na(medrent12) ~ median(medrent12, na.rm = TRUE),
#                 TRUE ~ medrent12),
#         tr_chrent = tr_medrent18 - tr_medrent12,
#         tr_pchrent = (tr_medrent18 - tr_medrent12)/tr_medrent12,
#         rm_medrent18 = median(tr_medrent18, na.rm = TRUE), 
#         rm_medrent12 = median(tr_medrent12, na.rm = TRUE)) %>% 
#     select(-medrent12, -medrent18) %>% 
#     distinct() %>% 
#     group_by(GEOID) %>% 
#     filter(row_number() == 1) %>% 
#     ungroup()
#     
# save(tr_rents, file = paste0(data_dir, r_data_folder, 'tr_rents.Rdata'))
load(file = paste0(data_dir, r_data_folder, 'tr_rents.Rdata'))

# combined_tracts <- tracts(st[1], cb = TRUE, class = 'sp')

# for (i in 48:51) {
  # combined_tracts <- raster::union(combined_tracts,
                                   # tracts(st[i], cb = TRUE, class = 'sp'))
  # save(combined_tracts, 
       # file = paste0(data_dir, r_data_folder, "st_thru_", i, '.RData'))
# }

# stsp <- combined_tracts; rm(combined_tracts)




# join data to these tracts
# stsp@data <-
#     left_join(
#       stsp@data,
#         tr_rents,
#         by = "GEOID")# %>% select(c(1,5:23))
# 

load("C:/Users/madou/OneDrive - UCLA IT Services/1)_PS-Honors/police-killings-project_union_PC/udp_expansion_matt/data/R_data/stsp_backup.RData")
stsp <- stsp_backup
stsp@data <- stsp@data |> 
  select(
    GEOID,
    tr_medrent18,
    tr_medrent12,
    tr_chrent,
    tr_pchrent,
    rm_medrent18,
    rm_medrent12
  ) |> mutate(GEOID = as.numeric(GEOID))

cl <- makeCluster(8) # manually set to 8 cores
registerDoParallel(cl)
# Create neighbor matrix
# -----------------------------------------------------
    coords <- coordinates(stsp)
    IDs <- row.names(as(stsp, "data.frame"))
    stsp_nb <- poly2nb(stsp) # nb
    
# save(stsp_nb, file = paste0(data_dir, r_data_folder, 'stsp_nb.RData'))
# load(file = paste0(data_dir, r_data_folder, 'stsp_nb.RData'))

    lw_bin <- nb2listw(stsp_nb, style = "W", zero.policy = TRUE)
    
# save(lw_bin, file = paste0(data_dir, r_data_folder, 'lw_bin.RData'))
# load(file = paste0(data_dir, r_data_folder, 'lw_bin.RData')) 

    knearneigh1 <- knearneigh(coords, k = 1)
  
    kern1 <- knn2nb(knearneigh1) # , row.names=IDs)
    
# save(kern1, file = paste0(data_dir, r_data_folder, 'kern1.RData'))
# load(file = paste0(data_dir, r_data_folder, 'kern1.RData'))

  dist <- unlist(nbdists(kern1, coords))
  
# save(dist, file = paste0(data_dir, r_data_folder, 'dist.RData'))
# load(file = paste0(data_dir, r_data_folder, 'dist.RData'))
  
  max_1nn <- max(dist)
  
# save(max_1nn, file = paste0(data_dir, r_data_folder, 'max_1nn.RData'))
# load(file = paste0(data_dir, r_data_folder, 'max_1nn.RData'))
  
  dist_nb <- dnearneigh(coords, d1=0, d2 = .1*max_1nn) # , row.names = IDs)

# save(dist_nb, file = paste0(data_dir, r_data_folder, 'dist_nb.RData'))
# load(file = paste0(data_dir, r_data_folder, 'dist_nb.RData'))

  spdep::set.ZeroPolicyOption(TRUE)
  spdep::set.ZeroPolicyOption(TRUE)
  dists <- nbdists(dist_nb, coordinates(stsp))
  
# save(idw, file = paste0(data_dir, r_data_folder, 'dists.RData'))
# load(file = paste0(data_dir, r_data_folder, 'dists.RData'))
  
  idw <- lapply(dists, function(x) 1/(x^2))
  # lw_dist_idwW <- nb2listw(dist_nb, glist = idw, style = "W")
    
save(lw_dist_idwW, file = paste0(data_dir, r_data_folder, 'lw_dist_idwW.RData'))
# load(file = paste0(data_dir, r_data_folder, 'lw_dist_idwW.RData'))

# Filter out so that lw_dist_idwW is same length as stsp ------------------

# lw_dist_idwW_filter <- lw_dist_idwW
# lw_dist_idwW_filter$neighbours <- lw_dist_idwW$neighbours[!is.na(stsp$tr_chrent)]
# lw_dist_idwW_filter$weights <- lw_dist_idwW$weights[!is.na(stsp$tr_chrent)]
# 
# stsp@data <- stsp@data[!is.na(stsp$tr_chrent),]
# 
# rownames(stsp@data) <- seq(nrow(stsp@data))
#
# Create select lag variables ####
# ----------------------------------------------------- #

  stsp$tr_pchrent.lag <- lag.listw(lw_dist_idwW_filter, stsp$tr_pchrent)
  
# save(stsp, file = paste0(data_dir, r_data_folder, 'stsp_tr_pchrent_lag.RData'))
# load(file = paste0(data_dir, r_data_folder, 'stsp_tr_pchrent_lag.RData'))

  stsp$tr_chrent.lag <- lag.listw(lw_dist_idwW, stsp$tr_chrent)
  
# save(stsp, file = paste0(data_dir, r_data_folder, 'stsp_tr_chrent_lag.RData'))
# load(file = paste0(data_dir, r_data_folder, 'stsp_tr_chrent_lag.RData'))

  stsp$tr_medrent18.lag <- lag.listw(lw_dist_idwW, stsp$tr_medrent18)
  
# save(stsp, file = paste0(data_dir, r_data_folder, 'stsp_tr_medrent18_lag.RData'))
# load(file = paste0(data_dir, r_data_folder, 'stsp_tr_medrent18_lag.RData'))

################ #
# there was an issue with the columns names having an appended '.x' or '.y'
# Renaming
# =====================================================
# Join lag vars with df ####
# =====================================================

# stsp_data <- stsp@data # [, c(1, 11:23)]

lag <-  
    left_join(
        df |> rename(GEOID = FIPS), 
        stsp@data) %>% 
            mutate(GEOID = as.numeric(GEOID)) %>%
            select(c(1,14:22)) %>%
    mutate(
        tr_rent_gap = tr_medrent18.lag - tr_medrent18, 
        tr_rent_gapprop = tr_rent_gap/((tr_medrent18 + tr_medrent18.lag)/2),
        rm_rent_gap = median(tr_rent_gap, na.rm = TRUE), 
        rm_rent_gapprop = median(tr_rent_gapprop, na.rm = TRUE), 
        rm_pchrent = median(tr_pchrent, na.rm = TRUE),
        rm_pchrent.lag = median(tr_pchrent.lag, na.rm = TRUE),
        rm_chrent.lag = median(tr_chrent.lag, na.rm = TRUE),
        rm_medrent17.lag = median(tr_medrent18.lag, na.rm = TRUE), 
        dp_PChRent = case_when(tr_pchrent > 0 & 
                               tr_pchrent > rm_pchrent ~ 1, # ∆ within tract
                               tr_pchrent.lag > rm_pchrent.lag ~ 1, # ∆ nearby tracts
                               TRUE ~ 0),
        dp_RentGap = case_when(tr_rent_gapprop > 0 & tr_rent_gapprop > rm_rent_gapprop ~ 1,
                               TRUE ~ 0),
    ) 
# which(apply(X = lag[,300:406], MARGIN = 2, FUN = function(x) all(is.na(x))))

save(lag, file = paste0(data_dir, r_data_folder, 'lag.RData'))
# =====================================================
# PUMA
# =====================================================

# puma <-
#     get_acs(
#         geography = "public use microdata area", 
#         variable = "B05006_001", 
#         year = 2018, 
#         # wide = TRUE, 
#         geometry = TRUE, 
#         state = st, 
#         keep_geo_vars = TRUE
#     ) %>% 
#     mutate(
#         sqmile = ALAND10/2589988, 
#         puma_density = estimate/sqmile
#         ) %>% 
#     rename(PUMAID = GEOID)

# save(puma, file = paste0(data_dir, r_data_folder, 'puma.RData'))
load(file = paste0(data_dir, r_data_folder, 'puma.RData'))


stsf <- 
  stsp %>% 
  st_as_sf() %>% 
  st_transform(4269) %>% 
  st_centroid() %>%
  st_join(., puma) %>% 
  mutate(dense = case_when(puma_density >= 3000 ~ 1, TRUE ~ 0)) %>% 
  st_drop_geometry() %>% 
  select(GEOID, puma_density, dense) %>% 
  mutate(GEOID = as.numeric(GEOID))


# troubleshooting ---------------------------------------------------------
# library(sp)
# stsp_transformed <- spTransform(stsp, CRS("+init=epsg:4269"))
# 
# stsf$GEOID <- as.numeric(stsf$GEOID)
# stsf <- stsf[, c('GEOID', 'puma_density', 'dense')]
# 
# save(stsp, file = paste0(data_dir, r_data_folder, 'stsf.RData'))

lag <- left_join(lag, stsf)

# Stop the parallel backend
stopCluster(cl)

save(lag, file = paste0(data_dir, r_data_folder, 'lag_stsf_joined.RData'))
# =====================================================
# Export Data
# =====================================================
save.image(file = paste0(data_dir, r_data_folder, '3_lag_vars_everything.RData'))

write_csv(lag, file = paste0(data_dir, "/data/outputs/lags/lag.csv"))

