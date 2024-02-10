# =====================================================
# =====================================================
# DISPLACEMENT TYPOLOGY SET UP
# =====================================================
# =====================================================

if (!require(pacman)) install.packages("pacman")
pacman::p_load(googledrive, bit64, fs, data.table, tigris, tidycensus, tidyverse, spdep)

# 2/9/2024: I could not find this package.
# install.packages('colorout')

# options(width = Sys.getenv('COLUMNS'))

# =====================================================
# Pull in data
# =====================================================
# Note: Adjust the cities below if there are additional cities - 
# add your city here by importing corresponding database 
# you will need to update the 'data_dir' variable to the directory
# you're using

data_dir <- 'C:\\Users\\madou\\OneDrive - UCLA IT Services\\1)_PS-Honors\\police_killings_github\\udp_expansion_matt\\data\\outputs\\databases\\'
# csv_files <- fs::dir_ls(data_dir, regexp = "2018.csv$")
df <- read_csv(file = paste0(data_dir, 'zillow_database_2018.csv'))

# =====================================================
# Create rent gap and extra local change in rent
# =====================================================
#
# Tract data
# -----------------------------------------------------
# Note: Make sure to extract tracts that surround cities. For example, in 
# Memphis and Chicago, TN, MO, MS, and AL are within close proximity of 
# Memphis and IN is within close proximity of Chicago. 

### Tract data extraction function: add your state here
st <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", 
        "DE", "DC", "FL", "GA", "HI", "ID", "IL", 
        "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
        "MA", "MI", "MN", "MS", "MO", "MT", "NE", 
        "NV", "NH", "NJ", "NM", "NY", "NC", "ND", 
        "OH", "OK", "OR", "PA", "RI", "SC", "SD", 
        "TN", "TX", "UT", "VT", "VA", "WA", "WV", 
        "WI", "WY")

tr_rent <- function(year, state){
    get_acs(
        geography = "tract",
        variables = c('medrent' = 'B25064_001'),
        state = state,
        county = NULL,
        geometry = FALSE,
        cache_table = TRUE,
        output = "tidy",
        year = year,
        keep_geo_vars = TRUE
        ) %>%
    select(-moe) %>% 
    rename(medrent = estimate) %>% 
    mutate(
        county = str_sub(GEOID, 3,5), 
        state = str_sub(GEOID, 1,2),
        year = str_sub(year, 3,4) 
    )
}

### Loop (map) across different states
tr_rents18 <- 
    map_dfr(st, function(state){
        tr_rent(year = 2018, state) %>% 
        mutate(COUNTY = substr(GEOID, 1, 5))
    })

tr_rents12 <- 
    map_dfr(st, function(state){
        tr_rent(year = 2012, state) %>% 
        mutate(
            COUNTY = substr(GEOID, 1, 5),
            medrent = medrent*1.07)
    })
gc()

tr_rents <- 
    bind_rows(tr_rents18, tr_rents12) %>% 
    unite("variable", c(variable,year), sep = "") %>% 
    group_by(variable) %>% 
    spread(variable, medrent) %>% 
    group_by(COUNTY) %>%
    mutate(
        tr_medrent18 = 
            case_when(
                is.na(medrent18) ~ median(medrent18, na.rm = TRUE),
                TRUE ~ medrent18
            ),
        tr_medrent12 = 
            case_when(
                is.na(medrent12) ~ median(medrent12, na.rm = TRUE),
                TRUE ~ medrent12),
        tr_chrent = tr_medrent18 - tr_medrent12,
        tr_pchrent = (tr_medrent18 - tr_medrent12)/tr_medrent12,
        rm_medrent18 = median(tr_medrent18, na.rm = TRUE), 
        rm_medrent12 = median(tr_medrent12, na.rm = TRUE)) %>% 
    select(-medrent12, -medrent18) %>% 
    distinct() %>% 
    group_by(GEOID) %>% 
    filter(row_number() == 1) %>% 
    ungroup()

# If you have more than one state, use the following 
# code to knit together multiple states
#To add your state, duplicate the second to last line (including %>%) and add state
# abbreviation in "" (after 'tracts(')

gc()
save.image(file = '3_create_lags.RData')
load('3_create_lags.RData')
states <- raster::union(tracts(st[1], cb = TRUE, class = 'sp', year = 2018),
                                 tracts(st[2], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[3], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[4], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[5], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[6], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[7], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[8], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[9], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[10], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[11], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[12], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[13], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[14], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[15], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[16], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[17], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[18], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[19], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[20], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[21], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[22], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[23], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[24], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[25], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[26], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[27], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[28], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[29], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[30], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[31], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[32], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[33], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[34], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[35], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[36], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[37], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[38], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[39], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[40], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[41], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[42], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[43], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[44], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[45], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[46], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[47], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[48], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[49], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[50], cb = TRUE, class = 'sp', year = 2018))
print('Done')
states <- raster::union(states, tracts(st[51], cb = TRUE, class = 'sp', year = 2018))
print('Done')
save(list = states, file = 'states.RData')

stsp <- states

# join data to these tracts
stsp@data <-
    left_join(
        stsp@data %>% 
        mutate(GEOID = case_when(
            !is.na(GEOID.1) ~ GEOID.1, 
            !is.na(GEOID.2) ~ GEOID.2, 
            !is.na(GEOID.1.1) ~ GEOID.1.1, 
            !is.na(GEOID.1.2) ~ GEOID.1.2, 
            !is.na(GEOID.1.3) ~ GEOID.1.3, 
            !is.na(GEOID.1.4) ~ GEOID.1.4, 
            !is.na(GEOID.1.5) ~ GEOID.1.5), 
    ), 
        tr_rents, 
        by = "GEOID") %>% 
    select(GEOID:rm_medrent12)

#
# Create neighbor matrix
# -----------------------------------------------------
    coords <- coordinates(stsp)
    IDs <- row.names(as(stsp, "data.frame"))
    stsp_nb <- poly2nb(stsp) # nb
    lw_bin <- nb2listw(stsp_nb, style = "W", zero.policy = TRUE)

    kern1 <- knn2nb(knearneigh(coords, k = 1), row.names=IDs)
    dist <- unlist(nbdists(kern1, coords)); summary(dist)
    max_1nn <- max(dist)
    dist_nb <- dnearneigh(coords, d1=0, d2 = .1*max_1nn, row.names = IDs)
    spdep::set.ZeroPolicyOption(TRUE)
    spdep::set.ZeroPolicyOption(TRUE)
    dists <- nbdists(dist_nb, coordinates(stsp))
    idw <- lapply(dists, function(x) 1/(x^2))
    lw_dist_idwW <- nb2listw(dist_nb, glist = idw, style = "W")
    
#
# Create select lag variables
# -----------------------------------------------------

    stsp$tr_pchrent.lag <- lag.listw(lw_dist_idwW,stsp$tr_pchrent)
    stsp$tr_chrent.lag <- lag.listw(lw_dist_idwW,stsp$tr_chrent)
    stsp$tr_medrent18.lag <- lag.listw(lw_dist_idwW,stsp$tr_medrent18)

# =====================================================
# Join lag vars with df
# =====================================================

lag <-  
    left_join(
        df, 
        stsp@data %>% 
            mutate(GEOID = as.numeric(GEOID)) %>%
            select(GEOID, tr_medrent18:tr_medrent18.lag)) %>%
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

# =====================================================
# PUMA
# =====================================================

puma <-
    get_acs(
        geography = "public use microdata area", 
        variable = "B05006_001", 
        year = 2018, 
        # wide = TRUE, 
        geometry=TRUE, 
        state = st, 
        keep_geo_vars = TRUE
    ) %>% 
    mutate(
        sqmile = ALAND10/2589988, 
        puma_density = estimate/sqmile
        ) %>% 
    rename(PUMAID = GEOID)

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

lag <- left_join(lag, stsf)
                  
# =====================================================
# Export Data
# =====================================================

# saveRDS(df2, "~/git/displacement-typologies/data/rentgap.rds")
fwrite(lag, "~/git/displacement-typologies/data/outputs/lags/lag.csv")
