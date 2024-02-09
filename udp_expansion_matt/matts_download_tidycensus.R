library(sf)
library(tidyverse)
library(tidycensus)
UDP_Recode <- read_csv("UDP_Recode.csv")

# 2018 5-year ACS variables
UDP_Recode_2018 <- UDP_Recode |> 
  filter(Year == 2018)
# names(UDP_Recode_2018$OriginalCode) <- UDP_Recode_2018$Recode
# UDP_Recode_2018$OriginalCode

states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", 
            "DE", "DC", "FL", "GA", "HI", "ID", "IL", 
            "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
            "MA", "MI", "MN", "MS", "MO", "MT", "NE", 
            "NV", "NH", "NJ", "NM", "NY", "NC", "ND", 
            "OH", "OK", "OR", "PA", "RI", "SC", "SD", 
            "TN", "TX", "UT", "VT", "VA", "WA", "WV", 
            "WI", "WY")
acs_2018 <- new.env()
for (state in states) {
  get_acs(
    geography = "tract",
    state = state,
    variables = UDP_Recode_2018$OriginalCode,
    year = 2018,
    survey = "acs5",
    output = "wide",
    geometry = FALSE
  ) |> assign(
    state,
    value = _,
    envir = acs_2018
  )
}
  
acs_2018_noGEO <- st_drop_geometry(acs_2018)

# 2012 5-year ACS variables
UDP_Recode_2012 <- UDP_Recode |> 
  filter(Year == 2012)
# names(UDP_Recode_2012$OriginalCode) <- UDP_Recode_2012$Recode
# UDP_Recode_2012$OriginalCode
acs_2012 <- new.env()
for (state in states) {
  get_acs(
    geography = "tract",
    state = state,
    variables = UDP_Recode_2012$OriginalCode,
    year = 2012,
    survey = "acs5",
    output = "wide",
    geometry = FALSE
  ) |>   assign(
    state,
    value = _,
    envir = acs_2012)
}

acs_2012_noGEO <- st_drop_geometry(acs_2012)


# Merge 2012 and 2018 (same geometries)
merged_2012_2018 <- full_join(
  x = acs_2012_noGEO,
  y = acs_2018_noGEO,
  by = "NAME"
)
merged_2012_2018 <- merged_2012_2018 |> rename(FIPS = GEOID.x)
write_csv(merged_2012_2018, file = "merged_2012_2018.csv")

# sf1 sumfile variables; year 2000
UDP_Recode_2000 <- UDP_Recode |> 
  filter(Year == 2000)
names(UDP_Recode_2000$OriginalCode) <- UDP_Recode_2000$Recode
UDP_Recode_2000$OriginalCode

UDP_Recode_2000_sf1 <- UDP_Recode_2000 |> 
  filter(Recode %in% c("pop_00", "white_00", "hu_00", "ohu_00", "rhu_00")) |> 
  mutate(sumfile = "sf1")
UDP_Recode_2000_sf1

# download 2000 sf1 data

# sf1_2000 <- get_decennial(
#   geography = "tract",
#   state = states,
#   variables = UDP_Recode_2000_sf1$OriginalCode,
#   sumfile = unique(UDP_Recode_2000_sf1$sumfile),
#   year = unique(UDP_Recode_2000_sf1$Year),
#   output = "wide",
#   geometry = FALSE
# )

# sf1_2000_noGEO <- st_drop_geometry(sf1_2000)

# sf3 sumfile variables; year 2000
UDP_Recode_2000_sf3 <- UDP_Recode_2000 |> 
  filter(!Recode %in% c("pop_00", "white_00", "hu_00", "ohu_00", "rhu_00")) |> 
  mutate(sumfile = "sf3")
tail(UDP_Recode_2000_sf3)

# Download sf3; year 2000

# sf3_2000 <- get_decennial(
#   geography = "tract",
#   state = states,
#   variables = UDP_Recode_2000_sf3$OriginalCode,
#   sumfile = unique(UDP_Recode_2000_sf3$sumfile),
#   year = unique(UDP_Recode_2000_sf3$Year),
#   output = "wide",
#   geometry = FALSE
# )

# sf3_2000_noGEO <- st_drop_geometry(sf3_2000)

merged_2000 <- merge(
  sf1_2000_noGEO,
  sf3_2000_noGEO,
  by = "NAME"
)

merged_2000 <- merged_2000 |> rename(FIPS = GEOID.x)

write_csv(merged_2000, file = "merged_2000.csv")

# sf3 sumfile variables; year 1990
UDP_Recode_1990_sf3 <- UDP_Recode |> 
  filter(Year == 1990) |> 
  mutate(sumfile = "sf3")
UDP_Recode_1990_sf3
tail(UDP_Recode_1990_sf3)

# sf3_1990 <- get_decennial(
#   geography = "tract",
#   state = states,
#   variables = UDP_Recode_1990_sf3$OriginalCode,
#   sumfile = "sf3",
#   year = 1990,
#   output = "wide",
#   geometry = FALSE
# )

# 1990 sf3 files from Social Explorer
# API call has been disabled

# R13437364_SL140 <- read_csv("C:/c_delete/R13437364_SL140.csv")
# UDP_Recode <- read_csv("UDP_Recode.csv")

# orig_colnames <- colnames(R13437364_SL140)

colnames(R13437364_SL140) <- orig_colnames
colnames(R13437364_SL140) <- colnames(R13437364_SL140) |> 
  gsub(pattern = "Geo_", replacement = "") |> 
  gsub(pattern = "STATE", replacement = "state") |> 
  gsub(pattern = "COUNTY", replacement = "county") |> 
  gsub(pattern = "TRACT", replacement = "tract") |> 
  gsub(pattern = "STF3_", replacement = "") |> 
  gsub(pattern = "A_", replacement = "A") |> 
  gsub(pattern = "_", replacement = "0")

UDP_Recode_1990 <- UDP_Recode |> 
  filter(Year == 1990)

R13437364_SL140_subset <-  R13437364_SL140[,c(1:10, which(colnames(R13437364_SL140) %in% UDP_Recode_1990$OriginalCode))]

# Rename columns 

indices <- match(colnames(R13437364_SL140_subset), UDP_Recode_1990$OriginalCode)[colnames(R13437364_SL140_subset) %in% UDP_Recode_1990$OriginalCode]

colnames(R13437364_SL140_subset) <- c(colnames(R13437364_SL140_subset)[1:10], UDP_Recode_1990$Recode[indices])

#Subset for Illinois
IL_R13437364_SL140 <- R13437364_SL140_subset |> filter(state == "17" & county == "031")

write_csv(R13437364_SL140_subset, file = "R13437364_SL140_subset.csv")
write_csv(R13437364_SL140_subset, file = "R13437364_SL140_1990.csv")

