# Install and load the terra package
# install.packages("terra")
library(terra)

# Convert the SpatialPolygonsDataFrame to a 'terra' object
terra_poly <- vect(stsp)

# Check if there are any self-intersections in the polygons
all(is.valid(terra_poly))



library(sf)

# Convert the SpatialPolygonsDataFrame to sf object manually
stsp_as_sf <- st_as_sf(stsp, coords = 1:2)

# Add attributes
stsp_as_sf <- stsp_as_sf %>% st_set_geometry(NULL) %>% st_sf(stsp)


# Capture the output of str(stsp)
output <- capture.output(str(stsp))

# Specify the file path for the output .txt file
file_path <- "stsp_structure.txt"

# Write the captured output to a .txt file
writeLines(output, file_path)


stsf <- 
  stsp %>% 
  st_as_sf() %>% 
  st_transform(4269) %>% 
  st_centroid() %>%
  st_join(., puma) %>% 
  mutate(dense = case_when(puma_density >= 3000 ~ 1, TRUE ~ 0)) %>% 
  st_drop_geometry()
  

# Load required libraries
library(sp)

# Assuming `stsp` is your SpatialPolygonsDataFrame
# Perform the necessary operations
stsp$dense <- ifelse(stsp$puma_density >= 3000, 1, 0)

# Drop geometry if necessary
stsp <- stsp[, !grepl("^geometry$", names(stsp))]

# Resulting object stsp will have the new 'dense' column added without converting to sf
