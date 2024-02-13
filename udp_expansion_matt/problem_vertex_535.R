# Assuming combined_tracts is a SpatialPolygonsDataFrame
class(stsp)

# Access the edges of combined_tracts
edges <- stsp@polygons

# Assuming each element of edges represents a polygon
# You may need to adjust this based on the actual structure of your data
# Iterate over the polygons to find Edge 535
for (i in 1:length(edges)) {
  polygon <- edges[[i]]
  # Assuming polygon@ID represents the ID of each polygon
  if (polygon@ID == 535) {
    # Print or inspect the polygon to identify Edge 535
    print(polygon)
    break  # Break the loop once Edge 535 is found
  }
}
