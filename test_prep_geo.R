library(epidscan)
library(sf)
library(dplyr)

# 1. Test SF (Lat/Long)
print("--- Test 1: SF Lat/Long ---")
# Create SF points in WGS84
df_ll <- data.frame(id = 1:2, lat = c(10, 11), lon = c(100, 101))
sf_ll <- st_as_sf(df_ll, coords = c("lon", "lat"), crs = 4326)

geo_ll <- prep_geo(sf_ll, loc_id = id)
print(geo_ll$spec$coord_type) # Should be "latlong"
# Output should be Lat(10,11), Long(100,101) => coord1=Lat, coord2=Long
print(head(geo_ll$data))

# 2. Test SF (Projected)
print("--- Test 2: SF Projected ---")
# Create SF points in EPSG:3857 (Mercator)
sf_proj <- st_transform(sf_ll, 3857)
geo_proj <- prep_geo(sf_proj, loc_id = id)
print(geo_proj$spec$coord_type) # Should be "cartesian" (auto)
# Output should be X, Y => coord1=X, coord2=Y
print(head(geo_proj$data))

# 3. Test SF (Forced LatLong on Projected -> Warning)
print("--- Test 3: SF Forced LatLong (Expect Warning) ---")
try({
    geo_forced <- prep_geo(sf_proj, loc_id = id, coord_type = "latlong")
    print(geo_forced$spec$coord_type) # "latlong"
    print(head(geo_forced$data))
})

# 4. Test DF (Auto Detect Lat/Long)
print("--- Test 4: DF Auto Lat/Long ---")
df_df_ll <- data.frame(id = 1, x = 100, y = 10) # Long=100, Lat=10
geo_df_ll <- prep_geo(df_df_ll, loc_id = id, coords = c("x", "y"))
print(geo_df_ll$spec$coord_type) # "latlong"
# coord1 should be Lat(10), coord2 should be Long(100)
print(head(geo_df_ll$data))

# 5. Test DF (Auto Detect Cartesian)
print("--- Test 5: DF Auto Cartesian ---")
df_df_cart <- data.frame(id = 1, x = 500000, y = 1000000)
geo_df_cart <- prep_geo(df_df_cart, loc_id = id, coords = c("x", "y"))
print(geo_df_cart$spec$coord_type) # "cartesian"
# coord1 should be X, coord2 should be Y
print(head(geo_df_cart$data))

# 6. Test DF (Forced Cartesian)
print("--- Test 6: DF Forced Cartesian on small numbers ---")
geo_df_forced <- prep_geo(df_df_ll, loc_id = id, coords = c("x", "y"), coord_type = "cartesian")
print(geo_df_forced$spec$coord_type) # "cartesian"
# coord1=X(100), coord2=Y(10)
print(head(geo_df_forced$data))
