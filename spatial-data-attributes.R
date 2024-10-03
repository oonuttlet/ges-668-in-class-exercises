library(tigris)
library(tidyverse)
library(sf)
library(arcgislayers)
options(tigris_use_cache = T)

us_states <- states(year = 2022)
us_highways <- primary_roads()

st_crs(us_states)
st_crs(us_highways)

st_drop_geometry(us_states)

us_states |>
  st_drop_geometry() |>
  group_by(DIVISION) |>
  summarize(mean_aland = mean(ALAND))

# bad: assigning a CRS that doesn't match the geometry -- DEFINE PROJECTION
# st_crs(us_states) <- 3857 don't run please

# good: using st_transform to convert the geometry to a new CRS -- PROJECT
us_states <- st_transform(us_states, crs = 3857)

storms_sf <- storms |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) |>
  st_transform(3857)

us_states |>
  st_crop(storms_sf) |>
  plot()

storms_wstates <- storms_sf |>
  st_join(us_states, left = F) |>
  glimpse()

loads <- arc_read("https://gis.chesapeakebay.net/ags/rest/services/WIP/Loads_2018_01_07_20/MapServer/0")

cb_counties <- counties(cb = T)
fips_codes <- fips_codes |>
  mutate(GEOID = paste0(state_code, county_code))

cb_counties_joined <- cb_counties |>
  left_join(fips_codes, by = join_by(GEOID)) |>
  mutate(ST = state,
         CNTYNAME = str_to_upper(NAME)) |>
  st_drop_geometry() |>
  distinct(ST, CNTYNAME, .keep_all = T)

loads |>
  left_join(cb_counties_joined,
            by = join_by("ST", "CNTYNAME"))
