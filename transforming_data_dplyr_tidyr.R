### CLASS 3 09-11-2024

library(dplyr)
library(ggplot2)

storms |>
  filter(
    name %in% c("Maria", "Marie", "Mary", 
                "Marcus", "Michael")
  ) |>
  distinct(name, year)

# What is one other question we can answer with this dataset?
# How many observations are hurricanes?

storms |>
  filter(status == "hurricane")  |>
  distinct(name, year)

# how many observations were category 3?

storms |>
  select(name, year, category) |>
  filter(category == 3)

# manipulate variables

storms |> select(
  any_of(c("year", "name"))
)

storms |>
  select(
    ends_with(
      "diameter"
      )
  )

storms |>
  select(
    where(is.numeric)
  )

ggplot(storms)+
  geom_point(aes(wind, pressure))

storms |>
  mutate(
    wind_pressure_ratio = wind/pressure
  ) |>
  select(
    name, year, contains(c("wind","pressure"))
  ) |>
  ggplot(aes(wind_pressure_ratio)) +
    geom_histogram()

### PARSONS PROBLEMS

# wind > 130
# storms |> 
#   filter(
#   library(dplyr)
#   year == 2010,
# )

# 423516

library(dplyr)

storms |> 
  filter(year == 2010,
         wind > 130
         )

storms |> 
  select(c(wind, pressure, ends_with("diameter")))

# mutate()

library(dplyr)

storms |>
  mutate(ratio = pressure/wind,
         .before = everything()) |>
  arrange((ratio)) |>
  relocate(wind, pressure, status, category, .after = ratio)

storms |>
  group_by(year, name) |>
  mutate(lag_wind = lag(wind),
         wind_change = wind-lag_wind,
         lag_status = lag(status),
         .before = everything()) |>
  View()


### TRANSFORMING SPATIAL DATA

library(sf)
options(tigris_use_cache = TRUE)

storms_sf <- st_as_sf(storms, coords = c("long", "lat"), crs = 4326)

us_states <- tigris::states()

storms_sf <- st_transform(storms_sf, crs = 3857)
us_states <- st_transform(us_states, crs = 3857)

filter(
  storms_sf,
  wind > 50
) # attribute filter

st_int_storms <- function(x,y) {
  filter(
  x,
  as.logical(st_intersects(geometry, y, sparse = T))
)} # spatial filter

bracket_storms <- function(x,y) {x[y,]} # same filter, different syntax

st_flt_storms <- function(x,y) {
  x |>
    st_filter(y)
}

microbenchmark::microbenchmark(st_int_storms(storms_sf, us_states),
                               bracket_storms(storms_sf, us_states),
                               st_flt_storms(storms_sf, us_states)) # benchmark

storms_categories <- storms_sf |>
  group_by(category) |>
  summarise(); plot(storms_categories)

storms_tracks <- storms_sf |>
  group_by(year, name) |>
  summarise(do_union = F) |>
  st_cast("LINESTRING"); plot(storms_tracks["year"])


# which states have been touched by a category 4 storm?

us_states |>
  st_filter(filter(storms_sf, category == 4))
