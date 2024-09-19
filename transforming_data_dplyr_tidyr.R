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


