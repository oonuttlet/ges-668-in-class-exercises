library(Tmisc)
library(tidyverse)

df <- quartet

df_sum <- df |>
  group_by(set) |>
  summarize(mean_x = mean(x),
            mean_y = mean(y),
            sd_x = sd(x),
            sd_y = sd(y),
            pearsons = cor(x, y))

ggplot(data = df, aes(x = x, y = y)) +
  geom_point()+
  geom_smooth(aes(color = set)
              , method = "lm"
              , alpha = 0.2
              , linewidth = 0.5)+
  facet_wrap(~set)

library(rnaturalearth)
library(sf)
library(tidyverse)

coastline <- ne_coastline()
countries <- ne_countries()

ggplot(storms, aes(wind))+
  geom_histogram(fill = "skyblue")

ggplot(storms,aes(year))+
  geom_bar(fill = "#8899ff")

ggplot(storms, aes(status))+
  geom_bar(fill = "#3355dd")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

storms_sf <- storms |>
  st_as_sf(coords = c("long", "lat"),
           crs = 4269)

ggplot(storms_sf) + 
  geom_sf()

ggplot(storms, aes(lat)) +
  geom_freqpoly(binwidth = 0.5)+
  coord_flip()+
  geom_vline(aes(xintercept = 39.29)) +
  labs(x = "Latitude",
       y = "Storm Frequency")+
  annotate("text", x = 41.5, y = 300, label = "Baltimore, MD (39.7N)")

### CLASS 3 09-11-2024 MAPPING WITH GGPLOT2

ggplot(data = storms_sf)+
  geom_sf(aes(color = status), size = 0.1) +
  geom_sf(data = coastline, size = 0.1)

storms_bbox <- st_bbox(storms_sf)

ggplot(data = storms_sf)+
  geom_sf(aes(color = status), size = 0.3)+
  geom_sf(data = coastline, color = "black")+
  coord_sf(xlim = storms_bbox[c("xmin", "xmax")],
           ylim = storms_bbox[c("ymin", "ymax")])

ggplot(data = countries) +
  geom_sf(fill = "white")+
  geom_sf(data = coastline, color = "black")+
  geom_sf(data = storms_sf, aes(color = status), size = 0.3)+
  coord_sf(xlim = storms_bbox[c("xmin", "xmax")],
           ylim = storms_bbox[c("ymin", "ymax")]) +
  scale_color_brewer(palette = "Set1")

storms_sf_summ <- storms_sf |>
  group_by(year, name) |>
  summarize(category = case_when(
    !all(is.na(category)) ~ max(category, na.rm = T),
    TRUE ~ NA
  )) |>
  st_cast("LINESTRING") |>
  st_transform(3857)

storms_sf_summ_bbox <- storms_sf_summ |>
  st_bbox()

ggplot(countries) +
  geom_sf(fill = "white", color = "black")+
  geom_sf(data = filter(storms_sf_summ, !is.na(category)), aes(color  = category), linewidth = 1) +
  coord_sf(xlim = storms_bbox[c("xmin", "xmax")],
           ylim = storms_bbox[c("ymin", "ymax")])



