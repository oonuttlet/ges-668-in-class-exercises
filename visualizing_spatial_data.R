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
  st_as_sf(coords = c("long", "lat"))

ggplot(storms_sf) + 
  geom_sf()

ggplot(storms, aes(lat)) +
  geom_freqpoly(binwidth = 0.5)+
  coord_flip()+
  geom_vline(aes(xintercept = 39.7))+
  geom_text(aes(x = 41.5, y = 300), label = "Baltimore, MD (39.7N)")


