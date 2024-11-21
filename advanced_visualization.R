library(tidyverse)
library(sf)
library(hrbrthemes)
library(ragg)

g <- ggplot(mpg, aes(class))

g +
  geom_bar(aes(weight = displ)) +
  theme_modern_rc()

library(ggtext)
library(marquee)

cars <- sub("(\\w+)", "{.red ***\\1***}", rownames(mtcars))
cars

text_box_style <- modify_style(
  classic_style(base_size = 2),
  "body",
  padding = skip_inherit(trbl(10)),
  border_radius = 3
)

md_text <- 
  "# Lorem Ipsum
Lorem ipsum dolor sit amet, *consectetur* adipiscing elit, sed do eiusmod tempor incididunt ut
labore et dolore magna **aliqua**. Ut enim ad minim veniam, quis nostrud exercitation ullamco
laboris nisi ut aliquip ex ea commodo _consequat_. Duis aute irure dolor in reprehenderit in
voluptate velit esse cillum dolore eu fugiat nulla ~pariatur~. Excepteur sint occaecat
cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

ggplot(mtcars) + aes(disp, mpg, label = cars) + 
  geom_marquee(size = 2) + 
  annotate(GeomMarquee, 
           label = md_text, 
           x = 450, 
           y = 35, 
           style = text_box_style, 
           size = 2, 
           fill = "lightgrey",
           width = 0.3, 
           hjust = "right",
           vjust = "top"
  )

library(cols4all)

cols4all::c4a_gui()

library(plotly)

g1 <- ggplot(mtcars) + aes(disp, mpg, label = cars) +
  geom_point()

ggplotly(g1)
