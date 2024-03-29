library(tidyverse)

ggplot(diamonds, aes(x = carat)) +
  geom_bar(width = 0.5)

