library(tidyverse)

ggplot(diamonds, aes(x = carat)) +
  geom_bar(width = 0.5)

diamonds |>
  filter(carat < 1) |> 
  ggplot(aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

# unusual values
ggplot(diamonds, aes(x = y)) +
  geom_histogram(binwidth = 0.5)

ggplot(diamonds, aes(x = y)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

# unusual
diamonds |>
  filter(y < 3 | y > 20) |>
  select(price, x, y, z) |>
  arrange(y)

# exercises
ggplot(diamonds, aes(x = x))+
  geom_histogram(binwidth = 0.1)
#  coord_cartesian(ylim = c(0,50))

ggplot(diamonds, aes(x = y))+
  geom_histogram(binwidth = 0.1)

ggplot(diamonds, aes(x = z)) +
  geom_histogram(binwidth = 0.1) +
  coord_cartesian(ylim = c(0,50))

View(diamonds |> 
  filter(x == 0 | y == 0 | z == 0 |
           x > 10 | y > 10 | z > 10))

# unusual values
diamonds_zero_as_na <- diamonds |> 
  mutate(
    y = if_else(y < 3 | y > 20, NA, y),
    z = if_else(z == 0, NA, z),
    x = if_else(x == 0, NA, x),
  ) 

diamonds_zero_as_na |> 
  #filter(is.na(x) | is.na(y) | is.na(z))
  ggplot(aes(x = x, y = y)) +
  geom_point(alpha = 0.5)

nycflights13::flights |>
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + (sched_min / 60)
  ) |>
  ggplot(aes(x = sched_dep_time)) +
  geom_freqpoly(aes(color = cancelled), binwidth = 1/4)

# exercises
diamonds_zero_as_na |> 
  ggplot(aes(x = x)) +
  geom_histogram(binwidth = 0.2)

diamonds_zero_as_na |> 
  ggplot(aes(x = x)) +
  geom_bar()

nycflights13::flights |>
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + (sched_min / 60)
  ) |>
  ggplot(aes(x = sched_dep_time)) +
  geom_freqpoly(binwidth = 1/4) +
  facet_wrap(~cancelled, scales = "free_y")

# a categorical and a numerical variable
ggplot(diamonds, aes(x = price)) +
  geom_freqpoly(aes(color = cut), binwidth = 500, linewidth = 1)
?after_stat
ggplot(diamonds, aes(x = price, y = after_stat(density))) +
  geom_freqpoly(aes(color = cut), binwidth = 500, linewidth = 0.75)

ggplot(diamonds, aes(x = fct_reorder(cut, price, median), y = price)) +
  geom_boxplot()

# exercises
library(lvplot)
ggplot(diamonds, aes(x = cut, y = price)) +
  geom_lv()
?geom_lv

ggplot(diamonds, aes(x = cut, y = price)) +
  geom_violin()

ggplot(diamonds, aes(x = price)) +
  geom_histogram() +
  facet_wrap(~cut)

ggplot(diamonds, aes(x = price)) +
  geom_freqpoly(aes(color = cut))

ggplot(diamonds, aes(x = price)) +
  geom_density(aes(color = cut), linewidth = 1)

ggplot(diamonds, aes(x = cut, y = price)) +
  geom_jitter()

# why are fair diamonds more expensive, let's check weight volume
diamonds_zero_as_na |> 
  mutate(
    cut,
    volume = x * y * z,
    price_to_weight_and_volume = price / carat / volume,
    weight_volume = carat * volume
  ) |> 
  ggplot(aes(x = weight_volume |> log(), y = price)) +
  geom_point() +
  facet_wrap(~cut)
