library(tidyverse)

# how to tidy these tables?
# tidy
table1

# group by (country, year) and mutate to add cases and population as columns
table2
tidy_table2 <- table2 |> 
  pivot_wider(
    names_from = type,
    values_from = count
  )
tidy_table2 == table1

?pivot_wider

# match "[number]/" in rate to cases, and "/[number]" to population
table3 |> 
  separate_wider_delim(rate, delim = "/", names = c("cases", "population")) |> 
  mutate(
    cases = parse_number(cases),
    population = parse_number(population)
  )

billboard
billboard_longer <- billboard |>
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE
  ) |>
  mutate(
    week = parse_number(week)
  )

?pivot_longer
?starts_with

billboard_longer

billboard_longer |> 
  ggplot(aes(x = week, y = rank, group = track)) +
  geom_line(alpha = 0.25, linewidth = 1) +
  scale_y_reverse()
?geom_line

who2
who2 |> 
  pivot_longer(
    cols = !c(country,year),
    names_to = c("diagnosis", "gender", "age"),
    names_sep = "_",
    values_to = "count"
  )

# mixed variables and values
household
household |>
  pivot_longer(
    cols = !family,
    names_to = c(".value", "child"),
    names_sep = "_",
    values_drop_na = TRUE
  ) |> 
  mutate(child = parse_number(child))

?pull

vignette("pivot", package="tidyr")
