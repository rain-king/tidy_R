library(nycflights13)
library(tidyverse)
View(flights)

flights |>
  filter(dest == "IAH") |>
  group_by(year, month, day) |>
  summarize(
    arr_delay = mean(arr_delay, na.rm = TRUE)
  )

?mean

flights |>
  filter(month == 1 | month == 2) 

flights |>
  filter(month %in% c(1,2))

flights |>
  arrange(year, month, day, dep_time)

flights |>
  arrange(desc(dep_delay))

flights |>
  distinct(origin, dest, .keep_all = TRUE)

flights |>
  count(origin, dest, sort = TRUE)

# exercises
#1
flights |>
  filter(arr_delay >= 120)

View(flights |>
  filter(dest %in% c("IAH", "HOU")))

?flights
View(airlines)

flights |>
  filter(carrier %in% c("UA", "AA", "DL"))

flights |>
  filter(month %in% c(7,8,9))

flights |>
  filter(dep_delay <= 0 & arr_delay > 120)

# is air_time arr_time - dep_time? no, compare the number of rows
# this is because one can't do modular arithmetic on HHMM time, it's
# done as decimal in R even with module 2400
flights |>
  filter((arr_time - dep_time)%%2400 == air_time)
# hard and wrong solution
flights |>
  filter(dep_delay >= 60, # sched_time - time > 30
         (sched_arr_time - sched_dep_time)%%2400 - (arr_time - dep_time)%%2400
          > 30)
# easy solution (correct solution)
flights |>
  filter(dep_delay >= 60, dep_delay - arr_delay > 30)

flights |>
  arrange(desc(dep_delay))
flights |>
  arrange(dep_time)

View(flights |>
  mutate(speed = distance/air_time*60) |> 
  arrange(desc(speed)))

View(flights |>
  filter(year == 2013) |>
  distinct(month, day)) |> # check for number of entries
  arrange(month, day) # check for leap year

View(flights |>
  group_by(flight, distance) |>
  arrange(desc(distance)) |>
  summarize())

# columns
flights_speed <- flights |>
    group_by(flight, carrier, origin, dest, speed = distance/air_time*60) |>
    summarise()

flights_speed |>
  ggplot(aes(color= carrier, x=reorder(carrier,speed,median), y = speed))+
  geom_boxplot()

delay_gains <- flights |>
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours,
    .keep = "used"
  )

# exercises
flights |>
  select(dep_time, sched_dep_time, dep_delay)

?any_of
variables <- c("year", "month", "day", "dep_delay", "arr_delay")
delay_gains |>
  select(any_of(variables))

flights |> select(contains("TIME", ignore.case = TRUE))
?contains

# move air_time to the beginning and rename
flights |>
  relocate(air_time, .before = dep_time) |>
  rename(air_time_min = air_time)

# section groups

flights |>
  group_by(month) |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE)
  )

carrier_speed <- flights |>
  mutate(speed = distance/air_time*60) |> 
  group_by(carrier) |>
  summarise(
    speed = median(speed, na.rm = TRUE),
    n = n()
  ) |>
  arrange(desc(speed))

merge(airlines, carrier_speed) |>
  arrange(desc(speed))

flights |>
  group_by(dest) |>
  slice_max(arr_delay, n = 1) |>
  relocate(dest) |>
  arrange(desc(arr_delay))

flights |>
  summarize(
    delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .by = c(origin, dest)
  )

# EXERCISES
flights |> 
  group_by(carrier) |> 
  summarise(avg_arr_delay = mean(arr_delay, na.rm = TRUE)) |> 
  arrange(desc(avg_arr_delay))

# flights most delayed at departure per destination
flights |>
  group_by(dest) |>
  slice_max(dep_delay, n = 1) |>
  relocate(dest,dep_delay) |>
  arrange(desc(dep_delay))

flights |>
  group_by(hour) |> 
  summarise(
    median_dep_delay = median(dep_delay, na.rm = TRUE)
    ) |>
  ggplot(aes(x = hour, y = median_dep_delay)) +
  geom_point()

?flights
?slice_min
?count

merge(
  airlines,
  flights |>
    count(carrier)
  ) |>
  arrange(desc(n))

df <- tibble(
  x = 1:5,
  y = c("a", "b", "a", "a", "b"),
  z = c("K", "K", "L", "L", "K")
)

df |>
  group_by(y)
df |>
  arrange(y)
df |>
  group_by(y) |>
  summarize(mean_x = mean(x))
# group_by results in groups y, z, but summarize outputs groups y,
# subsequent group is ungrouped since last and only group is dropped
df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x)) |>
  summarize(mean_mean_x = mean(mean_x))
# in this case summarize does not group
df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x), .groups = "drop")
?summarize
df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x))
df |> # each row shows the mean of its group
  group_by(y, z) |>
  mutate(mean_x = mean(x))

# hard exercise
# compare delay per carrier vs per destination
# considering delays in airports depends on origin rather than destination
View(flights |>
  group_by(carrier, dest) |>
  summarize(
    mean_arr_delay = mean(arr_delay, na.rm = TRUE)
  ) |>
  arrange(desc(mean_arr_delay))
)

flights |> group_by(carrier, dest) |> summarize(n())

# section Case: aggregate and sample size
batters <- Lahman::Batting |>
  group_by(playerID) |>
  summarize(
    performance = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    n = sum(AB, na.rm = TRUE)
  )
batters |> 
  filter(n > 100) |> 
  ggplot(aes(x = n, y = performance)) +
  geom_point(alpha= 1/10)+
  geom_smooth(se = FALSE)

