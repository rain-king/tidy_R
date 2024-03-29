library(tidyverse)

students <- read_csv(
  "https://raw.githubusercontent.com/hadley/r4ds/main/data/students.csv"
)

write_csv(students, "data/students.csv")

students <- read_csv("data/students.csv", na = c("N/A", "NA", ""))

students |> janitor::clean_names()

students <- students |> # can be done with janitor::clean_names
  rename(
    student_id = `Student ID`,
    full_name = `Full Name`
  )

students <- students |> 
  janitor::clean_names()

students <- students |> 
  mutate(
    meal_plan = factor(meal_plan),
    age = parse_number(if_else(age == "five", "5", age))
  )

students

# exercises
?read_csv()
?read_tsv()

annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)

annoying |>
  rename(one = `1`, two = `2`)

annoying |> 
  select(`1`)

annoying |> 
  ggplot(aes(x=`1`, y=`2`)) +
  geom_point()

annoying <- annoying |>
  mutate(
    `3` = `2` / `1`
  )
annoying

annoying |>
  rename(one = `1`, two = `2`, three = `3`)
