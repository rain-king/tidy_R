library(tidyverse)

# aesthetics
# exercises
mpg |> 
  ggplot(aes(x = displ, y = hwy)) +
  geom_point(shape = 24, fill = "pink")

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy), color = "blue")

?geom_point

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy, color = displ < 5))

# geometric objects
View(mpg)

ggplot(mpg, aes(x = displ, y = hwy, shape = drv)) +
  geom_smooth()

ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() + 
  geom_smooth(aes(linetype=drv), se = TRUE)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_point(
    data = mpg |> filter(class == "2seater"),
    color = "red"
  ) +
  geom_point(
    data = mpg |> filter(class == "2seater"),
    shape = "circle open", size = 3, color = "red"
  )

library(ggridges)
ggplot(mpg, aes(x = hwy, y = drv, fill = drv, color = drv)) +
  geom_density_ridges(alpha = 0.5, show.legend = FALSE)

# exercises
ggplot(mpg, aes(x = displ, y = hwy, color = drv, fill = drv)) +
  geom_line()

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(size = 3) +
  geom_smooth(se = FALSE)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth(aes(group = drv), se = FALSE, linewidth = 2) +
  geom_point(size = 3)
  

ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point(size = 3) +
  geom_smooth(se = FALSE)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv), size = 3) +
  geom_smooth(se = FALSE, linewidth = 2)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv), size = 3) +
  geom_smooth(aes(linetype =  drv),se = FALSE, linewidth = 2)
?aes

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(size = 4, color = "white", fill = "white") +
  geom_point(aes(color = drv), size = 2)

# facets
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_grid(drv ~ cyl)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_wrap(~cty)

# exercises
ggplot(mpg) +
  geom_point(aes(x = drv, y = cyl))

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_wrap(~drv)

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_wrap(~cyl)
?facet_wrap

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_wrap(~ class, nrow = 2)

ggplot(mpg, aes(x = displ)) +
  geom_histogram() +
  facet_grid(drv ~ .)
ggplot(mpg, aes(x = displ)) +
  geom_histogram() +
  facet_grid(. ~ drv)

# these two plots are similar
ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) +
  facet_wrap(~drv, ncol = 1)

# statistical transformation 
ggplot(diamonds, aes(x = cut)) +
  geom_bar()

View(diamonds)
diamonds
?diamonds
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point()


diamonds |> 
# use count()
  summarize(n=n(), .by = cut) |> 
  arrange(cut) |> 
  ggplot(aes(x = cut, y = n)) +
  labs(y = "count") +
  geom_col()

diamonds |>
  count(cut) |>
  ggplot(aes(x = cut, y = n)) +
  geom_bar(stat = "identity")

ggplot(diamonds, aes(x = cut, y = after_stat(prop), group = 1)) +
  geom_bar()
