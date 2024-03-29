library(tidyverse)
library(palmerpenguins)
library(ggthemes)

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()

# set of exercises 1
ggplot(
  data = penguins,
  mapping = aes(x = bill_length_mm, y = bill_depth_mm)
) +
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method="lm", mapping = aes(color = species, shape = species)) +
  scale_color_colorblind()

ggplot(
  data = penguins,
  mapping = aes(x = bill_depth_mm, y = species)
) +
  geom_boxplot(mapping = aes(color = species, shape = species), na.rm = TRUE) +
  labs(
    caption = "Data comes from the palmerpenguins package"
  ) +
  scale_color_colorblind()

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = bill_depth_mm)
) +
  geom_point() +
  geom_smooth()

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = island, shape = island)) +
  geom_smooth(se = TRUE) +
  scale_color_colorblind()

# section examples
ggplot(penguins, aes(x=fct_infreq(species))) +
  geom_bar()

ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200)

ggplot(penguins, aes(x = body_mass_g)) +
  geom_density()

# set of exercises 2
ggplot(penguins, aes(y = fct_infreq(species))) +
  geom_bar()

ggplot(penguins, aes(x = species)) +
  geom_bar(color = "red")
ggplot(penguins, aes(x = species)) +
  geom_bar(fill = "red")

?geom_histogram #bins variable

View(diamonds)
ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.01, fill = "blue")
ggplot(diamonds, aes(x=carat)) +
  geom_density()

# visualizing relationships
View(penguins)
ggplot(penguins, aes(x = species, y = body_mass_g))+
  geom_boxplot()

ggplot(penguins, aes(x = body_mass_g, color = species))+
  geom_density(linewidth = 0.75)

ggplot(penguins, aes(x=body_mass_g, color=species, fill=species))+
  geom_density(alpha=0.5)

ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar()

ggplot(penguins, aes(x=island, fill=species)) +
  geom_bar(position="fill")

# three or more variables
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = island))

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  facet_wrap(~island)

#exercises
View(mpg)
ggplot(mpg, aes(x=displ, y=hwy, color = year)) +
  geom_point()+
  geom_smooth(method="lm")

ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm,
                     color = species, shape = species))+
  geom_point()+
  geom_smooth(method="lm")

ggplot(
  data = penguins,
  mapping = aes(
    x = bill_length_mm, y = bill_depth_mm,
    color = species, shape = species
  )
) +
  geom_point() +
  labs(color = "Species", shape="Species")

# % of island population by species
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")
# % of species in each island
ggplot(penguins, aes(x = species, fill = island)) +
  geom_bar(position = "fill")

?ggsave
