library(tidyverse)
diamonds |> 
  ggplot(aes(x = carat, y = price)) +
  geom_hex()
ggsave("diamonds.png")

write_csv(diamonds, "data/diamonds.csv")