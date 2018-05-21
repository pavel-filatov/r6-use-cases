library(tidyverse)

set.seed(42)

temp <- cross_df(list(x = 1:1e2, y = 1:1e2))


temp %>% 
  mutate(z = ((x - 50) ** 2 + (y + 30) ** 2) + rnorm(1e4, 0, .3)) %>% 
  ggplot(aes(x, y, fill = z)) +
  geom_tile(show.legend = FALSE) + 
  scale_fill_gradientn(colours = c(rwave::wmpalette[c(1:4)])) + 
  theme(
    line = element_blank(),
    text = element_blank(),
    panel.background = element_blank()
  ) + 
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  coord_equal()



