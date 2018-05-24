library(tidyverse)

f <- png::readPNG("featr.png")[, , 1]
featrco <- png::readPNG("featrco.png")[, , 2]

set.seed(42)

as_tibble(featrco) %>% 
  add_column(y = seq_len(nrow(featrco))) %>% 
  gather(x, value, -y) %>% 
  mutate(
    x = parse_number(x), 
    value = 1 - value + rnorm(length(featrco), 0, 0.06) + rbernoulli(1, 0.6)
  ) %>% 
  # filter(between(y, 8, 56)) %>%
  ggplot(aes(x, y, fill = value)) + 
  geom_tile(show.legend = FALSE) + 
  coord_equal() + 
  scale_y_reverse() +
  theme(text = element_blank(), line = element_blank(), panel.background = element_blank()) +
  scale_fill_gradientn(colours = c("white", rwave::wmpalette[4]))
  