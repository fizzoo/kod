library("tidyverse")
library("magrittr")

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))


demo <- tribble(
  ~cut,         ~freq,
  "Fair",       1610,
  "Good",       4906,
  "Very Good",  12082,
  "Premium",    13791,
  "Ideal",      21551
)

ggplot(data = demo) +
  geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")


ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))

ggplot(data = diamonds) +
  stat_smooth(
    mapping = aes(x = cut, y = depth)
  )

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_count() +
  geom_smooth()


ggplot(mpg, aes(x = as.factor(cyl), y=cty)) +
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point()

diamonds %>%
  count(color, cut)

diamonds %>%
  count(color, cut) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
    geom_tile(mapping = aes(fill = n))

ggplot(diamonds) +
  geom_point()

diamonds %>%
  select(carat, cut, clarity, depth) %>%
  gather %>%
  ggplot(aes(x=value)) +
  facet_wrap(~key) +
  geom_bar()

ggplot(diamonds, aes(x=value)) +
  facet_wrap(~variable) +
  geom_histogram()

diamonds %>%
  ## select(carat, depth, table, x, y, z) %>%
  gather %>%
  ggplot(aes(value)) +
  facet_wrap(~key, scales='free_x') +
  stat_count()
