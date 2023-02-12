library(tidyverse)

random_num <- 1000
n <- vector(mode = "list", length = random_num)

for (i in 1:random_num) {
 n[[i]] <- matrix(rnorm(i^2), nrow = i)
}

1:10 %>%
  map(~rnorm(1000)) %>%
  map(~tibble(mean = mean(.x), sd = sd(.x), n = length(.x)))


# 1.29.23 FINISH ARRESTS BLOG POST



df <- 2:1000 %>%
  map(~rnorm(.x)) %>%
  map_dfr(~tibble(mean = mean(.x), sd = sd(.x), n = length(.x)))

ggplot(df, aes(x = n, y = abs(mean)))+
  geom_point()+
  geom_smooth()

ggplot(df, aes(x = n, y = sd))+
  geom_point()+
  geom_smooth()

# 2.5.23

# assignment:
# finish up the blogpost
# using map() from purrr:
# 1) select ages from arrest time at random with replacement 2,000 times
# 2) plot mean and sd from the distributions
# https://mattkmiecik.shinyapps.io/boot-perm-dash/