library("RSocrata")
library("RColorBrewer")
library("tidyverse")

arrests <- 
  as_tibble(
    read.socrata("https://data.cityofevanston.org/resource/25em-v4fn.json")
  ) %>%  
  mutate(
    day_of_the_week = factor(day_of_the_week),
    day_of_the_week = fct_relevel(
      day_of_the_week, 
      c(
        "Mon", 
        "Tue", 
        "Wed", 
        "Thu", 
        "Fri", 
        "Sat", 
        "Sun" 
      )
    )
  ) %>%
  separate(
    col = arrest_date, 
    into = c("Year", "Month", "Date"), 
    remove = FALSE
  ) %>%
  mutate(Year = as.numeric(Year), 
         age = as.numeric(age), 
         arrest_time = as.numeric(arrest_time),
         street_number = as.numeric(street_number)) %>%
  filter(Year > 2016)

arrests_stats <- arrests%>%
  filter(!is.na(age))%>%
  summarise(
    mean_age  = mean(age), 
    sd_age = sd(age), 
    n = n(), 
    median_age = median(age),
    margin = qt(0.975,df=n-1)*sd_age/sqrt(n),
    ll = mean_age - margin,
    ul = mean_age + margin)

ggplot(arrests, aes(x = age))+
  geom_histogram(binwidth = 2)+
  geom_vline(xintercept = arrests_stats$mean_age)

arrests_ci <- arrests%>%
  filter(!is.na(age))

t.test(age~1, data = arrests_ci)

bootres <- 1:2000%>%
  map(~sample(x = arrests_ci$age, replace = TRUE))

bootres_df <- bootres%>%
  map_dfr(~as_tibble(.x), .id = "ITER")

ggplot(bootres[[1]], aes())

ggplot(bootres_df %>%
         filter(ITER == "1"), aes(x = value))+
  geom_density()


ggplot(bootres_df, aes(x = value, group  = ITER))+
  geom_density()
  

# Assignment 12 FEB 2023 ----
# 1 calculate the mean, sd, median for each iteration
# 2 plot the mean of each iteration in ggplot
# 3 calculate the 95% confidence interval of all the iterations combined
# 3 tip use the means from step #1 to calculate the 95% CI