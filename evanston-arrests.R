# Evanston Police Dataset 

#* Assignment:
#* 1) Learn the API and pull the evanston police dataset into R
#* Police dataset link: https://data.cityofevanston.org/Police/Evanston-Arrests/25em-v4fn
#* API docs: https://dev.socrata.com/foundry/data.cityofevanston.org/25em-v4fn
#* RSocrata: https://github.com/Chicago/RSocrata

install.packages("RSocrata")

library("RSocrata")
library("tidyverse")

arrests <- 
  as_tibble(
    read.socrata("https://data.cityofevanston.org/resource/25em-v4fn.json")
    ) %>%  
  mutate(
      day_of_the_week = factor(day_of_the_week),
      day_of_the_week = fct_relevel(
        day_of_the_week, 
        values = c(
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
  mutate(Year = as.numeric(Year)) %>%
  filter(Year > 2016)
  

#Race and age
#Race and gender
#Arrests by day of week
#Arrests by month
#Arrests by year

ggplot(arrests, aes(x = day_of_the_week))+
  geom_bar()+
  facet_wrap(~Year)

ggplot(arrests, aes(x = Year))+
  geom_bar()

arrests %>%
  count(Year)

ggplot(arrests, aes(x = Month))+
  geom_bar() +
  facet_wrap(~Year, nrow = 1)

chi_data_df <- 
  arrests %>% 
  count(Year, Month) %>%
  pivot_wider(
    id_cols = Month, 
    names_from = Year, 
    values_from = n, 
    names_prefix = "m_"
    )

chi_data_mat <- chi_data_df %>% select(-Month) %>% as.matrix(.)
rownames(chi_data_mat) <- chi_data_df$Month

# chi-square test of independence
chisq.test(chi_data_mat) # significant!

# Assignment
# 1. Compute and plot the average age of the arrests across males and females
# 2. Average age across races
# 3. Average age across sex and race


