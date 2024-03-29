# Evanston Police Dataset 

#* Assignment:
#* 1) Learn the API and pull the evanston police dataset into R
#* Police dataset link: https://data.cityofevanston.org/Police/Evanston-Arrests/25em-v4fn
#* API docs: https://dev.socrata.com/foundry/data.cityofevanston.org/25em-v4fn
#* RSocrata: https://github.com/Chicago/RSocrata

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

ggplot(arrests%>%
         filter(complete.cases(sex)), aes(x = age))+
  geom_histogram(binwidth = 2)+
  coord_cartesian(xlim = c(0,90))+
  scale_x_continuous(breaks = seq(0,90,10))+
  facet_wrap(~sex)

arrests_gender <- arrests %>%
  filter(complete.cases(sex, age)) %>%
  group_by(sex) %>%
  summarise(
    gender_mean = mean(age), 
    gender_median = median(age), 
    gender_sd = sd(age), 
    gender_sample = n(), 
    gender_min = min(age), 
    gender_max = max(age)
    )

t.test(age~sex, data = arrests, alternative = "two.sided")

ggplot(arrests_gender, aes(x = sex,y = gender_mean))+
  geom_boxplot()

arrests_race <- arrests %>%
  filter(complete.cases(race, age))%>%
  group_by(race) %>%
  summarise(
    race_mean = mean(age), 
    race_median = median(age), 
    race_sd = sd(age), 
    race_sample = n(), 
    race_min = min(age), 
    race_max = max(age)
  )

ggplot(arrests_race, aes(x = race, y = race_mean))+
  geom_point(
    data = arrests%>%filter(complete.cases(race, age)), 
    aes(y = age), 
    position = position_jitter(.22), 
    alpha = 1/2, 
    color = "lightgrey", 
    shape = 1)+
  geom_errorbar(
    aes(ymin = race_mean - race_sd, ymax = race_mean + race_sd), 
    width = 0.2, color = "darkolivegreen")+
  geom_point(aes(size = race_sample), color = "darkolivegreen")+
  theme_bw()+
  labs(
    x = "Race", 
    y = "Average Age", 
    caption = "Error bars are sd", 
    title = "Arrests in Evanston by Age and Race"
    )
  
#ASSIGNMENT:
#1. MAKE GENDER MEAN AGE GRAPH
#2. HISTOGRAM OF SEX+RACE WITH AGE AS MEASURING VARIABLE use facet_grid()
#3. CALCULATE ALL THE SUMMARY STATISTICS FOR SEX+RACE AND AGE
#4. REPLICATE THIS MEAN AGE PLOT WITH SEX+RACE

arrests_race_gender <- arrests %>%
  group_by(sex, race) %>%
  summarise(racesex_mean = mean(age, na.rm = TRUE))

ggplot(arrests_race_gender, aes(x = race, y = racesex_mean))+
  geom_boxplot()

ggplot(arrests_gender, aes(x = sex, y = gender_mean))+
  geom_point(
    data = arrests%>%filter(complete.cases(race, age)), 
    aes(y = age), 
    position = position_jitter(.22), 
    alpha = 1/2, 
    color = "lightgrey", 
    shape = 1)+
  geom_errorbar(
    aes(ymin = gender_mean - gender_sd, ymax = gender_mean + gender_sd), 
    width = 0.2, color = "darkolivegreen")+
  geom_point(aes(size = gender_sample), color = "darkolivegreen")+
  theme_bw()+
  labs(
    x = "Sex", 
    y = "Average Age", 
    caption = "Error bars are sd", 
    title = "Arrests in Evanston by Age and Sex"
  )

arrests_final <- arrests%>%
  filter(complete.cases(race, sex))

ggplot(arrests_final, aes(x = age))+
  geom_histogram(binwidth = 2)+
  coord_cartesian(xlim = c(0,90))+
  scale_x_continuous(breaks = seq(0,90,10))+
 facet_grid(race~sex)
  
arrests_racesex <- arrests_final %>%
  filter(complete.cases(age))%>%
  group_by(race, sex) %>%
  summarise(
    m = mean(age), 
    med = median(age), 
    sd = sd(age), 
    n = n(), 
    min = min(age), 
    max = max(age)
  )%>%
  ungroup()

ggplot(arrests_racesex, aes(x = race, y = med))+
  geom_point(
    data = arrests_final, 
    aes(y = age), 
    position = position_jitter(.22), 
    alpha = 1/2, 
    color = "lightgrey", 
    shape = 1)+
  geom_errorbar(
    aes(ymin = med - sd, ymax = med + sd), 
    width = 0.2, color = "darkolivegreen")+
  geom_point(aes(size = n), color = "darkolivegreen")+
  theme_bw()+
  labs(
    x = "Race", 
    y = "Median Age", 
    caption = "Error bars are sd", 
    title = "Arrests in Evanston by Race, Sex, and Age"
  )+
  facet_wrap(~sex)

arrests_bwmale <- arrests_final%>%
  filter(complete.cases(age))%>%
  filter(sex == "Male", race %in% c("White", "Black"))

t.test(age~race, data = arrests_bwmale, alternative = "two.sided")

ggplot(arrests, aes(x = arrest_time))+
  geom_histogram(binwidth = 5)

ggplot(arrests, aes(x = street_number))+
  geom_histogram(binwidth = 100)

arrests %>%
  count(street_number)%>%
  arrange(-n)

evanston_demo <- tibble(race = c("White", "Black", "Asian"), 
                        n = c(50537, 12888, 7186), 
                        percent = c(0.647, 0.165, 0.092))

#ASSIGNMENT 10/23/22
#1. Gather population data for Evanston (race, sex, race+sex)
#Population: 78,110
#White - 50,537 (64.7%), Black - 12,888 (16.5%), Asian - 7,186 (9.2%)
#Female - 40,773 (52.2%), 36,337 (47.8%)
#https://www.census.gov/quickfacts/fact/dashboard/evanstoncityillinois
#2. Organize script into blog post

arrests_race$n <- sum(arrests_race$race_sample)

arrests_race$percent <- 
  arrests_race$race_sample/arrests_race$sum_arrests

#arrests_race %>%
#  mutate(sum_arrests = sum(race_sample), 
#         percent_arrests = race_sample/sum_arrests)

arrests_percent <- rbind(arrests_race %>%
        select(percent, race, n = race_sample), evanston_demo) %>%
  mutate(observe = c(
    "Arrest", 
    "Arrest", "Arrest", "Arrest", "Population",
    "Population", "Population"))

ggplot(data = arrests_percent, aes(x = race, 
                                   y = percent*100, 
                                   group = observe, 
                                   fill = observe))+
  geom_bar(stat = "identity", position = position_dodge(), color = "Black")+
  labs(x = "Race", y = "Percent", fill = "Data Type", title = "Arrests in Evanston by Race Compared to Census Data")+
  theme_minimal()+
  scale_fill_brewer(palette = "Paired")

  
