getwd()
facebook_data <- read.csv('pseudo_facebook.tsv', sep = '\t')
library(ggplot2)
library(dplyr)
age_group <- group_by(pf, age)
pf.fc_by_age <- summarise(age_group,
                          friend_count_mean = mean(friend_count),
                          friend_count_median = median(friend_count),
                          n = n())
head(pf.fc_by_age)

new_groupings <- group_by(pf, age, gender)
pf.fc_by_age_gender <- summarise(new_groupings,
                          friend_count_mean = mean(friend_count),
                          friend_count_median = median(friend_count),
                          n = n())
head(pf.fc_by_age_gender)

ggplot(aes(x = age, y = friend_count),
       data = subset(pf, !is.na(gender))) +
  geom_line(aes(color = gender), stat = 'summary', fun.y = median)

pf.fc_by_age_gender <- pf %>%
  group_by(age, gender) %>%
  summarise(friend_count_mean = mean(friend_count),
            friend_count_median = median(friend_count),
            n = n()) %>%
  ungroup() %>%
  arrange(age)
head(pf.fc_by_age_gender)

install.packages('reshape2')
library(reshape2)

pf.fc_by_age_gender.wide <- dcast(pf.fc_by_age_gender, age ~ gender, value.var = 'friend_count_median')
head(pf.fc_by_age_gender.wide)


ggplot(aes(x = age, y = female/male), data = pf.fc_by_age_gender.wide) +
  geom_line() +
  geom_hline(yintercept = 1, alpha = 0.3, linetype = 2)

pf$year_joined <- floor(2014 - pf$tenure/365)

summary(pf$year_joined)
table(pf$year_joined)

year_joined.bucket <- cut(pf$year_joined, c(2004, 2009, 2011, 2012, 2014))


table(year_joined.bucket)

ggplot(aes(x = age, y = friend_count),
       data = subset(pf, !is.na(year_joined.bucket))) +
  geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = median)
summary(pf$year_joined.bucket)

library(gridExtra)
ggplot(aes(x = age, y = friend_count), 
       data = subset(pf, !is.na(gender) & !is.na(year_joined.bucket))) + 
  geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = mean) +
  geom_line(stat = "summary", fun.y = mean, linetype = 2)

with(subset(pf, tenure >= 1), summary(friend_count / tenure))

ggplot(aes(x = tenure, y = friendships_initiated / tenure),
       data = subset(pf, tenure >= 1)) +
  geom_line(color = year_joined.bucket)

ggplot(aes(x = 7 * round(tenure / 7), y = friendships_initiated / tenure),
       data = subset(pf, tenure > 0)) +
  geom_line(aes(color = year_joined.bucket),
            stat = "summary",
            fun.y = mean)
