# Find out the working dictionary path.
getwd() 
# In order to set new working dictionary path.
setwd()
# list the files which are available at the working dictionary
list.files()
# Loading the Facebook TSV file
facebook_data <- read.csv('pseudo_facebook.tsv', sep = '\t')
#Load the ggplot library. First Install and then Install
library(ggplot2)

# Loading the variables available in the dataset
names(pf)

# Ploting the Scatterplot of age Vs friend_count using qplot

qplot(x = age, y = friend_count, data = pf)
qplot(age, friend_count, data = pf)

# Ploting the Scatterplot of age Vs friend_count using ggplot
ggplot(aes(x = age, y = friend_count), data = pf) +
  geom_jitter(alpha = 1/20) +
  xlim(13, 90)

# To print summary of age variable
summary(pf$age)

ggplot(aes(x = age, y = friend_count), data = pf) +
  geom_point(alpha = 1/20, position = position_jitter(h = 0), color = 'orange') +
  xlim(13, 90) +
  coord_trans(y = "sqrt")

  
ggplot(aes(x = age, y = friendships_initiated), data = pf) + geom_point()

ggplot(pf, aes(x = age, y = friendships_initiated)) +
  geom_jitter(alpha = 1/20, position = position_jitter(h = 0)) +
  xlim(13, 90)+
  coord_trans(y = "sqrt")

library(dplyr)
age_group <- group_by(pf, age)
pf.fc_by_age <- summarise(age_group,
         friend_count_mean = mean(friend_count),
         friend_count_median = median(friend_count),
         n = n())
head(pf.fc_by_age)


pf %. %
  group_by(age) %. %
  summarise(age_group,
            friend_count_mean = mean(friend_count),
            friend_count_median = median(friend_count),
            n = n()) %. %
  arrange(age)


qplot(x = age, y = friend_count_mean, data = pf.fc_by_age) +
  geom_line()

ggplot(aes(x = age, y = friend_count_mean), data = pf.fc_by_age) + geom_line()


# Overlying Summaries with Raw Data
ggplot(aes(x = age, y = friend_count), data = pf) +
  geom_point(alpha = 1/20, position = position_jitter(h = 0), color = 'orange') +
  coord_cartesian(xlim = c(13, 90)) +
  coord_trans(y = "sqrt") +
  geom_line(stat = 'summary', fun.y = mean) +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .1), linetype = 2, color = 'Blue') +
  coord_trans(y = "sqrt") +
  geom_line(stat = 'summary', fun.y = median)

cor(pf$age, pf$friend_count)

with(subset(pf, age <= 70), cor.test(age, friend_count))

with(subset(pf, age <= 70), cor.test(age, friend_count, method = 'spearman'))

ggplot(aes(x = www_likes_received, y = likes_received), data = pf) +geom_point()

#Strong Correlation

ggplot(aes(x = www_likes_received, y = likes_received), data = pf) +geom_point() +
  xlim(0, quantile(pf$www_likes_received, 0.95)) +
  ylim(0, quantile(pf$likes_received, 0.95)) +
  geom_smooth(method = 'lm', color = 'red')

cor(pf$www_likes_received, pf$likes_received)

library(alr3)
data(Mitchell)  
?Mitchell

ggplot(aes(x = Month, y= Temp), data = Mitchell) +
  geom_point()
cor(Mitchell$Month, Mitchell$Temp)

ggplot(aes(x = Month, y= Temp), data = Mitchell) +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 203, 12))

ggplot(aes(x = age, y = friend_count_mean), data = pf.fc_by_age) +
  geom_line()

pf$age_with_months <- pf$age + (1 - dob_month) / 12

pf$age_with_months <- with(pf$age + (1 - dob_month / 12))

