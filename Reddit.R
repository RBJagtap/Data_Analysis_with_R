getwd()
reddit <- read.csv('reddit.csv')

table(reddit$employment.status)

summary(reddit)

levels(reddit$age.range)

library(ggplot2)
qplot(data = reddit, x = age.range)

reddit$age.range <- ordered(reddit$age.range, levels = c('Under 18', '18-24','25-34', '35-44', '45-54', '55-64', '65 of Above'))

reddit$age.range <- factor(reddit$age.range, levels = c('Under 18', '18-24','25-34', '35-44', '45-54', '55-64', '65 of Above'), ordered = T)
qplot(data = reddit, x = income.range)

levels(reddit$income.range)

reddit$income.range <- ordered(reddit$income.range, levels = c('Under 20000', '20000-29999','30000-39999', '40000-49999', '50000-69999', '70000-99999', '100000-149999', '150000 of Above'))

qplot(data = reddit, x = income.range)
library