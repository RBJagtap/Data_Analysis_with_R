
getwd()

list.files()

facebook_data <- read.csv('pseudo_facebook.tsv', sep = '\t')
library(ggplot2)
names(pf)
qplot(x = dob_day, data = pf)+
  scale_x_discrete(breaks=1:31)

qplot(x = dob_day, data = pf) +
  scale_x_continuous(breaks=1:31)

qplot(x=dob_day, data = pf) +
  scale_x_continuous(breaks = 1:31) +
  facet_wrap(~dob_month,ncol = 3)   # dob for each one for each one for all year

qplot(x = friend_count, data=pf) 

qplot(x = friend_count, data=pf, xlim = c(0, 1000)) # restrict friend)count to 1000

qplot(x = friend_count, data = pf) +
  scale_x_continuous(limits = c(0, 1000)) # Restrict friend count to 1000


qplot(x = friend_count, data = pf, binwidth = 25) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) #Setting min and max limit with sequence 

qplot(x = friend_count, data = pf, binwidth = 25) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) + 
  facet_wrap(~gender) #two seperate histograms for male and female

qplot(x = friend_count, data = na.omit(pf), binwidth = 10) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) +
  facet_wrap(~gender) # it will omit the NA histogram

qplot(x = friend_count, data = subset(pf, !is.na(~gender)), binwidth = 10) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) +
  facet_wrap(~gender) # it will also omit the NA histogram

table(pf$gender)

by(pf$friend_count, pf$gender, summary)


qplot(x =tenure/365, data = pf, binwidth = 1, 
      color = I('blue'), fill = I('#099DD9')) # Facebook account for number of days/  year

qplot(x =tenure/365, data = pf, 
      xlab = 'No of years using facebook',
      ylab = 'no of users in sample', binwidth = 1, 
      color = I('blue'), fill = I('#099DD9')) +
  scale_x_continuous(breaks = seq(1, 7, 1), limits = c(0, 7)) # max tenure upto 7 years with sequence of 1

qplot(x = age, data = pf, binwidth = 0.2, 
      color = I('red'), fill = I('#099DD9')) +
  scale_x_continuous(breaks = seq(0, 133, 5), limits = c(14, 40))

qplot(x = friend_count, data = subset(pf, !is.na(gender)), xlim = c(0, 1000))
qplot(x = friend_count, data = na.omit(pf), binwidth = 10) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) + 
  facet_wrap(~gender)
table(pf$gender)
by(pf$friend_count, pf$gender, summary)
qplot(x = friend_count, data = subset(pf, !is.na(gender)), binwidth = 10) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) + 
  facet_wrap(~gender)





library(ggplot2)   
qplot(x = dob_day, data = facebook_data) +
  scale_x_discrete(breaks = 1:31)


qplot(x =tenure/365, data = pf, binwidth = .1,
      xlab = 'Number of years using Facebook',
      ylab = 'Number of users in sample',
      color = I('blue'), fill = I('#F79420')) +
  scale_x_continuous(breaks = seq(1, 7, 1), lim = c(0, 7)) # labeling the X and Y axis 
 
qplot(x = age, data = pf, binwidth = 1, 
      color = I('red'), fill = I('#099DD9')) # age plot

qplot(x = friend_count, data = pf) # Friend count vs count

summary(pf$friend_count) 

summary(log10(pf$friend_count +1)) # log of friendcount and count

summary(sqrt(pf$friend_count)) # Square root of friendcount vs count
install.packages('gridExtra')
library(gridExtra)
p1 <- qplot(x = friend_count, data = pf)
p2 <- qplot(x = log10(pf$friend_count), data = pf)
p3 <- qplot(x = sqrt(pf$friend_count), data = pf)

grid.arrange(p1, p2, p3, ncol = 1) # For easy comparison all the plots are arranged in single page

p1 <- ggplot(aes(x = friend_count), data = pf) + geom_histogram()
p2 <- p1 + scale_x_log10()
p3 <- p1 + scale_x_sqrt() # this is using ggplot method

grid.arrange(p1, p2, p3, ncol = 1)

p4 <- ggplot(aes(x = friend_count), data = pf) + geom_histogram() + scale_x_log10()

grid.arrange(p2, p4, ncol = 2) 


qplot(x = friend_count, data = subset(pf, !is.na(gender)), binwidth = 10, geon = 'freqpoly', color = gender) +
  scale_x_continuous(lim = c(0, 1000), breaks = seq(0, 1000, 50))

qplot(x = www_likes, data = subset(pf, !is.na(gender)), geon = 'freqpoly', color = gender) +
  scale_x_continuous()

table(pf$www_likes)

by(pf$www_likes, pf$gender, summary)

by(pf$friendships_initiated, pf$gender, sum)

# Box plots


qplot(x = gender, y = friend_count, data = subset(pf, !is.na(gender)), geon = 'boxplot')

qplot(x = gender, y = friend_count, data = subset(pf, !is.na(gender)), geon = 'boxplot') +
  scale_y_continuous(limits = c(0, 1000))

qplot(x = gender, y = friend_count, data = subset(pf, !is.na(gender)), geon = 'boxplot') +
  coord_cartesian(ylim = c(0, 1000))

summary(pf$mobile_likes_received, percentage)
sum(pf$mobile_check_in)
sum(pf$mobile_likes_received == 1)/length(pf$likes)
