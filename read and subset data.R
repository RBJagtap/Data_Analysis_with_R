getwd()
setwd("C:/Users/Asus//Downloads")
statesInfo <- read.csv('stateData.csv')

a <- subset (statesInfo, state.region > 3)
a

b <- statesInfo[statesInfo$state.region > 3, ]
b

stateSubset <- subset (statesInfo, state.area > 8000)
stateSubset
head(stateSubset, 2)
dim(stateSubset)

