library(lubridate)

dat = read.csv("C:/PRATIK/MISC/Coding/Data Science Stats and ML (Coursera)/Practical_ML/gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

batsmod <- bats(tstrain)
testforecast <- forecast(batsmod, h = length(testing$visitsTumblr), level = 95)

sum(testing$visitsTumblr >= testforecast$lower[,"95%"] &
      testing$visitsTumblr <= testforecast$upper[,"95%"]) / 
  length(testing$visitsTumblr)
