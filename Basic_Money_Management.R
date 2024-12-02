install.packages("quantmod")
library(quantmod)

Basics = function(
    target = AAPL,
    past_N_days = 10
) {
  # Data
  price <- target[, 4] # Closing Price

  #Return Price
  returnofDaily <- quantmod::dailyReturn(price)
  returnofWeekly <- quantmod::weeklyReturn(price)
  returnofMonthly <- quantmod::monthlyReturn(price)

  statTable <- data.frame(rbind(
    Dailydata = round(c("Average"= mean(returnofDaily), "STD" = sd(returnofDaily), "SharpeRatio" = mean((returnofDaily)/sd(returnofDaily)), 4),
    Weeklydata = round("Average"= mean(returnofWeekly), "STD" = sd(returnofWeekly), "SharpeRatio" = mean((returnofWeekly)/sd(returnofWeekly)), 4),
    Monthlydata = round("Average"= mean(returnofMonthly), "STD" = sd(returnofMonthly), "SharpeRatio" = mean((returnofMonthly)/sd(returnofMonthly)), 4))))

}

# recent Report
statTableRecent <- statTable(cbind(
  RecentActive = round(mean(tail(returnofDaily, past_N_days)), 4),
  RecentSD = round(sd(tail(returnofDaily, past_N_days)), 4),
  RecentSharp = round(mean(tail(returnofDaily, past_N_days))/sd(tail(returnofDaily, past_N_days)), 4)
))

# Output
return(list(
  Statistics = statTable,
  Recentstats = statTableRecent
))