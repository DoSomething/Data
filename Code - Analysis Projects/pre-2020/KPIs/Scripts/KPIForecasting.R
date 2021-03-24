
library(tidyverse)
library(data.table)
library(dtplyr)


# Parameters --------------------------------------------------------------

baselineTrend = 'linear'
baseline = 1000
valuePreviousYears = 
  data.table(
    seq = seq(1,8,1),
    year = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020), 
    value = c(baseline, 2000, 3000, 4000, NA, NA, NA, NA)
    )

slr = lm(value ~ year, valuePreviousYears[year<2017])
valuePreviousYears[,forecast := predict(slr, newdata = valuePreviousYears)]
valuePreviousYears[year>=2016,forecastLog := max(valuePreviousYears$value, na.rm=T) + forecast^(.8) - 761.462]

test <- function(valuePreviousYears) {
  for (i in 1:nrow(valuePreviousYears)) {
    if (valuePreviousYears[i,year] < 2016) {
      valuePreviousYears[i,forecastExp := 0]
    } else if (valuePreviousYears[i,year]==2016) {
      valuePreviousYears[i,forecastExp := 4000]
    } else {
      prevVal = valuePreviousYears[i-1,forecastExp]
      valuePreviousYears[i,forecastExp := prevVal^1.05]
    }
  }
}

test(valuePreviousYears)

ggplot() + 
  geom_line(data=valuePreviousYears[year<2017], aes(x=year, y=forecast)) + 
  geom_line(data=valuePreviousYears, aes(x=year, y=forecast), linetype='longdash', color='black') + 
  geom_line(data=valuePreviousYears[year>=2016], aes(x=year, y=forecastLog), linetype='longdash', color='red') + 
  geom_line(data=valuePreviousYears[year>=2016], aes(x=year, y=forecastExp), linetype='longdash', color='blue') + 
  labs(y='Voters Registered', title='Registration Paths', x = 'Year') + ylim(0,25000)

valuePreviousYears[
  year >= 2018
  ,
  ':='(
    forecast = forecast + 1000,
    forecastLog = forecastLog + 1000 ,
    forecastExp = forecastExp + 1000
  )
]

ggplot() + 
  geom_line(data=valuePreviousYears[year<2017], aes(x=year, y=forecast)) + 
  geom_line(data=valuePreviousYears, aes(x=year, y=forecast), linetype='longdash', color='black') + 
  geom_line(data=valuePreviousYears[year>=2016], aes(x=year, y=forecastLog), linetype='longdash', color='red') + 
  geom_line(data=valuePreviousYears[year>=2016], aes(x=year, y=forecastExp), linetype='longdash', color='blue') + 
  labs(y='Voters Registered', title='Registration Paths', x = 'Year') + ylim(0,25000)

valuePreviousYears[
  ,
  forecastPathChange := ifelse(year < 2020, forecastExp, valuePreviousYears[year==2019,forecastLog] - valuePreviousYears[year==2019,forecastLog] + valuePreviousYears[year==2019,forecastExp] + 1000)
]

ggplot() + 
  geom_line(data=valuePreviousYears[year<2017], aes(x=year, y=forecast)) + 
  geom_line(data=valuePreviousYears, aes(x=year, y=forecast), linetype='longdash', color='black') + 
  geom_line(data=valuePreviousYears[year>=2016], aes(x=year, y=forecastLog), linetype='longdash', color='red') + 
  geom_line(data=valuePreviousYears[year>=2016], aes(x=year, y=forecastExp), linetype='longdash', color='blue') + 
  geom_line(data=valuePreviousYears[year>=2019], aes(x=year, y=forecastPathChange), linetype='dotted', color='blue') + 
  labs(y='Voters Registered', title='Registration Paths', x = 'Year') + ylim(0,25000)




