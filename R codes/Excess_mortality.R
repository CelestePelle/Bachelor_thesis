#Computing excess mortality of fifteen countries

library(nlme)
library(dplyr)
temperature <- read.csv("Temperatures.csv")
colnames(temperature) <- c('date', 'temp_max', 'temp_min', 'country')
world_mortality_df <- read.csv("world_mortality.csv")
who <- read.csv("WHO_official.csv")

countries_sorted <- c('Argentina', 'Australia', 'Bulgaria', 'Colombia', 'Egypt', 'Estonia', 'Japan', 'Luxembourg', 'Mexico', 'Namibia', 'Netherlands', 'Russia', 'Thailand', 'United Kingdom', 'United States')

## Determines the start date to calculate the excess mortality ##
## Input: data frame
## Output: t_0; begin of the COVID-19 pandemic
begin <- function(data){
  if(max(data$Unit>40)){ #if weekly data, start in week 10
    beg = 10
  }
  else if(max(data$Unit<20)){ #if monthly data, start in month 3
    beg = 3 
  }
  return(beg)
}

## Creates a data set in the desired form (with mortality and temperatures) ##
## Input: country
## Output: data frame of the country
create_df <- function(c){
  mortality <- dplyr::filter(world_mortality_df, country_name == c, world_mortality_df$year<2023)
  
  #make a column year_plus_time_unit; second month of the year 2015 -> 2015+1/12
  if(mortality$time_unit[1]=="weekly"){
    week = 1 #data is weekly
    for (i in seq_len(nrow(mortality))) {
      if (mortality$year[i] == 2015 || mortality$year[i] == 2020) { #2015, 2020 were rare years
        mortality$year_plus_time_unit[i] <- mortality$year[i] + ((mortality$time[i] - 1) / 53)
      }else
        mortality$year_plus_time_unit[i] <- mortality$year[i] + ((mortality$time[i] - 1) / 52)
    }
  }else{
    week = 0 #data is not weekly (but monthly)
    mortality$year_plus_time_unit <- mortality$year + ((mortality$time-1) / 12) 
  }
  
  temp0 <- dplyr::filter(temperature, country == c)
  temp0 <- temp0 %>%
    mutate(
      date = as.Date(date),
      year = year(date),
      unit = month(date) #if data is weekly, we need to adjust this
    )
  temp <- temp0
  #adjusting unit and only keeping data from 2015-2022
  if(week==1){ #if weekly data
    n <- rep(1:52, each=7) #normal year 
    rare <- rep(1:53, each=7) #rare year
    b <- rep(49:52, each = 7)
    temp$unit <- c(b,rare, n,n,n,n,rare,n,n,n) #2015 and 2020 rare years
    temp <- temp %>%
      filter(date >= as.Date("2014-12-29") & date <= as.Date("2023-01-01"))
  }else{
    temp <- dplyr::filter(temp, year > 2014 & year < 2023)
  }
  
  #getting averages per unit
  j <- 0
  sum_max <- c() #sums maximum temperatures per time unit
  sum_min <- c() #sums minimum temperatures per time unit
  count <- c() #counts number of days per unit (useful for monthly data)
  abs_min <- c() #collects the minimum temperature per time unit
  abs_max <- c() #collects the maximum temperature per time unit
  start = 1 
  if(mortality$time[1]==2){ #USA misses first week
    start = 8 #skip the first week
  }
  for(i in start:nrow(temp)){
    if(i==start || temp$unit[i]!=temp$unit[i-1]){ #if begin or new time unit
      j <- j + 1
      sum_max[j] <- 0
      sum_min[j] <- 0
      count[j] <- 0
      abs_max[j] <- -1000
      abs_min[j] <- 1000
    }
    sum_max[j] <- sum_max[j] + temp$temp_max[i]
    sum_min[j] <- sum_min[j] + temp$temp_min[i]
    count[j] <- count[j] + 1
    if(temp$temp_max[i] > abs_max[j]){
      abs_max[j] <- temp$temp_max[i]
    }
    if(temp$temp_min[i] < abs_min[j]){
      abs_min[j] <- temp$temp_min[i]
    }
  }
  if(week==1){
    temp0 <- dplyr::filter(temp0, date <= as.Date("2014-12-28"))
  }else
    temp0 <- dplyr::filter(temp0, year < 2015)
  
  j=1
  history_min = c(0,0,0,0)
  history_max = c(0,0,0,0)
  for(i in 1:nrow(temp0)){
    if(week==1){
      if(i>1 & ((i-1) %% 7 == 0)){
        j = j + 1
      }
      history_min[j] <- history_min[j] + temp0$temp_min[i]
      history_max[j] <- history_max[j] + temp0$temp_max[i]
    }else{
      history_min[4] <- history_min[4] + temp0$temp_min[i]
      history_max[4] <- history_max[4] + temp0$temp_max[i]
    }
  }
  if(week==1){
    history_min <- history_min / 7
    history_max <- history_max / 7
    
  }else{
    history_min <- history_min / 31 #December has 31 days
    history_max <- history_max / 31 
  }
  
  if(c=='Namibia'){ #because we don't have Namibia data from 2022
    sum_max <- sum_max[1:84]
    sum_min <- sum_min[1:84]
    count <- count[1:84]
    abs_min <- abs_min[1:84]
    abs_max <- abs_max[1:84]
  }
  
  temp_max <- sum_max/count #getting the average
  temp_min <- sum_min/count
  temp_max <- temp_max #- mean(temp_max)
  temp_min <- temp_min #- mean(temp_min)
  #preparing the lags 
  temp_max_1 <- c() #maximal temperature with 1 week lag
  temp_max_2 <- c() #lag 2 weeks
  temp_max_3 <- c() #lag 3 weeks
  temp_min_1 <- c() #minimal temperature with 1 week lag
  temp_min_2 <- c()
  temp_min_3 <- c()
  temp_max_1[1] <- history_max[4]  
  temp_max_2[1:2] <- history_max[3:4]  
  temp_max_3[1:3] <- history_max[2:4]   
  temp_min_1[1] <- history_min[4] 
  temp_min_2[1:2] <- history_min[3:4] 
  temp_min_3[1:3] <- history_min[2:4]  
  abs_min_1 <- c()
  abs_min_2 <- c()
  abs_min_3 <- c()
  abs_max_1 <- c()
  abs_min_1[1] <- 0
  abs_min_2[1:2] <- 0
  abs_min_3[1:3] <- 0
  abs_max_1[1] <- 0
  
  for(i in 2:length(temp_max)){
    temp_max_1[i] <- temp_max[i-1]
    temp_min_1[i] <- temp_min[i-1]
    abs_min_1[i] <- abs_min[i-1]
    abs_max_1[i] <- abs_max[i-1]
    if(i>2){
      temp_max_2[i] <- temp_max[i-2]
      temp_min_2[i] <- temp_min[i-2]
      abs_min_2[i] <- abs_min[i-2]
      if(i>3){
        temp_max_3[i] <- temp_max[i-3]
        temp_min_3[i] <- temp_min[i-3]
        abs_min_3[i] <- abs_min[i-3]
      }
    }
  }
  
  #making the year column
  if(week == 1){
    Year <- c()
    for(year in 2015:2022){
      x = 52
      if(year == 2015 || year == 2020){
        x = 53
      }
      Year <- c(Year, rep(year, x))
    }
  }else{ 
    Year <- rep(2015:2022, each=12)
  }
  if(start != 1){ #only for the USA data
    Year <- Year[-1]
  }
  Year <- Year[1:length(temp_min)] #for countries with data till 2021
  #making the rest of the new data frame
  Unit <- rle(mortality$time)$values
  Time <- mortality$year_plus_time_unit
  Number <- rep(1:length(Year), each=1) #an index column
  if(mortality$time[1]==2){
    Number <- rep(2:(length(Year)+1), each=1)
  }
  new_data <- data.frame(Year, Unit, Time, temp_min, temp_min_1, temp_min_2, temp_min_3, temp_max, temp_max_1, temp_max_2, temp_max_3, abs_min, abs_min_1, abs_min_2, abs_min_3, abs_max, abs_max_1, mortality$deaths, Number)
  colnames(new_data) <- c('Year', 'Unit', 'Time', 'Temp_min', 'Temp_min_1', 'Temp_min_2', 'Temp_min_3', 'Temp_max', 'Temp_max_1', 'Temp_max_2', 'Temp_max_3','Abs_min', 'Abs_min_1', 'Abs_min_2', 'Abs_min_3', 'Abs_max', 'Abs_max_1', 'Deaths', 'Number')
  return(new_data)
}

##  creating the predictor data frames for the models ##
##  input: data frame, selection of what years to use, m = 12 or 52, if test FALSE, include deaths, because the model is getting trained
##  output: data frame with the predictors
create_predictors <- function(data, selection, m, test){
  
  #one-hot encoding for weeks or months
  onehot <- matrix(0, sum(selection), m)
  for (i in 1:sum(selection)) {
    onehot[i, data[selection,2][i]] <- 1
  }
  
  #creating the other predictors
  max_sq <- data$Temp_max[selection] * data$Temp_max[selection]
  min_sq <- data$Temp_min[selection] * data$Temp_min[selection]
  predictors <- cbind(data$Year[selection], data$Number[selection], onehot) #years in the first column
  predictors <- as.data.frame(predictors)
  if(test==FALSE){
    predictors$deaths <- data$Deaths[selection]
  }
  predictors$temp_max <- data$Temp_max[selection]
  predictors$temp_max_1 <- data$Temp_max_1[selection]
  predictors$temp_max_2 <- data$Temp_max_2[selection]
  predictors$temp_max_3 <- data$Temp_max_3[selection]
  
  predictors$temp_min <- data$Temp_min[selection]
  predictors$temp_min_1 <- data$Temp_min_1[selection]
  predictors$temp_min_2 <- data$Temp_min_2[selection]
  predictors$temp_min_3 <- data$Temp_min_3[selection]
  
  predictors$max_sq <- max_sq
  predictors$min_sq <- min_sq
  
  predictors$abs_min <- data$Abs_min[selection]
  predictors$abs_min_1 <- data$Abs_min_1[selection]
  predictors$abs_min_2 <- data$Abs_min_2[selection]
  predictors$abs_min_3 <- data$Abs_min_3[selection]
  predictors$abs_max <- data$Abs_max[selection]
  predictors$abs_max_1 <- data$Abs_max_1[selection]
  if(test){
    colnames(predictors) <- c('Year', 'Index', paste0('Unit_', 1:m), 'Temp_max', 'Temp_max_1', 'Temp_max_2', 'Temp_max_3', 'Temp_min', 'Temp_min_1','Temp_min_2', 'Temp_min_3', 'Max_sq',  'Min_sq', 'Abs_min', 'Abs_min_1', 'Abs_min_2', 'Abs_min_3', 'Abs_max', 'Abs_max_1')
  }else{
    colnames(predictors) <- c('Year', 'Index', paste0('Unit_', 1:m), 'Deaths', 'Temp_max', 'Temp_max_1', 'Temp_max_2', 'Temp_max_3', 'Temp_min', 'Temp_min_1','Temp_min_2', 'Temp_min_3', 'Max_sq',  'Min_sq', 'Abs_min', 'Abs_min_1', 'Abs_min_2', 'Abs_min_3', 'Abs_max', 'Abs_max_1')
  }
  return(predictors)
}


i=0
excess <- c()
excess_1 <- c()
for(country in countries_sorted){
  i = i + 1
  data <- create_df(country)
  
  #extract data from 2015-2019 (pre-2020) (weeks < 53)
  pre <- (data$Year >= 2015) & (data$Year < 2020) & (data$Unit < 53)
  pre_covid <- data[pre,]
  pre_covid <- dplyr::filter(pre_covid, Unit<53)
  m <- max(pre_covid$Unit) #12 or 52, depending on weekly or monthly measures
  
  #creating the predictors
  predictors_full <- create_predictors(data, pre,m, test=FALSE)
  predictors1 <-predictors_full[,c(1,3:(m+3))] #includes year, time units and deaths
  predictors <- predictors_full[,c(1,3:(m+3),(m+5),(m+8),(m+9),(m+13))] #includes final selection + deaths
  
  #fitting the models
  model1 <- lm(Deaths ~ 0 + ., data=predictors1)  #eLife model
  model_final <- nlme::gls(Deaths ~ 0 + ., data=predictors, correlation = corAR1(form = ~1)) #final model
  
  phi <- coef(model_final$modelStruct$corStruct, unconstrained = FALSE) #correlation coefficient
  res <- residuals(model_final)
  last_res <- tail(res,1) #last residual
  
  #create predictors for 2020 
  data_2020_full <- (data$Year == 2020) 
  data_2020 <- (data$Year == 2020) & (data$Unit < 53)
  predictors2020_full <- create_predictors(data,data_2020,m,test=TRUE)
  predictors2020_1 <- predictors2020_full[,c(1,3:(m+2))]
  predictors2020 <- predictors2020_full[,c(1,3:(m+2),(m+4),(m+7),(m+8),(m+12))] 
  
  #predict baseline values for 2020 using the regression model
  baseline1 <- stats::predict(model1, newdata = predictors2020_1)
  baseline <- stats::predict(model_final, newdata = predictors2020)
  
  if (max(data[data_2020_full, 2])>52) { #if 2020 has a unit (week) 53
    baseline1[53] <- baseline1[52]
    baseline[53] <- baseline[52]
  }
  
  #predict() doesn't add the correlation structure of the errors,
  #we need to handle that manually
  res_future <- numeric(length(baseline))
  for(j in 1:length(res_future)){
    if(j==1){
      res_future[j] <- phi * last_res
    } else{
      res_future[j] <- phi * res_future[j-1]
    }
    baseline[j] <- res_future[j] + baseline[j]
  }
  
  #computing excess mortality
  ind2020 = data$Year == 2020
  ind2021 = data$Year == 2021
  
  diff2020_model1 = data$Deaths[ind2020] - baseline1
  diff2020 = data$Deaths[ind2020] - baseline
  if(m<20){
    diff2021_model1 = data$Deaths[ind2021] - baseline1
    diff2021 = data$Deaths[ind2021] - baseline
  }else{
    diff2021_model1 = data$Deaths[ind2021] - baseline1[-length(baseline1)]
    diff2021 = data$Deaths[ind2021] - baseline[-length(baseline)]
  }
  excess_1[i] <- sum(diff2020_model1[begin(data):length(diff2020_model1)]) + sum(diff2021_model1)
  excess[i] <- sum(diff2020[begin(data):length(diff2020)])+sum(diff2021)
}

excess_1 <- signif(excess_1, digits = 2)
excess <- signif(excess, digits = 2)
off <- signif(who$Cumulative_deaths, digits = 2)
percentage <- ((100/excess_1) * excess ) - 100

#creating data frame with the excess mortality numbers
excess_df <- data.frame(
  country = countries_sorted,
  excess_old = excess_1,
  excess_new = excess,
  diff = abs(excess_1-excess),
  diff_percentage = round(percentage, digits = 2),
  official = off
)
print(excess_df)
