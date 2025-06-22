#Performing cross-validation and computing average MSE over different models

library(nlme)
library(dplyr)

temperature <- read.csv("Temperatures.csv")
colnames(temperature) <- c('date', 'temp_max', 'temp_min', 'country')
world_mortality_df <- read.csv("world_mortality.csv")

countries_sorted <- c('Argentina', 'Australia', 'Bulgaria', 'Colombia', 'Egypt', 'Estonia', 'Japan', 'Luxembourg', 'Mexico', 'Namibia', 'Netherlands', 'Russia', 'Thailand', 'United Kingdom', 'United States')

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


## calculating MSE and MAE ##
## input: predictions from the model, actual values, matrices to fill, k corresponds with the k-th fold
## output: MSE_matrix and MAE_matrix
get_results <- function(predictions, actual, MSE_matrix, MAE_matrix, k) {
  for(i in 1:4){
    diff <- predictions[[i]] - actual
    MSE_matrix[i, k] <- mean(diff^2)
    MAE_matrix[i, k] <- mean(abs(diff))
  }
  return(list(MSE_matrix = MSE_matrix, MAE_matrix = MAE_matrix))
}

## printing results ## 
## input: MSE_matrix and MAE_matrix, containing all MSE and MAE values
print_results <- function(MSE_matrix, MAE_matrix){
  
  average_MSE <- numeric(4)  #to store the averages for MSE
  average_MAE <- numeric(4)  #to store the averages for MAE
  sd_MSE <- numeric(4)      #to store the standard deviations for MSE
  sd_MAE <- numeric(4)      #to store the standard deviations for MAE
  
  for(i in 1:4){
    average_MSE[i] <- sum(MSE_matrix[i,]) / length(MSE_matrix[i,])
    average_MAE[i] <- sum(MAE_matrix[i,]) / length(MAE_matrix[i,])
    sd_MSE[i] <- sd(MSE_matrix[i,])
    sd_MAE[i] <- sd(MAE_matrix[i,])
  }
  results <- data.frame(
    Model = c('eLife Model', 'eLife + temp', 'AR(1) no temp', 'AR(1) + temp'),  # Row numbers (1 to n)
    MSE = average_MSE,
    MAE = average_MAE,
    SD_MSE = sd_MSE,
    SD_MAE = sd_MAE
  )
  print(results)
}


for(c in 1:length(countries_sorted)){
  data <- create_df(countries_sorted[c])
  
  #data in the fold is training data, rest is test data
  fold_1 <- (data$Year %in% c(2015,2016,2017,2018)) & (data$Unit<53) #test 2019
  fold_2 <- (data$Year %in% c(2015,2016,2017,2019)) & (data$Unit<53) #test 2018
  fold_3 <- (data$Year %in% c(2015,2016,2018,2019)) & (data$Unit<53) #test 2017
  fold_4 <- (data$Year %in% c(2015,2017,2018,2019)) & (data$Unit<53) #test 2016
  
  #create matrices to store MSEs and MAEs.
  MSE_matrix <- matrix(0, nrow = 4, ncol = 4)
  rownames(MSE_matrix) <- paste0("Model_", 1:4)
  colnames(MSE_matrix) <- paste0("Fold_", 1:4)
  
  MAE_matrix <- matrix(0, nrow = 4, ncol = 4)
  rownames(MAE_matrix) <- paste0("Model_", 1:4)
  colnames(MAE_matrix) <- paste0("Fold_", 1:4)
  
  m <- max(data$Unit) #12 or 52, depending on weekly or monthly measures
  if(m == 53){ # we ignore week 53 in the training process of the models
    m = 52
  }
  for(k in 1:4){ #loop over the folds 
    #selection makes sure we only train the model on the training data
    selection <- get(paste0("fold_", k))
    #making data frames with predictors for the different models
    predictors_full <- create_predictors(data, selection, m, test=FALSE)
    predictors2 <- predictors_full[,c(1:(m+13))]
    predictors1 <-predictors2[,c(1:(m+3))]
    
    #fitting the models
    model1 <- lm(Deaths ~ 0 + .-Index, data = predictors1)  #model as in e-Life
    model2 <- lm(Deaths ~ 0 + .-Index, data = predictors2)  #model1 with avg temperature
    model3 <- nlme::gls(Deaths ~ 0 + . -Index, data=predictors1, correlation=corAR1(form= ~Index)) #AR(1) model
    model4 <- nlme::gls(Deaths ~ 0 + . -Index, data=predictors2, correlation=corAR1(form= ~Index)) #model3 with avg temperature
    models <- list(model1, model2, model3, model4)
    
    #storing correlation coefficients and the residuals for AR(1) models
    phi3 <- coef(model3$modelStruct$corStruct, unconstrained = FALSE) 
    phi4 <- coef(model4$modelStruct$corStruct, unconstrained = FALSE)
    res3 <- residuals(model3)
    res4 <- residuals(model4)
    
    #create predictor data frames for test year 
    test_year <- 2020 - k
    test_data <- (data$Year == test_year) & (data$Unit<53) #getting test instances
    predictors_test_full <- create_predictors(data, test_data, m, test=TRUE)
    predictors_test2 <- predictors_test_full[,c(1:(m+12))]
    predictors_test1 <- predictors_test_full[,c(1:(m+2))]
    #predictors_test2$redundant_column <- NULL
    predictors_test <- list(predictors_test1, predictors_test2, predictors_test1, predictors_test2)
    
    #predict number of deaths in test year 
    predictions <- list()
    for(i in 1:length(models)){
      predictions[[i]] <- stats::predict(models[[i]], newdata = predictors_test[[i]])
    }
    #for week 53 we use the same predictions as for week 52
    if(max(data$Unit[test_data] == 53)){
      for(i in 1:length(predictions)){
        predictions[[i]][53] <- predictions[[i]][52]
      }
    }
    
    #for AR(1) models, auto-regressive error terms need to be added manually
    res_future3 <- numeric(length(predictions[[3]]))
    res_future4 <- numeric(length(predictions[[3]]))
    usa = 0 
    if(data$Unit[1]!=1){ #if country is USA
      usa = 1
    }
    for(j in 1:length(predictions[[3]])){
      if(j==1){
        res_future3[j] <- phi3 * res3[m*(5-k)-usa] #res3[m*(5-k)] is last residual
        res_future4[j] <- phi4 * res4[m*(5-k)-usa]
      }else{
        res_future3[j] <- phi3 * res_future3[j-1]
        res_future4[j] <- phi4 * res_future4[j-1]
      }
      predictions[[3]][j] <- res_future3[j] + predictions[[3]][j]
      predictions[[4]][j] <- res_future4[j] + predictions[[4]][j]
    }
    #print(predictions)
    actual <- data$Deaths[test_data] #actual number of deaths in test year
    
    #with predictions and actual values, the MSE and MAE can be calculated and stored 
    matrix <- get_results(predictions, actual, MSE_matrix, MAE_matrix,k)
    MSE_matrix <- matrix$MSE_matrix
    MAE_matrix <- matrix$MAE_matrix
  }  
  #print_results(MSE_matrix, MAE_matrix)
  print(countries_sorted[c])
  print_results(MSE_matrix, MAE_matrix)
}
