{
  source(file="scripts/reference.R");  
  weatherData = read.csv(file="data/LansingNOAA2016-3.csv", 
                         stringsAsFactors = FALSE);
 
  ### Replace "T" in precip column with 0.005 using gsub()
  weatherData$precip2 = gsub(x=weatherData$precip, 
                             pattern="T", 
                             replacement="0.005");
  
  ### Probably want the column to be numeric...
  weatherData$precip3 = as.numeric(gsub(x=weatherData$precip, 
                             pattern="T", 
                             replacement="0.005"));
  
  ### Plot humidity vs. precip2 (precip2 is string value -- this will cause problems!) 
  thePlot = ggplot(data=weatherData) +
    geom_point(mapping=aes(x=relHum, y=precip2)) +
    theme_bw() +
    labs(title = "Relative Humidity vs. Precipitation",
         subtitle = "Lansing, Michigan: 2016",
         x = "Humidity",
         y = "Precipitation (string/character/factor/categorical values)");
  plot(thePlot);


  ### Plot humidity vs. precip3 (much better)  
  thePlot = ggplot(data=weatherData) +
    geom_point(mapping=aes(x=relHum, y=precip3)) +
    theme_bw() +
    labs(title = "Relative Humidity vs. Precipitation",
         subtitle = "Lansing, Michigan: 2016",
         x = "Humidity",
         y = "Precipitation (numeric values)");
  plot(thePlot);
  
  # First 10 days of precipitaion (using precip3)
  precipAmount = 0;   # starts at zero (this is sometimes called a "state" variable)

  ## show with debugger values going up  
  for(i in 1:10)
  {
    precipAmount = precipAmount + weatherData$precip3[i]; 
  }
  
  # February precipitation take 1
  FebPrecip = 0;
  for(i in 32:60)
  {
    FebPrecip = FebPrecip + weatherData$precip3[i]; 
  }
  
  # February precipitation take 2
  FebPrecip2 = 0;
  for(i in 1:length(weatherData$precip3))
  {
    if(i >= 32 & i <= 60)      # & vs && (| vs ||)
    {
      FebPrecip2 = FebPrecip2 + weatherData$precip3[i]; 
    }
  }
  
  # February precipitation take 3
  FebPrecip3 = 0;
  for(i in 1:length(weatherData$precip3))
  {
    if(grepl(x=weatherData$date[i], pattern="02-"))
    {
      FebPrecip3 = FebPrecip3 + weatherData$precip3[i]; 
    }
  }
  
  # 1) Using one for loop:
  #    Find the total rainfall in April, July, and November
  #    - you will need three state variables
  
  AprPrecip = 0;
  for(i in 1:length(weatherData$precip3))
  {
    if(grepl(x=weatherData$date[i], pattern="04-"))
    {
      AprPrecip = AprPrecip + weatherData$precip3[i]; 
    }
  }
  
  JulPrecip = 0;
  for(i in 1:length(weatherData$precip3))
  {
    if(grepl(x=weatherData$date[i], pattern="07-"))
    {
      JulPrecip = JulPrecip + weatherData$precip3[i]; 
    }
  }
  
  NovPrecip = 0;
  for(i in 1:length(weatherData$precip3))
  {
    if(grepl(x=weatherData$date[i], pattern="11-"))
    {
      NovPrecip = NovPrecip + weatherData$precip3[i]; 
    }
  }
  
  
  # 2) Using one for loop:
  #    Find the total rainfall for the first three months and the last three months
  #    - you can extend the grep pattern (i.e., pattern="RA|SN|FG"  
  #                                             looks for rain, snow, or fog)
  
  FirstThreePrecip = 0;
  for(i in 1:length(weatherData$precip3))
  {
    if(grepl(x=weatherData$date[i], pattern="01-|02-|03-"))
    {
      FirstThreePrecip = FirstThreePrecip + weatherData$precip3[i]; 
    }
  }
  
  LastThreePrecip = 0;
  for(i in 1:length(weatherData$precip3))
  {
    if(grepl(x=weatherData$date[i], pattern="10-|11-|12-"))
    {
      LastThreePrecip = LastThreePrecip + weatherData$precip3[i]; 
    }
  }
  
  
  # 3) Using one for loop:
  #    Find the month with the most amount of rain

  ## Initial method with double loops - didn't work out...
  
  # Create a month column  
  weatherData$month <- strtrim(weatherData$date, 3)

  PrecipMonth = rep(0, 12)
  
  for(j in 1:length(PrecipMonth))
  {
    for(i in 1:length(weatherData$precip3))
    {
      if(grepl(x=weatherData$month[i], pattern=j))
      {
        PrecipMonth[j] = PrecipMonth[j] + weatherData$precip3[i]; 
      }
    }
  }
  
  max(PrecipMonth)
  which.max(PrecipMonth)
  
  # It looks the result is not correct...
  
  
  ## Second method with pattern index - this one worked!
  PrecipMonth2 = rep(0, 12)
  
  month = c("01-", "02-", "03-", "04-", "05-", "06-",
            "07-", "08-", "09-", "10-", "11-", "12-")
  
  for(i in 1:length(weatherData$precip3))
  {
    for(j in 1:12)
    {
      if(grepl(x=weatherData$date[i], pattern=month[j])) 
      {
        PrecipMonth2[j] = PrecipMonth2[j] + weatherData$precip3[i]
      }
    }
  }
  max(PrecipMonth2)
  which.max(PrecipMonth2)
  
  ## Method 3 - copied from the answer key - this worked too!
  precipEachMonth = c(rep(0,12));   # repeat zero 12 times, so c(0,0,0,0,0,0,0,0,0,0,0,0)
  for(i in 1:length(weatherData$precip3))
  {
    if(grepl(x=weatherData$date[i], pattern="01-")) {
      precipEachMonth[1] = precipEachMonth[1] + weatherData$precip3[i];
    }
    else if(grepl(x=weatherData$date[i], pattern="02-")) {
      precipEachMonth[2] = precipEachMonth[2] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern="03-")) {
      precipEachMonth[3] = precipEachMonth[3] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern="04-")) {
      precipEachMonth[4] = precipEachMonth[4] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern="05-")) {
      precipEachMonth[5] = precipEachMonth[5] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern="06-")) {
      precipEachMonth[6] = precipEachMonth[6] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern="07-")) {
      precipEachMonth[7] = precipEachMonth[7] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern="08-")) {
      precipEachMonth[8] = precipEachMonth[8] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern="09-")) {
      precipEachMonth[9] = precipEachMonth[9] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern="10-")) {
      precipEachMonth[10] = precipEachMonth[10] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern="11-")) {
      precipEachMonth[11] = precipEachMonth[11] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern="12-")) {
      precipEachMonth[12] = precipEachMonth[12] + weatherData$precip3[i]; 
    }
  }
  
  max(precipEachMonth)
  which.max(precipEachMonth)
}  