source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-2.csv", 
                        stringsAsFactors = FALSE );

windSpeedQuant = quantile(weatherData$windSpeed, probs=c(.30, .70));

for(day in 1:nrow(weatherData))  # nrow(weatherData) = 366
{
  # if the value in windSpeed is less than or equal to the low quant value
  if(weatherData$windSpeed[day] <= windSpeedQuant[1])      # <= 6.4
  {
    weatherData$windSpeedLevel[day] = "Low";
  }
  # if the value in windSpeed is greater than or equal to the high quant value
  else if(weatherData$windSpeed[day] >= windSpeedQuant[2]) # >= 10.2
  {
    weatherData$windSpeedLevel[day] = "High";
  }
  else # the value in windSpeed is between 6.2 and 10.4
  {
    weatherData$windSpeedLevel[day] = "Medium";
  }
}

for(day in 1:nrow(weatherData))
{
  # if the direction is greater than 315 OR less than 45 degrees
  if(weatherData$windSusDir[day] >= 315 || 
     weatherData$windSusDir[day] < 45)
  {
    weatherData$windDir[day] = "North";
  }
  # if the direction is greater than 45 AND less than 135 degrees
  else if(weatherData$windSusDir[day] >= 45 && 
          weatherData$windSusDir[day] < 135)
  {
    weatherData$windDir[day] = "East";
  }
  # if the direction is greater than 135 AND less than 225 degrees
  else if(weatherData$windSusDir[day] >= 135 && 
          weatherData$windSusDir[day] < 225)
  {
    weatherData$windDir[day] = "South";
  }
  else # the direction is between 225 and 315 degrees
  {
    weatherData$windDir[day] = "West";
  }
}

for(day in 1:nrow(weatherData))
{
  if( day == 1)  # no day before the first day
  {
    weatherData$changeMaxTemp[day] = NA; 
  }
  else  # subtract previous day's maxTemp from current day's maxTemp
  {
    weatherData$changeMaxTemp[day] = weatherData$maxTemp[day] -
      weatherData$maxTemp[day-1];
  }
}

write.csv(weatherData, file="data/LansingNOAA2016-3.csv");

#### Part 2: Plot Wind Speed vs. wind direction
thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp)) +
  theme_bw() +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(thePlot);

#### Part 3: Violin plot of Wind Speed vs. wind direction
thePlot = ggplot(data=weatherData) +
  geom_violin(mapping=aes(x=windDir, y=changeMaxTemp),
              na.rm=TRUE) +
  theme_bw() +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(thePlot);

#### Part 4: Add error bars
thePlot = ggplot(data=weatherData) +
  stat_boxplot(mapping=aes(x=windDir, y=changeMaxTemp), 
               na.rm=TRUE,
               geom = "errorbar",
               width = 0.2) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp),
               na.rm=TRUE) +
  theme_bw() +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(thePlot);

### Part 5: Re-order the directions on the x-axis using factor(s)
# Save the list of directions in a factor
windDirFact = factor(weatherData$windDir,
                     levels=c("North", "East", "South", "West"));

thePlot = ggplot(data=weatherData) +
  stat_boxplot(mapping=aes(x=windDirFact, y=changeMaxTemp),
               na.rm=TRUE,
               geom = "errorbar",
               width = 0.2) +
  geom_boxplot(mapping=aes(x=windDirFact, y=changeMaxTemp),
               na.rm=TRUE) +
  theme_bw() +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(thePlot);

thePlot = ggplot(data=weatherData) +
  stat_boxplot(mapping=aes(x=windDirFact, y=changeMaxTemp),
               na.rm=TRUE,
               geom = "errorbar",
               width = 0.2) +
  geom_boxplot(mapping=aes(x=windDirFact, y=changeMaxTemp),
               na.rm=TRUE,
               outlier.shape = "@", 
               outlier.color = "red",
               outlier.alpha = 0.6, 
               outlier.size = 4 ) +
  theme_bw() +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(thePlot);

northVals=which(weatherData$windDir == "North");

southVals=which(weatherData$windDir == "South");

northMed = median(weatherData[northVals,"changeMaxTemp"], na.rm=TRUE);
southMed = median(weatherData[southVals,"changeMaxTemp"], na.rm=TRUE);

thePlot = ggplot(data=weatherData) +
  stat_boxplot(mapping=aes(x=windDirFact, y=changeMaxTemp),
               na.rm=TRUE,
               geom = "errorbar",
               width = 0.2) +
  geom_boxplot(mapping=aes(x=windDirFact, y=changeMaxTemp), 
               na.rm=TRUE,
               outlier.shape = "@",
               outlier.color = "red",
               outlier.alpha = 0.6,
               outlier.size = 4 ) +
  annotate(geom="text",    # North median
           x=1,
           y=20, 
           color="blue",   
           label=paste("median:", northMed) ) +
  annotate(geom="text",    # South median
           x=3, 
           y=-10, 
           color="red",    
           label=paste("median:", southMed) ) +
  theme_bw() +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(thePlot);






