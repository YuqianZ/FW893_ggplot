source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-2.csv", 
                        stringsAsFactors = FALSE );

pressureQuant = quantile(weatherData$stnPressure, probs=c(.20, .40, .60, .80));

for(day in 1:nrow(weatherData))  # nrow(weatherData) = 366
{
  # if the value in stnPressure is less than or equal to the low quant value
  if(weatherData$stnPressure[day] <= pressureQuant[1])      # <= 28.9
  {
    weatherData$pressureLevel[day] = "Low";
  }
  # if the value in windSpeed is greater than or equal to the high quant value
  else if(weatherData$stnPressure[day] <= pressureQuant[2]) # <= 29.1
  {
    weatherData$pressureLevel[day] = "Medium-low";
  }
  else if(weatherData$stnPressure[day] <= pressureQuant[3]) # <= 29.1
  {
    weatherData$pressureLevel[day] = "Medium";
  }
  else if(weatherData$stnPressure[day] <= pressureQuant[4]) # <= 29.2
  {
    weatherData$pressureLevel[day] = "Medium-high";
  }
  else # the value in stnPressure > 29.2
  {
    weatherData$pressureLevel[day] = "High";
  }
}

#### Plot Wind Sus Speed vs. Pressure Level
# Save the list of directions in a factor
pressureFact = factor(weatherData$pressureLevel,
                      levels=c("Low", "Medium-low", "Medium", "Medium-high", "High"));

thePlot = ggplot(data=weatherData) +
  stat_boxplot(mapping=aes(x=pressureFact, y=windSusSpeed), 
               na.rm=TRUE,
               geom = "errorbar",
               width = 0.2) +
  geom_boxplot(mapping=aes(x=pressureFact, y=windSusSpeed)) +
  theme_bw() +
  labs(title = "Wind Sus Speed vs. Pressure Level",
       subtitle = "Lansing, Michigan: 2016",
       x = "Pressure Level",
       y = "Wind Sus Speed");
plot(thePlot);

# Find the three outliers for low and high pressure levels
lowWeatherData = subset(weatherData, pressureLevel=="Low")
highWeatherData = subset(weatherData, pressureLevel=="High")

lowOrder = order(lowWeatherData$windSusSpeed, decreasing = TRUE)
highOrder = order(highWeatherData$windSusSpeed, decreasing = TRUE)

lowDates = lowWeatherData$dateYr[lowOrder[1:3]]
highDates = highWeatherData$dateYr[highOrder[1:3]]

thePlotX = ggplot(data=weatherData) +
  stat_boxplot(mapping=aes(x=pressureFact, y=windSusSpeed), 
               na.rm=TRUE,
               geom = "errorbar",
               width = 0.2) +
  geom_boxplot(mapping=aes(x=pressureFact, y=windSusSpeed),
               outlier.shape = 11,
               outlier.color = "grey20",
               outlier.alpha = 0.6,
               outlier.size = 1.5) +
  # Label low pressure level outliers
  annotate(geom="text",    # Low outliers
           x=1,
           y=49.5, 
           color="blue",   
           label=paste("Date:", lowDates[1]) ) +
  annotate(geom="text",    # Low outliers
           x=1,
           y=45.5, 
           color="blue",   
           label=paste("Date:", lowDates[2]) ) +
  annotate(geom="text",    # Low outliers
           x=1,
           y=42.5, 
           color="blue",   
           label=paste("Date:", lowDates[3]) ) +
  # Label high pressure level outliers
  annotate(geom="text",    # High outliers
           x=5,
           y=35, 
           color="red",   
           label=paste("Date:", highDates[1]) ) +
  annotate(geom="text",    # High outliers
           x=5,
           y=30, 
           color="red",   
           label=paste("Date:", highDates[2]) ) +
  annotate(geom="text",    # High outliers
           x=5,
           y=26, 
           color="red",   
           label=paste("Date:", highDates[3]) ) +
  theme_bw() +
  labs(title = "Wind Sus Speed vs. Pressure Level",
       subtitle = "Lansing, Michigan: 2016",
       x = "Pressure Level",
       y = "Wind Sus Speed");
plot(thePlotX);

### Get plot data:
plotData = ggplot_build(thePlotX)$data[[1]];

### Extract outliers from plot data
outliers = plotData$outliers;

### Outliers from first plot
firstBoxOut = outliers[[1]];


## testing ggstatsplot package and functions

library("tidyverse","ggstatsplot")

# <<<<<<< HEAD
boxplot(windSusSpeed~pressureLevel, data=weatherData)
ggstatsplot::ggbetweenstats(data=weatherData,
                 x = pressureLevel,
                 y = windSusSpeed)
# =======
thePlot = ggplot(data=weatherData) +
  stat_boxplot(mapping=aes(x=pressureFact, y=windSusSpeed), 
               na.rm=TRUE,
               geom = "errorbar",
               width = 0.2) +
  geom_boxplot(mapping=aes(x=pressureFact, y=windSusSpeed),
               outlier.shape = 11,
               outlier.color = "grey20",
               outlier.alpha = 0.6,
               outlier.size = 1.5) +
  theme_bw() +
  labs(title = "Wind Sus Speed vs. Pressure Level",
       subtitle = "Lansing, Michigan: 2016",
       x = "Pressure Level",
       y = "Wind Sus Speed");
ggstatsplot::ggbetweenstats(data=weatherData,
                 x = weatherData$pressureFact,
                 y = weatherData$windSusSpeed,
# >>>>>>> 3176414339584f80f005c11966b295a28586739f
                 outlier.tagging = TRUE,
                 outlier.label = dateYr)




