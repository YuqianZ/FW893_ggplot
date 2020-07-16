source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-3.csv", 
                        stringsAsFactors = FALSE );

relHumQuant = quantile(weatherData$relHum, probs=c(.30, .70));

for(day in 1:nrow(weatherData))  # nrow(weatherData) = 366
{
  # if the value in windSpeed is less than or equal to the low quant value
  if(weatherData$relHum[day] <= relHumQuant[1])      # <= 63
  {
    weatherData$humidityLevel[day] = "Low";
  }
  # if the value in windSpeed is greater than or equal to the high quant value
  else if(weatherData$relHum[day] >= relHumQuant[2]) # >= 75
  {
    weatherData$humidityLevel[day] = "High";
  }
  else # the value in windSpeed is between 6.2 and 10.4
  {
    weatherData$humidityLevel[day] = "Medium";
  }
}

# Create factor/label names
humidityFact = factor(weatherData$humidityLevel,
                      levels=c("Low", "Medium", "High"));

windLabels = c(Low = "Light Winds",
               Medium = "Medium Winds",
               High = "Strong Winds");

# Plot
thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=humidityFact, y=stnPressure,
                           fill=factor(windDir,
                                       levels=c("North", "East", "South", "West"))))+
  theme_bw() +
  facet_grid(facets= . ~ factor(windSpeedLevel,
                                levels=c("Low", "Medium", "High")),
             labeller=as_labeller(windLabels)) +
  labs(title = "Wind Pressurve vs. Relative Humidity",
       subtitle = "Lansing, Michigan: 2016",
       x = "Relative Humidity Level",
       y = "Wind Pressure",
       fill = "Wind Direction");
plot(thePlot);


