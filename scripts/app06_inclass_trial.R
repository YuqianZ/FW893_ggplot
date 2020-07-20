source(file="scripts/reference.R");  
weatherData = read.csv(file="data/LansingNOAA2016.csv", 
                       stringsAsFactors = FALSE);

# grepl() returned boolean values: TRUE if they contain RA or SN, FALSE otherwise
daysPrecip = grepl(x=weatherData$weatherType, pattern="RA|SN");

# loop through all rows of data: 366
for(day in 1:nrow(weatherData))  
{
  if(daysPrecip[day] == TRUE)  
  {
    weatherData$precipitation[day] = "Yes";   
  }
  else   
  {
    weatherData$precipitation[day] = "No";   
  }
}

# plot out
thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=precipitation, y=relHum)) +
  theme_bw() +
  labs(title = "Relative Humidity vs. Precipitation",
       subtitle = "Lansing, Michigan: 2016",
       x = "Precipitation",
       y = "Humidity");
plot(thePlot);

