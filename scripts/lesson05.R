source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016.csv", 
                        stringsAsFactors = FALSE );

#### Part 1: Add year to date vector and save back to data frame
theDate = weatherData$date;                    # save date column to vector
theDate = paste(theDate, "-2016", sep="");     # append -2016 to vector
theDate = as.Date(theDate, format="%m-%d-%Y"); # format vector as Date
weatherData$dateYr = theDate;                  # save vector to Data Frame      

#### Part 2: Set up the season and date variables
# create a season vector that has the same length as theDate vector
season = vector(mode="character", length=length(theDate));

# create date variables for the beginning of each season
springStart = as.Date("03-21-2016", format="%m-%d-%Y");
summerStart = as.Date("06-21-2016", format="%m-%d-%Y");
fallStart = as.Date("09-21-2016", format="%m-%d-%Y");
winterStart = as.Date("12-21-2016", format="%m-%d-%Y");

#### Part 3: Create a season vector based on theDates vector
for(i in 1:length(theDate)) # go through each date
{
  # if the date falls with the spring season
  if(theDate[i] >= springStart && theDate[i] < summerStart)
  {
    season[i] = "Spring";
  }
  # if the date falls with the summer season
  else if(theDate[i] >= summerStart && theDate[i] < fallStart)
  {
    season[i] = "Summer";
  }
  # if the date falls with the fall season
  else if(theDate[i] >= fallStart && theDate[i] < winterStart)
  {
    season[i] = "Fall";
  }
  # if the date falls with the winter season --
  #    using || because dates are not continuous)
  else if(theDate[i] >= winterStart || theDate[i] < springStart)
  {
    season[i] = "Winter";
  }
  else # something went wrong... always good to check
  {
    season[i] = "Error";
  }
}

#### Part 4: create a new column in weatherData called season and set it to
#         the season vector
weatherData$season = season;

#### Part 5: Create a histogram of temperatures for the year
plotData = ggplot( data=weatherData ) + 
  geom_histogram( mapping=aes(x=avgTemp, y=..count..) );
plot(plotData);

#### Part 6: Parameter changes to the histogram
plotData = ggplot( data=weatherData ) + 
  geom_histogram( mapping=aes(x=avgTemp, y=..count..),
                  bins=40,
                  color="grey20",
                  fill="darkblue");
plot(plotData);

#### Part 7: Change theme, add titles and labels
plotData = ggplot( data=weatherData ) +
  geom_histogram( mapping=aes(x=avgTemp, y=..count..),
                  bins=40,
                  color="grey20",
                  fill="darkblue") +
  theme_classic() +
  labs(title = "Temperature Histogram",
       subtitle = "Lansing, Michigan: 2016",
       x = "Average Temp (Fahrenheit)",
       y = "Count");
plot(plotData);

#### Part 8: Using binwidths and density
plotData = ggplot( data=weatherData ) + 
  geom_histogram(mapping=aes(x=avgTemp, y=..density..),
                 binwidth=4,
                 color="grey20",
                 fill="darkblue") +
  theme_classic() +
  labs(title = "Temperature Histogram",
       subtitle = "Lansing, Michigan: 2016",
       x = "Average Temp (Fahrenheit)",
       y = "Density");
plot(plotData);

#### Part 9: Add vertical lines representing mean and median
plotData = ggplot( data=weatherData ) + 
  geom_histogram(mapping=aes(x=avgTemp, y=..density..),
                 binwidth=4,
                 color="grey20",
                 fill="darkblue") +
  geom_vline(mapping=aes(xintercept=mean(avgTemp)),
             color="red",
             size=1.2) +
  geom_vline(mapping=aes(xintercept=median(avgTemp)),
             color="green",
             size=1.2) +
  theme_classic() +
  labs(title = "Temperature Histogram",
       subtitle = "Lansing, Michigan: 2016",
       x = "Average Temp (Fahrenheit)",
       y = "Density");
plot(plotData);

#### Part 10: Create a histogram for each season (faceting)
plotData = ggplot(data=weatherData) +
  geom_histogram(mapping=aes(x=avgTemp, y=..count..),
                 bins=40,
                 color="grey20",
                 fill="darkblue") +
  theme_classic() +
  facet_grid( facet= season ~ .) +
  # Group by season along the x-axis
  # facet_grid( facet= . ~ season) +
  labs(title = "Temperature Histogram",
       subtitle = "Lansing, Michigan: 2016",
       x = "Average Temp (Fahrenheit)",
       y = "Count");
plot(plotData);

#### Part 11: Create a stacked histogram for each season - Very informative process
plotData = ggplot(data=weatherData) + 
  geom_histogram(mapping=aes(x=avgTemp, y=..count.., fill=season),
                 bins=40,
                 color="grey20",
                 position="stack") +
  theme_classic() +
  labs(title = "Temperature Histogram",
       subtitle = "Lansing, Michigan: 2016",
       x = "Average Temp (Fahrenheit)",
       y = "Count");
plot(plotData);


#### End of Code: Save the modified data frame to a new CSV file
write.csv(weatherData, file="data/LansingNOAA2016-2.csv");

