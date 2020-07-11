source(file="scripts/reference.R");
weatherData = read.csv(file="data/LansingNOAA2016.csv", 
                       stringsAsFactors = FALSE);

#### Part 1: Add year to date values ####
# a) save the date vector from the data frame to the variable theDate
theDate = weatherData$date;         # get all values from the dates column
# theDate = weatherData[["date"]];  # equivalent to previous line
# theDate = weatherData[ , "date"]; # equivalent to previous 2 lines in base R

# b) append (paste) "-2016" to all values in theDate
theDate = paste(theDate, "-2016", sep="");   
# theDate = paste(theDate, "2016", sep="-");  # functionally equivalent to the previous line

# c) Save the values in Date format
# %m: 2-digit month (00-12)
# %d: 2-digit date (00-31)
# %Y: 4-digit year (2016) 
# lowercase y is a 2-digit year whereas capital Y is a 4-digit year
theDate = as.Date(theDate, format="%m-%d-%Y");

# d) Save theDate back to the data frame as a new column
weatherData$dateYr = theDate;
# weatherData[["dateYr"]] = theDate; # equivalent to previous line
# weatherData[, "dateYr"] = theDate; # equivalent to previous 2 lines in base R

# Convert F to C degrees
maxTempF = weatherData$maxTemp
maxTempC <- (maxTempF-32)*5/9
weatherData$maxTempC = maxTempC

minTempF = weatherData$minTemp
minTempC <- (minTempF-32)*5/9
weatherData$minTempC = minTempC

avgTempF = weatherData$avgTemp
avgTempC <- (avgTempF-32)*5/9
weatherData$avgTempC = avgTempC

# plotData = ggplot(data=weatherData) +
#   geom_line(mapping=aes(x=dateYr, y=maxTempC)) +
#   geom_line(mapping=aes(x=dateYr, y=minTempC));
  # geom_line(mapping=aes(x=dateYr, y=avgTempC));
  
# plot(plotData);

#### Make figure beautiful ###
plotData = ggplot(data=weatherData) +
  geom_line(mapping=aes(x=dateYr, y=maxTempC),
            color="palevioletred1") +
  geom_line(mapping=aes(x=dateYr, y=minTempC),
            color="aquamarine2") +
  # geom_line(mapping=aes(x=dateYr, y=avgTempC),
  #           color="gold2") +
  geom_smooth(mapping=aes(x=dateYr, y=avgTempC),
              color="orange", 
              method="loess",
              linetype=4,
              fill="lightblue") +
  labs(title = "Temperature vs. Date",
       subtitle = "Lansing, Michigan: 2016",
       x = "Date",
       y = "Temperature (C)") +
  theme(panel.background = element_rect(fill="grey25",
                                        size=2, color="grey0"),
        panel.grid.minor = element_line(color="grey50", linetype=4),
        panel.grid.major = element_line(color="grey100"),
        plot.background = element_rect(fill = "lightgreen"),
        plot.title = element_text(hjust = 1),
        plot.subtitle = element_text(hjust = 1),
        axis.text.x = element_text(color="blue", family="mono", size=9),
        axis.text.y = element_text(color="red", family="mono", size=9)) +
  scale_y_continuous(limits = c(-20,40),
                     breaks = seq(from=-20, to=40, by=20)) +
  scale_x_date(limits=c(as.Date("2016-03-21"), 
                        as.Date("2016-09-21")),
               date_breaks = "8 weeks", 
               date_labels = format("%b-%d-%Y"));

plot(plotData);
