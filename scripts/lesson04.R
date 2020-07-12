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

plotData = ggplot(data=weatherData) +
  geom_line(mapping=aes(x=dateYr, y=maxTemp)) +
  geom_line(mapping=aes(x=dateYr, y=minTemp));
plot(plotData);

#### Part 3: Add labels and colors ###
plotData = ggplot(data=weatherData) +
  geom_line(mapping=aes(x=dateYr, y=maxTemp),
            color="palevioletred1") +
  geom_line(mapping=aes(x=dateYr, y=minTemp),
            color="aquamarine2") +
  labs(title = "Temperature vs. Date",
       subtitle = "Lansing, Michigan: 2016",
       x = "Date",
       y = "Temperature (F)");
plot(plotData);

#### Part 4: Add average temperature and smooth it out ###
plotData = ggplot(data=weatherData) +
  geom_line(mapping=aes(x=dateYr, y=maxTemp),
            color="palevioletred1") +
  geom_line(mapping=aes(x=dateYr, y=minTemp),
            color="aquamarine2") +
  geom_smooth(mapping=aes(x=dateYr, y=avgTemp), 
              color="orange", 
              method="loess", 
              linetype=4, 
              fill="lightblue") +
  labs(title = "Temperature vs. Date",
       subtitle = "Lansing, Michigan: 2016",
       x = "Date",
       y = "Temperature (F)");
plot(plotData);

#### Part 5: Paneling changes ###
# grey0 is black, grey100 is white, and numbers in between are shades of grey
plotData = ggplot(data=weatherData) +
  geom_line(mapping=aes(x=dateYr, y=maxTemp),
            color="palevioletred1") +
  geom_line(mapping=aes(x=dateYr, y=minTemp),
            color="aquamarine2") +
  geom_smooth(mapping=aes(x=dateYr, y=avgTemp),
              color="orange", 
              method="loess",
              linetype=4,
              fill="lightblue") +
  labs(title = "Temperature vs. Date",
       subtitle = "Lansing, Michigan: 2016",
       x = "Date",
       y = "Temperature (F)") +
  # size and color relate to the border, fill is the inside color 
  theme(panel.background = element_rect(fill="grey25",
                                        size=2, color="grey0"),
        panel.grid.minor = element_line(color="grey50", linetype=4),
        panel.grid.major = element_line(color="grey100"));
plot(plotData);

#### Part 6: Making changes outside the panel ###
plotData = ggplot(data=weatherData) +
  geom_line(mapping=aes(x=dateYr, y=maxTemp),
            color="palevioletred1") +
  geom_line(mapping=aes(x=dateYr, y=minTemp),
            color="aquamarine2") +
  geom_smooth(mapping=aes(x=dateYr, y=avgTemp),
              color="orange", 
              method="loess",
              linetype=4,
              fill="lightblue") +
  labs(title = "Temperature vs. Date",
       subtitle = "Lansing, Michigan: 2016",
       x = "Date",
       y = "Temperature (F)") +
  # size and color relate to the border, fill is the inside color 
  theme(panel.background = element_rect(fill="grey25",
                                        size=2, color="grey0"),
        panel.grid.minor = element_line(color="grey50", linetype=4),
        panel.grid.major = element_line(color="grey100"),
        plot.background = element_rect(fill = "lightgreen"),
        plot.title = element_text(hjust = 0.45),
        plot.subtitle = element_text(hjust = 0.42),
        # change the x-axis text style or y-axis text style you can use the components: axis.text.x and axis.text.y
        axis.text = element_text(color="blue", family="mono", size=9));
plot(plotData);

#### Part 7: Scaling continuous values ###
plotData = ggplot(data=weatherData) +
  geom_line(mapping=aes(x=dateYr, y=maxTemp),
            color="palevioletred1") +
  geom_line(mapping=aes(x=dateYr, y=minTemp),
            color="aquamarine2") +
  geom_smooth(mapping=aes(x=dateYr, y=avgTemp),
              color="orange", 
              method="loess",
              linetype=4,
              fill="lightblue") +
  labs(title = "Temperature vs. Date",
       subtitle = "Lansing, Michigan: 2016",
       x = "Date",
       y = "Temperature (F)") +
  theme(panel.background = element_rect(fill="grey25",
                                        size=2, color="grey0"),
        panel.grid.minor = element_line(color="grey50", linetype=4),
        panel.grid.major = element_line(color="grey100"),
        plot.background = element_rect(fill = "lightgreen"),
        plot.title = element_text(hjust = 0.45),
        plot.subtitle = element_text(hjust = 0.42),
        axis.text = element_text(color="blue", family="mono", size=9))+
  scale_y_continuous(limits = c(-15,90),
                     breaks = seq(from=-15, to=90, by=20)); 

plot(plotData);

#### Part 8: Scaling discrete values (dates) ###
plotData = ggplot(data=weatherData) + 
  geom_line(mapping=aes(x=dateYr, y=maxTemp),
            color="palevioletred1") +
  geom_line(mapping=aes(x=dateYr, y=minTemp),
            color="aquamarine2") +
  geom_smooth(mapping=aes(x=dateYr, y=avgTemp),
              color="orange", 
              method="loess",
              linetype=4,
              fill="lightblue") +
  labs(title = "Temperature vs. Date",
       subtitle = "Lansing, Michigan: 2016",
       x = "Date",
       y = "Temperature (F)") +
  # size and color relate to the border, fill is the inside color 
  theme(panel.background = element_rect(fill="grey25",
                                        size=2, color="grey0"),
        panel.grid.minor = element_line(color="grey50", linetype=4),
        panel.grid.major = element_line(color="grey100"),
        plot.background = element_rect(fill = "lightgreen"),
        plot.title = element_text(hjust = 0.45),
        plot.subtitle = element_text(hjust = 0.42),
        axis.text = element_text(color="blue", family="mono", size=9))+
  scale_y_continuous(limits = c(-15,90),
                     breaks = seq(from=-15, to=90, by=20)) +
  scale_x_date(limits=c(as.Date("2016-03-21"), 
                        as.Date("2016-12-21")),
               date_breaks = "6 weeks", 
               date_labels = format("%m/%d"));
plot(plotData);




