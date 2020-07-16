source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-3.csv", 
                        stringsAsFactors = FALSE );

#### Part 1: A different way to arrange x-axis values
thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp),
               na.rm=TRUE) +
  scale_x_discrete(limits=c("North", "East", "South", "West")) +
  theme_bw() +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(thePlot);

### Part 2: Group boxplots by wind speed levels
thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp,
                           fill=windSpeedLevel), # GGPlot automatically generates a legend when the fill parameter is used.
               na.rm=TRUE) +
  scale_x_discrete(limits=c("North", "East", "South", "West")) +
  theme_bw() +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(thePlot);

### Part 3: Re-order group as factors
thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp,
                           fill=factor(windSpeedLevel,
                                       levels=c("Low", "Medium", "High"))),
               na.rm=TRUE) +
  theme_bw() +
  scale_x_discrete(limits = c("North", "East", "South", "West")) +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(thePlot);

### Part 4: Changing the legend title
thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp,
                           fill=factor(windSpeedLevel,
                                       levels=c("Low", "Medium", "High"))),
               na.rm=TRUE) +
  theme_bw() +
  scale_x_discrete(limits = c("North", "East", "South", "West")) +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)",
       fill = "Wind Speeds");
plot(thePlot);

### Part 5: Adding color using rgb()
thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp,
                           fill=factor(windSpeedLevel,
                                       levels=c("Low", "Medium", "High"))),
               na.rm=TRUE) +
  theme_bw() +
  scale_x_discrete(limits = c("North", "East", "South", "West")) +
  scale_fill_manual(values = c(rgb(red=1, green=1, blue=0),        # low
                               rgb(red=1, green=0.2, blue=0),      # medium
                               rgb(red=0.5, green=0, blue=0.8))) + # high
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)",
       fill = "Wind Speeds");   # changes the legend (fill) title
plot(thePlot);

# Where fill grouped by a variable within the plot, 
# facets uses the grouping variable (in this case: windSpeedLevel) to create multiple plots.

### Part 6: Using facets along the y-axis
thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp),
               na.rm=TRUE) +
  theme_bw() +
  scale_x_discrete(limits = c("North", "East", "South", "West")) +
  facet_grid(facets=windSpeedLevel ~ .) + # facet in vertical direction
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(thePlot);

### Part 7: Using facets along the x-axis
thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp),
               na.rm=TRUE) +
  theme_bw() +
  scale_x_discrete(limits = c("North", "East", "South", "West")) +
  facet_grid(facets=. ~ windSpeedLevel) + # facet in horizontal direction
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(thePlot);

### Part 8: Ordering facets
thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp), na.rm=TRUE) +
  theme_bw() +
  scale_x_discrete(limits = c("North", "East", "South", "West")) +
  facet_grid(facets= . ~ factor(windSpeedLevel,
                                levels=c("Low", "Medium", "High"))) +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(thePlot);

### Part 9: Filling and coloring facets
thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp),
               na.rm=TRUE,
               color="blue",   # outline color
               fill="red") +   # fill color
  theme_bw() +
  scale_x_discrete(limits = c("North", "East", "South", "West")) +
  facet_grid(facets= . ~ factor(windSpeedLevel,
                                levels=c("Low", "Medium", "High"))) +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(thePlot);

### Part 10: Filling and coloring facets
thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp),
               na.rm=TRUE,
               # Aesthetics must be either length 1 or the same as the data (12)
               color=c("blue", rep("black", 3), 
                       "green", rep("black", 3), 
                       "orange", rep("black", 3)),
               fill=c(rep(NA, 8), rep("red", 3), NA)) +
  theme_bw() +
  scale_x_discrete(limits = c("North", "East", "South", "West")) +
  facet_grid(facets=.~factor(windSpeedLevel,
                             levels=c("Low", "Medium", "High"))) +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(thePlot);

### Part 11: Changing facet labels
windLabels = c(Low = "Light Winds",
               Medium = "Medium Winds",
               High = "Strong Winds");

thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=windDir, y=changeMaxTemp),
               na.rm=TRUE,
               color=c("blue", rep("black", 3),
                       "green", rep("black", 3),
                       "orange", rep("black", 3)),
               fill=c(rep(NA, 8), rep("red", 3), NA)) +
  theme_bw() +
  scale_x_discrete(limits = c("North", "East", "South", "West")) +
  facet_grid(facets=.~factor(windSpeedLevel,
                             levels=c("Low", "Medium", "High")),
             # the function as_labeller() explicitly says that the vector is a vector of labels
             labeller=as_labeller(windLabels)) +
  labs(title = "Change in Temperature vs. Wind Direction",
       subtitle = "Lansing, Michigan: 2016",
       x = "Wind Direction",
       y = "Degrees (Fahrenheit)");
plot(thePlot);



