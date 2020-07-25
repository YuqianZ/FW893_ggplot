source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-3.csv", 
                        stringsAsFactors = FALSE );

#### Part 1: using grep to find days with a specific weather event
rainyDays = grep(weatherData$weatherType, pattern="RA");   # any day with rain
breezyDays = grep(weatherData$weatherType, pattern="BR");  # any breezy day

#### Part 2: Scatterplot for Humidity vs. Temperature on breezy days
plot1 = ggplot(data=weatherData[breezyDays,]) +
  geom_point(mapping=aes(x=avgTemp, y=relHum)) +
  theme_classic() +
  labs(title = "Humidity vs. Temperature (Breezy Days)",
       subtitle = "Lansing, Michigan: 2016",
       x = "Degrees (Fahrenheit)",
       y = "Relative Humidity");
plot(plot1);

#### Part 3: Combine event using set operations
rainyAndBreezy = intersect(rainyDays, breezyDays); # days with rain AND wind
rainyOrBreezy = union(rainyDays, breezyDays);      # days with rain OR wind
rainyNotBreezy = setdiff(rainyDays, breezyDays);   # days with rain but NOT wind
breezyNotRainy = setdiff(breezyDays, rainyDays);   # days with wind but NOT rain

#### Part 4: Creating plots for all rainy day/breezy day combinations
plot2 = ggplot(data=weatherData[rainyDays,]) +
  geom_point(mapping=aes(x=avgTemp, y=relHum)) +
  theme_classic() +
  labs(title = "Humidity vs. Temperature (rainy days)",
       subtitle = "Lansing, Michigan: 2016",
       x = "Degrees (Fahrenheit)",
       y = "Relative Humidity");

plot3 = ggplot(data=weatherData[rainyAndBreezy,]) +
  geom_point(mapping=aes(x=avgTemp, y=relHum)) +
  theme_classic() +
  labs(title = "Hum vs. Temp (Rainy AND Breezy)",
       subtitle = "Lansing, Michigan: 2016",
       x = "Degrees (Fahrenheit)",
       y = "Relative Humidity");

plot4 = ggplot(data=weatherData[rainyOrBreezy,]) +
  geom_point(mapping=aes(x=avgTemp, y=relHum)) +
  theme_classic() +
  labs(title = "Hum vs. Temp (Rainy or Breezy)",
       subtitle = "Lansing, Michigan: 2016",
       x = "Degrees (Fahrenheit)",
       y = "Relative Humidity");

plot5 = ggplot(data=weatherData[rainyNotBreezy,]) +
  geom_point(mapping=aes(x=avgTemp, y=relHum)) +
  theme_classic() +
  labs(title = "Hum vs. Temp (Rainy and NOT Breezy)",
       subtitle = "Lansing, Michigan: 2016",
       x = "Degrees (Fahrenheit)",
       y = "Relative Humidity");

plot6 = ggplot(data=weatherData[breezyNotRainy,]) +
  geom_point(mapping=aes(x=avgTemp, y=relHum)) +
  theme_classic() +
  labs(title = "Hum vs. Temp (Breezy and NOT Rainy)",
       subtitle = "Lansing, Michigan: 2016",
       x = "Degrees (Fahrenheit)",
       y = "Relative Humidity");

#### Part 5: Arranging plots on one canvas by rows 
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, 
             nrow=3);

#### Part 6: Arranging plots on canvas by columns
grid.arrange(plot6, plot5, plot4, plot3, plot2,
             ncol=3);

#### Part 7: Customize arrangements using matrix
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6,
             layout_matrix = rbind(c(4,5,6),
                                   c(3,2,1)));

#### Part 8: Add empty spaces to customized arrangement
grid.arrange(plot3, plot4, plot5,
             layout_matrix = rbind(c(NA,1,2),
                                   c(3,NA,NA)));

#### Part 9: Extending plots across rows and columns
grid.arrange(plot1, plot2, plot3, plot4, 
             layout_matrix = rbind(c(1,1,2),
                                   c(1,1,NA),
                                   c(4,3,3),
                                   c(4,NA,NA)));

#### Issue 1: Plots listed must be used in matrix
####          Plot 2 is not used in the layout_matrix
grid.arrange(plot1, plot2, plot3, plot4, plot5
             layout_matrix = rbind(c(1,1,5),
                                   c(1,1,NA),
                                   c(4,3,3),
                                   c(4,NA,NA)));

#### Issue 2: Invalid index error (there is no 5th plot)
grid.arrange(plot1, plot2, plot3, plot4
             layout_matrix = rbind(c(1,1,5),
                                   c(1,1,NA),
                                   c(4,3,3),
                                   c(4,NA,NA)));


# grid.arrange() makes two important assumptions:
#   
# All plots take up a rectangular space on the grid
# All plots are represented only once on the canvas

#### Issue 3: The grid will extend discontinuous plots to fill a rectangle
####          plot1 will be extended to a rectangle that is 2x2
grid.arrange(plot1, plot2,
             layout_matrix = rbind(c(1,NA,2),
                                   c(NA,1,NA)));

#### Issue 4: Overlapping plots -- priority goes to the later plot
####          In this case, plot2 overlaps plot1
grid.arrange(plot1, plot2,
             layout_matrix = rbind(c(1,NA,2),
                                   c(NA,NA,1)));

#
# grid.arrange() puts plots with higher indexes on top
#

#### Issue 5: Hidden plots due to overlapping
####          In this case, plot2 completely covers up plot1
grid.arrange(plot1, plot2,
             layout_matrix = rbind(c(2,NA,1),
                                   c(NA,NA,2)));

#### Issue 6: Error in plot placement
grid.arrange(plot1, plot2, plot3, plot4, 
             layout_matrix = rbind(c(1,1,4),
                                   c(1,2,NA),
                                   c(4,3,3),
                                   c(4,NA,NA)));
