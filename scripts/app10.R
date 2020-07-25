source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-3.csv", 
                        stringsAsFactors = FALSE );


#### 2. Histogram of tempDept with condition [sunny days]
sunnyDays = grep(weatherData$weatherType, pattern="SN");   # any day with sun

plot1 = ggplot(data=weatherData[sunnyDays,]) +
  geom_histogram(mapping=aes(x=tempDept, y=..count..),
                 fill="goldenrod2") +
  geom_vline(mapping = aes(xintercept = mean(tempDept)),
             color= "red",
             size=2,
             linetype=2) +
  geom_text(aes(x=mean(tempDept), y=6), label="Average TempDept", color="goldenrod3") +
  theme_classic() +
  labs(title = "TempDept Distribution in Rainy Days",
       subtitle = "Lansing, Michigan: 2016",
       x = "TempDept",
       y = "Count");
plot(plot1);


#### 3. Histogram of tempDept with more conditions
# [rainy days], [breezy days]
rainyDays = grep(weatherData$weatherType, pattern="RA");   # any day with rain
breezyDays = grep(weatherData$weatherType, pattern="BR");   # any day with breeze

plot2 = ggplot(data=weatherData[rainyDays,]) +
  geom_histogram(mapping=aes(x=tempDept, y=..count..),
                 fill="dodgerblue2") +
  geom_vline(mapping = aes(xintercept = mean(tempDept)),
             color= "red",
             size=2,
             linetype=2) +
  geom_text(aes(x=mean(tempDept), y=11.5), label="Average TempDept", color="dodgerblue3") +
  theme_classic() +
  labs(title = "TempDept Distribution in Breezy Days",
       subtitle = "Lansing, Michigan: 2016",
       x = "TempDept",
       y = "Count");
plot(plot2);

plot3 = ggplot(data=weatherData[breezyDays,]) +
  geom_histogram(mapping=aes(x=tempDept, y=..count..),
                 fill="palegreen2") +
  geom_vline(mapping = aes(xintercept = mean(tempDept)),
             color= "red",
             size=2,
             linetype=2) +
  geom_text(aes(x=mean(tempDept), y=17), label="Average TempDept", color="palegreen3") +
  theme_classic() +
  labs(title = "TempDept Distribution in Breezy Days",
       subtitle = "Lansing, Michigan: 2016",
       x = "TempDept",
       y = "Count");
plot(plot3);


#### 4. Histogram of tempDept with more conditions (AND)
sunnyAndBreezy = intersect(sunnyDays, breezyDays); # days with sun AND wind

plot4 = ggplot(data=weatherData[sunnyAndBreezy,]) +
  geom_histogram(mapping=aes(x=tempDept, y=..count..),
                 fill="chartreuse2") +
  geom_vline(mapping = aes(xintercept = mean(tempDept)),
             color= "red",
             size=2,
             linetype=2) +
  geom_text(aes(x=mean(tempDept), y=5), label="Average TempDept", color="chartreuse3") +
  theme_classic() +
  labs(title = "TempDept Distribution in Sunny AND Breezy Days",
       subtitle = "Lansing, Michigan: 2016",
       x = "TempDept",
       y = "Count");
plot(plot4);


#### 5. Histogram of tempDept with more conditions (OR)
rainyOrBreezy = union(rainyDays, breezyDays);      # days with rain OR wind

plot5 = ggplot(data=weatherData[rainyOrBreezy,]) +
  geom_histogram(mapping=aes(x=tempDept, y=..count..),
                 fill="seagreen3") +
  geom_vline(mapping = aes(xintercept = mean(tempDept)),
             color= "red",
             size=2,
             linetype=2) +
  geom_text(aes(x=mean(tempDept), y=22), label="Average TempDept", color="seagreen4") +
  theme_classic() +
  labs(title = "TempDept Distribution in Rainy OR Breezy Days",
       subtitle = "Lansing, Michigan: 2016",
       x = "TempDept",
       y = "Count");
plot(plot5);


#### 6. Arrange 5 plots on one canvas
grid.arrange(plot1, plot2, plot3, plot4, plot5, 
             nrow=2);

#### 7. Arrange 3 plots on one canvas
grid.arrange(plot1, plot2, plot5,
             layout_matrix = rbind(c(1,1,2,2),
                                   c(1,1,NA,NA),
                                   c(NA,5,5,5)));

