source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-3.csv", 
                        stringsAsFactors = FALSE );

#### 1: Convert trace rain to the numeric value 0.005
# Copy precip values to a new column, precipNum
weatherData$precipNum = weatherData$precip;

# Go through all rows in weatherData
for(i in 1:nrow(weatherData))
{
  # check precipNum value -- if the value is T, change to 0.05
  if(weatherData$precipNum[i] == "T")
  {
    weatherData$precipNum[i] = 0.005;
  }
}

# convert precipNum column to numeric
weatherData$precipNum = as.numeric(weatherData$precipNum);

#### 2: Plot Precipitation vs. Humidity
thePlot = ggplot(data=weatherData) +
  geom_text(mapping=aes(x=relHum, y=precipNum, color=1:nrow(weatherData),
                        label=avgTemp),
            size=2.5) +
  scale_color_gradientn(colors=c("blue","darkgreen","red")) +
                        # values=c(0, 0.2, 0.55, 0.85, 1),
                        # # breaks=c(winterIndex, springIndex,
                        # #          summerIndex, fallIndex),
                        # labels=c("winter","spring","summer","fall")) +
  scale_y_continuous(trans = "log10") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.key.height = unit(11, units="pt"),  # height
        legend.key.width = unit(30, units="pt"),   # width
        legend.direction = "horizontal",           # alignment
        legend.position = c(0.2, 0.88)) +         # position
  
  labs(title = "Precipitation vs. Humidity",
       subtitle = "Lansing, Michigan: 2016",
       x = "Relative Humidity",
       y = "Precipitation",
       color = "");
plot(thePlot);



