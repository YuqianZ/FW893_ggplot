source(file="scripts/reference.R");   # include the reference.r file

weatherData2 = read.csv(file="data/LansingNOAA2016.csv",
                       stringsAsFactors = FALSE); 

## This scatterplot shows the relationship between Daily Temperature Departure and Average Wind Speed
## Use Lansing, Michigan, 2016 data for example 
plotData = ggplot( data=weatherData2 ) +
  geom_point( mapping=aes(x=windSpeed, y=abs(tempDept)), 
              color=rgb(red = 0, green = .6, blue = .6), size=2, shape=18,
              alpha = 0.7 ) +
  theme_bw() + ## Make sure theme_bw() is called before other theme changes are made.
  labs(title = "Daily Temperature Departure vs. Average Wind Speed",
       subtitle = "Lansing, Michigan: 2016",
       x = "Average Wind Speed (m/s)",
       y = "Daily Temperature Departure (celsius degree)") +
  theme(axis.title.x=element_text(size=10, color="darkblue", face="bold"),
        axis.title.y=element_text(size=10, color="darkblue", face="bold"), 
        plot.title=element_text(size=16, face="bold", 
                                color ="darkblue"),
        plot.subtitle=element_text(size=12, face="bold", 
                                   color ="blue", family="serif"),
        axis.text.y = element_text(angle=45, hjust=1)) +
  geom_smooth(mapping=aes(x=windSpeed, y=abs(tempDept)),
              method="lm",
              color="red", 
              size=0.8, 
              linetype=3, 
              fill="lightpink");
plot(plotData);

## Save ggplot in jpeg
ggsave(
  "images/scatterplot.jpeg",
  plot = plotData,
  # device = "jpeg",
  # path = "images/scatterplot.jpeg",
  # scale = 1,
  width = 12,
  height = 7,
  units = c("cm"),
  # dpi = 300,
  # limitsize = TRUE,
)


## Save ggplot in png
ggsave(
  "images/pngFiles/scatterplot.png",
  plot = plotData,
  # device = "jpeg",
  # path = "images/scatterplot.jpeg",
  # scale = 1,
  width = 6,
  height = 8,
  units = c("in"),
  # dpi = 300,
  # limitsize = TRUE,
)