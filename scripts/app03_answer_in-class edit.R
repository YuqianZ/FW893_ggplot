{
  source(file="scripts/reference.R");  # this line will be in all your scripts
  weatherData = read.csv(file="data/LansingNOAA2016.csv", 
                         stringsAsFactors = FALSE);

  # This scatterplot show that...
  plotData = ggplot(data=weatherData) +
             geom_point(mapping=aes(x=abs(tempDept), y=windSpeed),
                        color=rgb(red=0, green=.6, blue=.6), 
                        size=2, 
                        shape=23,
                        alpha = 0.7 ) +
             theme_bw() +
             labs(title = "Wind Speed vs. Temperature Difference",
                  subtitle = "Lansing, Michigan: 2016",
                  x = "Temperature (F)",
                  y = "Humidity (%)") +
             scale_y_continuous(limits = c(0,20),
                                     breaks = seq(from=0, to=20, by=5)) +
             scale_x_continuous(limits = c(-1,29),
                                breaks = seq(from=-1, to=30, by=3)) + 
                 theme(axis.text.y=element_text(angle=30),
                   axis.title.x=element_text(size=15, face="italic", 
                                             color=rgb(red=.8, green=.3, blue=0)),
                   plot.title = element_text(hjust = 0.8),
                   plot.subtitle=element_text(face="bold.italic",
                                              color ="brown", hjust = 0.7),
                   panel.background = element_rect(fill="grey80",
                                                     size=1, color="grey0"),
                   panel.grid.minor = element_line(color="grey95", linetype=4),
                   panel.grid.major = element_line(color="grey95", linetype = 4),
                   plot.background = element_rect(fill = "burlywood1"),
                   aspect.ratio = 9/16) +
             # default confidence level is 95%
             geom_smooth(mapping=aes(x=abs(tempDept), y=windSpeed), 
                         method="lm",
                         color="purple", 
                         size=1.2, 
                         linetype=5, 
                         fill="yellow");   
  plot(plotData);
  
  ggsave(filename="images/plot.jpeg",
         plot=plotData,
         width = 5,
         height = 10,
         dpi = 360,
         units = "cm");
}  