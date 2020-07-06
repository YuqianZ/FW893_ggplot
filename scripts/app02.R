# execute the lines of code from reference.r
source(file="scripts/reference.r"); 

# read in CSV file and save the content to packageData
packageData2 = read.csv(file="data/accdeaths.csv");
  # View(packageData2)

plotData2 = ggplot( data=packageData2 ) +  
  geom_point( mapping=aes(x=time, y=value) ) + 
  ggtitle( label="Accidental Deaths in the US (1973-1979)" ) +
  scale_y_continuous( breaks = c(7000, 9000, 11000) ) +
  scale_x_continuous( breaks = seq(from=1973, to=1979, by=0.5)) +
  theme( axis.text.x=element_text(angle=45, hjust=1) ) +
  xlab("Years") +
  ylab("Number of Accidental Deaths");
plot(plotData2);
