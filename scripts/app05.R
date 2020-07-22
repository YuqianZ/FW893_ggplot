source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-2.csv", 
                        stringsAsFactors = FALSE );

# Histogram of average humidity
plotData = ggplot( data=weatherData ) + 
  geom_histogram( mapping=aes(x=relHum, y=..count..) );
plot(plotData);

# create a biMonth vector that has the same length as dateYr vector
dateYr <- weatherData$dateYr
biMonth = vector(mode="character", length=length(dateYr));

# create date variables for the beginning of each season
biMonth_1_Start = as.Date("01-01-2016", format="%m-%d-%Y");
biMonth_2_Start = as.Date("03-01-2016", format="%m-%d-%Y");
biMonth_3_Start = as.Date("05-01-2016", format="%m-%d-%Y");
biMonth_4_Start = as.Date("07-01-2016", format="%m-%d-%Y");
biMonth_5_Start = as.Date("09-01-2016", format="%m-%d-%Y");
biMonth_6_Start = as.Date("11-01-2016", format="%m-%d-%Y");

# Create a biMonth vector based on dateYr vector
for(i in 1:length(dateYr)) # go through each date
{
  # if the date falls within Jan and Feb
  if(dateYr[i] >= biMonth_1_Start && dateYr[i] < biMonth_2_Start)
  {
    biMonth[i] = "JanFeb";
  }
  # if the date falls within Mar and Apr
  else if(dateYr[i] >= biMonth_2_Start && dateYr[i] < biMonth_3_Start)
  {
    biMonth[i] = "MarApr";
  }
  # if the date falls within May and Jun
  else if(dateYr[i] >= biMonth_3_Start && dateYr[i] < biMonth_4_Start)
  {
    biMonth[i] = "MayJun";
  }
  # if the date falls within Jul and Aug
  else if(dateYr[i] >= biMonth_4_Start && dateYr[i] < biMonth_5_Start)
  {
    biMonth[i] = "JulAug";
  }
  # if the date falls within Sep and Oct
  else if(dateYr[i] >= biMonth_5_Start && dateYr[i] < biMonth_6_Start)
  {
    biMonth[i] = "SepOct";
  }
  # if the date falls within Nov and Dec
  else if(dateYr[i] >= biMonth_6_Start)
  {
    biMonth[i] = "NovDec";
  }
  else # something went wrong... always good to check
  {
    biMonth[i] = "Error";
  }
}

#         the biMonth vector
weatherData$biMonth = biMonth;

# Create a histogram for each biMonth (faceting)
plotData = ggplot(data=weatherData) +
  geom_histogram(mapping=aes(x=relHum, y=..count..),
                 bins=40,
                 color="grey20",
                 fill="darkblue") +
  theme_classic() +
  facet_grid( facet= biMonth ~ .) +
  # Group by season along the x-axis
  # facet_grid( facet= . ~ biMonth) +
  labs(title = "Relative Humidity Histogram",
       subtitle = "Lansing, Michigan: 2016",
       x = "Reletive Humidity (Percentage)",
       y = "Count");
plot(plotData);

# Create a stacked histogram for each biMonth - Very informative process
plotData = ggplot(data=weatherData) + 
  geom_histogram(mapping=aes(x=relHum, y=..count.., fill=biMonth),
                 bins=40,
                 color="grey20",
                 position="stack") +
  geom_vline(mapping=aes(xintercept=mean(relHum[biMonth=="JanFeb"])),
             color="red",
             size=1.2) +
  geom_vline(mapping=aes(xintercept=median(relHum[biMonth=="JulAug"])),
             color="green",
             size=1.2) +
  theme_classic() +
  theme(legend.position = c(0.12,0.78))+
  labs(title = "Relative Humidity Histogram",
       subtitle = "Lansing, Michigan: 2016",
       x = "Reletive Humidity (Percentage)",
       y = "Count");
plot(plotData);

