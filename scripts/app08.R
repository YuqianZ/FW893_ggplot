source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-3.csv", 
                        stringsAsFactors = FALSE );

# Create a month column
weatherData$month = format.Date(weatherData$dateYr, format="%b");
# Create a weather_type vector including only first two values of a weather type
weatherData$weather_type <- strtrim(weatherData$weatherType, 2)
# weatherData$weather_type[is.na(weatherData$weather_type)] <- "NA";

# 2. Cool days bar plot
thePlot1 = ggplot(data=weatherData) +
  geom_col(mapping=aes(x=month, y=coolDays, fill=weather_type),
           width=0.6) +
  scale_x_discrete(limits = month.abb) +
  # scale_fill_manual(values = C("cadetblue1", "chartreuse1", "chocolate1", "deepskyblue1",
  #                   "darkblue", "darkorchid1", "firebrick3", "darkgoldenrod1")) +
  theme_bw() +
  labs(title = "Monthly Cool Days",
       subtitle = "Lansing, Michigan: 2016",
       x = "Month",
       y = "Number of Cool Days",
       fill = "Weather Type");

plot(thePlot1);

# 3. Heat dats bar plot
thePlot2 = ggplot(data=weatherData) +
  geom_col(mapping=aes(x=month, y=heatDays, fill=weather_type),
           width=0.6) +
  scale_x_discrete(limits = month.abb) +
  # scale_fill_manual(values = C("cadetblue1", "chartreuse1", "chocolate1", "deepskyblue1",
  #                   "darkblue", "darkorchid1", "firebrick3", "darkgoldenrod1")) +
  geom_hline(mapping = aes(yintercept= sum(coolDays)),
             color= rgb(red=0, green=0.5, blue=0),
             size=2,
             linetype=2) +
  theme_bw() +
  labs(title = "Monthly Heat Days",
       subtitle = "Lansing, Michigan: 2016",
       x = "Month",
       y = "Number of Heat Days",
       fill = "Weather Type") +
  annotate("text", x = 8, y = 900, label = "Total cool days");
plot(thePlot2);

# 4. Combine plots
# thePlot1 + thePlot2

# value used to transform the data
coeff <- 0.25
thePlot3 = ggplot(data=weatherData) +
  geom_col(mapping=aes(x=month, y=coolDays),
           width=0.4, fill="blue") +
  geom_col(mapping=aes(x=month, y=heatDays),
           width=0.4, fill="red") +
  scale_y_continuous(
    name = "Number of Heat Days",
    sec.axis = sec_axis(~. *coeff, name = "Number of Cool Days")
  ) +
  scale_x_discrete(limits = month.abb) +
  # scale_fill_manual(values = C("cadetblue1", "chartreuse1", "chocolate1", "deepskyblue1",
  #                   "darkblue", "darkorchid1", "firebrick3", "darkgoldenrod1")) +
  theme_bw() +
  labs(title = "Monthly Heat/Cool Days",
       subtitle = "Lansing, Michigan: 2016",
       x = "Month");
plot(thePlot3)




