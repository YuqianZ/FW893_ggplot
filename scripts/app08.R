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
  scale_fill_manual(values = c("cadetblue1", "chartreuse1", "chocolate1", "deepskyblue1",
                    "darkblue", "darkorchid1", "firebrick3", "darkgoldenrod1")) +
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
  # scale_fill_manual(values = c("cadetblue1", "chartreuse1", "chocolate1", "deepskyblue1",
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

# Create variables for sum of heat/cool days by month
df <- weatherData
num <- aggregate(coolDays~month, df, length)
names(num)[2] <- "num"
totalCool <- aggregate(coolDays~month, df, sum)
names(totalCool)[2] <- "totalCool"
coolDaysT <- merge(num, totalCool)

totalHeat <- aggregate(heatDays~month, df, sum)
names(totalHeat)[2] <- "totalHeat"
daysT <- merge(coolDaysT, totalHeat)


library(hrbrthemes)
## Trial
# value used to transform the data
coeff <- 0.25
thePlot6 = ggplot(data=daysT, mapping=aes(x=month)) +

  geom_col(mapping=aes(y=totalCool/coeff),
           width=0.4, fill="blue", position = position_nudge(x=-0.3)) +
  geom_col(mapping=aes(y=totalHeat),
           width=0.4, fill="red", position = position_nudge(x=0.3)) +
  
  scale_y_continuous(
    name = "Number of Heat Days",
    sec.axis = sec_axis(~. *coeff, name = "Number of Cool Days")
  ) +
  scale_x_discrete(limits = month.abb) +
  
  theme_classic() +
  
  theme(
    axis.title.y.left = element_text(color = "red", size=12),
    axis.title.y.right = element_text(color = "blue", size=12)
  ) +
  
  labs(title = "Monthly Heat/Cool Days",
       subtitle = "Lansing, Michigan: 2016",
       x = "Month");

plot(thePlot6)



################################################################################

# value used to transform the data
# coeff <- 0.25
# thePlot4 = ggplot(data=daysT) +
#   geom_col(mapping=aes(x=month, y=totalHeat),
#            width=0.4, fill="red", position = position_nudge(x=0.3)) +
#   scale_y_continuous(
#     name = "Number of Heat Days",
#     sec.axis = sec_axis(~. *coeff, name = "Number of Cool Days")
#   ) +
#   scale_x_discrete(limits = month.abb) +
#   theme_bw() +
#   labs(title = "Monthly Heat/Cool Days",
#        subtitle = "Lansing, Michigan: 2016",
#        x = "Month");
# plot(thePlot4)
# 
# 
# 
# thePlot5 = ggplot(data=daysT) +
#   geom_col(mapping=aes(x=month, y=totalCool),
#              width=0.4, fill="blue", position = position_nudge(x=-0.3)) +
#   scale_y_continuous(
#     name = "Number of Heat Days",
#     sec.axis = sec_axis(~. *coeff, name = "Number of Cool Days")
#   )
# plot(thePlot5)
# 
# library(patchwork)
# thePlot4 + thePlot5

  
# coeff <- 0.25
# thePlot3 = ggplot(data=weatherData) +
#   geom_col(mapping=aes(x=month, y=coolDays),
#            width=0.4, fill="blue", position = position_nudge(x=-0.3)) +
#   geom_col(mapping=aes(x=month, y=heatDays),
#            width=0.4, fill="red", position = position_nudge(x=0.3)) +
#   scale_y_continuous(
#     name = "Number of Heat Days",
#     sec.axis = sec_axis(~. *coeff, name = "Number of Cool Days")
#   ) +
#   scale_x_discrete(limits = month.abb) +
#   theme_bw() +
#   labs(title = "Monthly Heat/Cool Days",
#        subtitle = "Lansing, Michigan: 2016",
#        x = "Month");
# plot(thePlot3)
