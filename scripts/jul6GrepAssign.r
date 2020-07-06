
  # execute the lines of code from reference.r 
  source(file="scripts/reference.r");
  
  # read in CSV file and save the content to packageData
  classData = read.csv(file="data/grepData.csv",
                       stringsAsFactor=FALSE);   # not needed as of R 4.0
  
  # look for patterns in the values:  | means or
  #  so isJune is looking for any value that has one of the four combinations of characters
  isJune  = grep(pattern = "6/|6-|Jun|jun", x=classData$date);
  isJuly  = grep(pattern = "7/|7-|Jul|jul", x=classData$date);

  # Create column called month -- assign values of June and July based on grep above
  classData$month[isJune] = "June";
  classData$month[isJuly] = "July";
  
  # Replace all non-numeric values with "" (i.e., remove them)
  # Note: often easier to exclude (using ^) than include
  windValues = gsub(pattern = "[^0123456789]", replacement="", 
                    x=classData$windspeed);
  windValues = as.numeric(windValues);   # make the vector numeric (this is NOT automatic)
  classData$windSpeedValues = windValues; # assign vector to a new column
  
  
  ## Assignment
  # 1) Find which rows in the temps column are Celsius (no unit means Fahrenheit)
  # 2) Extract the number from every row -- paid heed to signs and decimals
  # 3) Create a numeric vector from the extracted numbers
  # 4) Challenge: Create a numeric temperature column in the data frame where all 
  #               values are Fahrenheit.  formula: F = (9/5) * C + 32
  # 5) Email belinsky@msu.edu this script with subject "grep"
  
  
  ### Working on temp.
  
  # look for patterns in the values:  | means or
  #  so isCelsius is looking for any value that has one of the four combinations of characters
  isCelsius  = grep(pattern = "C|c|ce|Celsius", x=classData$temp);
  isFahrenheit  = grep(pattern = "F|f|Fahr|Fahrenheit", x=classData$temp);
  
  # Create column called tempMix -- assign values of C and F based on grep above
  classData$tempMix[isCelsius] = "Celsius";
  classData$tempMix[isFahrenheit] = "Fahrenheit";
  
  ### Remove non-numeric values but keep +/- signs and decimal points ??? ###
  # Replace all non-numeric values with "" (i.e., remove them)
  # Note: often easier to exclude (using ^) than include
  tempValues = gsub(pattern = "[^0123456789]", replacement="", 
                    x=classData$temp);
  tempValues = as.numeric(tempValues);   # make the vector numeric (this is NOT automatic)
  classData$temperatureValues = tempValues; # assign vector to a new column
  
  
  ### Convert all C to F ###
  
  
  
  ### Plot out
  
  # plotData = ggplot( data=classData ) +
  #   geom_point( mapping=aes(x=windSpeedValues, y=temperature), 
  #               color=rgb(red = 0, green = .6, blue = .6), size=2, shape=18,
  #               alpha = 0.7 ) +
  #   theme_bw() + ## Make sure theme_bw() is called before other theme changes are made.
  #   labs(title = "Daily Temperature Departure vs. Average Wind Speed",
  #        subtitle = "Lansing, Michigan: 2016",
  #        x = "Average Wind Speed (m/s)",
  #        y = "Daily Temperature Departure (celsius degree)") +
  #   theme(axis.title.x=element_text(size=8, color="darkblue", face="bold"),
  #         axis.title.y=element_text(size=8, color="darkblue", face="bold"), 
  #         plot.title=element_text(size=14, face="bold", 
  #                                 color ="darkblue"),
  #         plot.subtitle=element_text(size=10, face="bold", 
  #                                    color ="blue", family="serif"),
  #         axis.text.y = element_text(angle=45, hjust=1)) +
  #   geom_smooth(mapping=aes(x=windSpeed, y=abs(tempDept)),
  #               method="lm",
  #               color="red", 
  #               size=0.8, 
  #               linetype=3, 
  #               fill="lightpink");
  # plot(plotData);
  # 
  
