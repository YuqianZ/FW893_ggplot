### **** Summary of some of the structure Charlie uses in his code ***
# Source (instead of Run) your code -- forces you to structure it better
# copy line: Control-Shift-D (Command-Shift-D on Mac)
# comment out lines: Control-Shift-C (Command-Shift-C on Mac)
# { } -- fixes an obscure if-else bug in R
# = instead of <-: they do the same thing and = is standard in all programming languages
# Semicolons: explicitly tells R this is an end-of-common -- good for debugging
# 85 characters or less/line: 
#        Tools -> Global Options -> Code -> Display -> Margin Column = 85
# Reference file: for code that gets repeated in multiple scripts
#        For instance: a customized GGPlot theme

{
  # execute the lines of code from reference.r 
  source(file="scripts/reference.r");   
  
  # read in CSV file and save the content to packageData
  classData = read.csv(file="data/grepData.csv",
                       stringsAsFactor=FALSE);   # not needed as of R 4.0
  
  classData2 = classData;  # create copy so we can see the changes
  
  # look for patterns in the values:  | means or
  # both isJune and is July are looking for values that has one of four combinations of characters
  isJune  = grep(pattern = "6/|6-|Jun|jun", x=classData2$date);
  isJuly  = grep(pattern = "7/|7-|Jul|jul", x=classData2$date);

  # Create column called month -- assign values of June and July based on grep above
  classData2$month[isJune] = "June";      classData2$month[isJuly] = "July";
  
  # Replace all non-numeric values with "" (i.e., remove them)
  # Note: often easier to exclude (using ^) than include
  windValues = gsub(pattern = "[^0123456789]", replacement="", 
                    x=classData2$windspeed);
  windValues2 = as.numeric(windValues);    # make the vector numeric (this is NOT automatic)
  classData2$windSpedValues = windValues2; # assign vector to a new column
  
  ## **Assignment**
  # 1) Find which rows in the temp column are Celsius (no unit means Fahrenheit)
  # 2) Extract the number from every row using gsub()
  #       Make sure to keep signs and decimals
  # 3) Create a numeric vector from the extracted numbers
  # 4) Create three numeric columns: 
  #       - Fahrenheit (F) temperatures
  #       - Celsius (C) temperatures 
  #       - Kelvin (K) temperatures
  #               F->C: C = (5/9)*(F-32)
  #               C->F: F = (9/5)*F +32
  #               C->K: K = C + 273 
  #               F->K: K = (5/9)*(F-32) + 273
  # 5) Add file to applications folder in your GitHub project
  
  # PDF that is all about Grep -- gets kind of advanced, though
  # https://rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf
  
  
  ##### Hard example: extracting the day from the dates column 
  getDay1 = gsub(pattern = "[a-zA-Z0-9]*-", replacement="", 
                x=classData$date);
  getDay2 = gsub(pattern = "[a-zA-Z0-9]*/", replacement="", 
                x=getDay1);
  getDay3 = gsub(pattern = "[a-zA-Z]*", replacement="", 
                x=getDay2); 
  getDay4 = as.numeric(getDay3);
  
  # same as above in 1 step
  getDayAll = as.numeric(gsub(pattern = "[a-zA-Z0-9]*[-/]|[a-zA-Z]*", replacement="", 
                           x=classData$date));
}


### Working on temp.

# look for patterns in the values:  | means or
#  so isCelsius is looking for any value that has one of the four combinations of characters
isCelsius  = grep(pattern = "C|c|ce|Celsius", x=classData2$temp);
isFahrenheit  = grep(pattern = "F|f|Fahr|Fahrenheit", x=classData2$temp);

# Create column called tempMix -- assign values of C and F based on grep above
classData2$tempMix[isCelsius] = "Celsius";
classData2$tempMix[isFahrenheit] = "Fahrenheit";
classData2$tempMix[is.na(classData2$tempMix)] = "Fahrenheit";


### Remove non-numeric values but keep +/- signs and decimal points ###
# Replace all non-numeric values with "" (i.e., remove them)
# Note: often easier to exclude (using ^) than include
tempValues = gsub(pattern = "[^+-0123456789]", replacement="", 
                  x=classData2$temp);
tempValues = as.numeric(tempValues);   # make the vector numeric (this is NOT automatic)
classData2$temperatureValues = tempValues; # assign vector to a new column

### Convert all C to F ###

tempNum <- length(classData2$temperatureValues);
tempValue <- classData2$temperatureValues;
tempMix <- classData2$tempMix;

# Loop through all temperatures for conversion
for(i in 1:tempNum) # go through each temperature
{
  # if the temperature type is celsius degree, convert it
  if(tempMix[i] == "Celsius")
  {
    tempValue[i] = tempValue[i]*(9/5)+32;
  }
  else # leave the temperature as it is
  {
    tempValue[i] = tempValue[i];
  }
}

classData2$tempCovt = tempValue


### Plot out

plotData = ggplot( data=classData2 ) +
  geom_point( mapping=aes(x=month, y=tempCovt),
              color=rgb(red = 0, green = .6, blue = .6), size=2, shape=18,
              alpha = 0.7 ) +
  theme_bw() + ## Make sure theme_bw() is called before other theme changes are made.
  labs(title = "Daily Temperature vs. Month",
       subtitle = "Psuedo values [crazy:)]",
       x = "Month",
       y = "Daily Temperature (Fahrenheit degree)");
  # theme(axis.title.x=element_text(size=8, color="darkblue", face="bold"),
  #       axis.title.y=element_text(size=8, color="darkblue", face="bold"),
  #       plot.title=element_text(size=14, face="bold",
  #                               color ="darkblue"),
  #       plot.subtitle=element_text(size=10, face="bold",
  #                                  color ="blue", family="serif"),
  #       axis.text.y = element_text(angle=45, hjust=1)) +
  # geom_smooth(mapping=aes(x=windSpeed, y=abs(tempDept)),
  #             method="lm",
  #             color="red",
  #             size=0.8,
  #             linetype=3,
  #             fill="lightpink");
plot(plotData);


  
  
