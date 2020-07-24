library(ggplot2)
#set color variable equal to RGB code for spartan green (MSU marketing website)
SpartanGreen<-rgb(24,69,59, maxColorValue = 255)
#grab some data
data(swiss)
#plot data using "fill = SpartanGreen"
ggplot(data = swiss, aes(x=Agriculture, y=Fertility)) + geom_area(stat="identity", fill=SpartanGreen) +ggtitle("SPARTAN GREEN AREA GRAPH")