library(readxl)
standings_SEIS_seeds <- read_excel("standings_SEIS_seeds.xlsx", 
                                   sheet = "all")
View(standings_SEIS_seeds)

#Quarterfinals
#No. 1 St. Thomas 10, No. 9 North Dakota State 7
#No. 2 Grand Valley State 10, No. 10 Grand Canyon 8
#No. 6 Davenport 12, No. 3 Westminster 11
#No. 4 Dayton 12, No. 5 St. John's 4
#Semifinals
#No. 1 St. Thomas 14, No. 4 Dayton 13
#No. 2 Grand Valley State 15, No. 6 Davenport 11
#Championship
#No. 1 St. Thomas 9, No. 2 Grand Valley State 8

#Create dataframe
standings <- standings_SEIS_seeds

#Convert GF and GA to numeric
standings$GF <- as.numeric(as.character(standings$GF))
standings$GA <- as.numeric(as.character(standings$GA))

#Create factor for successful team
install.packages(c('tibble', 'dplyr', 'readr')) # I don't know if I need any of these
standings$success <- factor(ifelse(standings$Seed >0, "Y", "N"))

# Statistics based on Success
#can and should i make this into one function
tapply(standings$GF, standings$success, summary) #Gf
tapply(standings$GA, standings$success, summary) #GA
print("2012 Champions St. Thomas - GF: 238, GA: 97")

#Graphing libraries
install.packages('sm')
library(sm)

#Goals For
d_df <- density(standings$GF)
plot(d_df)

## Success 'Goals For' graph
success_labels <- factor(standings$success, levels = c(1, 2),
                         labels = c("Not Successful", "Successful"))
sm.density.compare(standings$GF, standings$success, xlab = "Goals For")
title(main = "Goals scored for teams if they are considered 'Successful'")
#legend
colfill <- c(2:(2+length(levels(success_labels))))
legend(locator(1), levels(success_labels), fill=colfill)

## Success 'Goals against' graph
success_labels <- factor(standings$success, levels = c(1, 2),
                         labels = c("Not Successful", "Successful"))
sm.density.compare(standings$GA, standings$success, xlab = "Goals Against")
title(main = "Goals scored against teams if they are considered 'Successful'")
#legend
colfill <- c(2:(2+length(levels(success_labels))))
legend(locator(1), levels(success_labels), fill=colfill)

#graph trend
standings$year <- factor(ifelse(standings$TEAM = endsWith("12"), "2012", 
                                ifelse(endsWith("13"), "2013",
                                       else(endsWith("14"), "2014", "0"))))