library(readxl)
standings_SEIS <- read_excel("standings_SEIS_seeds.xlsx")
View(standings_SEIS)

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
standings_12 <- standings_SEIS

#Convert GF and GA to numeric
standings_12$GF <- as.numeric(as.character(standings_12$GF))
standings_12$GA <- as.numeric(as.character(standings_12$GA))

#Create factor for successful team
install.packages(c('tibble', 'dplyr', 'readr')) # I don't know if I need any of these
standings_12$success <- factor(ifelse(standings_12$Seed >0, "Y", "N"))

# Statistics based on Success
      #can and should i make this into one function
tapply(standings_12$GF, standings_12$success, summary) #Gf
tapply(standings_12$GA, standings_12$success, summary) #GA
print("2012 Champions St. Thomas - GF: 238, GA: 97")

#Graphing libraries
install.packages('sm')
library(sm)

#Goals For
d_df <- density(standings_12$GF)
plot(d_df)

## Success 'Goals For' graph
success_labels <- factor(standings_12$success, levels = c(1, 2),
                         labels = c("Not Successful", "Successful"))
sm.density.compare(standings_12$GF, standings_12$success, xlab = "Goals For")
title(main = "Goals scored for teams if they are considered 'Successful'")
#legend
colfill <- c(2:(2+length(levels(success_labels))))
legend(locator(1), levels(success_labels), fill=colfill)

## Success 'Goals against' graph
success_labels <- factor(standings_12$success, levels = c(1, 2),
                         labels = c("Not Successful", "Successful"))
sm.density.compare(standings_12$GA, standings_12$success, xlab = "Goals Against")
title(main = "Goals scored against teams if they are considered 'Successful'")
#legend
colfill <- c(2:(2+length(levels(success_labels))))
legend(locator(1), levels(success_labels), fill=colfill)
