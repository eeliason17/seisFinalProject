library(readxl)
library(plyr)
library(tibble)
library(dplyr)
standings_SEIS_seeds <- read_excel("standings_SEIS_seeds.xlsx", 
                                   sheet = "all")
View(standings_SEIS_seeds)

#Create dataframe
standings <- standings_SEIS_seeds

#CLEANUP

#Convert GF and GA to numeric
standings$GF <- as.numeric(as.character(standings$GF))
standings$GA <- as.numeric(as.character(standings$GA))

#Create factor for successful team
standings$success <- factor(ifelse(standings$Seed >0, "Y", "N"))
standings$Champion <- as.factor(as.numeric(standings$Champion))

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
legend(250, .009, levels(success_labels), fill=colfill)

## Success 'Goals against' graph
success_labels_dga <- factor(standings$success, levels = c(1, 2),
                         labels = c("Not Successful", "Successful"))
sm.density.compare(standings$GA, standings$success, xlab = "Goals Against")
title(main = "Goals scored against teams if they are considered 'Successful'")
#legend
colfill <- c(2:(2+length(levels(success_labels_dga))))
legend(200, .01, levels(success_labels_dga), fill=colfill)

#successful teams
ss_stand <- subset(standings, success == "Y") #Added in data clean up
ss_champions <- filter(ss_stand, as.integer(Champion) == 2) 
#Added in data clean up

standings %>%
  ggplot(aes(x=Wins, y=Losses)) +
  geom_point(alpha = .3) +
  geom_point(data = ss_stand, 
             aes(x=Wins, y=Losses), color = 'blue', size = 2) +
  geom_point(data=ss_champions, 
             aes(x=Wins, y=Losses), color = 'green', size = 3)

ggplot(standings, aes(x=GF)) + geom_histogram(binwidth = 10)
ggplot(standings, aes(x=GA)) + geom_histogram(binwidth = 10)
ggplot(standings, aes(x=Wins)) + geom_histogram(binwidth = 1)
ggplot(standings, aes(x=Losses)) + geom_histogram(binwidth = 1)
ggplot(standings, aes(x=DIVWins)) + geom_histogram(binwidth = 1)
ggplot(standings, aes(x=DIVLosses)) + geom_histogram(binwidth = 1)

stand_num <- select_if(standings, is.numeric)
cor_stand <- cor(stand_num)
round(cor_stand, 2)

standings$playoffs <- ifelse(standings$Seed >0, 1, 0)

ss_stand %>%
  ggplot(aes(x=GF, y=GA)) +
  geom_point(alpha=.4) +
  geom_point(data=ss_champions, aes(x=GF, y=GA), color = 'green', size = 2)

#years to gf
standings %>%
  ggplot(aes(x=Year, y=GF)) +
  geom_point(alpha=.3)

stand2022 <- subset(standings, Year == "22")
not22Stand <- subset(standings, Year!= "22")

#graph trend
standings$year <- factor(ifelse(standings$TEAM = endsWith("12"), "2012", 
                                ifelse(endsWith("13"), "2013",
                                       else(endsWith("14"), "2014", "0"))))