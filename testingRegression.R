library(readxl)
standings_SEIS_seeds <- read_excel("standings_SEIS_seeds.xlsx", 
                                   sheet = "all")
View(standings_SEIS_seeds)

regression_standings <- standings_SEIS_seeds
regression_standings$playoffs <- ifelse(regression_standings$Seed > 0, 1, 0)

regression_standings$GF <- as.numeric(as.character(regression_standings$GF))
regression_standings$GA <- as.numeric(as.character(regression_standings$GA))

gf <- regression_standings$GF
ga <- regression_standings$GA
divWins <- regression_standings$DIVWins
divLosses <- regression_standings$DIVLosses
wins <- regression_standings$Wins
losses <- regression_standings$Losses
seeding <- regression_standings$Seed
playoffs <- regression_standings$playoffs

# All variables base to copy
# lm(success ~ gf + ga + divWins + divLosses + wins + losses + seeding + playoffs)
winsToGF <- lm(wins ~ gf + ga )
summary(winsToGF)

playoff_pred <- lm(playoffs ~ gf + ga + divWins + divLosses + wins)
summary(playoff_pred)
#losses .073, .942
