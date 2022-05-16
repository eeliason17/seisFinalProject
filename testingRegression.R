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



champion_pred <- lm(playoffs ~ gf + ga+ divWins + divLosses + wins + losses)
summary(champion_pred)

ss_stand$numChamp <- ifelse(ss_stand$Champion==1, 0, 1)
standings$numChamp <- ifelse(standings$Champion==1, 0, 1)

play_champion_pred <- lm(numChamp ~ GF + DIVWins+
                           Wins+ Losses+ Seed, data = ss_stand)
summary(play_champion_pred)
not22Stand <- subset(standings, Year!= "22")

champion_pred <- lm(numChamp ~ GF + DIVWins+
                           Wins+ Losses+ Seed, data = standings)
summary(champion_pred)

playoff_pred
seedings_lm <- lm(Seed ~ GF +  DIVLosses +
                    Wins, data = not22Stand)
summary(seedings_lm)

champion_pred <- lm(numChamp ~ GF + DIVWins+
                      Wins+ Losses+ Seed, data = not22Stand)
summary(champion_pred)
plot(champion_pred)

standings$numSuccess <- ifelse(standings$success=="Y", 1, 0)

glm(playoffs ~ gf + ga + divWins + divLosses + wins)

standings_num$Column1 <- NULL
standings_num$playoffs <- NULL
standings_num$Seed <- NULL
standings_num$numChamp <- NULL
min.playoff_pred <- lm(formula = numSuccess ~ 1, data = standings_num)
max.playoff_pred <- lm(formula = numSuccess ~ ., data = standings_num)
summary(min.playoff_pred)
fwd.playoff_pred <- step(min.playoff_pred, direction = 'forward', 
                         scope = formula(max.playoff_pred))
plot(fwd.playoff_pred$anova$AIC, type = 'l')

success_pred <- lm(numSuccess ~ DIVLosses + GF + Losses, data = standings_num)
summary(success_pred)
summary(fwd.playoff_pred)

test_2022 <- stand2022
pred_play_22 <- predict.lm(success_pred, newdata = stand2022_num)
test_2022$pred_play <- as.numeric(pred_play_22)
test_2022 <- test_2022[with(test_2022,order(-pred_play)),]
test_2022[1:16,]

not22Stand_num$Column1 <- NULL
not22Stand_num$Seed <- NULL
not22Stand_num$playoffs <- NULL
not22Stand_num$numSuccess <- NULL
not22Stand_num$numChamp <- ifelse(not22Stand_num$numChamp==1, 0,1)
noPlayoffChamp <- lm(numChamp ~ GF+ GA + Wins + 
                       Losses, data = not22Stand_num)
summary(noPlayoffChamp)
plot(noPlayoffChamp)


min.playoff_pred <- lm(formula = numSuccess ~ 1, data = standings_num)
max.playoff_pred <- lm(formula = numSuccess ~ ., data = standings_num)
summary(min.playoff_pred)
fwd.playoff_pred <- step(min.playoff_pred, direction = 'forward', 
                         scope = formula(max.playoff_pred))
plot(fwd.playoff_pred$anova$AIC, type = 'l')

success_pred <- lm(numSuccess ~ DIVLosses + GF + Losses, data = standings_num)
summary(success_pred)

test_2022$noPlayChamp <- as.numeric(noPlayChamp)
test_2022 <- test_2022[with(test_2022,order(-noPlayChamp)),]
test_2022[1:5,c(2,10,12,16)]

not22_df <- not22Stand

df_not22$TEAM <- NULL
min.champPred <- lm(formula = numChamp ~ 1, data = df_not22)
max.champPred <- lm(formula = numChamp ~ ., data = df_not22)
summary(min.champPred)
fwd.champPred <- step(min.champPred, direction = 'forward', 
                         scope = formula(max.champPred))
plot(fwd.champPred$anova$AIC, type = 'l')

champPred <- lm(numChamp ~ numSuccess + Seed + DIVWins + GF + Wins, data = df_not22)
summary(champPred)
