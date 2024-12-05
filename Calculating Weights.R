#install packages (if needed)
install.packages("jtools")
install.packages("dplyr")

#load installed packages
library(dplyr)
library(jtools)

#standardize coefficients for four_factors
four_factors_std <- four_factors |> 
  mutate(across(c(eFG, eFG.All., TOV_Perc., TOV_Perc.All., ORB_Perc., DRB_Perc., FTFactor, FTFactor.All.), 
                scale))

#create model for standardized four_factors
model_std <- glm(cbind(Wins, Losses) ~ eFG + eFG.All. + TOV_Perc. + TOV_Perc.All. + ORB_Perc. + DRB_Perc. + FTFactor + FTFactor.All., 
                 family = binomial(), data = four_factors_std)

#standardize coefficients for since_warriors
since_warriors_std <- since_warriors |> 
  mutate(across(c(eFG, eFG.All., TOV_Perc., TOV_Perc.All., ORB_Perc., DRB_Perc., FTFactor, FTFactor.All.), 
                scale))

#create model for standardized since_warriors
model_std2 <- glm(cbind(Wins, Losses) ~ eFG + eFG.All. + TOV_Perc. + TOV_Perc.All. + ORB_Perc. + DRB_Perc. + FTFactor + FTFactor.All., 
                 family = binomial(), data = since_warriors_std)

#standardize coefficients for before_warriors
before_warriors_std <- before_warriors |> 
  mutate(across(c(eFG, eFG.All., TOV_Perc., TOV_Perc.All., ORB_Perc., DRB_Perc., FTFactor, FTFactor.All.), 
                scale))

#create model for standardized before_warriors
model_std3 <- glm(cbind(Wins, Losses) ~ eFG + eFG.All. + TOV_Perc. + TOV_Perc.All. + ORB_Perc. + DRB_Perc. + FTFactor + FTFactor.All., 
                 family = binomial(), data = before_warriors_std)

#standardize coefficients for since_defense
since_defense_std <- since_defense |> 
  mutate(across(c(eFG, eFG.All., TOV_Perc., TOV_Perc.All., ORB_Perc., DRB_Perc., FTFactor, FTFactor.All.), 
                scale))

#create model for standardized since_defense
model_std4 <- glm(cbind(Wins, Losses) ~ eFG + eFG.All. + TOV_Perc. + TOV_Perc.All. + ORB_Perc. + DRB_Perc. + FTFactor + FTFactor.All., 
                 family = binomial(), data = since_defense_std)

#standardize coefficients for before_defense
before_defense_std <- before_defense |> 
  mutate(across(c(eFG, eFG.All., TOV_Perc., TOV_Perc.All., ORB_Perc., DRB_Perc., FTFactor, FTFactor.All.), 
                scale))

#create model for standardized before_defense
model_std5 <- glm(cbind(Wins, Losses) ~ eFG + eFG.All. + TOV_Perc. + TOV_Perc.All. + ORB_Perc. + DRB_Perc. + FTFactor + FTFactor.All., 
                 family = binomial(), data = before_defense_std)

#view standardized coefficients
summ(model_std, scale = TRUE)   #standardized four_factors
summ(model_std2, scale = TRUE)  #standardized since_warriors
summ(model_std3, scale = TRUE)  #standardized before_warriors
summ(model_std4, scale = TRUE)  #standardized since_defense
summ(model_std5, scale = TRUE)  #standardized before_defense
