#install packages
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")

#load installed packages
library(tidyverse)
library(ggplot2)
library(dplyr)

#read in dataset and inspect it
Capstone <- read.csv("Capstone(Stats).csv") #load in data
view(Capstone)                              #view dataset
str(Capstone)                               #list variables for future transformations

#create four_factors dataset
four_factors <- Capstone |> 
  select(League, Season, Team, G, FG, X3P, FGA, TOV, FTA, ORB, DRB, ORB.All., DRB.All.,FG.All.,X3P.All.,FGA.All.,FTA.All.,FT.All., TOV.All., FT, Wins, Losses) |> #select relevant columns
  mutate(eFG = ((FG + 0.5 * X3P)/FGA)) |> #create effective field goal percentage column
  mutate(eFG.All. = ((FG.All. + 0.5 * X3P.All.)/FGA.All.)) |> #create effective field goal percentage allowed column
  mutate(TOV_Perc. = ((100 * TOV)/(FGA + (.44 * FTA) + TOV))) |>  #create turnover percentage column
  mutate(TOV_Perc.All. = ((100 * TOV.All.)/(FGA.All. + (.44 * FTA.All.) + TOV.All.))) |>  #create turnover percentage allowed column
  mutate(ORB_Perc. = ((ORB)/(ORB+DRB.All.))) |> #create offensive rebounding percentage column
  mutate(DRB_Perc. = (100 * DRB/(DRB+ORB.All.))) |> #create defensive rebounding percentage column
  mutate(FTFactor = FT/FGA) |>  #create free throw factor column
  mutate(FTFactor.All. = FT.All./FGA.All.)  #create free throw factor allowed column

view(four_factors)  #view transformed dataset

#create subsets
since_warriors <- four_factors |> 
  filter(Season >= 2015)

before_warriors <- four_factors |> 
  filter(Season < 2015)

since_defense <- four_factors |> 
  filter(Season >= 2005)

before_defense <- four_factors |> 
  filter(Season < 2005)

#create correlation matrices for each dataset
V = cbind(four_factors$eFG, four_factors$eFG.All., four_factors$TOV_Perc., four_factors$TOV_Perc.All., four_factors$ORB_Perc., four_factors$DRB_Perc., four_factors$FTFactor, four_factors$FTFactor.All.)

W = cbind(since_warriors$eFG, since_warriors$eFG.All., since_warriors$TOV_Perc., since_warriors$TOV_Perc.All., since_warriors$ORB_Perc., since_warriors$DRB_Perc., since_warriors$FTFactor, since_warriors$FTFactor.All.)

X = cbind(before_warriors$eFG, before_warriors$eFG.All., before_warriors$TOV_Perc., before_warriors$TOV_Perc.All., before_warriors$ORB_Perc., before_warriors$DRB_Perc., before_warriors$FTFactor, before_warriors$FTFactor.All.)

Y = cbind(since_defense$eFG, since_defense$eFG.All., since_defense$TOV_Perc., since_defense$TOV_Perc.All., since_defense$ORB_Perc., since_defense$DRB_Perc., since_defense$FTFactor, since_defense$FTFactor.All.)

Z = cbind(before_defense$eFG, before_defense$eFG.All., before_defense$TOV_Perc., before_defense$TOV_Perc.All., before_defense$ORB_Perc., before_defense$DRB_Perc., before_defense$FTFactor, before_defense$FTFactor.All.)

#view correlation matrices
cor(V)
cor(W)
cor(X)
cor(Y)
cor(Z)

#create new dataset to view distribution of winning percentage
Capstone2 <- Capstone |> 
  mutate(Winning_Perc. = Wins/(Wins+Losses))

#create plot to view distribution of winning percentage
plot(Capstone2$Winning_Perc.)
