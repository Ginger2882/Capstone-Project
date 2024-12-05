#install packages (if needed)
install.packages("tidyverse")
install.packages("ggplot2")

#load libraries
library(tidyverse)
library(ggplot2)

#aggregate the data by season and calculate the mean for each factor
season_aggregates <- four_factors |> 
  group_by(Season) |> #group data by season
  summarize(
    eFG = mean(eFG), #calculate the mean of eFG%
    eFG.All. = mean(eFG.All.), #calculate the mean of eFG% Allowed
    TOV_Perc. = mean(TOV_Perc.), #calculate the mean of TOV Perc.
    TOV_Perc.All. = mean(TOV_Perc.All.), #calculate the mean of TOV Perc. Allowed
    ORB_Perc. = mean(ORB_Perc.), #calculate the mean of ORB%
    DRB_Perc. = mean(DRB_Perc.), #calculate the mean of DRB%
    FTFactor = mean(FTFactor), #calculate the mean of FT Factor
    FTFactor.All. = mean(FTFactor.All.) #calculate the mean of FT Factor Allowed
  )

#view aggregated data
view(season_aggregates)

#create plot for average eFG% by season
ggplot(season_aggregates, aes(x = Season, y = eFG)) +
  geom_line() + #add line plot
  labs(title = "Average eFG% by Season", #title the plot
       x = "Season", #label x-axis
       y = "eFG%") #label y-axis

#create plot for average eFG% Allowed by season
ggplot(season_aggregates, aes(x = Season, y = eFG.All.)) +
  geom_line() + #add line plot 
  labs(title = "Average eFG% Allowed by Season", #title the plot
       x = "Season", #label x-axis
       y = "eFG% Allowed") #label y-axis

#create plot for average TOV% by season
ggplot(season_aggregates, aes(x = Season, y = TOV_Perc.)) +
  geom_line() + #add line plot
  labs(title = "Average TOV% by Season", #title the plot
       x = "Season", #label x-axis
       y = "TOV%") #label y-axis

#create plot for average TOV% Allowed by season
ggplot(season_aggregates, aes(x = Season, y = TOV_Perc.)) +
  geom_line() + #add line plot
  labs(title = "Average TOV% Allowed by Season", #title the plot
       x = "Season", #label x-axis
       y = "TOV% Allowed") #label y-axis

#create plot for average ORB% by season
ggplot(season_aggregates, aes(x = Season, y = ORB_Perc.)) +
  geom_line() + #add line plot
  labs(title = "Average ORB% by Season", #title the plot
       x = "Season", #label x-axis
       y = "ORB%") #label y-axis

#create plot for average DRB% by season
ggplot(season_aggregates, aes(x = Season, y = DRB_Perc.)) +
  geom_line() + #add line plot
  labs(title = "Average DRB% by Season", #title the plot
       x = "Season", #label x-axis
       y = "DRB%") #label y-axis

#create plot for average FT Factor Allowed by season
ggplot(season_aggregates, aes(x = Season, y = FTFactor)) +
  geom_line() + #add line plot
  labs(title = "Average FT Factor by Season", #title the plot
       x = "Season", #label x-axis
       y = "FT Factor") #label y-axis

#create plot for average FT Factor Allowed by season
ggplot(season_aggregates, aes(x = Season, y = FTFactor.All.)) +
  geom_line() + #add line plot
  labs(title = "Average FT Factor Allowed by Season", #title the plot
       x = "Season", #label x-axis
       y = "FT Factor Allowed") #label y-axis
