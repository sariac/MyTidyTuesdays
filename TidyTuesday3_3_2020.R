# Get the Data

game_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv')

top_250 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/top_250.csv')

season_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/season_goals.csv')

#y= carreer goals; x= age; a line per player

#first step is to get another col with ageY (year) from existing age col which is (years-days).
install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(tidyr)

game_goals_sub<- game_goals %>%
  select("player", "age", "goals") %>%
  separate("age", c("ageY", NA))

#second, aggregate number of goals for age years per player

game_goals_aggr<- game_goals_sub %>%
  group_by(player, ageY) %>%
  summarise(goal_per_ageY=sum(goals))

#third, create a cumulative sum per player
game_goals_aggr2<- game_goals_aggr %>% 
  group_by(player) %>% mutate(cum_goals = cumsum(goal_per_ageY))

#take only top 8
top8 = c("Alex Ovechkin", "Wayne Gretzky", "Gordie Howe", "Jaromir Jagr", "Brett Hull", "Marcel Dionne", "Phil Esposito", "Mike Gartner")
game_goals_aggr2b<-game_goals_aggr2 %>%
  filter(player %in% top8)%>%
  mutate(ageY=as.numeric(ageY))

#actually because the data contained is from the 1980 onwards, we only have 5 of the 8, so we can plot
#the top 5 of the last 40years

#base plot
plot<-ggplot(game_goals_aggr2b, aes(y=cum_goals, x=ageY, color=player))+
  geom_line()

#b1

# Create a vector of the last (furthest right) y-axis values for each group
game_goals_aggr2b_ENDS <- game_goals_aggr2b %>% 
    group_by(player) %>%      
    top_n(1, ageY) %>% 
    pull(cum_goals)

b1<- plot +  scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0), sec.axis = sec_axis(~ ., breaks = game_goals_aggr2b_ENDS))+
        geom_line(size = 1.5, alpha = .8) +
        theme_minimal() +
        ggtitle("Goal trajectory of top-5 players of the last 40 years",
          subtitle = "#TidyTuesday 3-3-2020") +
        labs(x = "Age", y = "Career goals", caption = "Plot by @IacozzaSara")
