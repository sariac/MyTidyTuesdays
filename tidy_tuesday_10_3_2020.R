# Install via devtools::install_github("thebioengineer/tidytuesdayR")
#install.packages(devtools::install_github("thebioengineer/tidytuesdayR"))

# Get the Data
tuition_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv')

diversity_school <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/diversity_school.csv')

library(tidyverse)
#merge
merged<-dplyr::left_join(tuition_cost, diversity_school, by = c("name", "state"))
#I want to make 2 stacked-bar charts (public vs private) flipped where I have diversity information for each school color-coded in a single bar over the % of school students

#create 6 levels for category, select cols, filter out data
merged_preproc<-merged %>% mutate(category=recode(category,
                       `Two Or More Races`="Other",
                       `Unknown`="Other",
                       `Non-Resident Foreign`= "Other",
                       `American Indian / Alaska Native`= "Native",
                       `Native Hawaiian / Pacific Islander`= "Native",
                       `Hispanic`="Hispanic/Latin"
                       )) %>% 
                      select(name, state, state_code, type, total_enrollment, category, enrollment, in_state_total) %>%
                      filter(!category %in% c("Women", "Total Minority", "NA"))%>%
                      filter(type %in% c("Public", "Private"))
# group by State and type
grouped<- merged_preproc %>% 
                        group_by(state, type, category) %>%
  summarize(total_enrollment = mean(total_enrollment, na.rm = TRUE), 
            enrollment= mean(enrollment, na.rm = TRUE), in_state_total = mean(in_state_total)) %>%
  filter(!is.na(category))

# Stacked + percent
library(viridis)
library(hrbrthemes)
library(ggplot2)
library(rcartocolor)

# It's recommended to use a named vector
cols <- c("White" = "#A5AA99", "Other" = "#008695", "Native" = "#f97b72", "Hispanic/Latin" = "#EDAD08", "Black" = "#E73F74" , "Asian" = "#7F3C8D")


plot<- ggplot(grouped, aes(fill=category, y=enrollment, x=reorder(state,in_state_total ))) + 
  geom_bar(position="fill", stat="identity")+
  coord_flip()+
  facet_grid(. ~ type) +
  scale_fill_manual(values = cols) +
  ggtitle("Diversity in Private vs Pubblic Schools per State",
          subtitle = "Ranked by Tuition Costs") +
  labs(fill= "", x = "", y = "", caption = "#TidyTuesday 10-3-2020 Plot by @IacozzaSara")+
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0))+
  theme(legend.position = "top",
             plot.title = element_text(size=rel(2),
                                       lineheight=.5,
                                       family="Times",
                                       face="bold.italic",
                                       colour="black"),
             axis.text.x = element_text(size=rel(1.1),
                                        family="Courier",
                                        color="black"),
              axis.text.y = element_text(size=rel(1.1),
                                   family="Courier",
                                   face="bold",
                                   color="black"),
              legend.text = element_text(size=rel(1.1),
                                        lineheight=.5,
                                        family="Times",
                                        face="bold",
                                        colour="black"))
