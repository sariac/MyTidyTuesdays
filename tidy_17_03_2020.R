library(tidyverse)
#install.packages("schrute")
#this package is just the transcript from the Office (see vignette here: https://bradlindblad.github.io/schrute/articles/theoffice.html)
library(schrute)
mydata <- schrute::theoffice
dplyr::glimpse(mydata)



#should select only seasons where a character had at least 50 lines. Right from the beginning!
n_line_character_season<-mydata%>%
  group_by(character, season)%>%
  summarise(n_lines = n())

mydata_join<- left_join(mydata, n_line_character_season, by = c("character", "season"))
mydata_filtered<-mydata_join %>%
  filter(n_lines >=50)

#tokenize all of the lines with a few lines from the tidytext package. Every word in every line will be a separate entry
token.mydata <- mydata_filtered %>%
  tidytext::unnest_tokens(word, text)

#remove some stop words first (Stop words are generally the most common words in a language.)
stop_words <- tidytext::stop_words
tidy.token.mydata <- token.mydata %>%
  dplyr::anti_join(stop_words, by = "word") #is anti_join() like filter()

#which are the most common?
tidy.token.mydata %>%
  dplyr::count(word, sort = TRUE) 

tidy.token.mydata %>%
  dplyr::count(word, sort = TRUE) %>%
  dplyr::filter(n > 400) %>%
  dplyr::mutate(word = stats::reorder(word, n)) %>%
  ggplot2::ggplot(ggplot2::aes(word, n)) +
  ggplot2::geom_col() +
  ggplot2::xlab(NULL) +
  ggplot2::coord_flip() +
  ggplot2::theme_minimal()


#### sentiment analysis
library(tidytext)
get_sentiments("afinn") #AFINN lexicon score words from -5 (negative) to 5 (positive sentiment).

afinn <- tidy.token.mydata %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(character, season) %>%
  summarise(sentiment = mean(value) ) 

#only top characters
top_characters<-afinn %>% 
  group_by(character) %>%
  summarise(n= n())
list_top_character<-top_characters %>%
   filter(n >= 7) %>%
  select(character)

sentiment_top_character<- afinn %>%
  filter(character %in% list_top_character$character)

##########plot 
# Define and add annotation -------------------------------------
library(grid)
my_text <- "Positive"
my_grob = grid.text(my_text, x=0.8,  y=0.95, gp=gpar(col="black", fontsize=14, fontface="bold"))
my_text2 <- "Negative"
my_grob2 = grid.text(my_text2, x=0.2,  y=0.95, gp=gpar(col="black", fontsize=14, fontface="bold"))

library(ggplot2)
plot<-ggplot(sentiment_top_character, aes(y=sentiment, x= character, color= season))+
  geom_point(alpha = 0.75,show.legend = TRUE,size = 3)+
  coord_flip()+
  geom_hline(yintercept = 0, linetype="dashed") +
  stat_summary(fun= mean, color = "black", geom ="point", aes(group = 1), size = 4,
               show.legend = FALSE)+ 
  ylim(c(-1, 1))+
  theme_minimal()+ 
  labs(x = " ", 
       y = " ",
       title = "The Office main characters:",
       subtitle="the sentiment of their lines (across the show and per season)",
       caption = "#TidyTuesday 17-3-2020 Plot by @IacozzaSara") +
  scale_y_discrete(expand = c(0, 0))+
  theme(legend.position = "bottom",
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
    legend.text = element_text(size=rel(0.8),
                               lineheight=.2,
                               family="Times",
                               colour="black"))

plot1<-plot+ annotation_custom(my_grob)
plot2<-plot1+ annotation_custom(my_grob2)
final<-plot2 + scale_color_brewer(palette="Set3")
final

