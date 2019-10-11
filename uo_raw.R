library(tidyverse)
ufo_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")

ufo <- ufo_raw %>% mutate(
  date_time = mdy_hm(date_time),
  year = year(date_time),
  month = month(date_time, label = T),
  day= wday(date_time, label =T),
  hour = hour(date_time))

library(tidytext)

text_ufo <- ufo %>% select(date_time, description) %>% unnest_tokens(word,description ) %>% anti_join(stop_words) %>% filter(str_detect(word, "[a-z]")) 

library(cowplot)
library(magick)
library(grid) #for rasterGrob
believe_background <- png::readPNG("believe.png")

text_ufo %>%  count(word, sort=T)  %>% head(25) %>%  mutate(word = fct_reorder(word, n)) %>% ggplot(aes(x=word, y=n, fill=word, alpha=.5)) + annotation_custom(rasterGrob(believe_background, 
                                                                                                                                                                          width = unit(1,"npc"), 
                                                                                                                                                                          height = unit(1,"npc")), 
                                                                                                                                                               -Inf, Inf, -Inf, Inf) +
  
  geom_col()+ coord_flip() + labs(title = "top 25 words used in descriptions of UFO sightings", subtitle ="data from The National UFO Reporting Center", caption ="Stop words removed. Dataset from Tidytuesday", y = "Times a word appears in description of encounters") + theme(legend.position = "none") 