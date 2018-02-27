####################
# Data without api #
####################


webpage1 <- read_html("https://www.forbes.com/sites/games/2017/12/22/the-best-video-games-of-2017/#5804abb5696f")
webpage1
results1 <- webpage1 %>% html_nodes("u")
results1 <- results1[-11]  ### This data should not in the result.
results1
records1 <- vector("list", length = length(results1))
records1

for (i in 1:20) {
  
  game <- str_c(results1[i] %>% html_nodes("em") %>% html_text(trim = TRUE))
  company <- str_c(xml_contents(results1[i])[2])
  records1[[i]] <- data_frame(game=game, company=company)
  
}

df <- bind_rows(records1)
df

write_csv(df,"game & company")


#################
# data with api #
#################

library(httr)
library(jsonlite)
library(lubridate)
gameT<-GET(url="https://api.twitch.tv/kraken/games/top")
gameT
names(gameT)
content(gameT)
head(gameT)
data_frame(gameT)
gameTex<-rawToChar(gameT$content)
nchar(gameTex)
gamejson<-fromJSON(gameTex)
class(gamejson)
length(gamejson)
gamejson[[1]]
gamejson

