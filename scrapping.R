rm(list = ls())

library(xml2)
library(rvest)
library(jsonlite)
library(robotstxt)
library(RSocrata)
library(tidyr)
library(splitstackshape)

paths_allowed("https://mobile-legends.fandom.com/wiki/Mobile_Legends_Wiki")

url.char <- read_html("https://mobile-legends.fandom.com/wiki/Category:Heroes")

nds.char <- html_nodes(url.char, 
                       xpath = '//*[contains(concat( " ", @class, " " ),
                       concat( " ", "category-page__member-link", " " ))]')

chars <- html_text(nds.char) %>% sort()

pattern = "Hero|Squad|Category|Support|Fighter|Assassin|Mage|Tank|Marksman|Skins|Popol|Luo"

idx <- which(str_detect(chars, pattern))

chars.legit <- c(chars[-idx], "Masha", "Yi_Sun-Shin", "Carmilla", "Cecilion") %>%
  str_replace("Chang'e", "Chang%27e")%>%
  sort()

chars.skills <- list()
n.chars <- length(chars.legit)

for (i in 1:n.chars){
  url.skills <- read_html(paste0("https://mobile-legends.fandom.com/wiki/", chars.legit[[i]]))
  
  nds.skills <- html_nodes(url.skills, 
                           xpath = '//*[contains(concat( " ", @class, " " ),
                           concat( " ", "hero-attributes", " " ))]//table')
  
  char.info <- html_text(nds.skills)
  
  skills.set <- char.info
  
  
  chars.skills[[i]] <- skills.set
}

pattern2 <- "Magic Resistance |Movement Speed |Mana |Physical Attack |Attack Speed |Magic Power |HP Regen |Physical Defense |Mana Regen |Magical Defense |Basic Attk Crit Rate |HP |Ability Crit Rate |Armor |Regen "

chars.skills.df <- chars.skills

chars.skills.df %<>%
  unlist() %>% 
  str_replace_all("-", "0") %>%
  str_replace_all(pattern2, "") %>%
  trimws() %>%
  as.data.frame() %>%
  cSplit(., 1:ncol(.), sep=" ",
         stripWhite=TRUE, type.convert=FALSE) %>%
  set_colnames(c("MV_SPD", "MANA", "P_ATK", "ATK_SPD", "M_PWR",
               "HP_RGN", "P_DFN", "MANA_RGN", "MGC_DFN", "ATK_CR", "HP", "AB_CR")) 

########## Mannually correcting for empties #####
chars.skills.df[25,1] <- 240
chars.skills.df[43,1] <- 260
chars.skills.df[47,1] <- 255
chars.skills.df[75,1] <- 240
chars.skills.df[90,1] <- 240
chars.skills.df[91,1] <- 240

chars.skills.df[10,3] <- 119

chars.skills.df[9,4] <- 0.8
chars.skills.df[15,4] <- NA
chars.skills.df[16,4] <- NA
chars.skills.df[25,4] <- 0.8
chars.skills.df[47,4] <- 0.826
chars.skills.df[69,4] <- 0.814
chars.skills.df[87,4] <- 0.8
chars.skills.df[90,4] <- 0.8

chars.skills.df[2,5] <- 0
chars.skills.df[22,5] <- NA

chars.skills.df[65,3] <- NA
chars.skills.df[65,4] <- NA
chars.skills.df[65,5] <- 0
chars.skills.df[65,6] <- 19
chars.skills.df[65,7] <- 12
chars.skills.df[65,8] <- 0
chars.skills.df[65,9] <- 10
chars.skills.df[65,10] <- 0
chars.skills.df[65,11] <- 1948
chars.skills.df[65,12] <- NA





chars.roles.mat <- matrix(ncol = 1, nrow = n.chars)

for (i in 1:n.chars){
  url.skills <- read_html(paste0("https://mobile-legends.fandom.com/wiki/", chars.legit[[i]]))
  
  nds.skills <- html_nodes(url.skills, 
                           xpath = '//*[contains(concat( " ", @class, " " ),
                concat( " ", "font-size-11", " " ))]//a')
  
  char.role <- html_text(nds.skills)
  
  chars.roles.mat[i,1] <- char.role[[1]]
}

colnames(chars.roles.mat) <- "ROLE"

df_scrape <- data.frame(Id = 1:n.chars, NAME = chars.legit, 
                        data.matrix(chars.skills.df), chars.roles.mat) %>%
  select(-c("ATK_CR", "AB_CR"))

