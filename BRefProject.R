install.packages('rvest')
install.packages('plyr')
install.packages('tidyverse')
library(rvest)
library(plyr)
library(tidyverse)
library(readxl)

# for 2016-2019, get urls of MLB Value Batting
years <- c(2016:2019)
urls <- list()
for (i in 1:length(years)) {
  url <- paste0("https://www.baseball-reference.com/leagues/MLB/", years[i], "-value-batting.shtml")
  urls[[i]] <- url
}

# pull Players Value Batting Table for each year, store in one list
#yearly_players <- list()
#for (i in seq_along(urls)) {
  #value_table <- urls[[i]] %>%
    #read_html() %>%
    #html_nodes(xpath = '//comment()') %>%
    #html_text() %>%
    #paste(collapse='') %>%
    #read_html() %>%
    #html_node('#players_value_batting') %>%
    #html_table()
  # INSERT CODE HERE TO DELETE UNNECESSARY ROWS/COLUMNS
  #yearly_players[[i]] <- value_table
#}
#names(yearly_players) <- years

# Scrape hyperlinks for every player
player_urls <- list()
years <- 2016
for (i in seq_along(urls)) {
  p_url <- urls[[i]] %>%
    read_html() %>%
    
    html_nodes(xpath = '//comment()') %>%
    html_text() %>%
    paste(collapse='') %>%
    read_html() %>%
    
    html_nodes(xpath = "//a") %>% 
    html_attr('href') %>%
    as.data.frame()
  
  # remove rows that aren't players
  row_idx <- with(p_url, substr(., 2, 2) == "p")
  p_url <- data.frame("." = p_url[row_idx, ])
  # if player already exists in player_urls, delete row
  for (j in seq_len(i - 1)) {
    row_idx <- with(p_url, .%in% player_urls[[j]]$. == FALSE)
    p_url <- data.frame("." = p_url[row_idx, ])
  }
  # add hyperlinks from that year to list of data frames of hyperlinks
  player_urls[[i]] <- p_url
  player_urls[[i]]$Year <- years
  years <- years + 1
}

#convert list of dataframes to one big dataframe
player_urlids <- ldply(player_urls, data.frame)

#concatenate correct url
player_urlids$url <- paste0('https://baseball-reference.com', player_urlids$., '#all_batting_value') #actual url

# add position for each player

position <- vector()
for (i in seq_len(nrow(player_urlids))) {
  pos <- player_urlids$url[i] %>%
    read_html %>%
    html_node(xpath = '//p') %>%
    html_text()
  pos <- unlist(strsplit(pos[1], '    '))
  pos <- unlist(strsplit(pos[3], '\n'))[1]
  position <- c(position, pos)
  Sys.sleep(0.5)
}

# add positions to the data frame
player_urlids$pos <- position

#reduce player_urlids data frame to just the hitters
pos_player_urlids <- player_urlids[substr(player_urlids$pos, start = 1, stop = 9) != "Pitcher", ]

# create list of names
p_names <- vector()
for (i in seq_len(nrow(pos_player_urlids))) {
  player_name <- pos_player_urlids$url[i] %>%
    read_html %>%
    html_nodes(xpath = '//h1//span') %>%
    html_text()
  p_names <- c(p_names, player_name)
  print(player_name)
  print(i)
  Sys.sleep(0.5)
}

#create master list of position players
all_players <- vector("list", length = nrow(pos_player_urlids))
names(all_players) <- p_names

#scrape player value table for each player, store in list
for (i in 646:nrow(pos_player_urlids)) {
  pvt <- pos_player_urlids$url[i] %>% 
    read_html %>%
    html_nodes(xpath = '//comment()') %>%
    html_text() %>%
    paste(collapse='') %>%
    read_html() %>%
    html_node('#batting_value') %>%
    html_table()
  
  all_players[[i]] <- pvt
  print(i)
  Sys.sleep(0.5)
}



# save large data so don't need to scrape again
save(all_players, pos_player_urlids, p_names, player_urls, player_urlids, file = "BRefProject.RData")


# this list is the same as all_players, except it has removed excess rows and columns
all_players_abbr_tables <- lapply(all_players, function(x) {
  for (i in 2:nrow(x)) {
    if (nchar(x$Year[i]) > 4) {
      y <- i
      break
    }
  }
  x[1:(y - 1), c(1, 5, 6, 16, 19)]
})

# if there are multiple rows for the same year, merge into a single row
all_players_abbr_tables <- lapply(all_players_abbr_tables, function(x) {
  if (nrow(x) > 1) {
    duplicate_years <- c()
    for (i in 2:nrow(x)) {
      if (x$Year[i] == x$Year[i - 1]) {
        x$G[i] <- as.double(x$G[i]) + as.double(x$G[i - 1])
        x$PA[i] <- as.double(x$PA[i]) + as.double(x$PA[i - 1])
        x$WAR[i] <- as.double(x$WAR[i]) + as.double(x$WAR[i - 1])
        x$oWAR[i] <- as.double(x$oWAR[i]) + as.double(x$oWAR[i - 1])
        duplicate_years <- c(duplicate_years, (i - 1))
      }
    }
    if (length(duplicate_years) > 0) {
      x <- x[-duplicate_years, ]
      rownames(x) <- 1:nrow(x)
    }
  }
  x
})

# delete empty rows

all_players_abbr_tables <- lapply(all_players_abbr_tables, function(x) {
  empty_rows <- c()
  for (i in seq_len(nrow(x))) {
    if (nchar(x$Year[i]) == 0) {
      empty_rows <- c(empty_rows, i)
    }
  }
  if (length(empty_rows) > 0) {
    x <- x[-empty_rows, ]
    rownames(x) <- 1:nrow(x)
  }
  x
})

breakout_players <- c()
breakout_years <- c()
avg_owar <- c()
breakout_avg_owar <- c()
breakout_owar <- c()
for (i in seq_along(all_players_abbr_tables)) {
  if (nrow(all_players_abbr_tables[[i]]) > 1) {
    for (j in 2:nrow(all_players_abbr_tables[[i]])) {
      total_games <- sum(as.double(all_players_abbr_tables[[i]]$G[1:(j-1)]))
      total_owar <- sum(as.double(all_players_abbr_tables[[i]]$oWAR[1:(j-1)]))
      owar_per_162 <- total_owar / total_games * 162
      cur_games <- as.double(all_players_abbr_tables[[i]]$G[j])
      cur_owar <- as.double(all_players_abbr_tables[[i]]$oWAR[j])
      cur_owar_per_162 <- cur_owar / cur_games * 162
      # THE BELOW IF-STATEMENT IS THE FILTER - 1.75, 7, 2, 200 - 1.8, 6.5, 2.4, 200
      if (cur_owar_per_162 >= (owar_per_162 * 1.8) & cur_owar_per_162 >= 6.4 & cur_owar >= 2.5 & all_players_abbr_tables[[i]]$Year[j] > 2015 & total_games > 200) {
        breakout_players <- c(breakout_players, names(all_players_abbr_tables[i]))
        breakout_years <- c(breakout_years, all_players_abbr_tables[[i]]$Year[j])
        avg_owar <- c(avg_owar, owar_per_162)
        breakout_avg_owar <- c(breakout_avg_owar, cur_owar_per_162)
        breakout_owar <- c(breakout_owar, cur_owar)
        break # include this if you don't want more than one season for a player
      }
    }
  }
}

breakout_seasons <- data.frame("Player" = breakout_players, "Year" = breakout_years, "oWAR" = breakout_owar, "oWAR/162" = breakout_avg_owar, "Career oWAR/162" = avg_owar)
breakout_seasons

breakout_year <- read_excel("BreakoutPlayersStats.xlsx", sheet = "Breakout", col_types = c("guess", rep("numeric", 11)))
prev_year <- read_excel("BreakoutPlayersStats.xlsx", sheet = "Before", col_types = c("guess", rep("numeric", 11)))
differences <- read_excel("BreakoutPlayersStats.xlsx", sheet = "Differences", col_types = c("guess", rep("numeric", 11)))

boxplot(breakout_year$`oWAR / 162 games`, prev_year$`oWar/ 162 games`, ylab = "oWAR per 162 Games", names = c("Breakout Year", "Previous Year"), main = "oWAR per 162 Games among Breakout Players")
boxplot(breakout_year$`xwOBA Percentile`, prev_year$`xwOBA Percentile`, ylab = "xwOBA Percentile", names = c("Breakout Year", "Previous Year"), main = "xwOBA Percentile among Breakout Players")
boxplot(breakout_year$`xBA Percentile`, prev_year$`xBA Percentile`, ylab = "xBA Percentile", names = c("Breakout Year", "Previous Year"), main = "xBA Percentile among Breakout Players")
boxplot(breakout_year$`xOBP Percentile`, prev_year$`xOBP Percentile`, ylab = "xOBP Percentile", names = c("Breakout Year", "Previous Year"), main = "xOBP Percentile among Breakout Players")
boxplot(breakout_year$`xSLG Percentile`, prev_year$`xSLG Percentile`, ylab = "xSLG Percentile", names = c("Breakout Year", "Previous Year"), main = "xSLG Percentile among Breakout Players")
boxplot(breakout_year$`Barrel% Percentile`, prev_year$`Barrel% Percentile`, ylab = "Barrel% Percentile", names = c("Breakout Year", "Previous Year"), main = "Barrel% Percentile among Breakout Players")
boxplot(breakout_year$`Exit Velocity Percentile`, prev_year$`Exit Velocity Percentile`, ylab = "Exit Velocity Percentile", names = c("Breakout Year", "Previous Year"), main = "Exit Velocity Percentile among Breakout Players")
boxplot(breakout_year$`Whiff% Percentile`, prev_year$`Whiff% Percentile`, ylab = "Whiff% Percentile", names = c("Breakout Year", "Previous Year"), main = "Whiff% Percentile Among Breakout Players")



contact_players <- prev_year$`Whiff% Percentile` >= 80
boxplot(breakout_year[contact_players, ]$`xwOBA Percentile`, prev_year[contact_players, ]$`xwOBA Percentile`, ylab = "xwOBA Percentile", names = c("Breakout Year", "Previous Year"), main = "xwOBA Percentile among Contact Hitters")
boxplot(breakout_year[contact_players, ]$`Whiff% Percentile`, prev_year[contact_players, ]$`Whiff% Percentile`, ylab = "Whiff% Percentile", names = c("Breakout Year", "Previous Year"), main = "Whiff% Percentile among Contact Hitters")

power_players <- prev_year$`xSLG Percentile` >= 80
boxplot(breakout_year[power_players, ]$`xwOBA Percentile`, prev_year[power_players, ]$`xwOBA Percentile`, ylab = "xwOBA Percentile", names = c("Breakout Year", "Previous Year"), main = "xwOBA Percentile among Power Hitters")
boxplot(breakout_year[power_players, ]$`Whiff% Percentile`, prev_year[power_players, ]$`Whiff% Percentile`, ylab = "Whiff% Percentile", names = c("Breakout Year", "Previous Year"), main = "Whiff% Percentile among Power Hitters")

hard_hit_players <- prev_year$`Exit Velocity Percentile` >= 80
boxplot(breakout_year[hard_hit_players, ]$`xwOBA Percentile`, prev_year[hard_hit_players, ]$`xwOBA Percentile`, ylab = "xwOBA Percentile", names = c("Breakout Year", "Previous Year"))
boxplot(breakout_year[hard_hit_players, ]$`Whiff% Percentile`, prev_year[hard_hit_players, ]$`Whiff% Percentile`, ylab = "Whiff% Percentile", names = c("Breakout Year", "Previous Year"))
