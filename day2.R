# 0 Load dependencies --------------------------------------------

library(dplyr)
library(tidyr)

# 1 Load and wrangle data -------------------------------------------------

day2 <- read.delim("inputs/day2.txt", header = F, sep = "\n") %>% 
  mutate(game = row_number()) %>% 
  extract(V1, "subgames", regex = ": (.*)", remove = T)

# how many columns to make
lengths(lapply(day2$subgames, grepRaw, pattern = ";", all = T, fixed = T)) %>% max()+1 # max 6 columns (need to add 1 because the ; is separator, so there is one more subgame than semi-colon)

day2 <- day2 %>% 
  tibble() %>% #tibble to make it easier to work with
  separate(subgames, c("subgame1", "subgame2", "subgame3", "subgame4", "subgame5", "subgame6"), ";") %>% # separate by semi-colon into five columns
  pivot_longer(cols = starts_with("subgame"),
               names_to = "subgame",
               names_prefix = "subgame",
               values_to = "values",
               values_drop_na = F) %>% 
  extract(values, "blue", regex = "(\\d+) blue", remove = F) %>% 
  extract(values, "red", regex = "(\\d+) red", remove = F) %>% 
  extract(values, "green", regex = "(\\d+) green") %>% 
  mutate(blue = ifelse(is.na(blue), 0, blue),
         red = ifelse(is.na(red), 0, red),
         green = ifelse(is.na(green), 0, green)) %>%  # replace NAs with zero
  mutate(blue = as.numeric(blue),
         red = as.numeric(red),
         green = as.numeric(green))
  
# 2 Find max for each group -----------------------------------------------

# the elf numbers we're working with for possible games are:
# 12 red
# 13 green
# 14 blue

day2 <- day2 %>% 
  group_by(game) %>% 
  mutate(maxgreen = max(green), # find max for each group 
         maxred = max(red),
         maxblue = max(blue)) 

day2_max %>% 
  filter(maxgreen <= 13 & maxblue <= 14 & maxred <= 12) # filter to the games that are possible, no higher numbers than the elves' bag
sum(unique(day2_max$game))

# 3 Fewest Number of Cubes Possible ---------------------------------------

day2 %>% 
  mutate(power = maxgreen*maxred*maxblue) %>%# calculate power 
  filter(subgame == 1) %>% 
  ungroup() %>% summarize(sumpower = sum(power))
  
