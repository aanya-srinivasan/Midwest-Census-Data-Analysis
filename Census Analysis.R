library(tidyverse)
getwd()
setwd("C:/Users/srini/Downloads/EDA Labs")
dataset1 <- read.table(file = "data1.csv",
                       
                       header = T,
                       
                       sep = ",", 
                       
                       stringsAsFactors = TRUE)

View(dataset1)

dataset2 <- read.table(file = "data2.csv",
                       
                       header = T,
                       
                       sep = ",", 
                       
                       stringsAsFactors = TRUE)

View(dataset2)

dataset3 <- read.table(file = "data3.csv",
                       
                       header = T,
                       
                       sep = ",", 
                       
                       stringsAsFactors = TRUE)

View(dataset3)


#1 

data1 <- dataset1 %>% 
  
  pivot_longer(
    c(`IL`, `IN`, `IN`, `MI`, `OH`, `WI`), 
    names_to = "state", 
    values_to = "area",
    values_drop_na = TRUE)
View(data1)

#2

data2 <- dataset2 %>%
  
  separate(race, into = c("popwhite", "popblack", "popamerindian", "popasian", "popother"), sep = "/", convert = TRUE)
View(data2)
str(data2)

#3

joined_by_state <- left_join(data1, data2, by = "state")
View(joined_by_state)
dim_joined_by_state <- dim(joined_by_state)
print(dim_joined_by_state)
missing_values <- sum(is.na(joined_by_state))
print(missing_values)

joined_by_sc <- left_join(data1, data2, by = c("state", "county"))
View(joined_by_sc)
dim_joined_by_sc <- dim(joined_by_sc)
print(dim_joined_by_sc)
missing_values2 <- sum(is.na(joined_by_sc))
print(missing_values2)

#4

rjoined_by_state <- right_join(data1, data2, by = "state")
View(rjoined_by_state)
dim_rjoined_by_state <- dim(rjoined_by_state)
print(dim_rjoined_by_state)
missing_values3 <- sum(is.na(rjoined_by_state))
print(missing_values3)

rjoined_by_sc <- right_join(data1, data2, by = c("state", "county"))
View(rjoined_by_sc)
dim_rjoined_by_sc <- dim(rjoined_by_sc)
print(dim_rjoined_by_sc)
missing_values4 <- sum(is.na(rjoined_by_sc))
print(missing_values4)

#5

fjoined_by_state <- full_join(data1, data2, by = "state")
View(fjoined_by_state)
dim_fjoined_by_state <- dim(fjoined_by_state)
print(dim_fjoined_by_state)
missing_values5 <- sum(is.na(fjoined_by_state))
print(missing_values5)

fjoined_by_sc <- full_join(data1, data2, by = c("state", "county"))
View(fjoined_by_sc)
dim_fjoined_by_sc <- dim(fjoined_by_sc)
print(dim_fjoined_by_sc)
missing_values6 <- sum(is.na(fjoined_by_sc))
print(missing_values6)

#6
data <- inner_join(data1, data2, by = c("state", "county"))
View(data)

#7
data_tidy <- data %>%
  select(county:inmetro) %>%
  distinct ()
View(data_tidy)

#8
data_mutate <- mutate(data_tidy, popdensity = poptotal / area, percwhite = (popwhite / poptotal) * 100, percblack = (popblack / poptotal) *100 , percamerindan = (popamerindian / poptotal) *100, percasian = (popasian / poptotal) *100, percother = (popother / poptotal)*100)
View(data_mutate)

#9
data_join <- inner_join(data_mutate, dataset3, by = c("state", "county"))
View(data_join)

#10
data_mutate2 <- mutate(data_join, popbelowpoverty = (percbelowpoverty/100) * poptotal, popadultpoverty = (percadultpoverty/100) * poptotal)
View(data_mutate2)

#11

data_arranged <- data_mutate2 %>%
  relocate(inmetro, .after = everything ()) %>%
  relocate(poptotal, .after = area) %>%
  relocate(popdensity, .after = poptotal)
View(data_arranged)

#12

groupby_state <- data_arranged %>% group_by(state)
View(groupby_state)

total_pop <- groupby_state %>% summarize(total_pop = sum(poptotal)) %>% summarize(biggest = state[which.max(total_pop)], smallest = state[which.min(total_pop)])
print(total_pop)

total_area <- groupby_state %>% summarize(total_area = sum(area)) %>% summarize(biggest = state[which.max(total_area)], smallest = state[which.min(total_area)])
print(total_area)

avg_popdensity <- groupby_state %>% summarize(mean = mean(popdensity)) 
print(avg_popdensity)

groupby_stmetro <- data_arranged %>% group_by(state, inmetro)
View(groupby_stmetro)

avg_pdmetro <- groupby_stmetro %>% summarize(mean = mean(popdensity)) 
print(avg_pdmetro)

#13
av_racegroups_all <- data_arranged %>%
  summarize(
    avg_white = mean((percwhite), na.rm = T),
    avg_black = mean((percblack), na.rm = T),
    avg_amerindian = mean((percamerindan), na.rm = T),
    avg_asian = mean((percasian), na.rm = T),
    avg_other = mean((percother), na.rm = T))
print(av_racegroups_all)

av_racegroups_state <- groupby_state %>% summarize(
  avg_white = mean((percwhite), na.rm = T),
  avg_black = mean((percblack), na.rm = T),
  avg_amerindian = mean((percamerindan), na.rm = T),
  avg_asian = mean((percasian), na.rm = T),
  avg_other = mean((percother), na.rm = T))
print(av_racegroups_state)

av_racegroups_metro <- groupby_stmetro %>% summarize(
  avg_white = mean((percwhite), na.rm = T),
  avg_black = mean((percblack), na.rm = T),
  avg_amerindian = mean((percamerindan), na.rm = T),
  avg_asian = mean((percasian), na.rm = T),
  avg_other = mean((percother), na.rm = T))
print(av_racegroups_metro)

#The average white population in each state was the highest, while the average population of american Indians and Asians were relatively the lowest. Metro area are more diverse than non-metro areas, as the percent of the white population is less in metro areas.

#14
av_belowpoverty <- groupby_state %>% summarize(
  mean_belowpoverty = mean (percbelowpoverty, na.rm = T))
print(av_belowpoverty)

av_belowpoverty_metro <- groupby_stmetro %>% summarize(
  mean_belowpoverty_metro = mean (percbelowpoverty, na.rm = T))
print(av_belowpoverty_metro)

#Based on the data, metro areas seem to have lower below poverty rates.

data_arranged2 <- filter(data_arranged, poptotal > 50000)
View(data_arranged2)
groupby_stmetro_filter <- data_arranged2 %>% group_by(state, inmetro) 
View(groupby_stmetro_filter)

av_belowpoverty_metro2 <- groupby_stmetro_filter %>% summarize(
  mean_belowpoverty_metro = mean (percbelowpoverty, na.rm = T)) 
print(av_belowpoverty_metro2)

#After filtering the data to only include county populations over 50,000, the data shows that some states (ex. WI) have lower below poverty rates than the unfiltered data. This shows that counties with lower populations might have higher poverty rates.
