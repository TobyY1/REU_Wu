rm(list = ls())

library(haven)
library(formatR)
library(tidyverse)
library(dplyr)
library(ggplot2)

setwd("C:/Users/Bucke/Desktop/REU_Wu")

d = read_dta("pwt1001.dta")
d <- d %>% mutate(countrynum = 0)
d <- d %>% mutate(type = 0)

# Labeling each country with a number code
for (i in 0:182) {
  j = i * 70 +1
  while (j <= i * 70 + 71) {
    d[j, "countrynum"] = i
    j = j + 1
  }
}
d <- d[-c(12811), ]

d <- d %>% mutate(lrgdppc = log(rgdpo / pop))

d %>% filter(country == "United States") %>% ggplot(aes(x = year)) + 
  geom_point(aes(y = lrgdppc), color = 'black') + 
  geom_line(aes(y = lrgdppc), color = 'black') +
  coord_cartesian(ylim=c(5,13.1))
  

### Function to call to name each class
Class_giver <- function(country, intype){
  i = 1
  while (i <= 12810) {
    if (toString(d[i,'countrycode']) == country){
      print[d[i,'type']]
      d[i, 'type'] = intype
    }
    i = i +1
  }
}

Class_giver("USA", 1)

numwork <- function(input) {
  if (toString(d[12111,"countrycode"]) == "USA"){
    print("Here")
  }
}
numwork("USA")

print('were done')
