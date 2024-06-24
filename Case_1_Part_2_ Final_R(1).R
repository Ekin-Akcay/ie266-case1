library(corrplot)
library(ggmosaic)
library("tidyverse")
library("gridExtra")
library("ggpubr")
library(readxl)
library(writexl)
library(dplyr)
library("xlsx")
library("qcc")
# install.packages("ggpubr")
library("ggpubr")
# install.packages("BSDA")
library(BSDA)

farm_a <- read_xlsx("/Users/macbookair/Desktop/IE266/CaseStudy1/Agricultural_Impact_Data.xlsx", sheet = 2)
farm_b <- read_xlsx("/Users/macbookair/Desktop/IE266/CaseStudy1/Agricultural_Impact_Data.xlsx", sheet = 3)

# Part A

  # First Part
first_month_a <- filter(farm_a, farm_a$Week %in% c(1, 2, 3, 4))
harvest_emission_for_each_farm_a_first <- first_month_a |> group_by(Farm) |> summarise(Total_Harvest_Related_Emissions_per_Farm=sum(`Harvest-related Emissions`))
ggqqplot(harvest_emission_for_each_farm_a_first$Total_Harvest_Related_Emissions_per_Farm, title = "Normal Q-Q Plot for Total Harvest-Related Emissions per Farm (CO2eq kt/farm) for Region A in First Month")

last_month_a <- filter(farm_a, farm_a$Week %in% c(9, 10, 11, 12))
harvest_emission_for_each_farm_a_last <- last_month_a |> group_by(Farm) |> summarise(Total_Harvest_Related_Emissions_per_Farm=sum(`Harvest-related Emissions`))
ggqqplot(harvest_emission_for_each_farm_a_last$Total_Harvest_Related_Emissions_per_Farm, title = "Normal Q-Q Plot for Total Harvest-Related Emissions per Farm (CO2eq kt/farm) for Region A in Last Month")

# Checking whether variances are identical or not.
var.test(harvest_emission_for_each_farm_a_first$Total_Harvest_Related_Emissions_per_Farm, harvest_emission_for_each_farm_a_last$Total_Harvest_Related_Emissions_per_Farm,paired = FALSE, alpha = 0.05)
# Conducting t-test
t.test(harvest_emission_for_each_farm_a_first$Total_Harvest_Related_Emissions_per_Farm, harvest_emission_for_each_farm_a_last$Total_Harvest_Related_Emissions_per_Farm, paired=FALSE, var.equal = TRUE
       , alpha = 0.05, df = 38)

  # Second Part
storage_emission_for_each_farm_a_first <- first_month_a |> group_by(Farm) |> summarise(Total_Storage_Related_Emissions_per_Farm=sum(`Storage-related Emissions`))
storage_emission_for_each_farm_a_last <- last_month_a |> group_by(Farm) |> summarise(Total_Storage_Related_Emissions_per_Farm=sum(`Storage-related Emissions`))
ggqqplot(storage_emission_for_each_farm_a_first$Total_Storage_Related_Emissions_per_Farm, title = "Normal Q-Q Plot for Storage-Related Emissions per Farm for the First Month")
ggqqplot(storage_emission_for_each_farm_a_last$Total_Storage_Related_Emissions_per_Farm, title = "Normal Q-Q Plot for Storage-Related Emissions per Farm for the Last Month")

# Checking whether variances are identical or not.
var.test(storage_emission_for_each_farm_a_first$Total_Storage_Related_Emissions_per_Farm, storage_emission_for_each_farm_a_last$Total_Storage_Related_Emissions_per_Farm, alpha = 0.05)
# Conducting t-test
t.test(storage_emission_for_each_farm_a_first$Total_Storage_Related_Emissions_per_Farm, storage_emission_for_each_farm_a_last$Total_Storage_Related_Emissions_per_Farm, paired=FALSE, var.equal = TRUE
       , alpha = 0.05, df = 38)

# Part B

  #First Part
# Filtering data according to the conditions provided in this part.
farm_a_week1 <-farm_a |> filter(Week == 1) 
farm_b_week1 <-farm_b |> filter(Week == 1) 

var.test(farm_a_week1$`Harvest Amount (t)`,farm_b_week1$`Harvest Amount (t)`, paired = FALSE, alpha = 0.05)
t.test(farm_a_week1$`Harvest Amount (t)`,farm_b_week1$`Harvest Amount (t)` , paired = FALSE, alpha = 0.25,df = 43, alternative = "two.sided", var.equal = TRUE)


farm_a_week2 <-farm_a |> filter(Week == 2) 
farm_b_week2 <-farm_b |> filter(Week == 2) 

var.test(farm_a_week2$`Harvest Amount (t)`,farm_b_week2$`Harvest Amount (t)`, paired = FALSE, alpha = 0.05)
t.test(farm_a_week2$`Harvest Amount (t)`,farm_b_week2$`Harvest Amount (t)` , paired = FALSE, alpha = 0.25,df = 43, alternative = "two.sided", var.equal = TRUE)


farm_a_week3 <-farm_a |> filter(Week == 3) 
farm_b_week3 <-farm_b |> filter(Week == 3) 

var.test(farm_a_week3$`Harvest Amount (t)`,farm_b_week3$`Harvest Amount (t)`, paired = FALSE, alpha = 0.05)
t.test(farm_a_week3$`Harvest Amount (t)`,farm_b_week3$`Harvest Amount (t)` , paired = FALSE, alpha = 0.25,df = 43, alternative = "two.sided", var.equal = TRUE)


farm_a_week4 <-farm_a |> filter(Week == 4) 
farm_b_week4 <-farm_b |> filter(Week == 4) 

var.test(farm_a_week4$`Harvest Amount (t)`,farm_b_week4$`Harvest Amount (t)`, paired = FALSE, alpha = 0.05)
t.test(farm_a_week4$`Harvest Amount (t)`,farm_b_week4$`Harvest Amount (t)` , paired = FALSE, alpha = 0.25,df = 43, alternative = "two.sided", var.equal = TRUE)


farm_a_week5 <-farm_a |> filter(Week == 5) 
farm_b_week5 <-farm_b |> filter(Week == 5) 

var.test(farm_a_week5$`Harvest Amount (t)`,farm_b_week5$`Harvest Amount (t)`, paired = FALSE, alpha = 0.05)
t.test(farm_a_week5$`Harvest Amount (t)`,farm_b_week5$`Harvest Amount (t)` , paired = FALSE, alpha = 0.25,df = 43, alternative = "two.sided", var.equal = TRUE)


farm_a_week6 <-farm_a |> filter(Week == 6) 
farm_b_week6 <-farm_b |> filter(Week == 6) 

var.test(farm_a_week6$`Harvest Amount (t)`,farm_b_week6$`Harvest Amount (t)`, paired = FALSE, alpha = 0.05)
t.test(farm_a_week6$`Harvest Amount (t)`,farm_b_week6$`Harvest Amount (t)` , paired = FALSE, alpha = 0.25,df = 43, alternative = "two.sided", var.equal = TRUE)


farm_a_week7 <-farm_a |> filter(Week == 7) 
farm_b_week7 <-farm_b |> filter(Week == 7) 

var.test(farm_a_week7$`Harvest Amount (t)`,farm_b_week7$`Harvest Amount (t)`, paired = FALSE, alpha = 0.05)
t.test(farm_a_week7$`Harvest Amount (t)`,farm_b_week7$`Harvest Amount (t)` , paired = FALSE, alpha = 0.25,df = 43, alternative = "two.sided", var.equal = TRUE)


farm_a_week8 <-farm_a |> filter(Week == 8) 
farm_b_week8 <-farm_b |> filter(Week == 8) 

var.test(farm_a_week8$`Harvest Amount (t)`,farm_b_week8$`Harvest Amount (t)`, paired = FALSE, alpha = 0.05)
t.test(farm_a_week8$`Harvest Amount (t)`,farm_b_week8$`Harvest Amount (t)` , paired = FALSE, alpha = 0.25,df = 43, alternative = "two.sided", var.equal = TRUE)


farm_a_week9 <-farm_a |> filter(Week == 9) 
farm_b_week9 <-farm_b |> filter(Week == 9) 

var.test(farm_a_week9$`Harvest Amount (t)`,farm_b_week9$`Harvest Amount (t)`, paired = FALSE, alpha = 0.05)
t.test(farm_a_week9$`Harvest Amount (t)`,farm_b_week9$`Harvest Amount (t)` , paired = FALSE, alpha = 0.25,df = 43, alternative = "two.sided", var.equal = TRUE)


farm_a_week10 <-farm_a |> filter(Week == 10) 
farm_b_week10 <-farm_b |> filter(Week == 10) 

var.test(farm_a_week10$`Harvest Amount (t)`,farm_b_week7$`Harvest Amount (t)`, paired = FALSE, alpha = 0.05)
t.test(farm_a_week10$`Harvest Amount (t)`,farm_b_week7$`Harvest Amount (t)` , paired = FALSE, alpha = 0.25,df = 43, alternative = "two.sided", var.equal = TRUE)


farm_a_week11 <-farm_a |> filter(Week == 11) 
farm_b_week11 <-farm_b |> filter(Week == 11) 

var.test(farm_a_week11$`Harvest Amount (t)`,farm_b_week11$`Harvest Amount (t)`, paired = FALSE, alpha = 0.05)
t.test(farm_a_week11$`Harvest Amount (t)`,farm_b_week11$`Harvest Amount (t)` , paired = FALSE, alpha = 0.25,df = 43, alternative = "two.sided", var.equal = TRUE)


farm_a_week12 <-farm_a |> filter(Week == 12) 
farm_b_week12 <-farm_b |> filter(Week == 12) 

var.test(farm_a_week12$`Harvest Amount (t)`,farm_b_week7$`Harvest Amount (t)`, paired = FALSE, alpha = 0.05)
t.test(farm_a_week12$`Harvest Amount (t)`,farm_b_week7$`Harvest Amount (t)` , paired = FALSE, alpha = 0.25,df = 43, alternative = "two.sided", var.equal = TRUE)

  # Second Part
# Filtering data according to the conditions provided in this part.  
farm_a_week1B <-farm_a |> filter(Week == 1) 
farm_b_week1B <-farm_b |> filter(Week == 1) 

var.test(farm_a_week1B$`On farm loss (t)`/ farm_a_week1B$`Harvest Amount (t)`,farm_b_week1B$`On farm loss (t)`/ farm_b_week1B$`Harvest Amount (t)`, paired = FALSE, alpha = 0.05)
t.test(farm_a_week1B$`On farm loss (t)`/ farm_a_week1B$`Harvest Amount (t)`,farm_b_week1B$`On farm loss (t)`/ farm_b_week1B$`Harvest Amount (t)` , paired = FALSE, alpha = 0.25,df = 43, alternative = "two.sided", var.equal = TRUE)


farm_a_week2B <-farm_a |> filter(Week == 2) 
farm_b_week2B <-farm_b |> filter(Week == 2) 

var.test(farm_a_week2B$`On farm loss (t)`/ farm_a_week2B$`Harvest Amount (t)`,farm_b_week2B$`On farm loss (t)`/ farm_b_week2B$`Harvest Amount (t)`, paired = FALSE, alpha = 0.05)
t.test(farm_a_week2B$`On farm loss (t)`/ farm_a_week2B$`Harvest Amount (t)`,farm_b_week2B$`On farm loss (t)`/ farm_b_week2B$`Harvest Amount (t)` , paired = FALSE, alpha = 0.25,df = 43, alternative = "two.sided", var.equal = TRUE)



farm_a_week3B <-farm_a |> filter(Week == 3) 
farm_b_week3B <-farm_b |> filter(Week == 3) 

var.test(farm_a_week3B$`On farm loss (t)`/ farm_a_week3B$`Harvest Amount (t)`,farm_b_week3B$`On farm loss (t)`/ farm_b_week3B$`Harvest Amount (t)`, paired = FALSE, alpha = 0.05)
t.test(farm_a_week3B$`On farm loss (t)`/ farm_a_week3B$`Harvest Amount (t)`,farm_b_week3B$`On farm loss (t)`/ farm_b_week3B$`Harvest Amount (t)` , paired = FALSE, alpha = 0.25,df = 43, alternative = "two.sided", var.equal = TRUE)



farm_a_week4B <-farm_a |> filter(Week == 4) 
farm_b_week4B <-farm_b |> filter(Week == 4) 

var.test(farm_a_week4B$`On farm loss (t)`/ farm_a_week4B$`Harvest Amount (t)`,farm_b_week4B$`On farm loss (t)`/ farm_b_week4B$`Harvest Amount (t)`, paired = FALSE, alpha = 0.05)
t.test(farm_a_week4B$`On farm loss (t)`/ farm_a_week4B$`Harvest Amount (t)`,farm_b_week4B$`On farm loss (t)`/ farm_b_week4B$`Harvest Amount (t)` , paired = FALSE, alpha = 0.25,df = 43, alternative = "two.sided", var.equal = TRUE)



farm_a_week5B <-farm_a |> filter(Week == 5) 
farm_b_week5B <-farm_b |> filter(Week == 5) 

var.test(farm_a_week5B$`On farm loss (t)`/ farm_a_week5B$`Harvest Amount (t)`,farm_b_week5B$`On farm loss (t)`/ farm_b_week5B$`Harvest Amount (t)`, paired = FALSE, alpha = 0.05)
t.test(farm_a_week5B$`On farm loss (t)`/ farm_a_week5B$`Harvest Amount (t)`,farm_b_week5B$`On farm loss (t)`/ farm_b_week5B$`Harvest Amount (t)` , paired = FALSE, alpha = 0.25,df = 43, alternative = "two.sided", var.equal = TRUE)



farm_a_week6B <-farm_a |> filter(Week == 6) 
farm_b_week6B <-farm_b |> filter(Week == 6) 

var.test(farm_a_week6B$`On farm loss (t)`/ farm_a_week6B$`Harvest Amount (t)`,farm_b_week6B$`On farm loss (t)`/ farm_b_week6B$`Harvest Amount (t)`, paired = FALSE, alpha = 0.05)
t.test(farm_a_week6B$`On farm loss (t)`/ farm_a_week6B$`Harvest Amount (t)`,farm_b_week6B$`On farm loss (t)`/ farm_b_week6B$`Harvest Amount (t)` , paired = FALSE, alpha = 0.25,df = 43, alternative = "two.sided", var.equal = TRUE)



farm_a_week7B <-farm_a |> filter(Week == 7) 
farm_b_week7B <-farm_b |> filter(Week == 7) 

var.test(farm_a_week7B$`On farm loss (t)`/ farm_a_week7B$`Harvest Amount (t)`,farm_b_week7B$`On farm loss (t)`/ farm_b_week7B$`Harvest Amount (t)`, paired = FALSE, alpha = 0.05)
t.test(farm_a_week7B$`On farm loss (t)`/ farm_a_week7B$`Harvest Amount (t)`,farm_b_week7B$`On farm loss (t)` / farm_b_week7B$`Harvest Amount (t)`, paired = FALSE, alpha = 0.25,df = 43, alternative = "two.sided", var.equal = TRUE)



farm_a_week8B <-farm_a |> filter(Week == 8) 
farm_b_week8B <-farm_b |> filter(Week == 8) 

var.test(farm_a_week8B$`On farm loss (t)`/ farm_a_week8B$`Harvest Amount (t)`,farm_b_week8B$`On farm loss (t)`/ farm_b_week8B$`Harvest Amount (t)`, paired = FALSE, alpha = 0.05)
t.test(farm_a_week8B$`On farm loss (t)`/ farm_a_week8B$`Harvest Amount (t)`,farm_b_week8B$`On farm loss (t)`/ farm_b_week8B$`Harvest Amount (t)` , paired = FALSE, alpha = 0.25,df = 43, alternative = "two.sided", var.equal = TRUE)



farm_a_week9B <-farm_a |> filter(Week == 9) 
farm_b_week9B <-farm_b |> filter(Week == 9) 

var.test(farm_a_week9B$`On farm loss (t)`/ farm_a_week9B$`Harvest Amount (t)`,farm_b_week9B$`On farm loss (t)`/ farm_b_week9B$`Harvest Amount (t)`, paired = FALSE, alpha = 0.05)
t.test(farm_a_week9B$`On farm loss (t)`/ farm_a_week9B$`Harvest Amount (t)`,farm_b_week9B$`On farm loss (t)`/ farm_b_week9B$`Harvest Amount (t)` , paired = FALSE, alpha = 0.25,df = 43, alternative = "two.sided", var.equal = TRUE)



farm_a_week10B <-farm_a |> filter(Week == 10) 
farm_b_week10B <-farm_b |> filter(Week == 10) 

var.test(farm_a_week10B$`On farm loss (t)`/ farm_a_week10B$`Harvest Amount (t)`,farm_b_week10B$`On farm loss (t)`/ farm_b_week10B$`Harvest Amount (t)`, paired = FALSE, alpha = 0.05)
t.test(farm_a_week10B$`On farm loss (t)`/ farm_a_week10B$`Harvest Amount (t)`,farm_b_week10B$`On farm loss (t)`/ farm_b_week10B$`Harvest Amount (t)` , paired = FALSE, alpha = 0.25,df = 43, alternative = "two.sided", var.equal = TRUE)



farm_a_week11B <-farm_a |> filter(Week == 11) 
farm_b_week11B <-farm_b |> filter(Week == 11) 

var.test(farm_a_week11B$`On farm loss (t)`/ farm_a_week11B$`Harvest Amount (t)`,farm_b_week11B$`On farm loss (t)`/ farm_b_week11B$`Harvest Amount (t)`, paired = FALSE, alpha = 0.05)
t.test(farm_a_week11B$`On farm loss (t)`/ farm_a_week11B$`Harvest Amount (t)`,farm_b_week11B$`On farm loss (t)` / farm_b_week11B$`Harvest Amount (t)`, paired = FALSE, alpha = 0.25,df = 43, alternative = "two.sided", var.equal = TRUE)


farm_a_week12B <-farm_a |> filter(Week == 12) 
farm_b_week12B <-farm_b |> filter(Week == 12) 

var.test(farm_a_week12B$`On farm loss (t)`/ farm_a_week12B$`Harvest Amount (t)`,farm_b_week12B$`On farm loss (t)`/ farm_b_week12B$`Harvest Amount (t)`, paired = FALSE, alpha = 0.05)
t.test(farm_a_week12B$`On farm loss (t)`/ farm_a_week12B$`Harvest Amount (t)`,farm_b_week12B$`On farm loss (t)`/ farm_b_week12B$`Harvest Amount (t)` , paired = FALSE, alpha = 0.25,df = 43, alternative = "two.sided", var.equal = TRUE)

  # Third Part
farm_a_week1c <-farm_a |> filter(Week == 1) 
var.test(farm_a_week1c$'Harvest-related Emissions', farm_a_week1c$'Storage-related Emissions')
t.test(farm_a_week1c$'Harvest-related Emissions', farm_a_week1c$'Storage-related Emissions', paired = TRUE, var.equal =TRUE, conf.level = 0.95, alternative = "greater")

farm_a_week2c <-farm_a |> filter(Week == 2) 
var.test(farm_a_week2c$'Harvest-related Emissions', farm_a_week2c$'Storage-related Emissions')
t.test(farm_a_week2c$'Harvest-related Emissions', farm_a_week2c$'Storage-related Emissions', paired = TRUE,var.equal =TRUE,conf.level = 0.95, alternative = "greater")

farm_a_week3c <-farm_a |> filter(Week == 3) 
var.test(farm_a_week3c$'Harvest-related Emissions', farm_a_week3c$'Storage-related Emissions')
t.test(farm_a_week3c$'Harvest-related Emissions', farm_a_week3c$'Storage-related Emissions', paired = TRUE,var.equal =TRUE,conf.level = 0.95, alternative = "greater")

farm_a_week4c <-farm_a |> filter(Week == 4) 
var.test(farm_a_week4c$'Harvest-related Emissions', farm_a_week4c$'Storage-related Emissions')
t.test(farm_a_week4c$'Harvest-related Emissions', farm_a_week4c$'Storage-related Emissions',paired = TRUE,var.equal =TRUE,conf.level = 0.95, alternative = "greater") 

farm_a_week5c <-farm_a |> filter(Week == 5) 
var.test(farm_a_week5c$'Harvest-related Emissions', farm_a_week5c$'Storage-related Emissions')
t.test(farm_a_week5c$'Harvest-related Emissions', farm_a_week5c$'Storage-related Emissions', paired = TRUE,var.equal =TRUE,conf.level = 0.95, alternative = "greater")

farm_a_week6c <-farm_a |> filter(Week == 6) 
var.test(farm_a_week6c$'Harvest-related Emissions', farm_a_week6c$'Storage-related Emissions')
t.test(farm_a_week6c$'Harvest-related Emissions', farm_a_week6c$'Storage-related Emissions', paired = TRUE,var.equal =TRUE,conf.level = 0.95, alternative = "greater") 

farm_a_week7c <-farm_a |> filter(Week == 7) 
var.test(farm_a_week7c$'Harvest-related Emissions', farm_a_week7c$'Storage-related Emissions')
t.test(farm_a_week7c$'Harvest-related Emissions', farm_a_week7c$'Storage-related Emissions', paired = TRUE,var.equal =TRUE,conf.level = 0.95, alternative = "greater")

farm_a_week8c <-farm_a |> filter(Week == 8)
var.test(farm_a_week8c$'Harvest-related Emissions', farm_a_week8c$'Storage-related Emissions')
t.test(farm_a_week8c$'Harvest-related Emissions', farm_a_week8c$'Storage-related Emissions', paired = TRUE,var.equal =TRUE,conf.level = 0.95, alternative = "greater")

farm_a_week9c <-farm_a |> filter(Week == 9) 
var.test(farm_a_week9c$'Harvest-related Emissions', farm_a_week9c$'Storage-related Emissions')
t.test(farm_a_week9c$'Harvest-related Emissions', farm_a_week9c$'Storage-related Emissions',paired = TRUE, var.equal =TRUE,conf.level = 0.95, alternative = "greater")

farm_a_week10c <-farm_a |> filter(Week == 10) 
var.test(farm_a_week10c$'Harvest-related Emissions', farm_a_week10c$'Storage-related Emissions')
t.test(farm_a_week10c$'Harvest-related Emissions', farm_a_week10c$'Storage-related Emissions', paired = TRUE,var.equal =TRUE,conf.level = 0.95, alternative = "greater") 

farm_a_week11c <-farm_a |> filter(Week == 11) 
var.test(farm_a_week11c$'Harvest-related Emissions', farm_a_week11c$'Storage-related Emissions')
t.test(farm_a_week11c$'Harvest-related Emissions', farm_a_week11c$'Storage-related Emissions', paired = TRUE,var.equal =TRUE,conf.level = 0.95, alternative = "greater")

farm_a_week12c <-farm_a |> filter(Week == 12) 
var.test(farm_a_week12c$'Harvest-related Emissions', farm_a_week12c$'Storage-related Emissions')
t.test(farm_a_week12c$'Harvest-related Emissions', farm_a_week12c$'Storage-related Emissions',paired = TRUE,var.equal =TRUE, conf.level = 0.95, alternative = "greater")

# Part C

library(tidyverse)
library(stringr)

part_c_a <- farm_a %>% mutate(Status_level= case_when(Status == "Efficient" ~ 1, Status == "Not Efficient" ~ 0))
part_c_b <- farm_b %>% mutate(Status_level= case_when(Status == "Efficient" ~ 1, Status == "Not Efficient" ~ 0))

alpha <- 0.05

p_a <- sum(part_c_a$Status_level) / length(part_c_a$Status_level)
p_b <- sum(part_c_b$Status_level) / length(part_c_b$Status_level)

(p_a - p_b) - qnorm(1 - alpha) * sqrt((p_a * (1 - p_a) / 20) + (p_b * (1 - p_b) / 25)) # This value is the lower bound for proportion test.

