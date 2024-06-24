# install.packages("tidyverse")
# install.packages("gridExtra")
# install.packages("readxl")
# install.packages("writexl")
# install.packages("ggpubr")
# install.packages("dplyr")
# install.packages("ggmosaic")
# install.packages("corrplot")
# install.packages("ggplot2")
# install.packages("xtabs")

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

case_data <- read_xlsx("/Users/macbookair/Desktop/IE266/CaseStudy1/Agricultural_Impact_Data.xlsx")

# A Part

part_a <-case_data |> 
  group_by(SubRegion, Item) %>% 
  summarise(Total_for_each_Item_over_Subregion=sum(`Production (t)`),
            .groups = 'drop') %>% group_by(SubRegion, add = TRUE) %>%
  mutate(total_Subregion = sum(Total_for_each_Item_over_Subregion))

part_a$Relative <- part_a$Total_for_each_Item_over_Subregion / part_a$total_Subregion

ggplot(part_a, aes(fill=Item, y=Relative, x=SubRegion)) + 
    geom_bar(position="fill", stat="identity") + theme_minimal()+ theme(axis.text.x = element_text(angle=90, hjust=1)) +scale_fill_manual(values=c("#9fc8c8", "#1f6f6f", "#54a1a1")) + labs(x= "SubRegion",y = "Frequency Distribution of the Items")
ggsave("IE 266 Case_1 Part_1_a.jpeg")

# B Part

# Filtering the data so it covers the rice production in year 2020
rice2020_emissions <- case_data %>%
  filter(Year == 2020 & Item == "Rice") %>%
  select(Country, `Emissions (CO2eq kt)`)

# Finding the total rice emissions
total_rice_emissions <- sum(rice2020_emissions$`Emissions (CO2eq kt)`)


# Adding a 'percentage_CO2' Column to our data this will help us find the top countries
rice2020_emissions_with_percentage <- rice2020_emissions %>%
  mutate(Percentage_CO2 = (`Emissions (CO2eq kt)` / total_rice_emissions) * 100)



# Sorting the data in a decreasing order according to co2 percentages. 
sorted_emissions <- rice2020_emissions_with_percentage %>%
  arrange(desc(`Percentage_CO2`))


# Adding another column to show the cumulative percentages and filtering the 80%
top_countries_emissions <- sorted_emissions %>%
  mutate(Cumulative_Percentage = cumsum(Percentage_CO2)) %>%
  filter(Cumulative_Percentage <= 80)



# Plotting the contribution using a Pie chart 
ggplot(top_countries_emissions, aes(x = "", y = Percentage_CO2, fill = Country)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "GHG Emissions due to Rice in 2020 (Top Countries Contributing to 80%)", 
       fill = "Country") +
  # Adding and rounding the percentages
  geom_text(aes(label = paste0(round(Percentage_CO2, 1), "%")), size = 3 ,
            position = position_stack(vjust = 0.5)) +
  theme_minimal()
ggsave("IE 266 Case_1 Part_1_b_pie.jpeg")

# Changing the filter to 85% so we can observe in which country it surpasses 80%
top_countries_emissions <- sorted_emissions %>%
  mutate(Cumulative_Percentage = cumsum(Percentage_CO2)) %>%
  filter(Cumulative_Percentage <= 85)


# Factoring the data
top_countries_emissions$Country <- factor(top_countries_emissions$Country, levels = top_countries_emissions$Country)


# Ploting a Pareto Chart to observe co2 percantage contribution by country
ggplot(top_countries_emissions, aes(x = Country)) +
  geom_bar(aes(y = Percentage_CO2), fill = 'darkred', stat = "identity") +
  geom_point(aes(y = Cumulative_Percentage), color = rgb(0, 0, 0), pch = 15, size = 1.5) +
  geom_path(aes(y = Cumulative_Percentage, group = 1), colour = "red", lty = 8, size = 0.7) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6)) +
  scale_y_continuous(limits = c(0, 85)) +
  geom_hline(yintercept = 80, linetype = "dotted", color = "black", size = 1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6, size = 9.5,  color = "black")) + 
  labs(title = " Countries' GHG emissions due to rice in 2020 ", x = 'Country', y ='CO2 Emission Percentages')
ggsave("IE 266 Case_1 Part_1_b_paretto.jpeg")

# C Part

q1_c <- filter(case_data, case_data$Year %in% c('1999', '2009', '2019'), case_data$Development %in% c('Developed'), case_data$Item %in% c('Cereals'))

q1_c$`Cereal Production Amount per Capita` <- q1_c$`Production (t)` / q1_c$Population

q1_c %>%
  ggplot(aes(x = `Cereal Production Amount per Capita`, y = "")) + labs(y = "") + geom_boxplot(fill = 'steelblue')+ facet_wrap(~Year)
ggsave("IE 266 Case_1 Part_1_c.jpeg")

# D Part

  # Part 1
rice_data <- case_data[case_data$Item == "Rice" & case_data$Year %in% 2011:2020,]
rice_production <- tapply(rice_data$'Production (t)', rice_data$Country, sum)
ghg_emissions_total <- tapply(rice_data$'Emissions (CO2eq kt)'*1000, rice_data$Country, sum)
rice_all <- data.frame(Country = names(rice_production), Production = rice_production, Emissions = ghg_emissions_total)
rice_production_sorted <- sort(rice_production, decreasing = TRUE)
top12 <- data.frame(Rice_Production = head(rice_production_sorted,12), Total_Emission = ghg_emissions_total[names(head(rice_production_sorted,12))])
View(top12)

  #Part 2
rice_data_2 <- case_data[case_data$Item == "Rice" & case_data$Year %in% 1995:2020,]
ghg_average <- tapply(rice_data_2$'Emissions (CO2eq kt)', rice_data_2$Country, mean)
sorted_ghg <- sort(ghg_average, decreasing = TRUE)
histogram_countries <- names(sorted_ghg)[21:(length(sorted_ghg)-20)]
histogram_data <- data.frame(Average_Emission = sorted_ghg[histogram_countries], Countries = histogram_countries)
breaks <- seq(0, max(histogram_data$Average_Emission))
ggplot(histogram_data, aes(x = Average_Emission)) + 
  geom_histogram(color = "darkred", fill = "darkred", binwidth = 450) +
  stat_bin(binwidth = 450, geom = "text", aes(label = ..count..), vjust = -0.5) + 
  labs(x = "Average GHG Emission", y = "Frequency", 
       title = "Average GHG Emissions of Countries from Rice Between Years 1995-2020")
ggsave("IE 266 Case_1 Part_1_d.jpeg")

# E Part

q1_e <- case_data |>
  subset(select = c(5, 6, 8, 9, 10))  |> group_by(Year, Item)|> summarize('Total_Production' = sum(`Production (t)`), 'Total_Emission' = sum(`Emissions (CO2eq kt)`))
q1_e$Emission_Efficiency <- q1_e$Total_Production / q1_e$Total_Emission

q1_e %>%
  ggplot(aes(Year, Emission_Efficiency)) + geom_line(aes(color = Item)) + scale_color_manual(values = c("red", "blue", "darkgreen")) + labs(title = "Relation between Emission Efficiency and Items over Given Years")
ggsave("IE 266 Case_1 Part_1_e.jpeg")
View(q1_e[which.max(q1_e$Emission_Efficiency), ]) # Gives the most efficient item which is cereal.

# E Part Continued

q1_e_second <- subset(case_data, select = c(3, 4, 5, 8 ,9, 10))

q1_e_second <- q1_e_second |>
  filter(Year == '2020', Item == 'Rice') |> group_by(Country, Year, Item, `Production (t)`, `Emissions (CO2eq kt)`)

q1_e_second$Emission_Efficiency <- q1_e_second$`Production (t)` / q1_e_second$`Emissions (CO2eq kt)`

q1_e_sorted <- q1_e_second[order(q1_e_second$Emission_Efficiency, decreasing = TRUE), ][1:100, ]

View(q1_e_sorted) # Contingency Table for Part E

# Contingency table

filtered_data <- case_data %>%
  filter(Item == 'Rice' & Year == 2020) %>% arrange(desc(`Production (t)`)) %>%  slice(1:100) %>%  
  mutate(`Emission Efficiency` =  `Production (t)` / `Emissions (CO2eq kt)` ) %>% select(4, 12)

filtered_data$Development <- factor(filtered_data$Development, levels = c("Developed", "Developing", "Underdeveloped"))

# Define labels for the intervals
interval_labels <- c("0-250", "251-500", "501-750", "751-1000", "1001-1250", "1251-1500", "1501-1750", "1751-2000", "2001-2250", "2251-2500", "2501-5000")

# Define intervals for Emission Efficiency 
efficiency_intervals <- cut(filtered_data$`Emission Efficiency`, breaks = c(0, 250, 500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 5000), labels = interval_labels)

# Create the contingency table
contingency_table <- table(filtered_data$Development, efficiency_intervals)
print(contingency_table)
# For the relation part 

q1_e_sorted |>
  ggplot(aes(Development, Emission_Efficiency)
  )+ theme_minimal() + theme(legend.position = "none") + scale_color_manual(values = c('Developed' = "#1771D7", 'Developing' = "#C53021", 'Underdeveloped' = "#13A613")) + geom_point(aes(color = Development)) + coord_flip() + geom_smooth(method = 'loess') + labs(title = "Relation between Development Status of Countries and Emission Efficiency")
ggsave("IE 266 Case_1 Part_1_e_for_relation.jpeg")

# F Part

q1_f <- case_data |> filter( Item == 'Rice' & Year == 2020)

cor.test(q1_f$`Production (t)`, q1_f$`Emissions (CO2eq kt)`, method = "pearson")

q1_f |>
  ggplot(aes(`Production (t)`, `Emissions (CO2eq kt)`),
         ) + theme(legend.position = "none")+  geom_point() + geom_smooth(
                                                              color = "red") + labs(title ="Relation between Production Amounts and GHG Emissions") 
ggsave("IE 266 Case_1 Part_1_f1.jpeg")

q1_f_corr <- q1_f |> subset(select = c(9, 10, 11))

corrplot(cor(q1_f_corr),
         method = "number",
         type = "upper")
ggsave("IE 266 Case_1 Part_1_f1_corr.jpeg")

cor.test(q1_f$`Freshwater withdrawals (kiloliter)`, q1_f$`Emissions (CO2eq kt)`, method = "pearson")

q1_f |>
  ggplot(aes(`Freshwater withdrawals (kiloliter)`, `Emissions (CO2eq kt)`)
         )+ theme(legend.position = "none")+  geom_point() + geom_smooth(color = "blue") + labs(title = "Relation between Freshwater Withdrawals and GHG Emissions")
ggsave("IE 266 Case_1 Part_1_f2.jpeg")
