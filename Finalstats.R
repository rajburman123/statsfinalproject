---
 title: "Final Project"
subtitle: "STAT 230 Spring 2025"
author: "Raj Burman and Yaman Ozturk"
format:
html:
embed-resources: true
---
  
#Questions
#Question 1- What is the relation between sale Model and Electric Range? 2. 
#What is the average Electric Range?

# Data
  
library(tidyverse)
EV <- read_csv("Electric_Vehicle_Population_Data.csv")

#categorical- VIN (1-10), County, City, State
#categorical ordinal- Make, Model, Electric Vehicle Type, 
#numeric discrete- Postal Code, Model Year, Electric Range, Base MSRP


#Create at least one histogram of one of your numeric variables.

EV |>
  ggplot(aes(x = `Electric Range`)) +
  geom_histogram()

#Create at least one boxplot of one of your numeric variables 

ggplot(data = EV, aes(x = as.factor(Model), y = `Electric Range`)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Model by Electric Range",
       x = "Model",
       y = "Electric Range")

#Summarize your data
  
EV %>%
  group_by('Model') %>%
  summarise(
    mean_value = mean('Electric Range', NA.rm = TRUE),
    sd_value = sd('Electric Range', NA.rm = TRUE),
    min_value = min('Electric Range', NA.rm = TRUE),
    max_value = max('Electric Range', NA.rm = TRUE)
  )

#ANOVA
  
  anova_result <- aov(`Electric Range` ~ Model, data = EV)
summary(anova_result)

#What is the average Electric Range?

mean(EV$`Electric Range`, na.rm = TRUE)

#which vehicle Make is most used?

which.max(table(EV$Make))

#what is the average electric range in Olympia city?

mean(EV$`Electric Range`[EV$City == "Olympia"], na.rm = TRUE)

#which Make is most used in King County?

king_data <- subset(EV, County == "King")
table(king_data$Make)
which.max(table(king_data$Make))

#answers- Since the p-value (2e-16) is less than 0.05, we reject the null hypothesis. 
#This means there is a statistically significant difference between the means of
#at least two groups in this dataset. At least one group differs significantly
#from the others.  

#The average electric Range amonge all the vehicles is, 45.56228. Out of all the 
#vehicles, TESLA is most used with a frequency of 40. The average electric 
#range of all electric cars in Olympia city is, 50.06738. In King County, the 
#most used is Make is TESLA, with a frequency of 40.


