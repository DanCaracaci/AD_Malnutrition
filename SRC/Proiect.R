library(openintro)
library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)

nutrition <- read.csv("nutrition2.0.csv", sep = ";")
nutrition$Year <- as.numeric(nutrition$Year)

summary(nutrition)
glimpse(nutrition)

#1 Wasting over the years(china)
nutrCHN <- filter(nutrition, nutrition$ISO.code == "CHN")

ggplot(nutrCHN, aes(x = Year, y = Wasting)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(x = "Year", y = "Wasting (%)") +
  ggtitle("Wasting Over the Years(CHINA)")


#2 National indicators over the years (Bangladesh)
nutrBangladesh <- filter(nutrition, nutrition$Country == "BANGLADESH")

data_long <- nutrBangladesh %>%
  pivot_longer(cols = c(Severe.Wasting, Wasting, Overweight, Stunting, Underweight),
               names_to = "Category",
               values_to = "Percentage")

ggplot(data_long, aes(x = Year, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Percentage (%)") +
  ggtitle("Nutritional Indicators Over the Years(Bangladesh)") +
  theme(legend.position = "top")   


#3 Median Overweight for Low and High PIB Countries
nutritionLastYear <- nutrition %>%
  group_by(Country) %>%
  filter(Year == max(Year)) %>%
  ungroup()

average_pib <- mean(nutritionLastYear$PIB.per.capita, na.rm = TRUE)

low_pib_data <- nutritionLastYear %>%
  filter(PIB.per.capita < average_pib)

high_pib_data <- nutritionLastYear %>%
  filter(PIB.per.capita >= average_pib)

median_overweight_low_pib <- median(low_pib_data$Overweight, na.rm = TRUE)

median_overweight_high_pib <- median(high_pib_data$Overweight, na.rm = TRUE)

bar_data <- data.frame(
  Group = c("Low PIB", "High PIB"),
  Median_Overweight = c(median_overweight_low_pib, median_overweight_high_pib)
)

ggplot(bar_data, aes(x = Group, y = Median_Overweight)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.5) +
  labs(x = "PIB Group", y = "Median Overweight%") +
  ggtitle("Median Overweight for Low and High PIB Countries")


#4 PIB - Stunting relation
ggplot(nutritionLastYear, aes(x = PIB.per.capita, y = Stunting)) +
  geom_point() +
  labs(x = "PIB", y = "Stunting rate") +
  ggtitle("PIB - Stunting relation") 


#5 Severe Wasting distribution
ggplot(nutritionLastYear, aes(x = Severe.Wasting)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(x = "Severe Wasting", y = "Frequency") +
  ggtitle("Severe Wasting distribution") +
  theme_minimal()


#liniar models

model1 <- lm(Stunting ~ Underweight+
               PIB.per.capita+
               Wasting+
               Severe.Wasting+
               Overweight, data = nutrBangladesh)

model2 <- lm(Stunting ~ Underweight, data = nutrBangladesh)
model3 <- lm(Stunting ~ PIB.per.capita, data = nutrBangladesh)
model4 <- lm(Stunting ~ Wasting, data = nutrBangladesh)
model5 <- lm(Stunting ~ Overweight, data = nutrBangladesh)

summary(model1)

anova(model1,model2,model3,model4,model5)
