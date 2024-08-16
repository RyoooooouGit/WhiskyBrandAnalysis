library(dplyr)
library(asbio)
library(ggplot2)

brandData <- read.csv("Whisky_Brand.csv")

goodBrandData <- brandData %>% filter(WB.Ranking != "G", WB.Ranking != "F", WB.Ranking != "E", Votes >= 100)

numOfBrandByArea <- brandData %>% 
  filter(Area != "") %>%
  group_by(Area) %>%
  summarise(NumOfBrand = n()) %>%
  arrange(desc(NumOfBrand))

numOfGoodBrandByArea <- goodBrandData %>% 
  filter(Area != "") %>%
  group_by(Area) %>%
  summarise(NumOfGoodBrand = n()) %>%
  arrange(desc(NumOfGoodBrand)) %>%
  mutate(Area = factor(Area, levels = Area))

ggplot(numOfGoodBrandByArea, aes(x = "", y = NumOfGoodBrand, fill = Area)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Pie Chart of Good Whisky Brand", fill = "Area") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))




numOfGoodWhiskyByArea <- goodBrandData %>% 
  filter(Area != "") %>%
  group_by(Area) %>%
  summarise(NumOfWhisky = sum(Whiskies, na.rm = TRUE)) %>%
  arrange(desc(NumOfWhisky)) %>%
  mutate(Area = factor(Area, levels = Area))

ggplot(numOfGoodWhiskyByArea, aes(x = "", y = NumOfWhisky, fill = Area)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Pie Chart of Good Whisky", fill = "Area") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))



par(mfrow = c(2,2))
whiskyInScotland <- brandData %>% filter(Area == "Scotland")
hist(whiskyInScotland$Rating, breaks = seq(0, 100, by=1), main = "Histogram of Whisky Rating in Scotland", xlab = "rating", ylab = "number of brands")
boxplot(whiskyInScotland$Rating, main = "Boxplot of Whisky Rating in Scotland", ylab = "rating", horizontal = TRUE)
whiskyInUS <- brandData %>% filter(Area == "United States")
hist(whiskyInUS$Rating, breaks = seq(0, 100, by=1), main = "Histogram of Whisky Rating in United States", xlab = "rating", ylab = "number of brands")
boxplot(whiskyInUS$Rating, main = "Boxplot of Whisky Rating in United States", ylab = "rating", horizontal = TRUE)




whiskyInScotland <- whiskyInScotland %>% filter(!is.na(Rating))
whiskyInUS <- whiskyInUS %>% filter(!is.na(Rating))

n1 = nrow(whiskyInScotland)
n2 = nrow(whiskyInUS)

zCriticalValue = qnorm(1 - 0.05)

xbar1 = mean(whiskyInScotland$Rating)
xbar2 = mean(whiskyInUS$Rating)
sd1 = sd(whiskyInScotland$Rating)
sd2 = sd(whiskyInUS$Rating)
z = (xbar1 - xbar2)/sqrt((sd1^2/n1 + sd2^2/n2))
pValue = pnorm(z)

