#GAPMINDER REVISITED
#Libraries used
library(ggplot2)
library(tidyr)
library(dplyr)
library(plotly)
library(gridExtra)

#1. The variable(s) you investigated, your observations, and any summary statistics
#Data used are Agriculture land (Percentage), Agriculture (Percentage of GDP) and Agriculture indicator of Employment
ALAND <- read.csv("Agriculture land.csv", header = T, row.names = 1, check.names = F)
AGDP <- read.csv("Agriculture (p of GDP).csv", header = T, row.names = 1, check.names = F)
AEMPL <- read.csv("indicator_t agriculture employ.csv", header = T, row.names = 1, check.names = F)

# Rename first empty column and assign new name for dataframe
new_land <- data.frame(name=rownames(ALAND), ALAND, row.names=NULL, check.names = FALSE )
new_GDP <- data.frame(name=rownames(AGDP), AGDP, row.names=NULL, check.names = FALSE )
new_empl <- data.frame(name=rownames(AEMPL), AEMPL, row.names=NULL, check.names = FALSE )

head(new_land)
# RESHAPE
# Checking the structure of each dataframe before reshaping
str(new_land)
str(new_GDP)
str(new_empl)
# Since data was untidy, Reshaped the data using tidyr::gather
tidy_land <- tidyr::gather(new_land, "Year", "Land", 2:53)
tidy_GDP <- tidyr::gather(new_GDP, "Year", "GDP", 2:52)
tidy_empl <- tidyr::gather(new_empl, "Year", "EMPLY", 2:30)
head(tidy_land)
#To work on data within the same period, Using left_join 
#to create new combined dataframe of same period

new_GDP_empl_land <- left_join(tidy_GDP, tidy_empl, by=c("Year","name")) %>%
  left_join(., tidy_land, by=c("Year","name"))
#check newly joined data
tidy_GDP
head(tidy_GDP)

head(tidy_empl)
head(tidy_land)
head(new_GDP_empl_land)


#Selected sampled countries
sample_countries <- dplyr::filter(new_GDP_empl_land, !is.na(GDP),!is.na(EMPLY),!is.na(Land), name %in% c('Japan', 'Thailand','United States'))
head(sample_countries)
colnames(sample_countries)[1] <- 'country'

# Line Graph of Agriculture (Percentage of GDP) variable for the selected countries 
p_gdp <- ggplot(subset(sample_countries, as.numeric(Year)%%2 == 0)) +
  geom_line(aes(x=Year, y=GDP, colour=country, group=country)) +
  theme(legend.position="bottom") +
  labs(x='Year', y='GDP') 

p_land <- ggplot(subset(sample_countries, as.numeric(Year)%%2 == 0)) +
  geom_line(aes(x=Year, y=Land, colour=country, group=country)) +
  theme(legend.position="bottom") +
  labs(x='Year', y='Agriculture Land') 

p_empl <- ggplot(subset(sample_countries, as.numeric(Year)%%2 == 0)) +
  geom_line(aes(x=Year, y=EMPLY, colour=country, group=country)) +
  theme(legend.position="bottom") +
  labs(x='Year', y='Agriculture Employment') 

#Creating a plotly plot of one variable
#p1 <- ggplotly(p_gdp)
#p2 <- ggplotly(p_empl)
#p3 <- ggplotly(p_empl)
p1
#Reproduce the three plots into one page
g <- grid.arrange(p_gdp,p_empl,p_land, ncol= 1)
ggsave(filename = "plotlyOutput.JPEG",g)

