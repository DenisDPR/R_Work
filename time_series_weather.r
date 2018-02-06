library("dplyr")
library(dplyr)
library(tidyr)
library(plotly)
library(tidyverse)
library(gridExtra)
library(ggplot2)
library(visreg)
library(corrplot)
library(plyr)
library(tidyquant)
library(scales)
library(lubridate) # work with dates
library(xts)

miya_in <-read.csv('miya_in.csv', header = T, check.names = T)
miya_out <-read.csv('miya_out.csv', header = T, check.names = T)
miya_wind <-read.csv('wind_miyaura.csv', header = T, check.names = T)
miya_rain <-read.csv('miya_rain.csv', header = T, check.names = T)

head(miya_wind)
colnames(miya_wind) <- c("date", "WindAngle","WindStrength","GustAngle","GustStrength")
colnames(miya_in) <- c("date", "temp","Hum","CO2","Noise","Pressure")
colnames(miya_out) <- c("date", "temp","Hum")
colnames(miya_rain) <- c("date", "rain")

miya_in$date <- as.Date.factor(miya_in$date, format = "%Y-%m-%d")
miya_out$date <- as.Date.factor(miya_out$date, format = "%Y-%m-%d")
miya_wind$date <- lubridate::date(miya_wind$date)
miya_rain$date <- lubridate::date(miya_rain$date)

head(miya_rain)
str(miya_wind)
summary(miya_in)

## Function to calculate mean of Temp, GDD, CO2, Hum
summary_miya_in <- function(data)
{
  df1 = data %>%
    tq_transmute(select = temp,mutate_fun = apply.daily,FUN = max)
  colnames(df1) <- c("date", "temp_max")

  df2 = data %>%
    tq_transmute(select = temp,mutate_fun = apply.daily,FUN = min)
  colnames(df2) <- c("date", "temp_min")

  df3 = data %>%
    tq_transmute(select = Hum,mutate_fun = apply.daily,FUN = max)
  colnames(df3) <- c("date", "Hum_max")

  df4 = data %>%
    tq_transmute(select = Hum,mutate_fun = apply.daily,FUN = min)
  colnames(df4) <- c("date", "Hum_min")

  df5 = data %>%
    tq_transmute(select = CO2,mutate_fun = apply.daily,FUN = max)
  colnames(df5) <- c("date", "CO2_max")
  df5$CO2_max=round((na.spline(df5$CO2_max)),digits = 1)

  df6 = data %>%
    tq_transmute(select = CO2,mutate_fun = apply.daily,FUN = min)
  colnames(df6) <- c("date", "CO2_min")
  df6$CO2_min=round((na.spline(df6$CO2_min)),digits = 1)
  

  df11 <- merge(df1, df2, by = c( "date"))
  df22 <- merge(df3, df4, by = c( "date"))
  df33 <- merge(df5, df6, by = c( "date"))
  
  df333 <-merge(df11, df22, by = c( "date"))
  df444 <-merge(df333, df33, by = c( "date"))
  
  df444$mean_temp <- (df444$temp_max + df444$temp_min) / 2
  
  df444$mean_Hum <- (df444$Hum_max + df444$Hum_min) / 2
  df444$mean_CO2 <- (df444$CO2_max + df444$CO2_min) / 2
  
  return(df444)
}

Inside <- summary_miya_in(miya_in)
head(Inside)

summary_miya_out <- function(data)
{
  df1 = data %>%
    tq_transmute(select = temp,mutate_fun = apply.daily,FUN = max)
  colnames(df1) <- c("date", "temp_max")

  df2 = data %>%
    tq_transmute(select = temp,mutate_fun = apply.daily,FUN = min)
  colnames(df2) <- c("date", "temp_min")

  df3 = data %>%
    tq_transmute(select = Hum,mutate_fun = apply.daily,FUN = max)
  colnames(df3) <- c("date", "Hum_max")

  df4 = data %>%
    tq_transmute(select = Hum,mutate_fun = apply.daily,FUN = min)
  colnames(df4) <- c("date", "Hum_min")

  df11 <- merge(df1, df2, by = c( "date"))
  df22 <- merge(df3, df4, by = c( "date"))
  df333 <-merge(df11, df22, by = c( "date"))
  
  df333$mean_temp <- (df333$temp_max + df333$temp_min) / 2
  
  df333$mean_Hum <- (df333$Hum_max + df333$Hum_min) / 2
  
  return(df333)
}
Outside <- summary_miya_out(miya_out)
head(Outside)

summary_miya_wind <- function(data)
{
  df1 = data %>%
    tq_transmute(select = WindAngle,mutate_fun = apply.daily,FUN = max)
  colnames(df1) <- c("date", "WindAngle_max")
  
  df2 = data %>%
    tq_transmute(select = WindAngle,mutate_fun = apply.daily,FUN = min)
  colnames(df2) <- c("date", "WindAngle_min")
  
  df3 = data %>%
    tq_transmute(select = WindStrength,mutate_fun = apply.daily,FUN = max)
  colnames(df3) <- c("date", "WindStrength_max")
  
  df4 = data %>%
    tq_transmute(select = WindStrength,mutate_fun = apply.daily,FUN = min)
  colnames(df4) <- c("date", "WindStrength_min")
  
  df5 = data %>%
    tq_transmute(select = GustAngle,mutate_fun = apply.daily,FUN = max)
  colnames(df5) <- c("date", "GustAngle_max")
  
  df6 = data %>%
    tq_transmute(select = GustAngle,mutate_fun = apply.daily,FUN = min)
  colnames(df6) <- c("date", "GustAngle_min")
  
  df7 = data %>%
    tq_transmute(select = GustStrength,mutate_fun = apply.daily,FUN = max)
  colnames(df7) <- c("date", "GustStrength_max")
  
  df8 = data %>%
    tq_transmute(select = GustStrength,mutate_fun = apply.daily,FUN = min)
  colnames(df8) <- c("date", "GustStrength_min")
  
  df11 <- merge(df1, df2, by = c( "date"))
  df22 <- merge(df3, df4, by = c( "date"))
  df33 <- merge(df5, df6, by = c( "date"))
  df44 <- merge(df7, df8, by = c( "date"))
  
  df333 <-merge(df11, df22, by = c( "date"))
  df444 <-merge(df33, df44, by = c( "date"))
  df555 <-merge(df333, df444, by = c( "date"))
  
  return(df555)
}

wind <- summary_miya_wind(miya_wind)
head(wind)

summary_miya_rain <- function(data)
{
  df1 = data %>%
    tq_transmute(select = rain,mutate_fun = apply.daily,FUN = max)
  colnames(df1) <- c("date", "rain_max")
  
  df2 = data %>%
    tq_transmute(select = rain,mutate_fun = apply.daily,FUN = min)
  colnames(df2) <- c("date", "rain_min")
  
  df11 <- merge(df1, df2, by = c( "date"))
  
  return(df11)
}
rain <- summary_miya_rain(miya_rain)
head(rain)


Inside$CO2_max=round((na.spline(Inside$CO2_max)),digits = 1)
Inside$CO2_min=round((na.spline(Inside$CO2_min)),digits = 1)

write.csv(Inside, file = "Inside_module.csv") 
write.csv(Outside, file = "Outside_module.csv") 
write.csv(wind, file = "wind.csv") 
write.csv(rain, file = "rain.csv") 


p_in <- ggplot(aes(x=date, y = mean_temp), data = Inside) + 
  geom_line(colour = "#00a4e6", size = 0.5) + 
  ggtitle("Daily Mean Temperature\n") + 
  labs(x = NULL, y = "Mean Temperature ") + 
  scale_x_date(labels = date_format("%b %y")) +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(color = "gray50", size = 0.09),
        #panel.grid.major.x = element_blank(),
        legend.position="none",
        panel.background = element_blank(),
        axis.text.y = element_text(colour = "#64382C", size = 20),
        #axis.text.x = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))
