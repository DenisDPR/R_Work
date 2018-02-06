library(zoo)
require(zoo)
library(zoo)
library(ggplot2)
library(gtable)
library(grid)
library(extrafont)
library(dplyr)

mydb = dbConnect(MySQL(), user='xxxx', password='xxx', dbname='xxxx', host='xxxxxx')
#dbListTables(mydb)
rs = dbSendQuery(mydb,"SELECT sdate as Date,ROUND((max_temp+min_temp)/2-10,1) as Mean_T 
                 FROM daily_temp 
                 WHERE sensor = '70:ee:50:27:2e:da' 
                 ORDER BY sdate ASC")
df1 <- fetch(rs, n = -1) 
dbClearResult(rs)

head(df1)

write.csv(df1, file = "df.csv") 
df <-read.csv('df.csv', header = T, check.names = T)

dfResult <- function(df){
  df$date <- as.Date.factor(df$Date, format = "%Y-%m-%d")
  df <- subset(df, select = -c(Date))
  df2 <- data.frame(date=seq(min(df$date), max(df$date), by="day"))
  df3 <- merge(df2, df, by="date", all=TRUE)
  idx <- which(is.na(df3$Mean_T))
  for (id in idx) 
    df3$Mean_T[id] <- df3$Mean_T[id-1]
  #Create add new data frame of cumulative Mean_T
  df4 <- cbind(df3,GDD = cumsum(df3$Mean_T))
  return (df4)
}
resu = dfResult(df)

startDate <- as.Date("2017-07-30")
PresentDate <- Sys.Date()
#Function to Filter Date
myfunc <- function(x,y){resu[resu$date >= x & resu$date <= y,]}
df_gdd <- myfunc(startDate,PresentDate)

head(df_gdd)
p1 <- ggplot(aes(x=date, y = Mean_T), data = df_gdd) + 
  geom_line(colour = "#12954E", size = 1) + 
  ggtitle("(a) Daily Growing Degree Days\n")  + labs(y = "GDD") +
  theme(#panel.grid.minor = element_blank(), 
    panel.grid.major = element_line(color = "#12954E", size = 0.1),
    # panel.grid.major.x = element_blank(),
    panel.background = element_blank(), 
    axis.text.y = element_text(color = "#12954E", size = 20),
    axis.title.y = element_text(colour = "#12954E",size = 20),
    axis.text.x = element_text(size = 20),
    plot.title = element_text(hjust = 0.6,colour = "#12954E",size = 14),
    axis.title.x=element_blank(),
    # axis.text.x=element_blank(),
    axis.ticks.x=element_blank())+
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()

p2 <- ggplot(aes(x=date, y = GDD), data = df_gdd) + 
  geom_line(colour = "#00a4e6", size = 1) + 
  ggtitle(" (b) Cumulative Growing Degree Days\n")  + labs(y = "Cumulative GDD") +
  theme(#panel.grid.minor = element_blank(), 
    panel.grid.major = element_line(color = "#00a4e6", size = 0.1),
    #panel.grid.major.x = element_blank(),
    panel.background = element_blank(), 
    axis.text.y = element_text(color = "#00a4e6", size = 20),
    axis.title.y = element_text(colour = "#00a4e6",size = 20),
    axis.text.x = element_text(size = 20),
    plot.title = element_text(hjust = 0.6,colour = "#00a4e6",size = 14),
    axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    axis.ticks.x=element_blank())+
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()

plot_cum_gdd <- grid.arrange(p1,p2, ncol= 1)

ggsave(filename = "plot_cum_gdd.JPEG",plot_cum_gdd)
