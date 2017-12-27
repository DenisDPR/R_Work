library(dplyr)
library(tidyr)
library(tidyverse)
library(gridExtra)
library(ggplot2)

###########   DATE SET   #########
NO3 <-read.csv('TOGUCHI_NO3.csv', header = T, check.names = T)
ECS <-read.csv('TOGUCHI_EC.csv', header = T, check.names = T)

head(NO3,2)
head(ECS,2)
################################################################################
##  BLOCK   vid      PLANT START       HARVEST START     END HARVEST           ##
##  C        v1       2017-07-18        2017-09-18         ?2017-11-27 (until) ##
##  D        v1       2017-05-26       2017-07-25          2017-10-16          ##
##  F        v1       2017-08-16       2017-10-23         ?2017-11-27          ##
##  F        v2      2017-08-16         ?                                      ##
################################################################################
## START AND END DATES of 'start of Plant' to 'Start of harvesting'
End_C_After <- as.Date("2017-11-27")
start_C_After <- as.Date("2017-09-18")

End_D_After <- as.Date("2017-10-10")
start_D_After <- as.Date("2017-07-25")

End_F_After <- as.Date("2017-11-27")
start_F_After <- as.Date("2017-10-23")

#####  Function to Filter Date ###############
filterDate <- function(df,x,y){
  df$Date <- as.Date.factor(df$Date, format = "%Y-%m-%d")
  df[df$Date >= x & df$Date <= y,]
}

## C Block  V1 Variety ####
C_EC_DF_After <- filterDate(subset(ECS, Block == 'C'),start_C_After,End_C_After)
C_NO3_DF_After <- filterDate(subset(NO3, Block == 'C'),start_C_After,End_C_After)

##### D Block  V1 Variety #####
D_EC_DF_After <- filterDate(subset(ECS, Block == 'D'),start_D_After,End_D_After)
D_NO3_DF_After <- filterDate(subset(NO3, Block == 'D'),start_D_After,End_D_After)
##### F Block  V1 variety ######
F_EC_DF_After <- filterDate(subset(ECS, Block == 'F'),start_F_After,End_F_After)
F_NO3_DF_After <- filterDate(subset(NO3, Block == 'F'),start_F_After,End_F_After)

# Bind the three Blocks C,D,F
EC_CDF_After <-dplyr::bind_rows(C_EC_DF_After,D_EC_DF_After,F_EC_DF_After)
NO3_CDF_After <-dplyr::bind_rows(C_NO3_DF_After,D_NO3_DF_After,F_NO3_DF_After)
head(NO3_CDF_After,2)
## PLOT 1 ## 
p1_After <- ggplot(aes(y=EC, x=Date),data = EC_CDF_After) +
  geom_line(aes(color=Block)) + labs(x=NULL, y='EC') +
  theme(legend.position="none",
        panel.grid.minor.x = element_blank()) +
  theme_bw()
p1_After
ggsave(filename = "toguchi_p1_After.JPEG",p1_After)
## PLOT 2 ## 

p4_After <- ggplot(aes(y=Measure, x=Date),data = NO3_CDF_After) +
  geom_line(aes(color=Block))+ labs(x='Date', y='NO3') +
  theme(legend.position="none",
        panel.grid.minor.x = element_blank()) +
  theme_bw()
p4_After
ggsave(filename = "toguchi_p4_After.JPEG",p4_After)
## PLOT 3 Combined ## 
g_After <- grid.arrange(p1_After,p4_After, ncol= 1)

ggsave(filename = "toguchi_EC_NO3.JPEG",g_After)
