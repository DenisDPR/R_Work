library(dplyr)
library(tidyr)
library(plotly)
#install.packages("tidyverse")
library(tidyverse)
library(gridExtra)
library(ggplot2)

##################################
###########   DATE SET   #########
##################################
######################################
CATION <-read.csv('TOGUCHI_CATION.csv', header = T, check.names = T)
NO3 <-read.csv('TOGUCHI_NO3.csv', header = T, check.names = T)
ECS <-read.csv('TOGUCHI_EC.csv', header = T, check.names = T)
PH <-read.csv('TOGUCHI_PH.csv', header = T, check.names = T)

head(CATION,2)
head(PH,2)
head(ECS,2)
tail(NO3,2)

#df$date <- as.Date.factor(df$Date, format = "%Y-%m-%d")
################################################################################
##  BLOCK   vid      PLANT START       HARVEST START     END HARVEST           ##
##  C        v1       2017-07-18        2017-09-18         ?2017-11-27 (until) ##
##  D        v1       2017-05-26        2017-07-25          2017-10-16          ##
##  F        v1       2017-08-16        2017-10-23         ?2017-11-27          ##
##  F        v2      2017-08-16         ?                                      ##
################################################################################
#

## START AND END DATES of start of plant to start of harvesting
End_C_After <- as.Date("2017-11-27")
start_C_After <- as.Date("2017-09-18")

End_D_After <- as.Date("2017-10-10")
start_D_After <- as.Date("2017-07-25")

End_F_After <- as.Date("2017-11-27")
start_F_After <- as.Date("2017-10-23")

#PresentDate <- Sys.Date()
##############################################
#####  Function to Filter Date ###############
##############################################
filterDate <- function(df,x,y){
  df$Date <- as.Date.factor(df$Date, format = "%Y-%m-%d")
  df[df$Date >= x & df$Date <= y,]
}

## DATASET TO BE FILTERED

## C V1 ####3
C_CATION_DF_After <- filterDate(subset(CATION, Block == 'C'),start_C_After,End_C_After)
C_EC_DF_After <- filterDate(subset(ECS, Block == 'C'),start_C_After,End_C_After)
C_PH_DF_After <- filterDate(subset(PH, Block == 'C'),start_C_After,End_C_After)
C_NO3_DF_After <- filterDate(subset(NO3, Block == 'C'),start_C_After,End_C_After)
C_CATION_DF_After
##### D V1 #####
D_CATION_DF_After <- filterDate(subset(CATION, Block == 'D'),start_D_After,End_D_After)
D_EC_DF_After <- filterDate(subset(ECS, Block == 'D'),start_D_After,End_D_After)
D_PH_DF_After <- filterDate(subset(PH, Block == 'D'),start_D_After,End_D_After)
D_NO3_DF_After <- filterDate(subset(NO3, Block == 'D'),start_D_After,End_D_After)
##### F V1 ######
F_CATION_DF_After <- filterDate(subset(CATION, Block == 'F'),start_F_After,End_F_After)
F_EC_DF_After <- filterDate(subset(ECS, Block == 'F'),start_F_After,End_F_After)
F_PH_DF_After <- filterDate(subset(PH, Block == 'F'),start_F_After,End_F_After)
F_NO3_DF_After <- filterDate(subset(NO3, Block == 'F'),start_F_After,End_F_After)

# Bind the three Blocks C,D,F
CATION_CDF_After <- dplyr::bind_rows(C_CATION_DF_After,D_CATION_DF_After,F_CATION_DF_After)
EC_CDF_After <-dplyr::bind_rows(C_EC_DF_After,D_EC_DF_After,F_EC_DF_After)
PH_CDF_After <-dplyr::bind_rows(C_PH_DF_After,D_PH_DF_After,F_PH_DF_After)
NO3_CDF_After <-dplyr::bind_rows(C_NO3_DF_After,D_NO3_DF_After,F_NO3_DF_After)

CATION_CDF_After

#####Daily Block Mean #####
aggregate(CATION_CDF_After$Cation, list(CATION_CDF_After$Block), mean)
aggregate(EC_CDF_After$EC, list(EC_CDF_After$Block), mean)
aggregate(PH_CDF_After[,3:4], list(PH_CDF_After$Block), mean)
aggregate(NO3_CDF_After[,3:4], list(NO3_CDF_After$Block), mean)

## COMBINED DAILY AFTER ######################

daily_block_df <- function(dat1,dat2,dat3,dat4){
  # Combine daily chem
  df11 <- merge(dat1, dat2, by = c( "Date","Block"))
  df22 <- merge(dat3, dat4, by = c( "Date","Block"))
  df33 <-merge(df11, df22, by = c( "Date","Block"))
  colnames(df33) <- c("date","Block","EC_Before",
                      "EC_After", "EC","EC_Stress", 
                      "PH_Before", "PH_After","NO3",
                      "NO3_Supply","Cation")
  
  return(df33)
}

Combine_daily_Block_C_After <- daily_block_df(C_EC_DF_After,C_PH_DF_After,C_NO3_DF_After,C_CATION_DF_After)
Combine_daily_Block_D_After <- daily_block_df(D_EC_DF_After,D_PH_DF_After,D_NO3_DF_After,D_CATION_DF_After)
Combine_daily_Block_F_After <- daily_block_df(F_EC_DF_After,F_PH_DF_After,F_NO3_DF_After,F_CATION_DF_After)

######   Weekly Cation Function ############
############################################
#install.packages("tidyquant")
library(tidyquant)

weekly_cation_res <- function(data,q){
  dfCation <- data %>%
    tq_transmute(select     = Cation,
                 mutate_fun = apply.weekly,
                 FUN        = mean)
  dfCation$Block <- q
  return(dfCation)
}

weekly_c_cation_df_After <- weekly_cation_res(C_CATION_DF_After,'C')
weekly_d_cation_df_After <- weekly_cation_res(D_CATION_DF_After,'D')
weekly_f_cation_df_After <- weekly_cation_res(F_CATION_DF_After,'F')

######  Weekly EC Function ################
###############################################
weekly_EC_res <- function(data,q){
  dfEC <- data %>%
    tq_transmute(select = c(EC_Before = Before,EC_After = After,EC,EC_Stress = Stress ),
                 mutate_fun = apply.weekly,
                 FUN        = mean)
  dfEC$Block <- q
  return(dfEC)
}
weekly_c_EC_df_After <- weekly_EC_res(C_EC_DF_After,'C')
weekly_d_EC_df_After <-weekly_EC_res(D_EC_DF_After,'D')
weekly_f_EC_df_After <-weekly_EC_res(F_EC_DF_After,'F')
weekly_c_EC_df_After
#####   Weekly PH Function   ######
###############################################
weekly_PH_res <- function(data,q){
  dfPH <- data %>%
    tq_transmute(select = c(PH_Before = Before,PH_After = After),
                 mutate_fun = apply.weekly,
                 FUN        = mean)
  dfPH$Block <- q
  return(dfPH)
}
weekly_c_PH_df_After <- weekly_PH_res(C_PH_DF_After,'C')
weekly_d_PH_df_After <-weekly_PH_res(D_PH_DF_After,'D')
weekly_f_PH_df_After <-weekly_PH_res(F_PH_DF_After,'F')
weekly_c_PH_df_After

######   Weekly NO3 Function
###############################################
weekly_NO3_res <- function(data,q){
  dfNO3 <- data %>%
    tq_transmute(select = c(NO3 = Measure,NO3_Supply = Supply),
                 mutate_fun = apply.weekly,
                 FUN        = mean)
  dfNO3$Block <- q
  return(dfNO3)
}
weekly_c_NO3_df_After <- weekly_NO3_res(C_NO3_DF_After,'C')
weekly_d_NO3_df_After <-weekly_NO3_res(D_NO3_DF_After,'D')
weekly_f_NO3_df_After <-weekly_NO3_res(F_NO3_DF_After,'F')
weekly_f_NO3_df_After
#### WEEKLY BLOCK  #######
WEEKLY_CATION_CDF_After <- dplyr::bind_rows(weekly_d_cation_df_After,weekly_d_cation_df_After,weekly_f_cation_df_After)
WEEKLY_EC_CDF_After <- dplyr::bind_rows(weekly_c_EC_df_After,weekly_d_EC_df_After,weekly_f_EC_df_After)
WEEKLY_PH_CDF_After <- dplyr::bind_rows(weekly_c_PH_df_After,weekly_d_PH_df_After,weekly_f_PH_df_After)
WEEKLY_NO3_CDF_After <- dplyr::bind_rows(weekly_c_NO3_df_After,weekly_d_NO3_df_After,weekly_f_NO3_df_After)
WEEKLY_NO3_CDF_After

#########   BLOCK DATASET ###########################
######################################################
library(plyr)
##### Block function to combine all dataset
block_df <- function(dat1,dat2,dat3,dat4){
  df11 <- merge(dat1, dat2, by = c( "Date","Block"))
  df22 <- merge(dat3, dat4, by = c( "Date","Block"))
  df33 <-merge(df11, df22, by = c( "Date","Block"))
  return(df33)
}

block_C_After <- block_df(weekly_c_NO3_df_After,weekly_c_PH_df_After,weekly_c_EC_df_After,weekly_c_cation_df_After)
block_D_After <- block_df(weekly_d_NO3_df_After,weekly_d_PH_df_After,weekly_d_EC_df_After,weekly_d_cation_df_After)
block_F_After <- block_df(weekly_f_NO3_df_After,weekly_f_PH_df_After,weekly_f_EC_df_After,weekly_f_cation_df_After)
block_C_After
block_D_After
str(block_C_After)



##########################################################################
################    MOVING AVERAGES  #####################################
##########################################################################

roll_average <- function(df){
  df2 = df %>%
  #group_by(site, year) %>%
  #arrange(site, year, day) %>%
    mutate(Measure_cum_5 = rollsum(x = Measure, 5, align = "right", fill = NA),
           Measure_moving_3_aver_prev = rollapply(data = Measure, 
                                          width = 3, 
                                          FUN = mean, 
                                          align = "right", 
                                          fill = NA, 
                                          na.rm = T),
           Supply_cum_5 = rollsum(x = Supply, 5, align = "right", fill = NA))
  return(df2)
}
roll_average(C_NO3_DF_After)

## How are the variable correlated
library(corrplot) # For the corr plot
#install.packages("corrplot")
library(corrplot) 
block_C_corr_After = cor(block_C_After[3:11])
block_c_cor_matrx_After <-corrplot(block_C_corr_After, method = "number") #

block_D_corr_After = cor(block_D_After[3:11])
block_d_cor_matrx_After <-corrplot(block_D_corr_After, method = "number") #

block_F_corr_After = cor(block_F_After[3:11])
block_f_cor_matrx_After <-corrplot(block_F_corr_After, method = "number") #

##### block grouped by Chem Element ########
head(CATION_CDF,2)
head(NO3_CDF,2)
head(PH_CDF,2)
head(EC_CDF,2)
##################################

p1_After <- ggplot(aes(y=EC, x=Date),data = EC_CDF_After) +
  geom_line(aes(color=Block)) + labs(x=NULL, y='EC') +
  theme(
    panel.grid.minor.x = element_blank())

p2_After <- ggplot(aes(y=Cation, x=Date),data = CATION_CDF_After) +
  geom_line(aes(color=Block)) + labs(x=NULL, y='Cation') +
  theme(
    panel.grid.minor.x = element_blank())

p3_After <- ggplot(aes(y=After, x=Date),data = PH_CDF_After) +
  geom_line(aes(color=Block)) + labs(x= NULL, y='PH') +
  theme(
    panel.grid.minor.x = element_blank())
p4_After <- ggplot(aes(y=Measure, x=Date),data = NO3_CDF_After) +
  geom_line(aes(color=Block))+ labs(x='Date', y='NO3')

g_After <- grid.arrange(p1_After,p2_After,p3_After,p4_After, ncol= 1)

ggsave(filename = "toguchi_chem_After.JPEG",g_After)
#ggplotly(p)

###############################################
######## Mean of Block C, D, F###################
###############################################
head(block_C,2)
head(block_D,2)
head(block_F,2)

mean(block_C_After$NO3)
mean(block_D_After$NO3)
mean(block_F_After$NO3)
########################################
##### ANOVA DAILY ################################

anova(aov(CATION_CDF_After$Cation ~ CATION_CDF_After$Block))
anova(aov(EC_CDF_After$EC ~ EC_CDF_After$Block))
anova(aov(PH_CDF_After$After ~ PH_CDF_After$Block))
anova(aov(NO3_CDF_After$Measure ~ NO3_CDF_After$Block))

##### ANOVA WEEKLY ################################

anova(aov(WEEKLY_CATION_CDF_After$Cation ~ WEEKLY_CATION_CDF_After$Block))
anova(aov(WEEKLY_EC_CDF_After$EC ~ WEEKLY_EC_CDF_After$Block))
anova(aov(WEEKLY_PH_CDF_After$PH_Before ~ WEEKLY_PH_CDF_After$Block))
anova(aov(WEEKLY_PH_CDF_After$PH_After ~ WEEKLY_PH_CDF_After$Block))
anova(aov(WEEKLY_NO3_CDF_After$NO3 ~ WEEKLY_NO3_CDF_After$Block))
head(WEEKLY_EC_CDF_After)
## Significant in all the blocks chemicals of PH, NO3, EC, Cation

head(CATION_CDF,2)
head(NO3_CDF,2)
head(PH_CDF,2)
head(EC_CDF,2)

##################################################################################################
##########################       HARVEST          ################################################
##################################################################################################

block_df_merged<- function(dat1,dat2){
  df11 <- merge(dat1, dat2, by = c( "Block"))
  return(df11)
}
block_df_merged(week_harv_cum_harv,block_C_After)

res1 = lm(week_harv_cum_harv$Total ~ block_C_After$NO3)

