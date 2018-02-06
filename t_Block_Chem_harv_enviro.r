library(RMySQL)
library(zoo)
require(zoo)
library(zoo)
library(ggplot2)
library(gtable)
library(grid)
library(extrafont)
library(dplyr)


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
theme_set(theme_gray(base_size=12, base_family="HiraKakuProN-W3"))
p1 <- ggplot(aes(x=date, y = Mean_T), data = df_gdd) + 
  geom_line(colour = "#12954E", size = 1) + 
  ggtitle("(a) Daily Growing Degree Days\n")  + labs(y = "GDD") +
  theme(#panel.grid.minor = element_blank(), 
    panel.grid.major = element_line(color = "#12954E", size = 0.1),
    # panel.grid.major.x = element_blank(),
    panel.background = element_blank(), 
    axis.text.y = element_text(color = "#12954E", size = 12),
    axis.title.y = element_text(colour = "#12954E",size = 12),
    axis.text.x = element_text(size = 14),
    plot.title = element_text(hjust = 0.6,colour = "#12954E",size = 14),
    axis.title.x=element_blank(),
    # axis.text.x=element_blank(),
    axis.ticks.x=element_blank())+
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()



p2 <- ggplot(aes(x=date, y = GDD), data = df_gdd) + 
  geom_line(colour = "#00a4e6", size = 1) + 
  ggtitle(" (b) 累積温度 Degree Days\n")  + labs(y = "Cumulative GDD") +
  theme(#panel.grid.minor = element_blank(), 
    panel.grid.major = element_line(color = "#00a4e6", size = 0.1),
    #panel.grid.major.x = element_blank(),
    panel.background = element_blank(), 
    axis.text.y = element_text(color = "#00a4e6", size = 12),
    axis.title.y = element_text(colour = "#00a4e6",size = 12),
    axis.text.x = element_text(size = 14),
    plot.title = element_text(hjust = 0.6,colour = "#00a4e6",size = 14),
    axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    axis.ticks.x=element_blank())+
  scale_y_continuous(expand = c(0, 0)) +
  theme_set(theme_gray(base_size=12, base_family="HiraKakuProN-W3"))#+
  #theme_bw()



mikami_plot_cum_gdd <- grid.arrange(p1,p2, ncol= 1)

ggsave(filename = "mikami_plot_cum_gdd.JPEG",mikami_plot_cum_gdd)

################# PLOTS FOR CHEM #########################
EC_CDF <-read.csv('EC_CDF.csv', header = T, check.names = T)
CATION_CDF <-read.csv('CATION_CDF.csv', header = T, check.names = T)
PH_CDF <-read.csv('PH_CDF.csv', header = T, check.names = T)
NO3_CDF <-read.csv('NO3_CDF.csv', header = T, check.names = T)

EC_CDF_After <-read.csv('EC_CDF_After.csv', header = T, check.names = T)
CATION_CDF_After <-read.csv('CATION_CDF_After.csv', header = T, check.names = T)
PH_CDF_After <-read.csv('PH_CDF_After.csv', header = T, check.names = T)
NO3_CDF_After <-read.csv('NO3_CDF_After.csv', header = T, check.names = T)


p1 <- ggplot(aes(y=EC, x=Date),data = EC_CDF) +
  geom_line(aes(color=Block)) + labs(x="Date", y="EC (" * mu ~ "/cm)") +
  scale_x_date(labels = date_format("%b %Y"))+
  theme(panel.grid.minor.x = element_blank()) +
  theme(legend.position="none",
        axis.text.x=element_text(angle=45,hjust=1)) +   
  theme_bw()
p2 <- ggplot(aes(y=Cation, x=Date),data = CATION_CDF) +
  geom_line(aes(color=Block)) + labs(x=NULL, y='Calcium Solution') +
  scale_x_date(labels = date_format("%b %Y"))+
  theme(panel.grid.minor.x = element_blank()) +
  theme(legend.position="none") +   theme_bw()

p3 <- ggplot(aes(y=After, x=Date),data = PH_CDF) +
  geom_line(aes(color=Block, show.legend=FALSE)) + labs(x= NULL, y='pH') +
  scale_x_date(labels = date_format("%b %Y"))+
  theme(panel.grid.minor.x = element_blank()) +
  theme(legend.position="none") + theme_bw()

p4 <- ggplot(aes(y=Measure, x=Date),data = NO3_CDF) +
  geom_line(aes(color=Block))+ labs(x='Year (2017)', y='NO3 Solution') +
  scale_x_date(labels = date_format("%b %Y"))+
  theme(legend.position="bottom") +
  theme_bw()

g <- grid.arrange(p1,p3,p2,p4, ncol= 1,
                  top = "DAILY SOLUTION BEFORE START OF HARVEST")                                               
ggsave(filename = "toguchi_chem_before.PNG",g) 
##########PLOT OF COMBINED CHEM IN GROUP AFTER #############
p1_After <- ggplot(aes(y=EC, x=Date),data = EC_CDF_After) +
  geom_line(aes(color=Block)) + labs(x=NULL, y="EC (" * mu ~ "/cm)") +
  scale_x_date(labels = date_format("%b %Y"))+
  theme(panel.grid.minor.x = element_blank()) +
  theme(legend.position="none")+
  theme_bw()

p2_After <- ggplot(aes(y=Cation, x=Date),data = CATION_CDF_After) +
  geom_line(aes(color=Block)) + labs(x=NULL, y='Calcium Solution') +
  scale_x_date(labels = date_format("%b %Y"))+
  theme(panel.grid.minor.x = element_blank()) +
  theme(legend.position="none")+
  theme_bw()

p3_After <- ggplot(aes(y=After, x=Date),data = PH_CDF_After) +
  geom_line(aes(color=Block)) + labs(x= NULL, y='pH') +
  scale_x_date(labels = date_format("%b %Y"))+
  theme(panel.grid.minor.x = element_blank()) +
  theme(legend.position="none")+
  theme_bw()

p4_After <- ggplot(aes(y=Measure, x=Date),data = NO3_CDF_After) +
  geom_line(aes(color=Block))+ labs(x='Year (2017)', y='NO3 Solution') + 
  scale_x_date(labels = date_format("%b %Y"))+
  theme(legend.position="bottom")+
  theme_bw()

g_After <- grid.arrange(p1_After,p3_After,p2_After,p4_After, ncol= 1,
                        top = "DAILY SOLUTION AFTER START OF HARVEST ")               # 
ggsave(filename = "toguchi_chem_after.PNG",g_After)

########## ALL CHEM DAILY B4 and After In Each Block ##################################
NO3_CDF_COMBINED <-dplyr::bind_rows(NO3_CDF,NO3_CDF_After)
CATION_CDF_COMBINED <-dplyr::bind_rows(CATION_CDF,CATION_CDF_After)
EC_CDF_COMBINED <-dplyr::bind_rows(EC_CDF,EC_CDF_After)
PH_CDF_COMBINED <-dplyr::bind_rows(PH_CDF,EC_CDF_After)
tail(NO3_CDF_COMBINED)
########## PLOT OF COMBINED CHEM IN GROUP BEFORE #############
p1_all <- ggplot(aes(y=EC, x=Date),data = EC_CDF_COMBINED) +
  geom_line(aes(color=Block)) + labs(x=NULL, y='EC') +
  theme(legend.position="none")+
  theme_bw()
p2_all <- ggplot(aes(y=Cation, x=Date),data = CATION_CDF_COMBINED) +
  geom_line(aes(color=Block)) + labs(x=NULL, y='Calcium Solution') +
  theme(legend.position="none")+
  theme_bw()
p3_all <- ggplot(aes(y=After, x=Date),data = PH_CDF_COMBINED) +
  geom_line(aes(color=Block)) + labs(x= NULL, y='pH') + 
  theme(legend.position="none")+
  theme_bw()
p4_all <- ggplot(aes(y=Measure, x=Date),data = NO3_CDF_COMBINED) +
  geom_line(aes(color=Block))+ labs(x='Date', y='NO3 Solution') +
  theme(legend.position="bottom")+
  theme_bw()
before_after <- grid.arrange(p1_all,p2_all,p3_all,p4_all, ncol= 1,
                             top = "Application of Required Chemical Solutions for the whole 
                             Growth Period Per Block") 
ggsave(filename = "toguchi_chem_before_after.JPEG",before_after)

####################################################################################################
###################   MOVING AVERAGEs FOR REGRESSION ###############################################

data_c <-read.csv('data_for_MRA_c.csv', header = T, check.names = T)
data_d <-read.csv('data_for_MRA_d.csv', header = T, check.names = T)
data_f <-read.csv('data_for_MRA_f.csv', header = T, check.names = T)
str(data_c)

moving_average_daily <- function(df,k){
  df2 = df %>%
    mutate(
      Cumm_CO2 = rollapply(data = mean_CO2, width = k, FUN = sum, align = "right", fill = NA, na.rm = T), #Cumulative
      Cumm_GDD = rollapply(data = GDD, width = k, FUN = sum, align = "right", fill = NA, na.rm = T),#Cumulative
      NO3_moving = rollapply(data = NO3, width = k, FUN = mean, align = "right", fill = NA, na.rm = T))#Moving average
      
  df3 <- subset(df2, select = c(date,
                                NO3_moving,
                                Cumm_GDD,
                                Cumm_CO2,
                                Total))
  df4 <- subset(df3, !is.na(Total) & (Total !=0.0)) 
  return(df4)
}

############       NO3 NITRATE          ################
block_C_mra_7_no3 <- moving_average_daily(data_c,7)    
block_D_mra_7_no3 <- moving_average_daily(data_d,7)
block_F_mra_7_no3 <- moving_average_daily(data_f,7)

##########################   M R A   ###############################################
####################################################################################

t_1 <- lm(Total ~  0 + NO3_moving,data = block_C_mra_7_no3)
t_2 <- lm(Total ~  0 + NO3_moving,data = block_D_mra_7_no3)
t_3 <- lm(Total ~  0 + NO3_moving,data = block_F_mra_7_no3)

t1_1 <- lm(Total ~  0 + NO3_moving + Cumm_CO2,data = block_C_mra_7_no3)
t1_2 <- lm(Total ~  0 + NO3_moving + Cumm_CO2,data = block_D_mra_7_no3)
t1_3 <- lm(Total ~  0 + NO3_moving + Cumm_CO2,data = block_F_mra_7_no3)


###############################################################################
mtable(t_1,t_2,t_3,t1_1,t1_2,t1_3,sdigits = 3) # three digit formated

# Combining,renaming and selecting some values 
mtable_no3 <- mtable("Model 1"=t_1,
                    "Model 1"=t_2, 
                    "Model 1"=t_3,
                    "Model 2"=t1_1,
                    "Model 2 "=t1_2, 
                    "Model 2"=t1_3,
                    summary.stats=c("R-squared","adj. R-squared","p","F","AIC","BIC","N"))

(mtable_no3 <- relabel(mtable_no3,
                      "(Intercept)" = "Constant",
                      Cumm_GDD = "Cumulative GDD",
                      NO3_moving = "7-Day Moving Average Nitrate"))

mtable_no3 <- relabel(mtable_no3,
                 "\\emph{Intercept}"="<em>Intercept</em>",
                 fixed=TRUE)
show_html(mtable_no3)


###############################################################################
#######################          EC                         ################

moving_average_daily_ec <- function(df,k){
  df2 = df %>%
    mutate(EC_moving = rollapply(data = EC, width = k, FUN = mean, align = "right", fill = NA, na.rm = T))
  df3 <- subset(df2, select = c(date,
                                EC_moving,
                                sweetness))
  
  df4 <- subset(df3, !is.na(sweetness) & (sweetness !=0.0)) 
  return(df4)
}
block_C_mra <- moving_average_daily_ec(data_c,4)    
block_D_mra <- moving_average_daily_ec(data_d,4)
block_F_mra <- moving_average_daily_ec(data_f,4)
str(block_C_mra)
block_C_mra_7 <- moving_average_daily_ec(data_c,7)    
block_D_mra_7 <- moving_average_daily_ec(data_d,7)
block_F_mra_7 <- moving_average_daily_ec(data_f,7)

##########################   M R A   ###############################################
####################################################################################

t1 <- lm(sweetness ~  0 + EC_moving,data = block_C_mra)
t2 <- lm(sweetness ~  0 + EC_moving,data = block_D_mra)
t3 <- lm(sweetness ~  0 + EC_moving,data = block_F_mra)

t31 <- lm(sweetness ~  0 + EC_moving,data = block_C_mra_7)
t32 <- lm(sweetness ~  0 + EC_moving,data = block_D_mra_7)
t33 <- lm(sweetness ~  0 + EC_moving,data = block_F_mra_7)


###############################################################################
###############   NO3 NITRATE   ###############################################
mtable(t1,t2,t3,t31,t32,t33,sdigits = 3) # three digit formated

# Combining,renaming and selecting some values 
mtable_ec <- mtable("Model 1_4-day"=t1,
                    "Model 1_4-day"=t2, 
                    "Model 1_4-day"=t3,
                    "Model 2_7-day"=t31,
                    "Model 2_7-day "=t32, 
                    "Model 2_7-day"=t33,
                    summary.stats=c("R-squared","adj. R-squared","p","F","AIC","BIC","N"))

(mtable_ec <- relabel(mtable_ec,
                      "(Intercept)" = "Constant",
                      EC_moving = "Moving Average EC"))

mt_EC <- relabel(mtable_ec,
                 "\\emph{Intercept}"="<em>Intercept</em>",
                 fixed=TRUE)
show_html(mt_EC)

###############################################################################


########## With Predicted results ########################
write.csv(block_D_mra_2, file = "block_C_mra_2_predict.csv") 
write.csv(block_C_mra_2, file = "block_D_mra_2_predict.csv") 
write.csv(block_F_mra_2, file = "block_F_mra_2_predict.csv") 




#### SHOW THE RESIDUALS
block_C_mra_2 <- na.omit(block_C_mra)
block_C_mra_2$predicted <- round(predict(t1), digits = 1)

block_D_mra_2 <- na.omit(block_D_mra)
block_D_mra_2$predicted <- round(predict(t2), digits = 1)

block_F_mra_2 <- na.omit(block_F_mra)
block_F_mra_2$predicted <- round(predict(t3), digits = 1)




####################################################################################
####################################################################################

