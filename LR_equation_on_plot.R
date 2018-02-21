library(zoo)
library(ggplot2)
library(gtable)
library(grid)
library(extrafont)
library(dplyr)
library(tidyr)
library("reshape2")
library(lubridate) 


dat_1 <-read.csv('mikami_LRA.csv', header = T, check.names = T)
dat_1 <-read.csv('togu_MR_c.csv', header = T, check.names = T)
dat_1 <-read.csv('togu_MR_f.csv', header = T, check.names = T)
head(dat_1)

lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}


dat_2 = dat_1 %>%
  gather(key,value, y, y_predictions)
p3 <- ggplot(aes(y=value, x=x),data = dat_2) +
  geom_line(aes(color=key)) + 
  labs(x = "Cumulative GDD (Degree Days)", y = "Cumulative Harvest (KG)")+
  scale_color_manual(labels = c("Actual harvest", "Estimated harvest"), values = c("blue", "red")) +
  theme(legend.position="top",
        legend.text=element_text(size=rel(1.5)),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title = element_text(size = 20),
        #panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        axis.ticks        = element_line(),
        panel.grid.minor.y = element_line(colour = "gray50"),
        panel.grid.minor.x = element_line(colour = "gray50"),
        panel.grid.major = element_line(color = "gray50", size = 0.09)) #+
 # theme_bw()
p4 <- p3 + geom_text(x = 1800, y = 20000, label = lm_eqn(dat_1), parse = TRUE,size=7)
p4

################ Using Cumulative GDD and Cumulative Harvest ################################  
###############  for Block C (Top) and F (Bottom) in harvest determination.################ 

dat_1 <-read.csv('togu_MR_c.csv', header = T, check.names = T)
#dat_1 <-read.csv('togu_MR_f.csv', header = T, check.names = T)

head(dat_1)
lm_eqn_2 <- function(df){
  m <- lm(Cum_Harvest ~ Cum_GDD + Cum_CO2, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}


dat_1$date <- as.Date.factor(dat_1$date, format = "%Y-%m-%d")
dat_2 = dat_1 %>%
  gather(key,value, Cum_Harvest, y_predictions)
p3 <- ggplot(aes(y=value, x=date),data = dat_2) +
  geom_line(aes(color=key)) + 
  labs(y = "Cumulative Harvest (KG)", x = "DATE (2017)") +
  scale_color_manual(labels = c("Actual harvest(kG)", "Estimated harvest (KG)"), values = c("blue", "red")) +
  theme(legend.position="top",
        legend.text=element_text(size=rel(1.7)),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title = element_text(size = 20),
        #panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        axis.ticks        = element_line(),
        panel.grid.minor.y = element_line(colour = "gray50"),
        panel.grid.minor.x = element_line(colour = "gray50"),
        panel.grid.major = element_line(color = "gray50", size = 0.09))
texdat =  ymd("2017-10-25")
p44 <- p3 + geom_text(x = texdat , y = 400, label = lm_eqn_2(dat_1), parse = TRUE,size=7)
p44
