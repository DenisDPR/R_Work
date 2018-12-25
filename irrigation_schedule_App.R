library(shiny)
library(shinythemes)
library(RMySQL)
#require(RMySQL)
require(DBI)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)

#write.csv(df, file = "togu_harv.csv")
#columnHeaders <- c("column_to_skip","date","block","plot","line","sample_1","sample_2","sample_3")
#columnClasses <- c("NULL", "factor", "factor", "factor","factor","numeric","numeric","numeric")

irr_b <- read.csv('irri_before.csv', header = T, sep = ",")
irr_b$Mean_moisture <- round((irr_b$sample_1 + irr_b$sample_2 + irr_b$sample_3)/3,1)
irr_bef = irr_b %>% group_by(date,block,plot) %>% 
  summarise(moisture =mean(Mean_moisture,na.rm=T))
irr_bef$date <- lubridate::date(irr_bef$date)
irr_bef<- as.data.frame(irr_bef)


#Function to Filter block and vid
df_block_vid <- function(df_sub, blck, plt)
{
  block_vid <- subset(df_sub, block == blck & plot == plt,select = c("date","block","plot","moisture"))
  return(block_vid)
}

#Function to draw bar graph input as dataframe and weeks
bar_gra <- function(df_block){
  df_block2 = df_block %>% drop_na()
  p = ggplot(aes(x = date, y = moisture), data =  df_block2) +
    geom_line(aes(y=moisture))+
    geom_point(color = "blue")+
    scale_x_date(#limits = c(Sys.Date() -(7*q),NA),
      date_breaks = "7 days",
      labels = function(x) format(x, "%m-%d")) +
    labs(x = "Date", y = "Moisture (mV) ") +
    coord_cartesian(ylim = c(0,700))+
    #labs(title = ~ atop(paste('Moisture Content Per Plot'),
   #                     paste(scriptstyle(italic("After Irrigation"))))) +
    theme_minimal()+
    theme_bw() +
    geom_hline(yintercept=300,color = "blue", size=0.3)+
    
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20),
          axis.text.x = element_text(color = "black",size=rel(1.2),angle=45,hjust=1),
          axis.text.y = element_text(color = "black",size=rel(1.5)),
          text = element_text(size=12),
          legend.box = "horizontal",
          legend.title=element_blank(),
          plot.title = element_text(hjust = 0.5))
         # axis.text.x=element_text(angle=45,hjust=1))
  return(p)
}
### APP######
ui <- 
  fluidPage(
    theme = shinytheme("readable"),
    
    # Name Page
    titlePanel("IRRIGATION "),
    #Generate a row with a sidebar
    sidebarLayout(
      sidebarPanel(
        helpText("Depletion Factor"),
        helpText("Select Block"),
        
        #sliderInput(inputId = "num",
        #            label = h2("Weekly"),
         #           value = 10, min = 1, max = 16),
        selectInput("Input1", h2("BLOCK "), choices = c("BLOCK 1"="BLOCK_1",
                                                       "BLOCK 2"="BLOCK_2",
                                                       "BLOCK 3" = "BLOCK_3"
                                                       )),
        helpText("Select Plot"),
        selectInput("Input2", h2("PLOT"), choices = c("DRIP 1"="DRIP_1",
                                                  "DRIP 2"="DRIP_2",
                                                  "BUND 1"="BUND_1",
                                                  "BUND 2"="BUND_2",
                                                  "FURROW 1"="FURR_1",
                                                  "FURROW 2"="FURR_2")),
        helpText("Threshold for Depletion Factor is shown by the blue horizontal line. When moisture level is below, Irrigation is to be done")
        
      ),
      mainPanel(
        h2("Average Moisture Content",align="center"),
        div(plotlyOutput("plot",width = "100%",height = "100%"),align = "center")
      )
    )
  )
server <- function(input, output)
{
  output$plot <- renderPlotly({
    df_block_vid = df_block_vid(irr_bef, (input$Input1), (input$Input2))
    bar_gra(df_block_vid)
  })
}

shinyApp(ui = ui, server = server)
