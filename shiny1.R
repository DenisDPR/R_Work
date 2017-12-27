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
columnHeaders <- c("column_to_skip","date","block","vid","A","B","LOSS")
columnClasses <- c("NULL", "factor", "factor", "factor","numeric", "numeric","numeric")
df <- read.csv("togu_harv.csv", header = TRUE, sep = ",", col.names = columnHeaders, colClasses = columnClasses)
df$date <- as.Date.factor(df$date, format = "%Y-%m-%d")
# Tidy data
df_gather <- gather(df, "GRADE", "HARVEST", 4:6)
#Function to Filter block and vid
df_block_vid <- function(df_sub, blck, vd)
{
  block_vid <- subset(df_sub, block == blck & vid == vd,
                      select = c("date","GRADE","HARVEST"))
  return(block_vid)
}
#Function to draw bar graph input as dataframe and weeks
bar_gra <- function(df_block,q){
  p = ggplot(aes(x = date, 
                 y = HARVEST, 
                 fill=GRADE), 
             data =  df_block) +
    geom_histogram(binwidth = 0.1, stat = "identity", position = "stack") +
    scale_x_date(limits = c(Sys.Date() -(7*q),NA),
                 date_minor_breaks = "1 day",
                 date_labels = "%b %d") + 
    labs(x = NULL, y = NULL) + 
    theme(legend.position="top",
          panel.background = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major = element_line(color = "#12954E", size = 0.1),
          panel.grid.minor = element_line(color = "#12954E", size = 0.005),
          
          axis.title.x = element_text(colour = "#12954E",size = 10),
          axis.title.y = element_text(colour = "#12954E",size = 10),
          axis.text.x = element_text(color = "#12954E", size = 14),
          axis.text.y = element_text(color = "#12954E", size = 10)
    )
  return(p)
}
### APP######
ui <- 
  fluidPage(
    theme = shinytheme("readable"),
    
    # Name Page
    titlePanel("収穫記録"),
    #Generate a row with a sidebar
    sidebarLayout(
      sidebarPanel(
        helpText("過去数週間の収穫記録"),
        
        sliderInput(inputId = "num",
                    label = h2("週間前"),
                    value = 15, min = 1, max = 16),
        selectInput("Input1", h2("ブロック名"), choices = c("BLOCK A"="A",
                                                       "BLOCK B"="B",
                                                       "BLOCK C" = "C",
                                                       "BLOCK D"= "D",
                                                       "BLOCK E" = "E",
                                                       "BLOCK F"="F")),
        helpText("ハウス選択"),
        selectInput("Input2", "種類", choices = c("AIKO"="v2",
                                                "NORMAL"="v1",
                                                "CHABERU"="v3"))
      ),
      mainPanel(
        plotlyOutput("hist",width = "100%",height = "140%")
      )
    )
  )
server <- function(input, output)
{
  output$hist <- renderPlotly({
    df_block_vid = df_block_vid(df_gather, (input$Input1), (input$Input2))
    bar_gra(df_block_vid,(input$num))
  })
}

shinyApp(ui = ui, server = server)
