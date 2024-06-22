


# Vici Report

library(shiny)
library(shinydashboard)
library(readxl)
library(tidyr)
library(dplyr)
library(data.table)
library(googlesheets4)
library(lubridate)
library(ggplot2)
library(plotly)
library(maps)
library(tibble)
library(readr)


func_tidy <- function(dt){
  names(dt) <- tolower(names(dt))
  names(dt) <- gsub(" ","_", names(dt))
  dt <- as.data.table(dt)
  return(dt)
  
}

dt = fread("~/InfoP/VICI/data individual/AGENT_TIME_Jun 18, 2024.csv")

dt = func_tidy(dt)

dt = dt[-c(1:3),]

names(dt) = as.character(dt[1,])

dt = dt[-1,]

dt = func_tidy(dt)

dt = dt[!user %like% 'Test']

dt = dt[!user %like% 'Agent']

# average wait time
ave_wait = dt[user !="TOTALS",.(wait)]

ave_wait$wait = hms(ave_wait$wait)
ave_wait$wait = period_to_seconds(ave_wait$wait)

ave_wait = mean(ave_wait$wait)

ave_wait = formatC(ave_wait/3600, digits = 2) %>% as.numeric()

#---------------------
# average talk time
ave_talk = dt[user !="TOTALS",.(talk)]

ave_talk$talk = hms(ave_talk$talk)
ave_talk$talk = period_to_seconds(ave_talk$talk)

ave_talk = mean(ave_talk$talk)

ave_talk = formatC(ave_talk/3600, digits = 2) %>% as.numeric()

#-------------------
# average dispo
ave_dispo = dt[user !="TOTALS",.(dispo)]

ave_dispo$dispo = hms(ave_dispo$dispo)
ave_dispo$dispo = period_to_seconds(ave_dispo$dispo)

ave_dispo = mean(ave_dispo$dispo)

ave_dispo = formatC(ave_dispo/3600, digits = 2) %>% as.numeric()

#----------------------
# average pause
ave_pause = dt[user !="TOTALS",.(pause)]

ave_pause$pause = hms(ave_pause$pause)
ave_pause$pause = period_to_seconds(ave_pause$pause)

ave_pause = mean(ave_pause$pause)

ave_pause = formatC(ave_pause/3600, digits = 2) %>% as.numeric()

#----------------------
# average dead time

ave_dead = dt[user !="TOTALS",.(dead)]

ave_dead$dead = hms(ave_dead$dead)
ave_dead$dead = period_to_seconds(ave_dead$dead)

ave_dead = mean(ave_dead$dead)

ave_dead = formatC(ave_dead/3600, digits = 2) %>% as.numeric()

#-----------------------
# average connected
ave_connected = dt[user !="TOTALS",.(connected)]

ave_connected$connected = hms(ave_connected$connected)
ave_connected$connected = period_to_seconds(ave_connected$connected)

ave_connected = mean(ave_connected$connected)

ave_connected = formatC(ave_connected/3600, digits = 2) %>% as.numeric()

#---------------
# average lagged
ave_lagged = dt[user !="TOTALS",.(lagged)]

ave_lagged$lagged = hms(ave_lagged$lagged)
ave_lagged$lagged = period_to_seconds(ave_lagged$lagged)

ave_lagged = mean(ave_lagged$lagged)

ave_lagged = formatC(ave_lagged/3600, digits = 2) %>% as.numeric()



# number of agents
num_agents = dt[user !="TOTALS"] %>% nrow() %>% as.numeric()

total_calls = dt[user =="TOTALS",calls] %>% as.numeric()

# wait percentage
wait_p = dt[user == "TOTALS", `wait_%`] %>% parse_number()

# talk time percentage
talk_time_p = dt[user == "TOTALS", `talk_time_%`] %>% parse_number()

# dispo time percentage
dispo_time_p = dt[user == "TOTALS", `dispotime_%`] %>% parse_number()


## dead time percentage
dead_time_p = dt[user == "TOTALS", `dead_time_%`] %>% parse_number()

# pause time percentage
pause_time_p = dt[user == "TOTALS",`pausetime_%`] %>% parse_number()

# Wait %
# p1 <- plot_ly(
#   domain = list(x = c(0, 1), y = c(0, 1)),
#   value = wait_p,
#   title = list(text = "Wait %"),
#   type = "indicator",
#   mode = "gauge+number") 
# 
# p1 <- p1 %>%
#   layout(margin = list(l=20,r=30))

#------------------

# Talk Time %
# p2 <- plot_ly(
#   domain = list(x = c(0, 1), y = c(0, 1)),
#   value = talk_time_p,
#   title = list(text = "Talk Time %"),
#   type = "indicator",
#   mode = "gauge+number") 
# 
# p2 <- p2 %>%
#   layout(margin = list(l=20,r=30))


# wait time
p1 <- plot_ly(
  domain = list(x = c(0, 1), y = c(0, 1)),
  value = wait_p,
  title = list(text = "Wait %"),
  type = "indicator",
  mode = "gauge+number+delta",
  delta = list(reference = 20),
  gauge = list(
    axis =list(range = list(NULL, 100)),
    steps = list(
      list(range = c(0, 20), color = "steelblue"),
      list(range = c(20, 50), color = "gray"),
      list(range = c(50,100), color = "orange")),
    threshold = list(
      line = list(color = "red", width = 4),
      thickness = 0.75,
      value = 20))) 
p1 <- p1 %>%
  layout(margin = list(l=20,r=30))


#------

# talk time
p2 <- plot_ly(
  domain = list(x = c(0, 1), y = c(0, 1)),
  value = talk_time_p,
  title = list(text = "Talk Time %"),
  type = "indicator",
  mode = "gauge+number+delta",
  delta = list(reference = 80),
  gauge = list(
    axis =list(range = list(NULL, 100)),
    steps = list(
      list(range = c(0, 20), color = "orange"),
      list(range = c(20, 50), color = "gray"),
      list(range = c(50,100), color = "steelblue")),
    threshold = list(
      line = list(color = "red", width = 4),
      thickness = 0.75,
      value = 80))) 
p2 <- p2 %>%
  layout(margin = list(l=20,r=30))

#------------

# Dispo Time %
p3 <- plot_ly(
  domain = list(x = c(0, 1), y = c(0, 1)),
  value = dispo_time_p,
  title = list(text = "Dispo Time %"),
  type = "indicator",
  mode = "gauge+number+delta",
  delta = list(reference = 80),
  gauge = list(
    axis =list(range = list(NULL, 100)),
    steps = list(
      list(range = c(0, 20), color = "orange"),
      list(range = c(20, 50), color = "gray"),
      list(range = c(50,100), color = "steelblue")),
    threshold = list(
      line = list(color = "red", width = 4),
      thickness = 0.75,
      value = 80))) 
p3 <- p3 %>%
  layout(margin = list(l=20,r=30))

# Dead Time %
p4 <- plot_ly(
  domain = list(x = c(0, 1), y = c(0, 1)),
  value = dead_time_p,
  title = list(text = "Dead Time %"),
  type = "indicator",
  mode = "gauge+number+delta",
  delta = list(reference = 20),
  gauge = list(
    axis =list(range = list(NULL, 100)),
    steps = list(
      list(range = c(0, 20), color = "steelblue"),
      list(range = c(20, 50), color = "gray"),
      list(range = c(50,100), color = "orange")),
    threshold = list(
      line = list(color = "red", width = 4),
      thickness = 0.75,
      value = 20))) 
p4 <- p4 %>%
  layout(margin = list(l=20,r=30))
 
# paused time percentage
p5 <- plot_ly(
  domain = list(x = c(0, 1), y = c(0, 1)),
  value = pause_time_p,
  title = list(text = "Pause Time %"),
  type = "indicator",
  mode = "gauge+number+delta",
  delta = list(reference = 20),
  gauge = list(
    axis =list(range = list(NULL, 100)),
    steps = list(
      list(range = c(0, 20), color = "steelblue"),
      list(range = c(20, 50), color = "gray"),
      list(range = c(50,100), color = "orange")),
    threshold = list(
      line = list(color = "red", width = 4),
      thickness = 0.75,
      value = 20))) 
p5 <- p5 %>%
  layout(margin = list(l=20,r=30))
 

# total calls
p6 <- plot_ly(
  type = "indicator",
  mode = "number+gauge+delta",
  gauge = list(shape = "bullet"),
  delta = list(reference = 5000),
  value = total_calls,
  domain = list(x = c(0, 1), y = c(0, 1)),
  title= list(text = "Total Calls"),
  height = 200)

# rank agents by call volume
rank_df = dt[user !="TOTALS",.(user,calls)]
rank_df$calls = as.numeric(rank_df$calls)
rank_df = rank_df[order(-calls)]

p7 = plot_ly(rank_df, y = ~reorder(user,calls), 
             x = ~calls, 
             type = 'bar', 
             orientation = "h",
             text = ~calls) %>% 
  layout(yaxis = list(title = ""))


ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = "VICI DASHBOARD"),
                    dashboardSidebar(),
                    dashboardBody(
                      fluidRow(
                        valueBox (value = num_agents, subtitle = "Number of Agents", width = 3, icon = icon("people-group"), color = "yellow" ),
                        valueBox (value = ave_wait, subtitle = "Ave Wait Time/hrs", color = "yellow", width = 3, icon = icon("hourglass-half")),
                        valueBox (value = ave_talk, subtitle = "Ave Talk Time/hrs", color = "yellow", width = 3, icon = icon("headset")),
                        valueBox (value = ave_dispo, subtitle = "Ave Dispo Time/hrs", color = "yellow", width = 3,icon = icon("desktop"))
                        
                      ),
                      
                      
                      fluidRow(
                        valueBox (value = ave_pause, subtitle = "Ave Pause Time/hrs", width = 3, icon = icon("phone-slash"), color = "yellow" ),
                        valueBox (value = ave_dead, subtitle = "Ave Dead Time/hrs", color = "yellow", width = 3, icon = icon("battery-empty")),
                        valueBox (value = ave_connected, subtitle = "Ave Connected Time/hrs", color = "yellow", width = 3, icon = icon("link")),
                        valueBox (value = ave_lagged, subtitle = "Ave Lagged Time/hrs", color = "yellow", width = 3,
                                  icon = icon("person-walking-with-cane"))
                        
                      ),
                      
                      
                      fluidRow(
                        box(title = "Wait %", width = 6, plotlyOutput("plot1"),
                            footer = tags$div(tags$span(style = "color:blue","Wait: Time the agent is waiting for the call"))),
                        box(title = "Talk Time %", width = 6, plotlyOutput("plot2"),
                            footer = tags$div(tags$span(style = "color:blue",
                                                        "Talk: Time the agent talks to a customer or is in a dead state")))
                        
                        
                      ),
                      
                      fluidRow(
                        box(title = "Dispo Time %", width = 6, plotlyOutput("plot3"),
                            footer = tags$div(tags$span(style = "color:blue",
                                                       "Dispo: Time the agent uses at the disposition screen"))),
                        box(title = "Dead Time %", width = 6,plotlyOutput("plot4"),
                            footer = tags$div(tags$span(style = "color:blue",
                                                        "Dead: Time the agent is in a call after the customer has hung up")))
                      ),
                      
                      fluidRow(
                        
                        box(title = "Pause Time %", width = 6, plotlyOutput("plot5"),
                            footer = tags$div(tags$span(style = "color:blue",
                                                        "Pause: Time the agent is in pause mode"))),
                        box(title = "Total Calls", width = 6, plotlyOutput("plot6"))  
                        
                      ),
                      
                      
                      fluidRow(
                        box(title = "Total Calls per Agent", width = 6, plotlyOutput("plot7"))
                        
                        
                        
                      )
                      
                     
                      
                      
                      
                      
                    )
                    
)

server <- function(input, output) {
  
  output$plot1 <- renderPlotly({
    p1
  })
  
  
  output$plot2 <- renderPlotly({
    p2
  })
 
  output$plot3 <- renderPlotly({
    p3
  })
  
  output$plot4 <- renderPlotly({
    p4
  })
  
  output$plot5 <- renderPlotly({
    p5
  })
  
  output$plot6 <- renderPlotly({
    p6
  })
  
  output$plot7 <- renderPlotly({
    p7
  })
  
}

shinyApp(ui, server)


