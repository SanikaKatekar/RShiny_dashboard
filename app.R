#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#load all necessary libraries
library(tidyr)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)
library(tidyverse)
library(shinythemes)
library(maps)

#Read csv file
data <- read.csv('/Users/sanikakatekar/Desktop/Previous_sems/R_dashboard/RShiny_dashboard_git/WHO-COVID-19.csv')

#Code for plotting new cases vs. date reported
data1 <- data %>%
  mutate(Date_reported = ymd(Date_reported)) %>%
  arrange(desc(Date_reported)) %>%
  filter(Country == "United States of America" | Country == "The United Kingdom")
plot_n <- ggplot(data1, aes(x=Date_reported, y=New_cases), group=Country) +
  geom_line(aes(color=Country), size = 0.5) + theme(legend.position = "none")

#code for plotting deaths vs. date reported
data2 <- data %>%
  mutate(Date_reported = ymd(Date_reported)) %>%
  arrange(desc(Date_reported)) %>%
  filter(Country == "United States of America" | Country == "The United Kingdom")
plot_d <- ggplot(data2, aes(x=Date_reported, y=New_deaths), group=Country) +
  geom_line(aes(color=Country), size = 0.5) + theme(legend.position = "none")

#Code for creating map
data3 <- data %>%
  mutate(Date_reported = ymd(Date_reported)) %>%
  arrange(desc(Date_reported)) %>%
  filter(Date_reported == "2021-07-12")

data3$Country[data3$Country == "United States of America"] <- "USA"
data3$Country[data3$Country == "Russian Federation"] <- "Russia"
data3$Country[data3$Country == "The United Kingdom"] <- "UK"
data3$Country[data3$Country == "Iran (Islamic Republic of)"] <- "Iran"
data3$Country[data3$Country == "Bolivia (Plurinational State of)"] <- "Bolivia"
data3$Country[data3$Country == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"

world_map <- map_data("world")
world_map <- subset(world_map, region != "Antarctica")
# world_map %>%
#   filter(region == "Russia")

plot_map <- ggplot(data3) +
  geom_map(
    dat = world_map, map = world_map, aes(map_id = region),
    fill = "white", color = "#7f7f7f", size = 0.25
  ) +
  geom_map(map = world_map, aes(map_id = Country, fill = New_cases), size = 0.25) +
  scale_fill_gradient(low = "#fff7bc", high = "#cc4c02", name = "Total Cases") +
  expand_limits(x = world_map$long, y = world_map$lat)




# Define UI for application 
ui <- fluidPage(
  
  #creating the main dashboard page
  dashboardPage( 
    
    #Specify the dashboard header
    dashboardHeader(title = "A Dashboard for viewing some COVID-19 trends", titleWidth = 700,tags$li(class="dropdown",tags$a(href="https://data.humdata.org/dataset/coronavirus-covid-19-cases-and-deaths","Dataset Link"))),
    
    #Specify the contents of the sidebar
    dashboardSidebar(
      sidebarMenu(id ="sidebar",
                  menuItem("Data", tabName = "data", icon = icon("database")), # make data tab
                  menuItem("Cases and deaths", tabName = "newc", icon=icon("chart-line")), #make graph tab
                  menuItem("Map Visualization", tabName = "map", icon=icon("map")) #make a map
        
      ) #sidebarMenu end bracket
    ), #dashboardSidebar end bracket
    
    #Specify the contents of the dashboard body
    dashboardBody(
      
      #create the three different tab items
      tabItems(
        
        #Create tab item of the "Data" menu item
        tabItem(tabName = "data",
                fluidRow(
                  valueBox("Date Range", "2020-01-03 to 2022-10-10", icon = NULL, color = "aqua", width = 4,
                           href = NULL),
                  valueBox("Source", "World Health Organisation", icon = NULL, color = "green", width = 4,
                           href = NULL),
                  valueBox("Location", "World", icon = NULL, color = "yellow", width = 4,
                           href = NULL),
                ),
                box(id = "t1", width = "20",
                       tabPanel("About the Dataset",tags$p("This dataset is the 'Coronavirus (COVID-19) Cases and Deaths' dataset. The source and the contributor of this dataset is the World Health Organization. The link to the dataset is present on the left hand corner of the dataset. This dataset contains 7 variables: Date_reported, Country, WHO_region, New_cases, Cumulative_cases, New_deaths, Cumulative_cases."),dataTableOutput("data_table"))
                  
                ) #Box end bracket 
        ), # "Data" tabItem end bracket
        
        #Create the "Cases and Deaths" tabItem
        tabItem(tabName = "newc",
                fluidRow(
                  valueBox("Date Range", "2020-01-03 to 2022-10-10", icon = NULL, color = "aqua", width = 4,
                           href = NULL),
                  valueBox("Source", "World Health Organisation", icon = NULL, color = "green", width = 4,
                           href = NULL),
                  valueBox("Location", "World", icon = NULL, color = "yellow", width = 4,
                           href = NULL),
                ),
                tabBox( id ="t2", width = "20",
                        tabPanel("New Cases",
                                 fluidRow(
                                   column(width = 8, tags$br() ,tags$p("This graph is a plot of new cases reported vs. the date these cases were reported on. This graph is plotted for only two countries from the 'WHO-COVID-19' dataset. The two countries shown in this plot are the United States of America and The United Kingdom. This is an interactive plot so feel free to move the cursor around the plot to check for the exact value of cases reported on a specific date. ")),
                                   column(width = 12, tags$br() ,plotlyOutput("plot_case"))
                                   
                                 ) #fluidRow end bracket
                                 ), #tabPanel end bracket
                        tabPanel("Deaths",
                                 fluidRow(
                                   column(width = 8, tags$br() ,tags$p("This graph is a plot of deaths reported vs. the date these deaths were reported on. This graph is plotted for only two countries from the 'WHO-COVID-19' dataset. The two countries shown in this plot are the United States of America and The United Kingdom.  This is an interactive plot so feel free to move the cursor around the plot to check for the exact value of deaths reported on a specific date.")),
                                   column(width = 12, tags$br() ,plotlyOutput("plot_death"))
                                 ) #fluidrow end bracket
                        ) #tabPanel end braket
                ) #tabBox end bracket
        ),# "Cases and Deaths" tabitem end brack
        
        #Create "Map Visualization" tabItem
        tabItem( tabName = "map", 
                 fluidRow(
                   valueBox("Date Range", "2020-01-03 to 2022-10-10", icon = NULL, color = "aqua", width = 4,
                            href = NULL),
                   valueBox("Source", "World Health Organisation", icon = NULL, color = "green", width = 4,
                            href = NULL),
                   valueBox("Location", "World", icon = NULL, color = "yellow", width = 4,
                            href = NULL),
                 ),
                 tabBox(id="t3",tags$p("This map indicates the deaths reported on 2021-07-12 across the globe."),width = 20),
                 box(plotOutput("map_p"), width=20, height = 60
                 ) # Box end bracket
        ) # "Map Visualization" tabItem end bracket
        
      ) # main tabItems end bracket
    ) # dashboardBody end bracket
  )
)

# Define server logic required to draw a histogram
#plotOutput("plot1_cases", click = "plot_click"), icon=icon("chart-pie")
server <- function(input, output) {
  output$data_table <- renderDataTable(data)
  output$plot_case <- renderPlotly(plot_n)
  output$plot_death <- renderPlotly(plot_d)
  output$map_p <- renderPlot({plot_map})
} #server end bracket

# Run the application 
shinyApp(ui = ui, server = server)
