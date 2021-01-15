library(shiny)
library(lubridate)
library(dplyr)
library(ggplot2)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("COVID-19-WeatherMap"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(width=2,
      
      p("COVID-19-WeatherMap-0.1.1 IBM@20210115"),
      
      fluidRow(
        dateRangeInput(
          inputId = "DateRange",
          label = "DateRange",
          start=as.Date("2020-07-01"),
          end=as.Date("2021-01-13"),
          min = as.Date("2020-07-01"),
          max = as.Date("2021-01-13"),
          weekstart=1)),
      
      fluidRow(
        checkboxGroupInput("RegionSelect", 
          h3("RegionSelect"), 
          choices = list("Österreich", 
                         "Wien", 
                         "Niederösterreich", 
                         "Burgenland", 
                         "Oberösterreich",
                         "Steiermark", 
                         "Kärnten", 
                         "Salzburg", 
                         "Tirol", 
                         "Vorarlberg"), 
          selected = c("Österreich","Wien"))),
      
      fluidRow(
        h3("Options"), 
        checkboxInput("LogScale", label="LogScale", value=TRUE))
    ),
  
    # Main panel for displaying outputs ----
    mainPanel(width=10,
     # h1("COVID-19-WeatherMap", align = "left"),
      
      tabsetPanel(type = "tabs",
                  
        tabPanel("TagesInzidenz",
          h4("Anzahl der Messwerte/h für Messstationen, Sensoren und Zonen", align = "left", style="color:gray"),
          p("[Menüauswahl: Zeitbereich]", align = "left", style="color:green"),
          plotOutput(outputId = "CovidPlot")),
                
        tabPanel("Messwerte Rohdaten",
          h4("Rohdaten der Messwerte eines Sensors einer Messstation im Zeitbereich", align = "left", style="color:black"),
          p("[Menüauswahl: Zeitbereich,Sensor]", align = "left", style="color:green"),
          dataTableOutput(outputId = "RawData"))
      )
    )
  )
)

#        tabPanel("Messwerte Verlauf",
#                 h4("Zeitreihe der Messwerte einer Sensorart von allen Messstationen", align = "left", style="color:gray"),
#                 p("[Menüauswahl: Zeitbereich,Sensor]", align = "left", style="color:green"),
#                 plotOutput(outputId = "CovidPlot")),
#                 plotOutput("sensorTimeSeriesPO", height = "75vh")),

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  df <- read.csv("./data/COVID-19-AGES-Curated.csv", sep=",", dec=".") %>%
    dplyr::mutate(Date=as.Date(Date,"%Y-%m-%d")) %>%
    dplyr::select(-Stamp) %>%
    dplyr::filter(Date>as.Date("2020-07-01"))

  output$RawData <- renderDataTable({
    df %>% 
      dplyr::select(Date,Region,rm7NewConfPop) %>% 
      dplyr::arrange(Date,Region) %>%
      tail(10)
  })
  
  output$CovidPlot <- renderPlot({
    Regions <- input$RegionSelect
    trans <- ifelse(input$LogScale, "log10", "identity")
    
    dp <- df %>% dplyr::filter(Region %in% Regions)
    
    ggplot(dp, aes(x=Date, y=rm7NewConfPop, color=Region))+
    geom_point()+geom_line()+
    geom_smooth(data=dp%>%dplyr::filter(Date<as.Date("2020-12-01")), aes(x=Date, y=rm7NewConfPop),method="lm", se=FALSE) +
    scale_y_continuous(trans=trans)  
  })
}


# start shiny app
shinyApp(ui = ui, server = server)


