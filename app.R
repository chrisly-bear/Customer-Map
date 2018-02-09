# Load libraries
# install.packages("DT")
library(data.table)
library(shiny)
library(leaflet)
library(shinythemes)
library(DT)
library(lubridate)



# Load data
d <- fread("demographics.csv")
d[ , JoinDate:=dmy(JoinDate, tz="UTC")]
d[ , Birthdate:=dmy(Birthdate, tz="UTC")]

# ui part -----------------------------------------------------------------

ui <- fluidPage(theme = shinytheme("simplex"),
                titlePanel("Customer Map"),
                tags$p("The map displays our customers' location. Customers can be filtered by Gender, Join Date, Date of Birth, and States. 
                       To display the data in table form, switch to the 'Table' tab. To download the filtered data, switch to the 'Download' tab."),
                tags$p("You will find the source code to this application on ", tags$a(href="https://github.com/chrisly-bear/customer-map", "GitHub", target="_blank"), "."),
                tags$br(),
                sidebarLayout(
                  sidebarPanel(tags$h4("Filters"),
                               tags$hr(),
                               checkboxGroupInput("genderWidget", 
                                                  label = tags$b("Gender"), 
                                                  choices = list("Female" = "f", 
                                                                 "Male" = "m",
                                                                 "Alien" = "alien"),
                                                  selected = c("f", "m", "alien")
                                                  ),
                               tags$hr(),
                               sliderInput("joinDateWidget", 
                                           label = tags$b("Join Date"),
                                           min = dmy("01.01.1965"),
                                           max = dmy("31.12.2011"),
                                           value = c(dmy("01.01.1965"), dmy("31.12.2011"))
                                           ),
                               tags$hr(),
                               # sliderInput("birthdateSlider",
                               #             label = tags$b("Birthdate"),
                               #             min = min(d$Birthdate),
                               #             max = max(d$Birthdate),
                               #             value = c(min(d$Birthdate), max(d$Birthdate))
                               # ),
                               # tags$hr(),
                               dateRangeInput("birthdateRange",
                                              label = tags$b("Date of Birth"),
                                              min = min(d$Birthdate),
                                              max = max(d$Birthdate),
                                              start = min(d$Birthdate),
                                              end = max(d$Birthdate)
                                              ),
                               tags$hr(),
                               selectInput("stateSelect",
                                           label = tags$b("States"),
                                           choices = unique(d[order(zip_state),zip_state]),
                                           multiple = TRUE,
                                           selected = unique(d[order(zip_state),zip_state])
                                           )
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Map", leafletOutput("mymap", height = "600px")),
                      tabPanel("Table", DT::dataTableOutput('mydata')),
                      tabPanel("Download",
                               tags$br(),
                               "Download the filtered data as a .csv file",
                               tags$br(), tags$br(),
                               downloadButton("downloadData", "Download"))
                    )
                  )
                )
)      


# server part -------------------------------------------------------------

server <- function(input, output, session) {
  
  d.shiny <- reactive({ 
    d[Gender %in% input$genderWidget &
        JoinDate >= input$joinDateWidget[1] & JoinDate <= input$joinDateWidget[2] &
        # Birthdate >= input$birthdateSlider[1] & Birthdate <= input$birthdateSlider[2] &
        Birthdate >= input$birthdateRange[1] & Birthdate <= input$birthdateRange[2] &
        zip_state %in% input$stateSelect,]
  })
  
  # Define map and fill map with data points  
  output$mymap <- renderLeaflet({
    map <- leaflet()
    map <- addTiles(map)
    map <- addMarkers(map, lng = d.shiny()[,zip_longitude], 
                      lat =d.shiny()[,zip_latitude], 
                      clusterOptions = markerClusterOptions())
    map <- setView(map, lat= 35, lng= -95, zoom = 4) # North America
  })
  
  # Table Output for Tab 
  output$mydata <- DT::renderDataTable(d.shiny())
  
  # Download Handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(d.shiny(), file)
    }
  )
  
}  

# End server


shinyApp(ui, server)