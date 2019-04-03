library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  
  dashboardHeader(title = "Fourier Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Prerecorded Instruments", tabName = "prerecordedinstr", icon = icon("dashboard"))
    )),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "prerecordedinstr",
              #orgfftpageUI("orgfftpage")
              
              fluidRow(
                box(title = "Your Output!", width = 8, solidHeader = TRUE, status = "success",
                    tabsetPanel(
                      tabPanel("Plot", plotOutput("plot1")),
                      tabPanel("Table", tableOutput("table"))  )),
                
                box(title = "Controls", width = 4, solidHeader = TRUE, status = "primary",
                    selectInput("select", label = "Chose Which Instrument",
                                choices = list("Alto Saxophone" = "GSaxlinear", "Finger Cymbals" = "Finger_cymbals_44100hz", 
                                               "Frame Drum" = "Frame_Drum_44100hz", "Paul Stumf Singing" = "PaulS_44100hz", 
                                               "Bongos" = "Bongo_44100hz", "Electric Bass" = "Electric_Bass_96000hz", 
                                               "Violin (Kalen)" = "Kalen_Violin_96000hz", "Trumpet (Luke)" = "Luke_Trumpet_96000hz",
                                               "Metal Xylo" = "Metal_Xylo_96000hz", "Guitar (Owen Sahnow)" = "OS_Guitar_96000hz", 
                                               "Piano" = "Piano_96000hz", "Wooden Xylo" = "Wooden_Xylo_96000hz"),
                                selected = "Alto Saxophone"),
                    
                    radioButtons("samplerate", label = "Which Sample Rate is this recorded with?",
                                 choices = list("44100 hz" = 44100, "96000 hz" = 96000), 
                                 selected = 44100),
                    
                    sliderInput("freqrange", label = "Which Data Points to Graph", min = 0, max = 3000, value = 300),
                    
                    sliderInput("numberpeaks", label = "Number of Peaks to Show", min = 0, max = 50, value = 2),
                    
                    sliderInput("pointsize", label = "Size of Points on The Graph", min = 0, max = 10, value = .8, step = .01),
                    
                    downloadButton('foo'),
                    
                    sliderInput("slength", label = "Length of Rebuilt Sound (sec)", 
                                min = 0, max = 10, value = 4),
                    actionButton("playsound", label = "Play The Rebuilt Sound!"),
                    tableOutput("badtable"))
              )
      )
    )
  )
)