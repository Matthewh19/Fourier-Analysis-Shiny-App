library(shiny)
library(shinydashboard)
library(ggplot2)
library(pracma)
library(readr)

#dasfdsad

GSaxlinear <- read_csv('GSaxlinear.csv')

Finger_cymbals_44100hz <- read_csv('Cut_Down44100_Finger_cymbals.csv')
Frame_Drum_44100hz <- read_csv('Cut_Down44100_Frame_Drum.csv')
PaulS_44100hz <- read_csv('Cut_Down44100_PaulS.csv')
Bongo_44100hz <- read_csv('Cut_Down44100Bongo.csv')
Electric_Bass_96000hz <- read_csv('Cut_Down96000_Electric_Bass.csv')
Kalen_Violin_96000hz <- read_csv('Cut_Down96000_Kalen_Violin.csv')
Luke_Trumpet_96000hz <- read_csv('Cut_Down96000_Luke_Trumpet.csv')
Metal_Xylo_96000hz <- read_csv('Cut_Down96000_Metal_Xylo.csv')
OS_Guitar_96000hz <- read_csv('Cut_Down96000_OS_Guitar.csv')
Piano_96000hz <- read_csv('Cut_Down96000_Piano.csv')
Wooden_Xylo_96000hz <- read_csv('Cut_Down96000_Wooden_Xylo.csv')



ui <- dashboardPage(
  
  dashboardHeader(title = "Fourier Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Prerecorded Instruments", tabName = "prerecordedinstr", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "prerecordedinstr",
              fluidRow(
                box(title = "Your Output!", width = 8, solidHeader = TRUE, status = "success",
                    tabsetPanel(
                      tabPanel("Plot", plotOutput("plot1")),
                      tabPanel("Table", tableOutput("table"))  )),
                
                box(title = "Controls", width = 4, solidHeader = TRUE, status = "primary",
                    selectInput("select", label = h3("Chose Which Instrument"),
                                choices = list("Alto Saxophone" = "GSaxlinear", "Finger Cymbals" = "Finger_cymbals_44100hz", 
                                               "Frame Drum" = "Frame_Drum_44100hz", "Paul Stumf Singing" = "PaulS_44100hz", 
                                               "Bongos" = "Bongo_44100hz", "Electric Bass" = "Electric_Bass_96000hz", 
                                               "Violin (Kalen)" = "Kalen_Violin_96000hz", "Trumpet (Luke)" = "Luke_Trumpet_96000hz",
                                               "Metal Xylo" = "Metal_Xylo_96000hz", "Guitar (Owen Sahnow)" = "OS_Guitar_96000hz", 
                                               "Piano" = "Piano_96000hz", "Wooden Xylo" = "Wooden_Xylo_96000hz"),
                                selected = "Alto Saxophone"),
                    
                    radioButtons("samplerate", label = h3("Which Sample Rate is this recorded with?"),
                                 choices = list("44100 hz" = 44100, "96000 hz" = 96000), 
                                 selected = 44100),
                    
                    sliderInput("freqrange", label = h3("Which Data Points to Graph"), min = 0, max = 3000, value = 300),
                    
                    sliderInput("numberpeaks", label = h3("Number of Peaks to Show"), min = 0, max = 50, value = 10),
                    
                    sliderInput("pointsize", label = h3("Size of Points on The Graph"), min = 0, max = 10, value = .8, step = .01),
                    
                    downloadButton('foo'))
              )
      ),
      
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)


server <- function(input, output) {
  Orgdata <- reactive({
    get(input$select)
  })
  
  Regfft <- reactive({
    ROrgdata <- Orgdata()
    as.numeric(abs(fft(ROrgdata$Linear)))
  })
  
  Normfft <- reactive({
    NOrgdata <- Orgdata()
    NRegfft <- Regfft()
    NRegfft/length(NOrgdata$Linear)
  })
  
  Time <- reactive({
    firstdata <- Orgdata()
    length(firstdata$Linear)/as.numeric(input$samplerate)
  })
  
  Frequency <- reactive({
    Normalizedfft <- Normfft()
    Timeforfreq <- Time()
    seq(from = 0, to = (length(Normalizedfft)-1)/Timeforfreq, by = 1/Timeforfreq)
  })
  
  NewFullfft <- reactive({
    data.frame("Frequency" = Frequency(), "fftValues" = Normfft())
  })
  
  firstFullfft <- reactive({
    #Frequencyrange <- c(input$freqrange)
    FFFullfft <- NewFullfft()
    trimfft <- head(FFFullfft, n = input$freqrange)
    trimfft
  })
  
  ##Peaks
  RegFullfft <- reactive({
    Regularfft <- firstFullfft()
    Regularfft["Peaks"] <- "Regular Point"
    Regularfft
  })
  
  TopPeaks <- reactive({
    RegularFFT <- RegFullfft()
    GPeaks <- findpeaks(RegularFFT$fftValues)
    colnames(GPeaks) <- c("fftValues", "Position/Index", "Index of Start", "Index of End")
    
    GPeaksdf <- as.data.frame(GPeaks)
    GPeaksdf["Peaks"] <- "Peak"
    GTopPeaks <- head(GPeaksdf[order(GPeaksdf$fftValues, decreasing = TRUE), ], input$numberpeaks)
    GTopPeaks
  })
  
  Fullfft <- reactive({
    FTopPeaks <- TopPeaks()
    Fullpeakfft <- RegFullfft()
    for (n in 1:length(FTopPeaks$fftValues)) {
      Index <- FTopPeaks[n, "Position/Index"]
      Fullpeakfft[Index, "Peaks"] <- "Peak"
    }
    Fullpeakfft
  })
  
  
  ##Makin the Visuals
  output$plot1 <- renderPlot({
    ggplot(data = Fullfft(), aes(x=Frequency, y=fftValues, color = Peaks)) +
      ggtitle(paste("Frequency vs fft values for ", input$select, sep = "")) +
      geom_point(size= input$pointsize) +
      xlab('Frequency') + ylab('fft values') 
  })
  
  output$table <- renderTable(Fullfft())
  
  PrintPlot <- reactive({ 
    ggplot(data = Fullfft(), aes(x=Frequency, y=fftValues, color = Peaks)) +
      ggtitle(paste("Frequency vs fft values for ", input$select, sep = "")) +
      geom_point(size= input$pointsize) +
      xlab('Frequency') + ylab('fft values') 
  })
  
  output$foo = downloadHandler(
    filename = function() {
      paste("graph-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      png(file, width = 811, height = 368, units = "px")
      plot(PrintPlot())
      dev.off()
    })
}

shinyApp(ui, server)

