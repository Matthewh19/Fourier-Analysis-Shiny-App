library(shiny)
library(shinydashboard)
library(ggplot2)
library(pracma)
library(readr)

server <- function(input, output) {
  # orgfftpage
  #callModule(orgfftpage, "orgfftpage")
  
  Orgdata <- reactive({
    get(input$select)
  })
  
  Normfft <- reactive({
    NOrgdata <- Orgdata()
    Regfft <- as.numeric(abs(fft(NOrgdata$Linear)))
    
    Normfft <- Regfft/length(NOrgdata$Linear)
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
  
  
  
  #########Rebuilding The Sound###########################################################
  theFullfft <- reactive({
    Data <- Orgdata()
    Gsaxfft <- abs(fft(Data$Linear))
    
    Normfft <- Gsaxfft/length(Data$Linear)
    
    Time <- length(Data$Linear)/(as.numeric(input$samplerate))
    Frequency <- seq(from = 0, to = (length(Normfft)-1)/Time, by = 1/Time)
    
    theFullfft <- data.frame("Frequency" = Frequency, "fftValues" = Normfft)
    return(theFullfft)
  })
  
  ##Adding in the Peaks
  NThePeaks <- reactive({
    Fullfft <- theFullfft()
    GPeaks <- findpeaks(Fullfft$fftValues)
    
    colnames(GPeaks) <- c("fftValues", "Position/Index", "Index of Start", "Index of End")
    GPeaksdf <- as.data.frame(GPeaks)
    GPeaksdf["Peaks"] <- "Peak"
    
    TopPeaks <- head(GPeaksdf[order(GPeaksdf$fftValues, decreasing = TRUE), ], (input$numberpeaks)*2)
    
    
    for (n in 1:length(TopPeaks$fftValues)) {
      Index <- TopPeaks[n, "Position/Index"]
      Fullfft[Index, "Peaks"] <- "Peak"
    }
    
    sNThePeaks <- subset(Fullfft, Peaks %in% "Peak")
    
    NThePeaks <- sNThePeaks[1:(input$numberpeaks), ]
    NThePeaks
  })
  
  
  
  ### Rebuilding the Sound
  
  SumSound <- reactive({
    ThePeaks <- NThePeaks()
    NewTime <- seq(1, as.integer((as.numeric(input$samplerate))*(input$slength)))
    SoundStuff <- data.frame("Time" = NewTime)
    
    for (n in 1:length(ThePeaks$Frequency)){
      SoundStuff[paste("Note", n, sep = " ")] <- ThePeaks[n, "fftValues"] * sin((ThePeaks[n, "Frequency"] * 2 * pi * NewTime)/(as.numeric(input$samplerate)))
    }
    
    SumSound <- as.numeric(rowSums(SoundStuff[,-1]))
    
    return(SumSound)
  })

  Soundwav <- reactive({
    Wave(left = SumSound(), samp.rate = 44100, bit = 32)
  })
  
  observe(
    savewav(Soundwav(), filename = "www/Soundwavexported.wav")
  )
  
  
  observeEvent(input$playsound, {
    insertUI(selector = "#playsound",
             where = "afterEnd",
             ui = tags$audio(src = "Soundwavexported.wav", type = "audio/wav", autoplay = NA, controls = NA, style="display:none;")
    )
  })
}

