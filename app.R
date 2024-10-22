library(shiny)
library(tidyverse)
source("helpers.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("PhonProv+: Explore English and Spanish infant-directed speech"),
  
  # Language selection button -- eventually will affect whether working with English or Spanish IDS (different info available for each)
  radioButtons("lang", 
               label = h3("Language:"),
               choices = list("English" = "eng", "Spanish" = "span"),
               inline = T,
               selected = "eng"),
  
  sidebarLayout(
    position = "right", 
    
    sidebarPanel(h2(("Selection panel")), 
                 
                 h4("Vowels"),
                 #radioButtons("units", 
                  #            label = ("Unit:"),
                   #           choices = list("Hz" = 1, "Barks" = 2), 
                    #          selected = 2),
                 
                 selectInput("vowelvar1",
                             label = ("Choose horizontal axis:"), 
                             choices = list("F1" = "f1", "F2" = "f2", "F3" = "f3", "Duration" = "duration"),
                             selected = "f2"
                 ), 
                 selectInput("vowelvar2",
                             label = ("Choose vertical axis:"), 
                             choices = list("F1" = "f1", "F2" = "f2", "F3" = "f3", "Duration" = "duration"),
                             selected = "f1"
                 ),
                 
                 checkboxInput("want_speaker", 
                               "Choose a speaker?"
                 ), 
                 
                 # Conditional panel for English speakers
                 conditionalPanel(
                   condition = 'input.lang == "eng" && input.want_speaker',
                   selectInput("english_speaker",
                               label = "Select English Speaker:",
                               choices = unique(englishVowels$speaker),
                               selected = NULL
                   )
                 ),
                 
                 # Conditional panel for Spanish speakers
                 conditionalPanel(
                   condition = 'input.lang == "span" && input.want_speaker',
                   selectInput("spanish_speaker",
                               label = "Select Spanish Speaker:",
                               choices = unique(spanishVowels$speaker),
                               selected = NULL
                   )
                 ),
                 
                 h4("Consonants"),
                 selectInput("phoneme", 
                             label = "Choose a phoneme:",
                             choices = sort(unique(englishIDS$ur)),
                             selected = "t"
                 ),
                 
                 selectInput("var", 
                             label = ("Choose a variable to graph:"),
                             choices = list("Phonetic realization" = "coding", "Preceding segment" = "before", "Following segment" = "after")
                 )
                 
    ),
    
    mainPanel(
      plotOutput("vowel_plot"),
      textOutput("most_frequent"),
      plotOutput("freq_plot")
    )
  )
)






server <- function(input, output) {
  
  langdata <- reactive({
    data <- switch(input$lang, eng = englishVowels, span = spanishVowels)
    
    # If want_speaker is selected, filter data by speaker
    if (input$want_speaker) {
      speaker_col <- switch(input$lang, eng = "english_speaker", span = "spanish_speaker")
      speaker_value <- input[[speaker_col]]
      data <- filter(data, speaker == speaker_value)
    }
    
    return(data)
  })
  
  data_subset <- reactive({
    req(input$phoneme)
    filter(englishIDS, ur %in% input$phoneme)
  })
  
  output$freq_plot <- renderPlot({
    if (input$lang == "eng") {
      ggplot(data = data_subset(), aes_string(x = input$var)) +
        geom_bar(aes(fill=position))  + 
        coord_flip() + 
        ggtitle("Frequency of variants")
    } else {
      # If input$lang is not "english", return NULL or an empty plot
      NULL  # or ggplot() or plot(NULL)
    }
  })
  
  output$vowel_plot <- renderPlot({
    ggplot(langdata(), aes_string(x = input$vowelvar1, y = input$vowelvar2)) +
      geom_point(aes(color = vowel), alpha = 0.5, size = 0.75) + 
      stat_ellipse(aes(color = vowel), level = 0.67, lwd = 1.2) + 
      scale_x_reverse() + scale_y_reverse() +
      ggtitle("Vowel plot")  # Dynamic title based on language
  })
  
}

shinyApp(ui = ui, server = server)
