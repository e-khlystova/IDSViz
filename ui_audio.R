library(shiny)
library(howler)
library(tuneR)
source("utils_audio.R")

print("original:")
sampa <- c('a','3','e','Q','p\\', 'r\\`', 's\\','a','[ar_?\\b_?\\@<\\a]' )
print(sampa)
print("IPA:")
IPA <- translate_sampa(sampa)
print(IPA)

#UI
ui_audio <- fluidPage(
  fluidRow (
    #IPA Input
    column(2,
      textInput("ipainput", h3("IPA:"))
    ),
    #Tier Input
    column(2,
      selectInput("select", h3("TextGrid Tier:"), 
                  choices = list("Phoneme" = "phones", "Syllable" = "syll", "Word" = "words", "Phonology"="phono", "Orthography"="ortho"), selected = "phones"),
    ),
    column(2,
      # Search for neighbor?
      checkboxInput(inputId = "search_neighbor", label = "Show Neighbor")
    ),
  ),
  sidebarLayout(
    sidebarPanel(
      #info Panel
      h1("info panel"),
      actionButton("prev", "Previous"),
      actionButton("nxt", "Next")
    ),
    mainPanel (
      uiOutput("audiotable"),
      textOutput("foo_bar")
    )
  ),
)

server_audio <- function(input, output) {
  NewSection = "https://cdn.pixabay.com/download/audio/2022/05/16/audio_db6591201e.mp3"
  
  go_prev <- observeEvent(input$prev, {
    cur_page <<- max(1, (cur_page-20))
    print("page is now")
    print(cur_page)
    output$audiotable <- renderUI({
      validate(
        need(input$ipainput, 'Please Enter a Phoneme.'),
      )
      foo_func(input$ipainput, input$select, input$search_neighbor, allGrids)
    })
  })
  
  go_next <- observeEvent(input$nxt, {
    cur_page <<- min(cur_max_page, (cur_page+20))
    print("page is now")
    print(cur_page)
    output$audiotable <- renderUI({
      validate(
        need(input$ipainput, 'Please Enter a Phoneme.'),
      )
      foo_func(input$ipainput, input$select, input$search_neighbor, allGrids)
    })
  }) 
  
  reset_pages_select <- observeEvent(input$select, {
    cur_page <<- 1
  })
  reset_pages_ipa <- observeEvent(input$ipainput, {
    cur_page <<- 1
  })
  reset_pages_neighbor <-eventReactive(input$search_neighbor, {
    cur_page <<- 1
  })
  
  output$audiotable <- renderUI({
    validate(
      need(input$ipainput, 'Please Enter a Phoneme.'),
    )
    foo_func(input$ipainput, input$select, input$search_neighbor, allGrids)
  })
}

shinyApp(ui = ui_audio, server = server_audio)
