library(shiny)
library(tidyverse)
source("helpers.R")
source("ui_vis.R")
source("ui_audio.R")
source("server_vis.R")
source("server_audio.R")

#MAKE A CREDITS TAB

# UI for the first app (visualization)
ui_visualization <- fluidPage(
  # Define UI for application that draws a histogram
  
  fluidPage(
    # Application title
    titlePanel("Visualize English and Spanish infant-directed speech"),
    
    # Language selection button -- eventually will affect whether working with English or Spanish IDS (different info available for each)
    radioButtons("lang", 
                 label = h2("Choose a language:"),
                 choices = list("English" = "eng", "Spanish" = "span"),
                 inline = T,
                 selected = "eng"),
    
    sidebarLayout(
      position = "right", 
      
      sidebarPanel(#h2(("Selection panel")), 
        
        h3("Vowel selections:"),
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
                      "Choose a speaker?", 
                      value = F
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
        
        conditionalPanel(
          condition = 'input.lang == "eng"', 
          h3("Consonant selections:"),
          selectInput("phoneme", 
                      label = "Choose a phoneme:",
                      choices = sort(unique(englishIDS$ur)),
                      selected = "t"
                      
          ), 
          
          selectInput("var", 
                      label = ("Choose a variable to graph:"),
                      choices = list("Phonetic realization" = "coding", "Preceding segment" = "before", "Following segment" = "after")
          ), 
          
          checkboxInput("want_position", 
                        "Position in word?", 
                        value = F
          ),
          
          conditionalPanel(
            condition = 'input.lang == "eng" && input.want_position',
            checkboxGroupInput("position",
                        label = "Select position:",
                        choices = c("I", "M", "F"),
                        selected = NULL
            )
          ),
          
        ),
        
        
        conditionalPanel(
          condition = 'input.lang == "span"'
          
        ), 
        
        
        
        
        
        
      ),
      
      mainPanel(
        plotOutput("vowel_plot"),
        textOutput("most_frequent"),
        plotOutput("freq_plot")
      )
    )
  )
  
)

# Server logic for the first app (visualization)
server_visualization <- function(input, output, session) {
  # Server logic for visualization app goes here
  # Server logic for the first app goes here
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
    filtered_data <- filter(englishIDS, ur %in% input$phoneme)
    
    if (!is.null(input$position) && length(input$position) > 0) {
      filtered_data <- filtered_data %>%
        filter(position %in% input$position)
    }
    
    return(filtered_data)
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

# UI for the second app (audio segmentation)
ui_audio_segmentation <- fluidPage(
  # UI code for audio segmentation app goes here
  titlePanel("Generate audio clips"),
  
  # Additional UI components for the second app go here
  
  # For example, you can add a new tab or section for the second app
  tabPanel("Audio",
           fluidPage(
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
             # sidebarLayout(
             #   sidebarPanel(
             #     #info Panel
             #     h1("info panel"),
             #     actionButton("prev", "Previous"),
             #     actionButton("nxt", "Next")
             #   ),
             mainPanel (
               uiOutput("audiotable"),
               textOutput("foo_bar")
             )
           ),
  )
  # )
)



#Globals
cur_page = 1
cur_max_page = 1

# Loading in textGrid Files
allGrids <- load_textGrids()
print("loaded grids")

foo_func <- function(ipa, select, search_neighbor, allGrids) {
  
  td <- tempdir()
  addResourcePath("aud", "./audio")
  addResourcePath("tmp", td)
  audio_files <- file.path("aud", list.files("./audio", ".wav$"))
  
  if (search_neighbor) {
    searchResults <- get_timestamps_for_neighbor(regex=ipa, tierSearch=select, allGrids)
  }
  else {
    searchResults <- get_timestamps_for(regex=ipa, tierSearch=select, allGrids)
  }
  if (length(searchResults[[1]]) == 0) {
    return(p("No Matches Found."))
  }
  
  cur_max_page <<- length(searchResults[[1]])
  result <- tagList(h2(paste('Audio Clips of', ipa, 'in the context of', select)))
  from <- max(1,cur_page)
  to <- min(length(searchResults[[1]]),(cur_page+20))
  for (x in from:to) {
    #find audio file
    addResourcePath("aud", "./audio")
    audio_files <- file.path("aud", list.files("./audio", ".wav$"))
    file_name <- gsub("textgrid/", "", gsub(".txt", ".wav", searchResults[[1]][[x]]))
    name_local_file <- file.path("./audio", file_name)
    temp_file <- file.path(td, paste0("temp", ipa, x, select, search_neighbor, ".wav"))
    writeWave(readWave(name_local_file, from=searchResults[[2]][[x]], to=searchResults[[3]][[x]], units="seconds"), filename=temp_file)
    seeked_audio_file <- file.path("tmp", paste0("temp", ipa, x, select, search_neighbor, ".wav"));
    if (search_neighbor) {
      tagListResult <- tagList(
        fluidRow(
          p(ipa),
          p("lefthand neighbor is: ",
            translate_sampa(searchResults[[4]][[x]])),
          p("righthand neighbor is: ",
            translate_sampa(searchResults[[5]][[x]])
          ),
          howler::howlerBasicModuleUI(
            id = paste0("clip","-",x),
            files = list(
              seeked_audio_file
            )
          )
        )
      )
    }
    else {
      tagListResult <- tagList(
        fluidRow(
          p(searchResults[[4]][[x]]),
          howler::howlerBasicModuleUI(
            id = paste0("clip","-",x),
            files = list(
              seeked_audio_file
            )
          )
        )
      )
    }
    result <- tagAppendChild(result, tagListResult)
  }
  sidebarPanel(style = "height: 90vh; overflow-y: auto;", result)
}

dummy_loop <- function () {
  result <- tagList(p('bruhhh:'))
  for(x in 1:5) {
    result <- tagAppendChild(result, tagList(p(paste(x))))
  }
  result
}


# Server logic for the second app (audio segmentation)
server_audio_segmentation <- function(input, output) {
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

# Combine both apps
ui_combined <- navbarPage(
  "IDSViz",
  tabPanel("Visualization", ui_visualization),
  tabPanel("Audio segmentation", ui_audio_segmentation)
)

server_combined <- function(input, output, session) {
  # Server logic for visualization tab
  server_visualization(input, output, session)
  
  # Server logic for audio segmentation tab
  server_audio_segmentation(input, output)
}



# Run the combined app
shinyApp(ui_combined, server_combined)





# # Define the main UI
# ui_vis <- fluidPage(
#     # Application title
#     titlePanel("Visualize English and Spanish infant-directed speech"),
#     
#     # Language selection button -- eventually will affect whether working with English or Spanish IDS (different info available for each)
#     radioButtons("lang", 
#                  label = h2("Choose a language:"),
#                  choices = list("English" = "eng", "Spanish" = "span"),
#                  inline = T,
#                  selected = "eng"),
#     
#     sidebarLayout(
#       position = "right", 
#       
#       sidebarPanel(#h2(("Selection panel")), 
#         
#         h3("Vowel selections:"),
#         #radioButtons("units", 
#         #            label = ("Unit:"),
#         #           choices = list("Hz" = 1, "Barks" = 2), 
#         #          selected = 2),
#         
#         selectInput("vowelvar1",
#                     label = ("Choose horizontal axis:"), 
#                     choices = list("F1" = "f1", "F2" = "f2", "F3" = "f3", "Duration" = "duration"),
#                     selected = "f2"
#         ), 
#         selectInput("vowelvar2",
#                     label = ("Choose vertical axis:"), 
#                     choices = list("F1" = "f1", "F2" = "f2", "F3" = "f3", "Duration" = "duration"),
#                     selected = "f1"
#         ),
#         
#         checkboxInput("want_speaker", 
#                       "Choose a speaker?", 
#                       value = F
#         ), 
#         
#         # Conditional panel for English speakers
#         conditionalPanel(
#           condition = 'input.lang == "eng" && input.want_speaker',
#           selectInput("english_speaker",
#                       label = "Select English Speaker:",
#                       choices = unique(englishVowels$speaker),
#                       selected = NULL
#           )
#         ),
#         
#         # Conditional panel for Spanish speakers
#         conditionalPanel(
#           condition = 'input.lang == "span" && input.want_speaker',
#           selectInput("spanish_speaker",
#                       label = "Select Spanish Speaker:",
#                       choices = unique(spanishVowels$speaker),
#                       selected = NULL
#           )
#         ),
#         
#         conditionalPanel(
#           condition = 'input.lang == "eng"', 
#           h3("Consonant selections:"),
#           selectInput("phoneme", 
#                       label = "Choose a phoneme:",
#                       choices = sort(unique(englishIDS$ur)),
#                       selected = "t"
#                       
#           ), 
#           
#           selectInput("var", 
#                       label = ("Choose a variable to graph:"),
#                       choices = list("Phonetic realization" = "coding", "Preceding segment" = "before", "Following segment" = "after")
#           )
#           
#         ),
#         
#         
#         conditionalPanel(
#           condition = 'input.lang == "span"'
#           
#         ), 
#         
#         
#         
#         
#         
#         
#       ),
#       
#       mainPanel(
#         plotOutput("vowel_plot"),
#         textOutput("most_frequent"),
#         plotOutput("freq_plot")
#       )
#     )
#   )
# 
# 
# 
# # Define the main server
# server <-  function(input, output, session) {
#   langdata <- reactive({
#     data <- switch(input$lang, eng = englishVowels, span = spanishVowels)
#     
#     # If want_speaker is selected, filter data by speaker
#     if (input$want_speaker) {
#       speaker_col <- switch(input$lang, eng = "english_speaker", span = "spanish_speaker")
#       speaker_value <- input[[speaker_col]]
#       data <- filter(data, speaker == speaker_value)
#     }
#     
#     return(data)
#   })
#   
#   data_subset <- reactive({
#     req(input$phoneme)
#     result <- filter(englishIDS, ur %in% input$phoneme)
#     print(head(result))
#     return(result)
#   })
#   
#   output$freq_plot <- renderPlot({
#     if (input$lang == "eng") {
#       ggplot(data = data_subset(), aes_string(x = input$var)) +
#         geom_bar(aes(fill=position))  + 
#         coord_flip() + 
#         ggtitle("Frequency of variants")
#     } else {
#       # If input$lang is not "english", return NULL or an empty plot
#       NULL  # or ggplot() or plot(NULL)
#     }
#   })
#   
#   output$vowel_plot <- renderPlot({
#     ggplot(langdata(), aes_string(x = input$vowelvar1, y = input$vowelvar2)) +
#       geom_point(aes(color = vowel), alpha = 0.5, size = 0.75) + 
#       stat_ellipse(aes(color = vowel), level = 0.67, lwd = 1.2) + 
#       scale_x_reverse() + scale_y_reverse() +
#       ggtitle("Vowel plot")  # Dynamic title based on language
#   })
#   
# }
# 
# 
# 
# # server <- function(input, output, session) {
# #   output$vis_tab <- renderUI({
# #     ui_vis()
# #   })
# #   
# #   output$audio_tab <- renderUI({
# #     ui_audio()
# #   })
# #   
# # 
# #   callModule(server_vis, "vis")  # Pass module ID
# #   callModule(server_audio, "audio")  # Pass module ID
# # }
# 
# # Run the app
#shinyApp(ui = ui, server = server)
