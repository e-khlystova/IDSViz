server_vis <- function(input, output) {
  
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
