library(howler)
library(tuneR)
library(DT)
source("utils_audio.R")


#TODO, IGNORE "_" WHEN SEARCHING FOR NEIGHBORS!

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


server_audio <- function(input, output, session) {
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
