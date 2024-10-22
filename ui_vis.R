# Define UI for application that draws a histogram
ui_vis <- function() {
  fluidPage(
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
}
