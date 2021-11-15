library(shiny)
# install.packages("wordcloud2")
library(wordcloud2)
# install.packages("tm")
library(tm)
# install.packages("colourpicker")
library(colourpicker)
library(RColorBrewer)
library(tidyverse)


ui <- fluidPage(
  h1("Word Cloud"),
  h4(tags$a(href = "https://www.antoinesoetewey.com/", "Antoine Soetewey (v1)")),
  h4(tags$a(href = "https://sites.google.com/site/nbreznau/", "Nate Breznau (v2)")),
  # Create a container for tab panels
  tabsetPanel(
    # Create a "Word cloud" tab
    tabPanel(
      title = "Word cloud",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = "source",
            label = "Word source",
            choices = c(
              "Example: I Have a Dream by MLK Jr" = "book",
              "Input your own text" = "own",
              "Upload a file (csv/txt)" = "file"
            )
          ),
          # Add the selector for the language of the text
          selectInput(
            inputId = "language",
            label = "Remove stopwords",
            choices = c("Do not remove", "Danish", "Dutch", "English", "Finnish", "French", "German", "Hungarian", "Italian", "Norwegian", "Portuguese", "Russian", "Spanish", "Swedish"),
            multiple = FALSE,
            selected = "Do not remove"
          ),
          checkboxInput("keep_punct", "Do not parse (keep punctuation, stopwords and spaces)", FALSE),
          conditionalPanel(
            condition = "input.source == 'own'",
            textAreaInput("text", "Enter text", rows = 7)
          ),
          # Wrap the file input in a conditional panel
          conditionalPanel(
            # The condition should be that the user selects
            # "file" from the radio buttons
            condition = "input.source == 'file'",
            fileInput("file", "Select a file")
          ),
          hr(),
          numericInput("num", "Number of words (minimum 3)",
                       value = 100, min = 3
          ),
          hr(),
          colourInput("col", "Background color", value = "white"),
          hr(),
          numericInput("fontsize", "Font size", value = 0.5, max = 3, step = 0.5),
          hr(),
          selectInput("palette", "Choose Colour palette",
                      list("Blues", "BuGn", "BuPu", "GnBu", "Greens", 
                           "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", 
                           "Purples", "RdPu", "Reds", "YlGn", 
                           "YlGnBu", "YlOrBr", "YlOrRd")),
          hr(),
          sliderInput("minRotation", "Minimum Rotation",
                      min = 0, max = 3.14159265359,
                      value = 0),
          sliderInput("maxRotation", "Maximum Rotation",
                      min = 0, max = 3.14159265359,
                      value = 1.5),
          checkboxInput("remove_words", "Remove specific words?", FALSE),
          conditionalPanel(
            condition = "input.remove_words == 1",
            textAreaInput("words_to_remove1", "Words to remove (one per line)", rows = 1)
          ),
          conditionalPanel(
            condition = "input.remove_words == 1 && input.words_to_remove1.length > 0",
            textAreaInput("words_to_remove2", "", rows = 1)
          ),
          conditionalPanel(
            condition = "input.remove_words == 1 && input.words_to_remove2.length > 0",
            textAreaInput("words_to_remove3", "", rows = 1)
          ),
          conditionalPanel(
            condition = "input.remove_words == 1 && input.words_to_remove3.length > 0",
            textAreaInput("words_to_remove4", "", rows = 1)
          ),
          conditionalPanel(
            condition = "input.remove_words == 1 && input.words_to_remove4.length > 0",
            textAreaInput("words_to_remove5", "", rows = 1)
          ),
          conditionalPanel(
            condition = "input.remove_words == 1 && input.words_to_remove5.length > 0",
            textAreaInput("words_to_remove6", "", rows = 1)
          ),
          conditionalPanel(
            condition = "input.remove_words == 1 && input.words_to_remove6.length > 0",
            textAreaInput("words_to_remove7", "", rows = 1)
          ),
          conditionalPanel(
            condition = "input.remove_words == 1 && input.words_to_remove7.length > 0",
            textAreaInput("words_to_remove8", "", rows = 1)
          ),
          conditionalPanel(
            condition = "input.remove_words == 1 && input.words_to_remove8.length > 0",
            textAreaInput("words_to_remove9", "", rows = 1)
          ),
          conditionalPanel(
            condition = "input.remove_words == 1 && input.words_to_remove9.length > 0",
            textAreaInput("words_to_remove10", "", rows = 1)
          ),

          hr(),
          HTML('<p>Report a <a href="https://github.com/AntoineSoetewey/word-cloud/issues">bug</a> or view the <a href="https://github.com/AntoineSoetewey/word-cloud">code</a>. See more information about this app in this <a href="https://statsandr.com/blog/draw-a-word-cloud-with-a-shiny-app/">article</a>.</p><p>Back to <a href="https://www.antoinesoetewey.com/">antoinesoetewey.com</a> or <a href="https://statsandr.com/">statsandr.com</a>.</p>')
        ),
        mainPanel(
          wordcloud2Output("cloud"),
          #plotOutput("cloud_save"),
          #downloadButton("cloud_save", "Save Cloud"),
          br(),
          br()
        )
      )
    ),
    # Create an "About this app" tab
    tabPanel(
      title = "About this app",
      br(),
      "Instructions on how to use this Shiny app:",
      br(),
      br(),
      HTML("<ul><p>The ideal file is a text file with word repetition = size of word in cloud.
       However, you can upload full-texts! Just be aware that this may lead to undesired words appearing.
       When uploading a file, make sure to upload a .csv or .txt file.
       If it is a .csv file, there should be only one column containing all words or sentences (see below for example files).
       Please note that if you want to include phrases and capital letters you need to tick the box 'Do not parse' (like the old Wordle website).
       Otherwise, numbers and punctuations will be automatically removed, as well as stop words in the language of your choice (via the dropdown selector)</p></ul>"),
      
      br(),
      br(),
      em("Source: DataCamp"),
      br(),
      br(),
      HTML('<p>Report a <a href="https://github.com/AntoineSoetewey/word-cloud/issues">bug</a> or view the <a href="https://github.com/AntoineSoetewey/word-cloud">code</a>. See more information about this app in this <a href="https://statsandr.com/blog/draw-a-word-cloud-with-a-shiny-app/">article</a>.</p><p>Back to <a href="https://www.antoinesoetewey.com/">antoinesoetewey.com</a> or <a href="https://statsandr.com/">statsandr.com</a>.</p>'),
      br(),
      br()
    )
  )
)

server <- function(input, output) {
  data_source <- reactive({
    if (input$source == "book") {
      data <- read.csv("ihaveadream.csv",
                       sep = "&",
                       stringsAsFactors = FALSE
      )
      data <- data[, 1]
      corpus <- Corpus(VectorSource(data))
      corpus <- tm_map(corpus, tolower)
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, removeNumbers)
        corpus <- tm_map(corpus, removeWords, stopwords(tolower("English")))
        corpus <- tm_map(corpus, removeWords, c(input$words_to_remove1))
        corpus <- tm_map(corpus, removeWords, c(input$words_to_remove2))
        corpus <- tm_map(corpus, removeWords, c(input$words_to_remove3))
        corpus <- tm_map(corpus, removeWords, c(input$words_to_remove4))
        corpus <- tm_map(corpus, removeWords, c(input$words_to_remove5))
        corpus <- tm_map(corpus, removeWords, c(input$words_to_remove6))
        corpus <- tm_map(corpus, removeWords, c(input$words_to_remove7))
        corpus <- tm_map(corpus, removeWords, c(input$words_to_remove8))
        corpus <- tm_map(corpus, removeWords, c(input$words_to_remove9))
        corpus <- tm_map(corpus, removeWords, c(input$words_to_remove10))
      tdm <- as.matrix(TermDocumentMatrix(corpus))
      data <- sort(rowSums(tdm), decreasing = TRUE)
      data <- data.frame(word = names(data), freq = as.numeric(data))
      
      
      ######### typed in text box (with parsing)
    } else if (input$source == "own" & input$keep_punct == FALSE) {
      data <- input$text
      corpus <- Corpus(VectorSource(data))
      corpus <- tm_map(corpus, tolower)
      if (input$language != "Do not remove") {
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, removeNumbers)
        corpus <- tm_map(corpus, removeWords, stopwords(tolower(input$language)))
        corpus <- tm_map(corpus, removeWords, c(input$words_to_remove1))
        corpus <- tm_map(corpus, removeWords, c(input$words_to_remove2))
        corpus <- tm_map(corpus, removeWords, c(input$words_to_remove3))
        corpus <- tm_map(corpus, removeWords, c(input$words_to_remove4))
        corpus <- tm_map(corpus, removeWords, c(input$words_to_remove5))
        corpus <- tm_map(corpus, removeWords, c(input$words_to_remove6))
        corpus <- tm_map(corpus, removeWords, c(input$words_to_remove7))
        corpus <- tm_map(corpus, removeWords, c(input$words_to_remove8))
        corpus <- tm_map(corpus, removeWords, c(input$words_to_remove9))
        corpus <- tm_map(corpus, removeWords, c(input$words_to_remove10))
      }
      tdm <- as.matrix(TermDocumentMatrix(corpus))
      data <- sort(rowSums(tdm), decreasing = TRUE)
      data <- data.frame(word = names(data), freq = as.numeric(data))
    
    ######### typed in text (without parsing)
    } else if (input$source == "own" & input$keep_punct == TRUE) {
      # need a df with 'word' and 'freq'
      data <- as.data.frame(unlist(strsplit(input$text,"\n")))
      colnames(data) <- "word"
      data <- data %>%
        group_by(word) %>%
        mutate(freq = n()) %>%
        ungroup() %>%
        select(word,freq)
      # one row per phrase
      data <- distinct(data) 
      data <- data[order(data$freq, decreasing = T),]
      
     ########## uploaded file (with parsing)
    } else if (input$source == "file" & input$keep_punct == FALSE) {
      data <- input_file() 
      corpus <- Corpus(VectorSource(data))
      corpus <- tm_map(corpus, tolower)
      if (input$language != "Do not remove") {
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, removeNumbers)
        corpus <- tm_map(corpus, removeWords, stopwords(tolower(input$language)))
        corpus <- tm_map(corpus, removeWords, c(input$words_to_remove1))
        corpus <- tm_map(corpus, removeWords, c(input$words_to_remove2))
        corpus <- tm_map(corpus, removeWords, c(input$words_to_remove3))
        corpus <- tm_map(corpus, removeWords, c(input$words_to_remove4))
        corpus <- tm_map(corpus, removeWords, c(input$words_to_remove5))
        corpus <- tm_map(corpus, removeWords, c(input$words_to_remove6))
        corpus <- tm_map(corpus, removeWords, c(input$words_to_remove7))
        corpus <- tm_map(corpus, removeWords, c(input$words_to_remove8))
        corpus <- tm_map(corpus, removeWords, c(input$words_to_remove9))
        corpus <- tm_map(corpus, removeWords, c(input$words_to_remove10))
      }
      tdm <- as.matrix(TermDocumentMatrix(corpus))
      data <- sort(rowSums(tdm), decreasing = TRUE)
      data <- data.frame(word = names(data), freq = as.numeric(data))
    } else if (input$source == "file" & input$keep_punct == TRUE) {
      data <- input_file()
      colnames(data) <- "word"
      data <- data %>%
        group_by(word) %>%
        mutate(freq = n()) %>%
        ungroup() %>%
        select(word,freq)
      # one row per phrase
      data <- distinct(data) 
      data <- data[order(data$freq, decreasing = T),]
    }
    
    return(data)
})

  

  
  create_wordcloud <- function(data, num_words = 100, background = "white", 
             col = brewer.pal(100, "Blues"), size = 1, minRotation = 0,
             maxRotation = 1.5) {
    
    # Make sure a proper num_words is provided
    if (!is.numeric(num_words) || num_words < 3) {
      num_words <- 3
    }
    
    # Grab the top n most common words
    data <- head(data, n = num_words)
    if (nrow(data) == 0) {
      return(NULL)
    }
    
  wordcloud2(data, backgroundColor = background,
             color = col,
             size = size,
             minRotation = minRotation,
             maxRotation = maxRotation)
  }
  output$cloud <- renderWordcloud2({
    create_wordcloud(data_source(),
                     num_words = input$num,
                     background = input$col,
                     col = rep_len(rev(c(brewer.pal(9, input$palette))), input$num), #paste0("brewer.pal(", 12, "\"", input$palette,"\""),
                     size = input$fontsize,
                     minRotation = input$minRotation,
                     maxRotation = input$maxRotation
    )
  })
  

}

shinyApp(ui = ui, server = server)