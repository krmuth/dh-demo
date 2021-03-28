library(dplyr)
library(gutenbergr)
library(tidytext)
library(stringr)
library(ggplot2)
library(tidyr)
library(wordcloud2)
library(igraph)
library(ggraph)


data(stop_words)


# Load and preprocess text ----

original_books <- gutenberg_download(c(370, 158, 1260, 1023, 219), meta_field = c("title"))

tidy_books <- original_books %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# Set subsets for for input selection

emma <- tidy_books %>%
  filter(title == "Emma")

bleak_house <- tidy_books %>%
  filter(title == "Bleak House")

heart_of_darkness <- tidy_books %>%
  filter(title == "Heart of Darkness")

jane_eyre <- tidy_books %>%
  filter(title == "Jane Eyre: An Autobiography")

moll_flanders <- tidy_books %>%
  filter(title == "The Fortunes and Misfortunes of the Famous Moll Flanders")



# User interface ----

 ui <- fluidPage(
   titlePanel("Intro to the Novel"),
   
   sidebarLayout(
     sidebarPanel(
       helpText("Explore public domain texts from the Level 1 module Intro to the Novel."),
       
       selectInput("var",
                   label = "Choose a novel to view.",
                   choices = c("All", "Moll Flanders", "Emma", "Jane Eyre", "Bleak House", "Heart of Darkness"),
                   selected = "All"),
     ),
     
     mainPanel(
       tabsetPanel(
         tabPanel("Wordcloud", 
                  wordcloud2Output("cloud"),
                  br(),
                  br(),
                  sliderInput("cloud_max",
                              label = "Number of words",
                              min = 10, max = 200, value = 100)),
         tabPanel("Common words",
                  plotOutput("plot"),
                  br(),
                  br(),
                  sliderInput("chart_max",
                              label = "Number of words",
                              min = 10, max = 30, value = 20)),
         tabPanel("Correlations", plotOutput("network")))
       )
     
   )
)
 

# Server logic ----

server <- function(input, output) {
  
  # Wordcloud tab ----
  
  output$cloud <- renderWordcloud2({
    
    # Select dataset
    
    books <- switch(input$var, 
                    "All" = tidy_books,
                    "Moll Flanders" = moll_flanders,
                    "Emma" = emma,
                    "Jane Eyre" = jane_eyre,
                    "Bleak House" = bleak_house,
                    "Heart of Darkness" = heart_of_darkness)
    
    # Count and order frequencies
    
    cloud_data <- books %>%
      count(word, sort = TRUE) %>%
      mutate(word = reorder(word, n)) %>%
      slice(1:input$cloud_max)
    
    # Render wordcloud
    
    wordcloud2(cloud_data, 
               size = .6, 
               fontFamily = "Helvetica", 
               fontWeight = "normal", 
               color = "#777777")
  })
  
  # Frequency tab ----
  
  output$plot <- renderPlot({
    
    # Select dataset
    
    books <- switch(input$var, 
                    "All" = tidy_books,
                    "Moll Flanders" = moll_flanders,
                    "Emma" = emma,
                    "Jane Eyre" = jane_eyre,
                    "Bleak House" = bleak_house,
                    "Heart of Darkness" = heart_of_darkness)
    
    # Count and sort frequencies, render bar chart
    
    books %>%
      count(word, sort = TRUE) %>%
      mutate(word = reorder(word, n)) %>%
      slice((1:input$chart_max))%>%
      ggplot(aes(n, word)) +
      geom_col() +
      labs(y = NULL)
  })
  
  # Bigram tab ----

  output$network <- renderPlot({
    
    # Prepare data for bigram graph
    
    books_bigrams <- original_books %>%
      unnest_tokens(bigram, text, token = "ngrams", n = 2)
    
    bigrams_separated <- books_bigrams %>%
      separate(bigram, c("word1", "word2"), sep = " ")
    
    bigrams_filtered <- bigrams_separated %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word)
    
    bigram_counts <- bigrams_filtered %>%
      count(word1, word2, sort = TRUE)
    
    bigram_graph <- bigram_counts %>%
      filter(n > 16) %>%
      graph_from_data_frame()
    
    bigram_graph %>%
      ggraph(layout = "fr") +
      geom_edge_link(color = "#c0c0c0") +
      geom_node_point(color = "#c0c0c0", size = 2) +
      geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
      theme_void()
  })
}

# Run app ----

shinyApp(ui, server)
