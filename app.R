library(dplyr)
library(curl)
library(gutenbergr)
library(tidytext)
library(ggplot2)
library(wordcloud2)
library(igraph)
library(ggraph)
library(widyr)

data(stop_words)

# Load and preprocess text ----

original_books <- gutenberg_download(c(370, 158, 1260, 1023, 219), meta_field = c("title"))

tidy_books <- original_books %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# Set subsets for for input selection

tidy_emma <- tidy_books %>%
  filter(title == "Emma")  

tidy_bleak_house <- tidy_books %>%
  filter(title == "Bleak House")

tidy_heart_of_darkness <- tidy_books %>%
  filter(title == "Heart of Darkness") 

tidy_jane_eyre <- tidy_books %>%
  filter(title == "Jane Eyre: An Autobiography") 

tidy_moll_flanders <- tidy_books %>%
  filter(title == "The Fortunes and Misfortunes of the Famous Moll Flanders") 


# User interface ----

 ui <- fluidPage(
   titlePanel("Intro to the Novel"),
   
   sidebarLayout(
     sidebarPanel(
       helpText("Explore public domain texts from the Level 1 module Intro to the Novel."),
       
       selectInput("var",
                   label = "Choose a novel to view.",
                   choices = c("Moll Flanders", "Emma", "Jane Eyre", "Bleak House", "Heart of Darkness"),
                   selected = "Moll Flanders"),
       br(),
       br(),
       br(),
       br(),
       br(),
       br(),
       br(),
       div(p(strong("Built by"), a("Katie Muth", href = "https://github.com/krmuth"), "with", a("RStudio,", href = "https://www.rstudio.com/"), a("Shiny,", href = "https://shiny.rstudio.com/"), "and", a("Project Gutenberg", href = "https://www.gutenberg.org/")),
           p(strong("R Packages:"), "dplyr, gutenbergr, tidytext, ggplot2, wordcloud2, igraph, ggraph, widyr"),
           ) #close div
     ), # close sidebarPanel
     
     mainPanel(
       tabsetPanel(
         tabPanel("Wordcloud", 
                  wordcloud2Output("cloud"),
                  br(),
                  sliderInput("cloud_max",
                              label = "Number of words",
                              min = 10, max = 200, value = 100)),
         tabPanel("Common words",
                  plotOutput("plot"),
                  br(),
                  sliderInput("chart_max",
                              label = "Number of words",
                              min = 10, max = 30, value = 20)),
         tabPanel("Correlations", plotOutput("pairs")),
         tabPanel("Clusters", plotOutput("network")) 


        ) # close tabsetPanel
       ) # close mainPanel
     
   ) # close sidebarLayout
) # close fluidPage
 

# Server logic ----

server <- function(input, output) {
  
  # Wordcloud tab ----
  
  output$cloud <- renderWordcloud2({
    
    # Select dataset
    
    books <- switch(input$var, 
                    "Moll Flanders" = tidy_moll_flanders,
                    "Emma" = tidy_emma,
                    "Jane Eyre" = tidy_jane_eyre,
                    "Bleak House" = tidy_bleak_house,
                    "Heart of Darkness" = tidy_heart_of_darkness)
    
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
                    "Moll Flanders" = tidy_moll_flanders,
                    "Emma" = tidy_emma,
                    "Jane Eyre" = tidy_jane_eyre,
                    "Bleak House" = tidy_bleak_house,
                    "Heart of Darkness" = tidy_heart_of_darkness)
    
    # Count and sort frequencies, render bar chart
    
    books %>%
      count(word, sort = TRUE) %>%
      mutate(word = reorder(word, n)) %>%
      slice((1:input$chart_max))%>%
      ggplot(aes(n, word)) +
      geom_col() +
      labs(y = NULL)
  })
  
  # Correlation tab ----
  
  output$pairs <- renderPlot({
    
    # Select dataset
  
  books <- switch(input$var,
                  "Moll Flanders" = tidy_moll_flanders,
                  "Emma" = tidy_emma,
                  "Jane Eyre" = tidy_jane_eyre,
                  "Bleak House" = tidy_bleak_house,
                  "Heart of Darkness" = tidy_heart_of_darkness)

  section_words <- books %>%
    mutate(section = row_number() %/% 10) %>%
    filter(section > 0)

  word_cors <- section_words %>%
    group_by(word) %>%
    filter(n() >= 20) %>%
    pairwise_cor(word, section, sort = TRUE)
  
  word_cors %>%
    filter(item1 %in% c("time", "day", "looked", "house")) %>%
    group_by(item1) %>%
    slice(1:6) %>%
    ungroup() %>%
    mutate(item2 = reorder(item2, correlation)) %>%
    ggplot(aes(item2, correlation)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ item1, scales = "free") +
    coord_flip()

  })




  # Cluster tab ----
  
  output$network <- renderPlot({
    
    # Select dataset
    
    books <- switch(input$var,
                    "Moll Flanders" = tidy_moll_flanders,
                    "Emma" = tidy_emma,
                    "Jane Eyre" = tidy_jane_eyre,
                    "Bleak House" = tidy_bleak_house,
                    "Heart of Darkness" = tidy_heart_of_darkness)
    
    section_words <- books %>%
      mutate(section = row_number() %/% 10) %>%
      filter(section > 0)
    
    word_cors <- section_words %>%
      group_by(word) %>%
      filter(n() >= 20) %>%
      pairwise_cor(word, section, sort = TRUE)
    
    word_cors %>%
      filter(correlation > .15) %>%
      graph_from_data_frame() %>%
      ggraph(layout = "fr") +
      geom_edge_link(aes(edge_alpha = correlation), color = "#c0c0c0", show.legend = FALSE) +
      geom_node_point(color = "#c0c0c0", size = 2) +
      geom_node_text(aes(label = name), repel = TRUE) +
      theme_void()
    
  })
  
  
}

# Run app ----

shinyApp(ui, server)
