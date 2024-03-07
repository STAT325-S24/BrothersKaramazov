library(shiny)
library(BrothersKaramazov)
library(tidyverse)
library(tidytext)
library(topicmodels)

BrothersKaramazov_words <- BrothersKaramazov |>
  filter(book != 0) |>
  unnest_tokens(word, text, token = "words") |>
  anti_join(stop_words, join_by(word)) |>
  group_by(book, word) |> 
  count() |>
  ungroup()


generate_top_words <- function(k_val, input_dtm, words_to_display) {
  bk_lda <- LDA(input_dtm, k = k_val, control = list(seed = 325))
  
  bk_topics <- tidy(bk_lda, matrix = "beta")
  
  bk_top_terms <- bk_topics |>
    group_by(topic) |>
    slice_max(beta, n = words_to_display) |> 
    ungroup() |>
    arrange(topic, -beta)
  return(bk_top_terms)
}

generate_lda_plot <- function(input_top_words) {
  to_return <- input_top_words |>
    mutate(term = reorder_within(term, beta, topic)) |>
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_y_reordered()
  return(to_return)
}


DELETE <- BrothersKaramazov |>
  filter(book_chapter == 12) |>
  select(text) |>
  head(6) |>
  pull(text) |>
  paste(collapse = " ", sep = "<br/>")
  


# Define UI for application that draws a histogram
ui <- navbarPage("'The Brothers Karamazov'", 
    tabPanel(
        title = "About the book",
         mainPanel(
           p("This is a sentence about the book."),
           uiOutput("tab"),
           wellPanel(
             DELETE
             
           )
           )),
    tabPanel(
      title = "Topic Modelling", 
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
        sidebarPanel(
          sliderInput("k_val",
                      "Number of topics:",
                      min = 2,
                      max = 9,
                      value = 4),
          textInput("rem",
                    "Remove these words (separate by commas):"),
          actionButton("makePlot", "Make the plot:")
          
        ),
        mainPanel(
          plotOutput("distPlot")
        )
      )
    
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  current_dtm <- reactive({
    a <- strsplit(input$rem, ",") |>
      unlist() |>
      str_trim()
    b <- tibble(word = a)
    to_return <- BrothersKaramazov_words |>
      anti_join(b, join_by(word)) |>
      cast_dtm(book, word, n)
    return(to_return)
  })
  
  observeEvent(input$makePlot, {
    output$distPlot <- renderPlot({
      to_plot <- generate_top_words(input$k_val, current_dtm(), 12)
      generate_lda_plot(to_plot)
    })
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)