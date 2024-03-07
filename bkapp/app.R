library(shiny)
library(BrothersKaramazov)
library(tidyverse)
library(tidytext)
library(topicmodels)

file_name <- "~/GitHub/BrothersKaramazov/data/anno_bk.Rds"
stopifnot(file.exists(file_name))
anno <- readRDS(file_name)

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
    
    ),
    tabPanel(
      title = "Named entity",
      sidebarLayout(
        sidebarPanel(
          radioButtons("e_or_t",
                      "Entity or Token:",
                      c("entity", 
                        "token")),
          selectInput("specific",
                      "Choose a specific type/entity:",
                      c())
        ),
        mainPanel(
          dataTableOutput("nameTable"),
          
        )
      )
      
    )
)

server <- function(input, output, session) {
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
  uSIE <- reactive({
    updateSelectInput(session, "specific",
                        choices = unique(anno$entity$entity_type)
      )
  })
  uSIT <- reactive({
    updateSelectInput(session, "specific",
                      choices = unique(anno$token$upos)
    )
  })
  
  
  observeEvent(input$makePlot, {
    output$distPlot <- renderPlot({
      to_plot <- generate_top_words(input$k_val, current_dtm(), 12)
      generate_lda_plot(to_plot)
    })
  })
  
  output$nameTable <- renderDataTable({
    if(input$e_or_t == "entity") {
      uSIE()
      temp <- anno$entity |>
        filter(entity_type == input$specific) |>
        group_by(entity) |>
        summarize(count = n(), avg_section = mean(doc_id), .groups = "drop") |>
        arrange(desc(count))
        
    } else if(input$e_or_t == "token") {
      uSIT()
      temp <- anno$token |>
        filter(upos == input$specific) |>
        group_by(lemma) |>
        summarize(count = n(), .groups = "drop") |>
        arrange(desc(count))
    } else {
      cat("fail 0 shouldnt reach line 130")
    }
    return(temp)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)