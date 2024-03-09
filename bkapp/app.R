library(shiny)
library(BrothersKaramazov)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(shinybusy)

generate_bkw <- function(grouping_var) {
  grouping_var <- enquo(grouping_var)
  BrothersKaramazov |>
    filter(book != 0) |>
    unnest_tokens(word, text, token = "words") |>
    anti_join(stop_words, join_by(word)) |>
    group_by(.data[[grouping_var]], word) |> 
    count() |>
    ungroup()
}

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
  topic_count <- input_top_words |> select(topic) |> unique() |> count()
  in_each <- nrow(input_top_words)/topic_count
  title_string <- paste0("Top ", in_each, " Words for each of ",
                         topic_count, " Topics")
                       
  to_return <- input_top_words |>
    mutate(term = reorder_within(term, beta, topic)) |>
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_y_reordered() +
    labs(x = "Beta", y = "Term", 
         title = title_string)
  return(to_return)
}

anno <- AnnotatedBK$entity |>
  filter(entity_type == "PERSON") |>
  group_by(entity) |>
  summarize(count = n()) |>
  filter(count > 10)

to_iter <- BrothersKaramazov |>
  filter(book_chapter == 11) |>
  filter(paragraph < min(paragraph) + 3) |>
  unnest_tokens(word, text, to_lower = FALSE) |>
  left_join(anno, join_by(word == entity)) |>
  select(-gutenberg_id, -part, -book, -chapter, -book_chapter,
         -linenumber)

excerpt <- ""
for(i in 1:nrow(to_iter)) {
  if(!is.na(to_iter[i,3])) {
    excerpt <- paste(excerpt," <strong>",to_iter[i,2], "</strong>", sep = "")
  } else {
    excerpt <- paste(excerpt, to_iter[i,2])
  }
  if(i == 2) {
    excerpt <- paste(excerpt, "<br>")
  }
  if(i != 1) {
    if(to_iter$paragraph[i] > to_iter$paragraph[i-1]) {
      excerpt <- paste(excerpt, "<br>")
    }
  }
}

ui <- navbarPage("'The Brothers Karamazov'", 
    tabPanel(
        title = "About the book",
         mainPanel(
           p("This is a sentence about the book."),
           uiOutput("tab"),
           tags$p(
             HTML(excerpt)
           )
           )),
    tabPanel(
      title = "Topic Modelling", 
      add_busy_spinner(spin = "cube-grid"),
      sidebarLayout(
        sidebarPanel(
          sliderInput("k_val",
                      "Number of topics:",
                      min = 2,
                      max = 9,
                      value = 4),
          selectizeInput("rem", "Add words to remove", "", 
                         multiple = TRUE, 
                         options = list(
                           'plugins' = list('remove_button'),
                           'create' = TRUE,
                           'persist' = TRUE
                         )),
          radioButtons("split", "How to divide up the book:",
                       choices = tail(names(BrothersKaramazov), 6)),
          actionButton("makePlot", "Make the plot!")
          
        ),
        mainPanel(
          plotOutput("distPlot"),
          plotOutput("through_novel")
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
    a <- input$rem |>
      unlist() |>
      str_trim()
    
    b <- tibble(word = a)
    
    to_return <- generate_bkw(input$split) |>
      anti_join(b, join_by(word)) 
    colnames(to_return) <- c("book", "word", "n")   # not good coding
    to_return <- to_return |>
      cast_dtm(book, word, n)
    return(to_return)
  })
  uSIE <- reactive({
    temp <- input$e_or_t
    updateSelectInput(session, "specific",
                        choices = unique(AnnotatedBK$entity$entity_type)
      )
  })
  uSIT <- reactive({
    temp <- input$e_or_t
    updateSelectInput(session, "specific",
                      choices = unique(AnnotatedBK$token$upos)
    )
  })
  observeEvent(input$makePlot, {
    output$distPlot <- renderPlot({
      to_plot <- generate_top_words(input$k_val, current_dtm(), 12)
      generate_lda_plot(to_plot)
    })
    output$through_novel <- renderPlot({
      bk_lda4 <- LDA(current_dtm(), k = 4,
                     control = list(seed = 1821))
      bk_gamma <- tidy(bk_lda4, matrix = "gamma") |>
        mutate(document = as.numeric(document))
      
      ggplot(bk_gamma, aes(x = document, y = gamma, color = factor(topic))) +
        geom_point(position = position_jitter(height = 0.02), alpha = 0.5) +
        geom_smooth(se = FALSE) +
        labs(x = paste(input$split), y = "Gamma (Probability this Doc is this Topic)", title = "erm")
        
    })
  })
  
  output$nameTable <- renderDataTable({
    if(input$e_or_t == "entity") {
      uSIE()
      temp <- AnnotatedBK$entity |>
        filter(entity_type == input$specific) |>
        group_by(entity) |>
        summarize(count = n(), .groups = "drop") |>
        arrange(desc(count))
        
    } else if(input$e_or_t == "token") {
      uSIT()
      temp <- AnnotatedBK$token |>
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

shinyApp(ui = ui, server = server)


