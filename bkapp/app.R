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

BrothersKaramazov |>
  filter(book_chapter == 11) |>
  filter(paragraph < min(paragraph) + 3) |>
  pull(text)

to_iter <- BrothersKaramazov |>
  filter(book_chapter == 11) |>
  filter(paragraph < min(paragraph) + 3) |>
  unnest_tokens(word, text, token = stringr::str_split, pattern = "[ \n]", to_lower = FALSE) |>
  left_join(anno, join_by(word == entity)) |>
  select(-gutenberg_id, -part, -book, -chapter, -book_chapter,
         -linenumber)

excerpt <- ""
for(i in 1:nrow(to_iter)) {
  if(i != 1) {
    if(to_iter$paragraph[i] > to_iter$paragraph[i-1]) {
      excerpt <- paste(excerpt, "<br>")
    }
  }
  if(!is.na(to_iter[i,3])) {
    excerpt <- paste(excerpt," <strong>",to_iter[i,2], "</strong>", sep = "")
  } else {
    excerpt <- paste(excerpt, to_iter[i,2])
  }
  if(i == 2) {
    excerpt <- paste(excerpt, "<br>")
  }
}

ui <- navbarPage("'The Brothers Karamazov'", 
    tabPanel(
        title = "About the book",
         mainPanel(
           p("'The Brothers Karamazov' is one of the most influential books in the world of literature.  Written from 1878 to 1880, it was the last work Fyodor Dostoyevsky would ever complete, and is widely regarded as his magnum opus.  The novel is comprised of 12 books and an epilogue, spanning roughly 800 pages.  Within each book we learn about the lives of several members of the Karamazov family, often hearing events from the perspective of everyone involved.  Each character has very different outlooks, such as Alyosha being a devout Christian, while his father is described as a 'sensualist.'  As such, the idea of seeing if we can separate the book into different 'topics' using Latent Dirichlet allocation is extremely appealing."),
           uiOutput("tab"),
           p("An excerpt from the book is presented below with names highlighted:"),
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
  
  foo <- eventReactive(input$makePlot, {
    to_plot <- generate_top_words(input$k_val,
                                  current_dtm(),
                                  12)
    generate_lda_plot(to_plot)
  })
  output$distPlot <- renderPlot(foo())
  
  foo2 <- eventReactive(input$makePlot, {
    title_string <- paste("Probability each", isolate(input$split), "is in a certain topic")
    bk_lda4 <- LDA(isolate(current_dtm()), k = isolate(input$k_val),
                   control = list(seed = 1821))
    bk_gamma <- tidy(bk_lda4, matrix = "gamma") |>
      mutate(document = as.numeric(document))
    if(nrow(bk_gamma) > 10000) {
       cat(nrow(bk_gamma), "foo")
       bk_gamma <- bk_gamma[sample(nrow(bk_gamma), 10000), ]
    }
    ggplot(bk_gamma, aes(x = document, y = gamma, color = factor(topic))) +
      geom_point(position = position_jitter(height = 0.02), alpha = 0.5) +
      geom_smooth(se = FALSE) +
      labs(x = paste(isolate(input$split)),
           y = "Gamma (Probability this Doc is this Topic)",
           title = title_string)
    
  })
  
  output$through_novel <- renderPlot(foo2())

  
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


