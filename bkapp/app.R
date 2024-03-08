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
  to_return <- input_top_words |>
    mutate(term = reorder_within(term, beta, topic)) |>
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_y_reordered()
  return(to_return)
}

one_chap <- BrothersKaramazov |>
  filter(book_chapter == 11) |>
  filter(paragraph < min(paragraph) + 3) |>
  unnest_tokens(word, text, to_lower = FALSE) 

a <- AnnotatedBK$entity |>
  filter(entity_type == "PERSON") |>
  group_by(entity) |>
  summarize(count = n()) |>
  filter(count > 10)

a <- left_join(one_chap, a, join_by(word == entity))
c <- a |>
  select(-gutenberg_id, -part, -book, -chapter, -book_chapter,
         -linenumber)
s <- ""
for(i in 1:nrow(c)) {
  if(!is.na(c[i,3])) {
    s <- paste(s ," <strong>",c[i,2], "</strong>", sep = "")
  } else {
    s <- paste(s, c[i,2])
  }
  if(i == 2) {
    s <- paste(s, "<br>")
  }
  if(i != nrow(c)) {
    cat(c$paragraph[i])
    if(c$paragraph[i] < c$paragraph[i+1]) {
      s <- paste(s, "<br>")
    }
  }
}
DELETE2 <- s
  
ui <- navbarPage("'The Brothers Karamazov'", 
    tabPanel(
        title = "About the book",
         mainPanel(
           p("This is a sentence about the book."),
           uiOutput("tab"),
           tags$p(
             HTML(DELETE2)
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
          selectizeInput("rem", "Add words to remove", "", 
                         multiple = TRUE, 
                         options = list(
                           'plugins' = list('remove_button'),
                           'create' = TRUE,
                           'persist' = TRUE
                         )),
          radioButtons("delete21", "uhhhh",
                       choices = tail(names(BrothersKaramazov), 6)),
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
    a <- input$rem |>
      unlist() |>
      str_trim()
    
    b <- tibble(word = a)
    
    to_return <- generate_bkw(input$delete21) |>
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
  })
  output$nameTable <- renderDataTable({
    if(input$e_or_t == "entity") {
      uSIE()
      temp <- AnnotatedBK$entity |>
        filter(entity_type == input$specific) |>
        group_by(entity) |>
        summarize(count = n(), avg_section = mean(doc_id), .groups = "drop") |>
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

# Run the application 
shinyApp(ui = ui, server = server)