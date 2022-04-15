##
## Daily Mail Emotion Analysis Shiny App
##

# load libraries
library(rvest)
library(dplyr)
library(tidyr)
library(tidytext)
library(tidyverse)
library(stopwords)
library(xml2)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(fresh)
library(showtext)
library(plotly)
library(stringr)
library(textdata)

# create functions
'%!in%' <- function(x,y)!('%in%'(x,y))

scrape_data <- function (x) {
  y <- read_html(x) %>%
    html_nodes("p") %>%
    html_text()
  return(y)
}

load_data <- function() {
  
  # stopwords
  stopwords <- c(data_stopwords_nltk$en, "one", " ", "", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "said", "day", "many", "around", "right", "another", "left", "opted", "meanwhile", "years", "january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "decemeber", "return", "today", "together", "could", "also", "last", "would", "should", "still", "back", "told", "per", "cent", "pictured", "first", "ms", "mr", "miss", "mrs", "year", "added", "made", "dame", "never", "say", "since", "like", "get", "later", "among", "found", "way", "including", "think", "met", "know", "old", "new")
  
  # list of nodes
  dailymail_rss <- "https://www.dailymail.co.uk/news/index.rss"
  nodes <- dailymail_rss%>%read_xml(options = "NOCDATA")%>%html_nodes('item')
  
  # get list of healines and links
  links_df <- map_df(nodes, function(item) {
    data.frame(title = str_squish(item%>%html_node('title') %>% html_text()),
               link = str_squish(item%>%html_node('link') %>%
                                   html_text()),
               stringsAsFactors=FALSE)
  }) %>% head(20)
  
  #links_df <- links_df %>% filter(grepl('Boris|Rishi', title))
  
  all_words <- data.frame()
  
  for (i in 1:length(links_df$link)) {
    # scrape article
    article_text <- scrape_data(links_df$link[i])
    
    # remove headline adverts by removing duplicated lines
    article_line <- data.frame(article_text = article_text[7:(length(article_text) - 13)]) %>% 
      mutate(headline = links_df[i, 1],
             article_id = i,
             line_id = seq(1:length(article_text)),
             line_no_punc = gsub('[^A-Za-z0-9 ]', "", trimws(article_text)),
             word_count = nchar(gsub('[^ ]', "", line_no_punc)) + 1) %>%
      group_by(article_text) %>% 
      mutate(n = n()) %>% 
      filter(n == 1) %>% 
      select(-n) %>% 
      mutate(article_text = gsub('[^A-Za-z0-9 ]', "", trimws(article_text)))
    
    word_it <- article_line %>%
      mutate(word = strsplit(article_text, " "),
      ) %>%
      unnest(word)
    
    word_it <- as.data.frame(word_it) %>% select(-article_text)
    
    if (i == 1) {
      all_words <- word_it
    } else {
      all_words <- rbind(all_words, word_it)
    }
    
  }
  
  all_words_id <- all_words %>% 
    group_by(article_id, line_id) %>% 
    mutate(word_id = row_number()) %>% 
    group_by(article_id) %>% 
    mutate(line_count = length(unique(line_id)))
  
  nrc_all <- read.csv("www/nrc.csv") 
  
  # identify words that appear in the emotion lexicon
  test_sent <- all_words_id %>%
    mutate(joy = as.numeric(word %in% nrc_all$word[nrc_all$sentiment == "joy"]),
           sadness = as.numeric(word %in% nrc_all$word[nrc_all$sentiment == "sadness"]),
           fear = as.numeric(word %in% nrc_all$word[nrc_all$sentiment == "fear"]),
           anger = as.numeric(word %in% nrc_all$word[nrc_all$sentiment == "anger"]),
           disgust = as.numeric(word %in% nrc_all$word[nrc_all$sentiment == "disgust"]),
           total = joy + sadness + fear + anger + disgust) %>%
    mutate(joy = joy / total,
           sadness = sadness / total,
           fear = fear / total,
           anger = anger / total,
           disgust = disgust / total) %>%
    select(-total) %>%
    pivot_longer(c(joy, sadness, fear, anger, disgust), names_to = "emotion", values_to = "values")
  
  return(test_sent)
}

by_article <- function(x) {
  word_perc <- x %>%
    mutate(word_perc = word_id / word_count,
           val_perc = values / line_count)
  
  word_perc$values[is.na(word_perc$word_perc)] <- 0
  word_perc <- word_perc %>% group_by(article_id) %>% mutate(article_val = sum(values, na.rm = TRUE))
  word_perc <- as.data.frame(word_perc)
  
  hide("loading_page")
  show("main_content")
  
  return(word_perc)
}

gfont <- "Poppins"
pal = c("anger" = "#b31d2a", 
        "disgust" = "#6fb329", 
        "sadness" = "#4b6fb3", 
        "joy" = "#d5950e", 
        "fear" = "#8a2bae")

jscode <-
  '$(document).on("shiny:connected", function(e) {
  var jsWidth = window.innerWidth;
  Shiny.onInputChange("GetScreenWidth",jsWidth);
});
'

## need to add a 'www' folder to store images - not sure where that's supposed to be
## something to do with a proper file structure - do more research

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  tags$script(jscode),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "news.css")
  ),
  
  use_googlefont(gfont),
  use_theme(create_theme(
    theme = "default",
    bs_vars_font(
      family_sans_serif = gfont
    )
  )),
  
  # App title
  titlePanel("The Daily Emotions"),
  
  # Sidebar layout
  sidebarLayout(
    useShinyjs(),
    
    div(class = "sidebar",
    
    # Sidebar panel descriptions, link & warning
    sidebarPanel(
      tags$head(tags$style(HTML("a {color: black;}"))),
      
      "The Daily Emotions tool scrapes the 20 most recent articles on The Daily Mail website, and analyses the language used within each article to extract emotional associations.",tags$br(),tags$br(),
      "The NRC Word-Emotion Association Lexicon is used to identify words with an association to range of core human emotions - words contained within the Lexicon are assigned relevant values for each associated emotion, and the totals of these values are displayed in the charts.",tags$br(),tags$br(),
      textOutput(outputId = "page_width"),
      tags$a(
        href="https://www.github.com/filmicaesthetic", 
        tags$img(src="https://cdn.iconscout.com/icon/free/png-256/github-3215409-2673827.png", 
                 title="Github", 
                 width="40",
                 height="40",
                 style="filter: invert(80%)")
      ),
      useShinyjs(),
      div(class = "warning",
            h4("WARNING"),
            "Please be aware that The Daily Mail 'generally fails to meet basic standards of accuracy and accountability', ",tags$a(href = "https://www.theguardian.com/media/2019/jan/23/dont-trust-daily-mail-website-microsoft-browser-warns-users", "according to Microsoft"), ", and headlines may contain offensive or upsetting language.")
        ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      useShinyjs(),
      div(
        id = "loading_page",
        class = "loading_page",
        img(src="loading_long.gif", align = "center", height = 40, width = 80, style = "margin-bottom: 5px;"), tags$br(),
        "Analysing articles..."
      ),
      hidden(
        div(
          id = "main_content",
          div(class = "plot_title",
              "The Daily Mail says you should feel..."), tags$br(),
          # Output: Charts
          plotlyOutput(outputId = "emotion_plot", height = 150),
          div(class = "plot_title",
              "20 Most Recent Articles Ordered by Level\nof Emotional Language Used"),tags$br(),
          plotOutput(outputId = "article_plot")
        )
      )
      )
    )
  )
)



# Define server
server <- function(input, output, session) {
  
  font_add_google(gfont, gfont)
  showtext_auto()
  
  emotion_data <- load_data()
  article_data <- by_article(emotion_data)
  
  observe({
    cat(input$GetScreenWidth)
  })
  
  
  output$emotion_plot <- renderPlotly({
    
    plot <- emotion_data %>% 
      group_by(emotion) %>%
      summarise(values = sum(values, na.rm = TRUE)) %>% 
      mutate(emotion = fct_reorder(emotion, values)) %>%
      ggplot(aes(y = values)) +
      geom_col(aes(x = 1, fill = emotion, text = paste0(str_to_title(emotion),"\n", round((values / sum(emotion_data$values, na.rm = TRUE)) * 100, 0), "%")), position = position_stack()) +
      labs(fill = NULL) +
      coord_flip() +
      scale_fill_manual(values = pal) +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text = element_blank(),
            axis.title = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 22),
            text = element_text(family = gfont),
            panel.grid = element_blank())
    
    ggplotly(plot, tooltip = c("text")) %>% config(displayModeBar = F) %>% layout(hoverlabel = list(font = "Poppins", align = "center", size = 40)) 
    
    
  })
  
  output$article_plot <- renderPlot({
    
    # emotion chart by article
    article_data %>% filter(values > 0) %>%
      mutate(headline = fct_reorder(as.factor(headline), -article_val)) %>%
      ggplot(aes(x = emotion, y = values)) +
      geom_col(aes(fill = emotion)) +
      scale_fill_manual(values = pal) +
      facet_wrap(~headline, labeller = labeller(headline = label_wrap_gen(30 + input$GetScreenWidth / 150)), ncol = ifelse(input$GetScreenWidth > 500, 2, 1)) +
      theme_minimal() +
      labs(fill = NULL) +
      theme(strip.text = element_text(size = 10),
            legend.position = "top",
            axis.text = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.spacing = unit(1, "cm"),
            axis.title = element_blank(),
            plot.title = element_text(size = 16, hjust = 0.5),
            text = element_text(family = gfont))
    
  }, height = reactive(1650 + (as.numeric(input$GetScreenWidth <= 500) * 1650)))
  
}

shinyApp(ui, server)
