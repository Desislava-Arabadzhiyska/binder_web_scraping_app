#This app goes through the main Reddit page for the First Law series and collects
#comments in order to do a basic analysis of the sentiments associated with the text
#Parts of this script are based on the tutorial by Samer Hijjazi (https://www.youtube.com/watch?v=U1BrIPmhx10)
#other parts are based on the Data Science Dojo tutorial (https://www.youtube.com/watch?v=NwtxrbqE2Gc)
#as well as Cosima Meyer's tutorial (https://www.youtube.com/watch?v=bvqur70ZmyM)
#https://mybinder.org/v2/gh/Desislava-Arabadzhiyska/binder_web_scraping_app/main?urlpath=shiny/ShinyApp/
#Load packages
library("tidyverse")#for data tidying etc.
library("RSelenium")#for interacting with the webpage
library('sentimentr') # for sentiment analysis
library('quanteda') #further text mining and nlp analysis
library('quanteda.textplots') # for textplots
library('wesanderson') #nicer visualization
library('stm') #structural topic model
library('shiny')

# Define UI for application that draws a histogram
ui <- fluidPage(
  skin = "red", #selects theme
  # Application title
  titlePanel("Get an initial look into 2 Reddit communities"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("w1", label = "Reddit Page 1", width = "100%", value = "https://www.reddit.com/r/TheFirstLaw/"),
      textInput("w2", label = "Reddit Page 2", width = "100%", value = "https://www.reddit.com/r/Cosmere/"),
      numericInput("desired_n_posts", "Posts per website", min = 0, value = 10),
      actionButton("go", "Scrape"),
      # uiOutput("actual_results")
      numericInput("how_many_comments", "Display top n comments from sentiment analysis", min = 1, value = 5),
      numericInput("n_topics", "Topics to estimate", min = 2, value = 3),
      numericInput("how_many_on_topic_plot", "Words per topic", min = 3, value = 5),
      numericInput("which_topic", "Display topic", min = 1, value = 1),
      numericInput("n_display_coms_per_top", "Display top n comments from selected topic", min = 1, value = 3)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      #plotOutput(outputId = "cloud")#,
      tags$p('Most positive comments from Website 1, Type1'),
      verbatimTextOutput("pos1type1"), 
      tags$p('Most positive comments from Website 2, Type1'),
      verbatimTextOutput("pos2type1"), 
      tags$p('Most negative comments from Website 1, Type1'),
      verbatimTextOutput("neg1type1"), 
      tags$p('Most negative comments from Website 2, Type1'),
      verbatimTextOutput("neg2type1"), 
      tags$p('Average Sentiment score 1'),
      verbatimTextOutput("score1type1"), 
      tags$p('Average Sentiment score 2'),
      verbatimTextOutput("score2type1"),
      tags$p('Word cloud Website 1'),
      plotOutput(outputId = "cloud1"), 
      tags$p('Frequency Plot Website 1'),
      plotOutput(outputId = "freq_p1"), 
      tags$p('Plot of Topics Website 1'),
      plotOutput(outputId = "topics_p1"), 
      tags$p('Texts of Topics Website 1'),
      verbatimTextOutput("topic_comments_text1"),
      tags$p('Word cloud Website 2'),
      plotOutput(outputId = "cloud2"),
      tags$p('Frequency Plot Website 2'),
      plotOutput(outputId = "freq_p2"), 
      tags$p('Plot of Topics Website 2'),
      plotOutput(outputId = "topics_p2"), 
      tags$p('Texts of Topics Website 2'),
      verbatimTextOutput("topic_comments_text2"),
      tags$p('Most positive comments from Website 1, Type2'),
      verbatimTextOutput("pos1type2"), 
      tags$p('Most positive comments from Website 2, Type2'),
      verbatimTextOutput("pos2type2"), 
      tags$p('Most negative comments from Website 1, Type2'),
      verbatimTextOutput("neg1type2"), 
      tags$p('Most negative comments from Website 2, Type2'),
      verbatimTextOutput("neg2type2"), 
      tags$p('Average Sentiment score 1, Type2'),
      verbatimTextOutput("score1type2"), 
      tags$p('Average Sentiment score 2, Type2'),
      verbatimTextOutput("score2type2")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  cs <- reactiveValues()
  each_sent_score <- reactiveValues()
  mydfm <- reactiveValues()
  freq_dat <- reactiveValues()
  model.stm <- reactiveValues()
  reduced_comments <- reactiveValues()
  dat <- reactiveValues()
  mydfmstm <- reactiveValues()
  
  observeEvent(input$go, {
    
    xtra_words <- c("much", "can", "just", "get", "also", "may", "really", "make", "go", "thing")#to be removed
    
    
    rs_driver_object <- rsDriver(browser = 'firefox',
                                 port = 4445L)#
    #activate client
    remDr <- rs_driver_object$client
    
    for (w in 1:2){
      #Navigate to website
      if (w == 1){
        remDr$navigate(input$w1)}
      else if (w == 2){
        remDr$navigate(input$w2)
      }
      Sys.sleep(5)
      
      #When you open the page you will likely be met by a pop-up asking about cookies - we want to reject the non-essential ones
      if (w == 1){
        tryCatch(
          {
            rejNE <- remDr$findElement(using = "xpath", '//*[text()="Reject non-essential"]')
            rejNE$clickElement()
            message("Successfully rejected cookies.")
          },
          error=function(e){}
        )
      }
      
      #We want to look at entries based on date, so we want to order based on the 'New' tab
      new_page_object <- remDr$findElement(using = 'link text', 'New')#specify how you would find the element e.g. classname, id etc.
      
      # Click the object to arrange the entries based on time of posting
      new_page_object$clickElement() 
      
      #If we leave it like this, it will only return the first couple of posts.
      #We would like to work with a few more examples, so we want to scroll further down.
      # For this we need javascript commands
      
      #scroll to the end of the webpage. Note that because reddit does not technically have an
      #end to the page, we might need to scroll a few times
      entries <- c()
      while (length(entries) < input$desired_n_posts){
        entries <- remDr$findElements(using = "class name", "SQnoC3ObvgnGjWt90zD9Z")#this class excludes promo materials, which is nice
        remDr$executeScript("window.scrollTo(0, document.body.scrollHeight);")
        Sys.sleep(5)
      }
      
      entries <- entries[1:input$desired_n_posts]
      
      # get ids and links
      
      comments <- c()
      lnks <- c()
      for (i in 1:length(entries)){
        lnks[i] <- entries[[i]]$getElementAttribute('href')
      }
      
      for (i in 1:length(lnks)){
        
        print(paste0('This is website ', as.character(w), ', entry ', as.character(i), ' out of ', as.character(length(lnks))))
        
        
        remDr$navigate(unlist(lnks[i]))
        Sys.sleep(1)
        
        #at this point we might run into spoilers, which we want to uncover
        check_for_spoilers <- tryCatch(
          {
            rejNE <- remDr$findElement(using = "xpath", '//*[text()="Click to see spoiler"]')
            rejNE$clickElement()
            message("Successfully uncovered spoilers.")
          },
          error=function(e){}
        )
        
        #the comments are listed with the following class name
        entr <-remDr$findElements(using = 'class name', "uI_hDmU5GSiudtABRz_37")
        t <- entr[[1]]
        
        t2 <- t$findChildElements(using = 'class name', '_1qeIAgB0cPwnLhDF9XSiJM')
        if (length(t2)>0){
          for (j in 1:length(t2)){
            comments <- append(comments, t2[[j]]$getElementText()[[1]])
          }
        }
      }
      
      comments <- unique(comments)
      comments <- comments[!grepl('Your comment has been removed', comments)]
      if (w == 1) {
        cs$a <- comments
        each_sent_score$a <- sentiment(get_sentences(comments))%>% filter(!is.na(word_count))%>% arrange(by = sentiment)
        
      }
      else if (w == 2){
        cs$b <- comments
        each_sent_score$b <- sentiment(get_sentences(comments))%>% filter(!is.na(word_count))%>% arrange(by = sentiment)
        
      }
    }
    
    remDr$close()
    #Kill the server when you're done
    system('taskkill /im java.exe /f')
    
    
    output$neg1type1 <- renderPrint({
      cs$a[c(each_sent_score$a[1:input$how_many_comments, 1][[1]])]
    })
    output$pos1type1 <- renderPrint({
      cs$a[c(each_sent_score$a[(dim(each_sent_score$a)[1]):(dim(each_sent_score$a)[1]-(input$how_many_comments-1)), 1][[1]])]
    })
    output$score1type1 <- renderPrint({
      mean(each_sent_score$a$sentiment)*100
    })
    
    output$neg2type1 <- renderPrint({
      cs$b[c(each_sent_score$b[1:input$how_many_comments, 1][[1]])]
    })
    output$pos2type1 <- renderPrint({
      cs$b[c(each_sent_score$b[(dim(each_sent_score$b)[1]):(dim(each_sent_score$b)[1]-(input$how_many_comments-1)), 1][[1]])]
    })
    output$score2type1 <- renderPrint({
      mean(each_sent_score$b$sentiment)*100
      
    })
    #Basic sentiment analysis
    
    #More text mining and analysis
    
    #####################################
    #####################################
    #####################################
    #####################################
    #####################################
    #####################################
    #####################################
    #####################################
    #####################################
    #####################################
    #####################################
    #####################################
    
    mycorpus <- corpus(cs$a)
    docvars(mycorpus, 'Textno') <- sprintf("%02d", 1:ndoc(mycorpus))
    
    toks <- tokens(mycorpus,
                   remove_numbers = TRUE,
                   remove_punct = TRUE,
                   remove_symbols = TRUE,
                   remove_url = TRUE,
                   split_hyphens = TRUE,
                   include_docvars = TRUE)
    
    toks_cl <- tokens_select(toks, #further cleaning
                             c("[\\d-]", "[[:punct:]]", "^.{1,2}$"),
                             selection = "remove",
                             valuetype = "regex",
                             verbose = TRUE)%>%
      tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma)#here instead of stemming we're looking at the lemmas
    
    mydfm$a <- dfm(toks_cl,
                   tolower = TRUE)%>%
      dfm_remove(c(xtra_words, stopwords("english")))
    
    freq_feature <- topfeatures(mydfm$a, 30)
    
    freq_dat$a <- data.frame(list(
      Identified_word = names(freq_feature),
      frequency = unname(freq_feature))
    )
    
    #2nd way of doing the sentiment analysis
    dict <- data_dictionary_LSD2015
    
    dfm_lsd <- dfm(mydfm$a)%>%dfm_lookup(dictionary = dict[1:2])
    
    dat$a <- convert(dfm_lsd,to = "data.frame")%>%
      mutate(overall = positive-negative,
             total = positive + negative,
             pper = positive/total,
             nper = negative/total,
             overall_p = pper-nper)%>%
      filter(!is.nan(overall_p))%>%
      arrange(overall_p, overall)%>%
      mutate(Nos = as.integer(gsub('text', '', doc_id)))
    
    #negative
    output$neg1type2 <- renderPrint({
      cs$a[dat$a$Nos[1:input$how_many_comments]]
    })
    
    #positive
    output$pos1type2 <- renderPrint({
      cs$a[dat$a$Nos[(nrow(dat$a)-(input$how_many_comments-1)):nrow(dat$a)]]
    })
    
    output$score1type2 <- renderPrint({
      mean(dat$a$overall_p)*100
    })
    
    #STM
    
    
    output$cloud1 <- renderPlot({
      quanteda.textplots::textplot_wordcloud(mydfm$a,
                                             min_count = 5,
                                             max_words = 100,
                                             color = wes_palette("FantasticFox1"))
      
    })
    
    output$freq_p1 <- renderPlot({
      plot(
        freq_dat$a %>%
          ggplot() +
          geom_segment(aes(
            x = reorder(Identified_word, frequency),
            xend = reorder(Identified_word, frequency),
            y = 0,
            yend = frequency
          ), color = "grey") +
          geom_point(aes(x = reorder(Identified_word, frequency), y = frequency)) +
          coord_flip() +
          xlab("") +
          ylab("Absolute frequency of the features")
      )
    })

    
      output$topics_p1 <- renderPlot({
        mydfmstm$a <- convert(mydfm$a, to = 'stm')
        
        model.stm$a <- stm(
          documents = mydfmstm$a$documents,
          vocab = mydfmstm$a$vocab,
          K = input$n_topics,
          #prevalence = ~ country + s(year),
          data = mydfmstm$a$meta,
          init.type = "Spectral"
        )
        
        reduced_comments$a <- cs$a[c(as.integer(gsub('text', '', names(mydfmstm$a$documents))))]
        
        plot(
          model.stm$a,
          type = "summary",
          n = input$how_many_on_topic_plot,
          text.cex = 1,
          main = "Topics",
          xlim = c(0, 1)
        )
      })
      
      output$topic_comments_text1 <- renderPrint({
        
        a <- findThoughts(
          model.stm$a,
          texts = reduced_comments$a,
          n = input$n_display_coms_per_top,
          topics = input$which_topic
        )
        b <- unlist(a[[2]][[1]])
        d <- c()
        for (i in 1: input$n_display_coms_per_top){
          d <- append(d, b[[i]])
        }
        d
      })

    #####################################
    #####################################
    #####################################
    #####################################
    #####################################
    #####################################
    #####################################
    #####################################
    #####################################
    #####################################
    #####################################
    #####################################
    
    mycorpus <- corpus(cs$b)
    docvars(mycorpus, 'Textno') <- sprintf("%02d", 1:ndoc(mycorpus))
    
    toks <- tokens(mycorpus,
                   remove_numbers = TRUE,
                   remove_punct = TRUE,
                   remove_symbols = TRUE,
                   remove_url = TRUE,
                   split_hyphens = TRUE,
                   include_docvars = TRUE)
    
    toks_cl <- tokens_select(toks, #further cleaning
                             c("[\\d-]", "[[:punct:]]", "^.{1,2}$"),
                             selection = "remove",
                             valuetype = "regex",
                             verbose = TRUE)%>%
      tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma)#here instead of stemming we're looking at the lemmas
    
    mydfm$b <- dfm(toks_cl,
                   tolower = TRUE)%>%
      dfm_remove(c(xtra_words, stopwords("english")))
    
    freq_feature <- topfeatures(mydfm$b, 30)
    
    freq_dat$b <- data.frame(list(
      Identified_word = names(freq_feature),
      frequency = unname(freq_feature))
    )
    
    #2nd way of doing the sentiment analysis
    dict <- data_dictionary_LSD2015
    
    dfm_lsd <- dfm(mydfm$b)%>%dfm_lookup(dictionary = dict[1:2])
    
    dat$b <- convert(dfm_lsd,to = "data.frame")%>%
      mutate(overall = positive-negative,
             total = positive + negative,
             pper = positive/total,
             nper = negative/total,
             overall_p = pper-nper)%>%
      filter(!is.nan(overall_p))%>%
      arrange(overall_p, overall)%>%
      mutate(Nos = as.integer(gsub('text', '', doc_id)))
    
    #negative
    output$neg2type2 <- renderPrint({
      cs$b[dat$b$Nos[1:input$how_many_comments]]
    })
    
    #positive
    output$pos2type2 <- renderPrint({
      cs$b[dat$b$Nos[(nrow(dat$b)-(input$how_many_comments-1)):nrow(dat$b)]]
    })
    
    output$score2type2 <- renderPrint({
      mean(dat$b$overall_p)*100
    })
    #STM
    
    
    output$cloud2 <- renderPlot({
      quanteda.textplots::textplot_wordcloud(mydfm$b,
                                             min_count = 5,
                                             max_words = 100,
                                             color = wes_palette("FantasticFox1"))
      
    })
    
    output$freq_p2 <- renderPlot({
      plot(
        freq_dat$b %>%
          ggplot() +
          geom_segment(aes(
            x = reorder(Identified_word, frequency),
            xend = reorder(Identified_word, frequency),
            y = 0,
            yend = frequency
          ), color = "grey") +
          geom_point(aes(x = reorder(Identified_word, frequency), y = frequency)) +
          coord_flip() +
          xlab("") +
          ylab("Absolute frequency of the features")
      )
    })

    
      output$topics_p2 <- renderPlot({
        mydfmstm$b <- convert(mydfm$b, to = 'stm')
        
        model.stm$b <- stm(
          documents = mydfmstm$b$documents,
          vocab = mydfmstm$b$vocab,
          K = input$n_topics,
          #prevalence = ~ country + s(year),
          data = mydfmstm$b$meta,
          init.type = "Spectral"
        )
        
        reduced_comments$b <- cs$b[c(as.integer(gsub('text', '', names(mydfmstm$b$documents))))]
        
        plot(
          model.stm$b,
          type = "summary",
          n = input$how_many_on_topic_plot,
          text.cex = 1,
          main = "Topics",
          xlim = c(0, 1)
        )
      })
      
      output$topic_comments_text2 <- renderPrint({

        a <- findThoughts(
          model.stm$b,
          texts = reduced_comments$b,
          n = input$n_display_coms_per_top,
          topics = input$which_topic
        )
        b <- unlist(a[[2]][[1]])
        d <- c()
        for (i in 1: input$n_display_coms_per_top){
          d <- append(d, b[[i]])
        }
        d
      })

    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
