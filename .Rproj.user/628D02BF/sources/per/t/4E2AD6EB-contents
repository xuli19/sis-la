library(shiny)

text_raw_data <- read.csv('all_code.csv',stringsAsFactors = FALSE)

### generate color ###
generateVcolor2 <- function(vertexObjects){
  colordataset <- c()
  for (i in vertexObjects) {
    if(i<=12)
      red <- max(255-i*20,0)
    else
      red <- i
    cr <- rgb(red=red,green=125,blue=125,max=255)
    colordataset <- append(colordataset,cr)
  }
  return(colordataset)
}

function(input, output) {
  
  require(tidytext) #clean text raw data
  require(tidyverse)
  require(widyr) #calculate the relation among terms
  require(ggraph) #visualization terms
  require(igraph)
  require(visNetwork)
  
  # student to student
  output$stos <- renderVisNetwork({
    if (input$week==1){
   
      persons_data_1 <- text_raw_data %>% filter(week == 'Week 02') %>% select('vert1_id','vert2_id') 
      graph_persons_1 <- graph_from_data_frame(persons_data_1)
      graph_persons_vis_1 <- toVisNetworkData(graph_persons_1)
      persons_color_1 <- generateVcolor2(as.integer(degree(graph_persons_1)))
      graph_persons_vis_1$nodes$color <- persons_color_1
      dvalue <- degree(graph_persons_1)
      graph_persons_vis_1$nodes$value <- dvalue[match(graph_persons_vis_1$nodes$id,names(dvalue))]
      
      visNetwork(graph_persons_vis_1$nodes,graph_persons_vis_1$edges) %>%
        visInteraction(hover = TRUE)%>%
        visEdges(arrows = list(to = list(enabled = TRUE, type = 'arrow'))) %>%
        visOptions(highlightNearest = list(degree=1,hover=T),nodesIdSelection = TRUE)
     }
    
    else if (input$week==2){
      persons_data_2 <- text_raw_data %>% filter(week == 'Week 08') %>% select('vert1_id','vert2_id') 
      graph_persons_2 <- graph_from_data_frame(persons_data_2)
      graph_persons_vis_2 <- toVisNetworkData(graph_persons_2)
      persons_color_2 <- generateVcolor2(as.integer(degree(graph_persons_2)))
      graph_persons_vis_2$nodes$color <- persons_color_2
      dvalue <- degree(graph_persons_2)
      graph_persons_vis_2$nodes$value <- dvalue[match(graph_persons_vis_2$nodes$id,names(dvalue))]
      
      visNetwork(graph_persons_vis_2$nodes,graph_persons_vis_2$edges) %>%
        visInteraction(hover = TRUE)%>%
        visEdges(arrows = list(to = list(enabled = TRUE, type = 'arrow'))) %>%
        visOptions(highlightNearest = list(degree=1,hover=T),nodesIdSelection = TRUE)
     }
    
    else  {
      persons_data_3 <- text_raw_data %>% filter(week == 'Week 10') %>% select('vert1_id','vert2_id') 
      graph_persons_3 <- graph_from_data_frame(persons_data_3)
      graph_persons_vis_3 <- toVisNetworkData(graph_persons_3)
      persons_color_3 <- generateVcolor2(as.integer(degree(graph_persons_3)))
      graph_persons_vis_3$nodes$color <- persons_color_3
      dvalue <- degree(graph_persons_3)
      graph_persons_vis_3$nodes$value <- dvalue[match(graph_persons_vis_3$nodes$id,names(dvalue))]
      
      visNetwork(graph_persons_vis_3$nodes,graph_persons_vis_3$edges) %>%
        visInteraction(hover = TRUE)%>%
        visEdges(arrows = list(to = list(enabled = TRUE, type = 'arrow'))) %>%
        visOptions(highlightNearest = list(degree=1,hover=T),nodesIdSelection = TRUE)
     }
    
 
    
  })
  
  
# student to term
  output$stot <- renderVisNetwork({
    if (input$week==1){
      text_raw_data_1 <- text_raw_data %>% filter(week == 'Week 02')
      text_message_person_1 <- as_tibble(text_raw_data_1[1:3])
      ###  show relationship between learner and terms ####
      text_person_words_1 <- text_message_person_1[c('vert1_id','message_text')]
      names(text_person_words_1) <- c('learners','messages')
      
      text_person_words_1 <- text_person_words_1 %>%
        unnest_tokens(bigram,messages,token = 'ngrams',n=2)
   
      text_person_words_1 <- text_person_words_1 %>%
        separate(bigram,c('wd1','wd2'),sep=' ') %>%
        count(learners,wd1,wd2,sort = TRUE)
      text_person_words_1 <- text_person_words_1 %>%
        filter(!wd1 %in% stop_words$word)%>%
        filter(!wd2 %in% stop_words$word)
      
      text_person_words_count2_1 <- text_person_words_1 %>%
        filter(n>=3)
      
      text_person_words_unite_1 <- text_person_words_count2_1 %>%
        unite('words',wd1:wd2,sep=' ')
      
      graph_person_words_1 <- graph_from_data_frame(text_person_words_unite_1)
      
      graph_person_words_vis_1 <- toVisNetworkData(graph_person_words_1)
      #color
      graph_person_words_color_1 <- generateVcolor2(
        as.integer(degree(graph_person_words_1))
      )
      graph_person_words_vis_1$nodes$color <- graph_person_words_color_1
      graph_person_words_vis_1$nodes$shape <- 'circle'
      
      
      visNetwork(graph_person_words_vis_1$nodes, graph_person_words_vis_1$edges) %>%
        visEdges(arrows = list(to = list(enabled = TRUE, type = 'arrow'))) %>%
        visOptions(highlightNearest = list(degree=1,hover=T))
    }
    
    
    else if (input$week==2){
      text_raw_data_2 <- text_raw_data %>% filter(week == 'Week 08')
      text_message_person_2 <- as_tibble(text_raw_data_2[1:3])
      ###  show relationship between learner and terms ####
      text_person_words_2 <- text_message_person_2[c('vert1_id','message_text')]
      names(text_person_words_2) <- c('learners','messages')
      
      text_person_words_2 <- text_person_words_2 %>%
        unnest_tokens(bigram,messages,token = 'ngrams',n=2)
      
      text_person_words_2 <- text_person_words_2 %>%
        separate(bigram,c('wd1','wd2'),sep=' ') %>%
        count(learners,wd1,wd2,sort = TRUE)
      text_person_words_2 <- text_person_words_2 %>%
        filter(!wd1 %in% stop_words$word)%>%
        filter(!wd2 %in% stop_words$word)
      
      text_person_words_count2_2 <- text_person_words_2 %>%
        filter(n>=3)
      
      text_person_words_unite_2 <- text_person_words_count2_2 %>%
        unite('words',wd1:wd2,sep=' ')
      
      graph_person_words_2 <- graph_from_data_frame(text_person_words_unite_2)
      
      graph_person_words_vis_2 <- toVisNetworkData(graph_person_words_2)
      #color
      graph_person_words_color_2 <- generateVcolor2(
        as.integer(degree(graph_person_words_2))
      )
      graph_person_words_vis_2$nodes$color <- graph_person_words_color_2
      graph_person_words_vis_2$nodes$shape <- 'circle'
      
      
      visNetwork(graph_person_words_vis_2$nodes, graph_person_words_vis_2$edges) %>%
        visEdges(arrows = list(to = list(enabled = TRUE, type = 'arrow'))) %>%
        visOptions(highlightNearest = list(degree=1,hover=T))
    }
    
    else {
      text_raw_data_3 <- text_raw_data %>% filter(week == 'Week 10')
      text_message_person_3 <- as_tibble(text_raw_data_3[1:3])
      ###  show relationship between learner and terms ####
      text_person_words_3 <- text_message_person_3[c('vert1_id','message_text')]
      names(text_person_words_3) <- c('learners','messages')
      
      text_person_words_3 <- text_person_words_3 %>%
        unnest_tokens(bigram,messages,token = 'ngrams',n=2)
      
      text_person_words_3 <- text_person_words_3 %>%
        separate(bigram,c('wd1','wd2'),sep=' ') %>%
        count(learners,wd1,wd2,sort = TRUE)
      text_person_words_3 <- text_person_words_3 %>%
        filter(!wd1 %in% stop_words$word)%>%
        filter(!wd2 %in% stop_words$word)
      
      text_person_words_count2_3 <- text_person_words_3 %>%
        filter(n>=3)
      
      text_person_words_unite_3 <- text_person_words_count2_3 %>%
        unite('words',wd1:wd2,sep=' ')
      
      graph_person_words_3 <- graph_from_data_frame(text_person_words_unite_3)
      
      graph_person_words_vis_3 <- toVisNetworkData(graph_person_words_3)
      #color
      graph_person_words_color_3 <- generateVcolor2(
        as.integer(degree(graph_person_words_3))
      )
      graph_person_words_vis_3$nodes$color <- graph_person_words_color_3
      graph_person_words_vis_3$nodes$shape <- 'circle'
      
      
      visNetwork(graph_person_words_vis_3$nodes, graph_person_words_vis_3$edges) %>%
        visEdges(arrows = list(to = list(enabled = TRUE, type = 'arrow'))) %>%
        visOptions(highlightNearest = list(degree=1,hover=T))
      
    }
    
    
  })
  
  
  
  # term to term
  output$ttot <- renderVisNetwork({
    if (input$week==1){
      text_raw_data_1 <- text_raw_data %>% filter(week == 'Week 02')
      text_message_person_1 <- as_tibble(text_raw_data_1[1:3])
      #split term by ngrams number ,2 as default used  #
      text_bigram_data_1 <- text_message_person_1 %>%
        unnest_tokens(bigram,message_text,token = 'ngrams',n=2)
      text_bigram_count_1 <- text_bigram_data_1 %>%
        count(bigram,sort = TRUE)
      #then you can count the associated words.
      
      #then split the associated words into n column ,the coexist term will be shown.
      text_bigram_count2_1 <- text_bigram_count_1 %>%
        separate(bigram,c('wd1','wd2'),sep=' ')
      
      # del unuseful words ,del stop words.
      text_bigram_count2_1 <- text_bigram_count2_1 %>%
        filter(!wd1 %in% stop_words$word,!wd2 %in% stop_words$word)
      
      # convert data to igraph data structure.
      text_bigram_count3_1 <- text_bigram_count2_1 %>%
        filter(n>=3)
      text_bigram_graph_1 <- graph_from_data_frame(text_bigram_count3_1) 
      
      # this graph is so not beautiful,we choos another tool for visualization
      library(visNetwork)
      text_vis_data_1 <- toVisNetworkData(text_bigram_graph_1)
      text_bigram_graph_color_1 <- generateVcolor2(as.integer(degree(text_bigram_graph_1)))
      # text_vis_data$nodes$shape <- 'circle'
      text_vis_data_1$nodes$color <- text_bigram_graph_color_1  #gradient color from red to green
      
      
      # change vertex size in terms of degree
      degree_value_1 <- degree(text_bigram_graph_1,mode='all')
      text_vis_data_1$nodes$size <- degree_value_1[match(text_vis_data_1$nodes$id,names(degree_value_1))]*5
      
      text_vis_data_1$nodes$shape <- 'circle'
      
      visNetwork(text_vis_data_1$nodes,text_vis_data_1$edges) %>%
        visEdges(arrows = list(to = list(enabled = TRUE, type = 'arrow'))) %>%
        visOptions(highlightNearest = list(degree=1,hover=T))
    }
    
    else if (input$week==2){
      text_raw_data_2 <- text_raw_data %>% filter(week == 'Week 08')
      text_message_person_2 <- as_tibble(text_raw_data_2[1:3])
      #split term by ngrams number ,2 as default used  #
      text_bigram_data_2 <- text_message_person_2 %>%
        unnest_tokens(bigram,message_text,token = 'ngrams',n=2)
      text_bigram_count_2 <- text_bigram_data_2 %>%
        count(bigram,sort = TRUE)
      #then you can count the associated words.
      
      #then split the associated words into n column ,the coexist term will be shown.
      text_bigram_count2_2 <- text_bigram_count_2 %>%
        separate(bigram,c('wd1','wd2'),sep=' ')
      
      # del unuseful words ,del stop words.
      text_bigram_count2_2 <- text_bigram_count2_2 %>%
        filter(!wd1 %in% stop_words$word,!wd2 %in% stop_words$word)
      
      # convert data to igraph data structure.
      text_bigram_count3_2 <- text_bigram_count2_2 %>%
        filter(n>=3)
      text_bigram_graph_2 <- graph_from_data_frame(text_bigram_count3_2) 
      
      # this graph is so not beautiful,we choos another tool for visualization
      library(visNetwork)
      text_vis_data_2 <- toVisNetworkData(text_bigram_graph_2)
      text_bigram_graph_color_2 <- generateVcolor2(as.integer(degree(text_bigram_graph_2)))
      # text_vis_data$nodes$shape <- 'circle'
      text_vis_data_2$nodes$color <- text_bigram_graph_color_2  #gradient color from red to green
      
      
      # change vertex size in terms of degree
      degree_value_2 <- degree(text_bigram_graph_2,mode='all')
      text_vis_data_2$nodes$size <- degree_value_2[match(text_vis_data_2$nodes$id,names(degree_value_2))]*5
      
      text_vis_data_2$nodes$shape <- 'circle'
      
      visNetwork(text_vis_data_2$nodes,text_vis_data_2$edges) %>%
        visEdges(arrows = list(to = list(enabled = TRUE, type = 'arrow'))) %>%
        visOptions(highlightNearest = list(degree=1,hover=T))
    }
    
    
    else {
      text_raw_data_3 <- text_raw_data %>% filter(week == 'Week 10')
      text_message_person_3 <- as_tibble(text_raw_data_3[1:3])
      #split term by ngrams number ,2 as default used  #
      text_bigram_data_3 <- text_message_person_3 %>%
        unnest_tokens(bigram,message_text,token = 'ngrams',n=2)
      text_bigram_count_3 <- text_bigram_data_3 %>%
        count(bigram,sort = TRUE)
      #then you can count the associated words.
      
      #then split the associated words into n column ,the coexist term will be shown.
      text_bigram_count2_3 <- text_bigram_count_3 %>%
        separate(bigram,c('wd1','wd2'),sep=' ')
      
      # del unuseful words ,del stop words.
      text_bigram_count2_3 <- text_bigram_count2_3 %>%
        filter(!wd1 %in% stop_words$word,!wd2 %in% stop_words$word)
      
      # convert data to igraph data structure.
      text_bigram_count3_3 <- text_bigram_count2_3 %>%
        filter(n>=3)
      text_bigram_graph_3 <- graph_from_data_frame(text_bigram_count3_3) 
      
      # this graph is so not beautiful,we choos another tool for visualization
      library(visNetwork)
      text_vis_data_3 <- toVisNetworkData(text_bigram_graph_3)
      text_bigram_graph_color_3 <- generateVcolor2(as.integer(degree(text_bigram_graph_3)))
      # text_vis_data$nodes$shape <- 'circle'
      text_vis_data_3$nodes$color <- text_bigram_graph_color_3  #gradient color from red to green
      
      
      # change vertex size in terms of degree
      degree_value_3 <- degree(text_bigram_graph_3,mode='all')
      text_vis_data_3$nodes$size <- degree_value_3[match(text_vis_data_3$nodes$id,names(degree_value_3))]*5
      
      text_vis_data_3$nodes$shape <- 'circle'
      
      visNetwork(text_vis_data_3$nodes,text_vis_data_3$edges) %>%
        visEdges(arrows = list(to = list(enabled = TRUE, type = 'arrow'))) %>%
        visOptions(highlightNearest = list(degree=1,hover=T))
      
    }
    
    
  })
  
  
  
  output$info <- renderUI({
    
    a("Shiny app designed by Fan Ouyang and Xu Li", href="https://github.com/fanouyang/la_tool_sin", target="_blank")
    
    
  })
  
  
}