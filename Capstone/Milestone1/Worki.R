setwd("~/datasciencecoursera/Capstone/Capstone train")
library(readtext)
library(quanteda)
library(tm)
library(tidytext)
library(tidyverse)
##files' length
en_files <- list.files("/data/final/en_US/")
nlines<-numeric()
for (i in 1:3){
        con <- file(paste0("/data/final/en_US/",en_files[i]), open = "rb")
        nlines[i] <- length(readLines(con,skipNul = TRUE))
        close(con)
}


chunk <- function(x, n) split(x, sort(rank(x) %% n))
chunks <- chunk(1:nlines[1],20)

blogs <- data.frame()

for (i in 1:length(chunks)){
con <- file(paste0("/data/final/en_US/",en_files[1]), open = "rb")
        tData_blogs <-readLines(con,skipNul = TRUE)[chunks[[i]]]
close(con)        
        
        tData_blogs<-VCorpus(VectorSource(tData_blogs))
        df <- tidy(tData_blogs)
        rm(tData_blogs)
        
        blogs<-rbind(df,blogs)
}

 
unigram_count <- blogs %>% 
        unnest_tokens(word, text)
        mutate(word = str_extract(word, "[a-z']+")) %>%
        count(word,sort=TRUE)
        unigram_count <- unigram_count[complete.cases(unigram_count),]

        
        bigram_count <- df %>%
                unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
                separate(bigram, c("word1","word2"), sep=" ") %>%
                mutate(word1 = str_extract(word1, "[a-z']+")) %>%
                mutate(word2 = str_extract(word2, "[a-z']+")) %>%
                count(word1,word2,sort=TRUE)
        bigram_count <- bigram_count[complete.cases(bigram_count),]
        
        library(igraph)
        
        bigram_graph <- bigram_count %>%
                filter(n > 100) %>%
                graph_from_data_frame()
        
        library(ggraph)
        set.seed(2016)
        
        a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
        
        ggraph(bigram_graph, layout = "fr") +
                geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                               arrow = a, end_cap = circle(.07, 'inches')) +
                geom_node_point(color = "lightblue", size = 5) +
                geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
                theme_void()