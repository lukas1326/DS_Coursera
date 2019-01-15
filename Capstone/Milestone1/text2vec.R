library(text2vec)
library(tidyverse)
library(doParallel)
library(data.table)
library(MODIS)
---------------------------------------------------------------------
#table with number of lines and file size 
en_files <- list.files("/data/final/en_US/")
nlines<-numeric()
size<-numeric()
for (i in 1:3){
        con <- file(paste0("/data/final/en_US/",en_files[i]), open = "rb")
        nlines[i] <- length(readLines(con,skipNul = TRUE))
        close(con)
        size[i]<-fileSize(paste0("/data/final/en_US/",en_files[i]),units = "MB")
}
LinesNumber <- data.table(file=en_files,lines= nlines,file.size=size)
----------------------------------------------------------------------
# register parallel backend
N_WORKERS = 4
registerDoParallel(N_WORKERS)
#obtain list of profanity and extract them from the text
#download.file("http://www.bannedwordlist.com/lists/swearWords.txt",destfile = "bad_words.txt")
bad_words<-read.delim("bad_words.txt",header = FALSE)
file_type <- c("twitter","blogs","news")
text.clean = function(x)                    # text data
{ require("tm")
        x  =  gsub("<.*?>", " ", x)               # regex for removing HTML tags
        x  =  iconv(x, "latin1", "ASCII", sub="") # Keep only ASCII characters
        x  =  gsub("[^[:alnum:]]", " ", x)        # keep only alpha numeric 
        x  =  tolower(x)                          # convert to lower case characters
        x  =  removeNumbers(x)                    # removing numbers
        x  =  stripWhitespace(x)                  # removing white space
        return(x)
}
vocabALL <- data.table()
for (i in 1:3){
        it_files_par = ifiles_parallel(file_paths = 
        paste0("/data/final/en_US/en_US.",file_type[i],".txt"))
        it_token_par = itoken_parallel(it_files_par, preprocessor = text.clean, tokenizer = word_tokenizer)
        vocab = create_vocabulary(it_token_par,stopwords = as.vector(bad_words$V1))
        vocab <- vocab[,c(1,2)]%>% mutate(file=file_type[i])
        vocabALL <- rbind(vocab,vocabALL)
}

#count unique words
z1<-vocabALL %>% group_by(file) %>% count(file)

#count words by file
z2<-vocabALL %>% group_by(file) %>% summarise(sum=sum(term_count))

#words statistics after cleaning
words<-inner_join(z1,z2)

#summary info by files
LinesNumber <- cbind(LinesNumber,words[,c(2,3)])

freq_tab<-vocab %>% 
        arrange(desc(term_count)) %>% 
        mutate(rank=row_number(),term_frequency=term_count/sum(term_count))
        
freq_tab %>% ggplot(aes(rank, term_frequency)) +
        geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
        scale_x_log10(labels = function(x) format(x, scientific = FALSE)) +
        scale_y_log10(labels = function(x) format(x, scientific = FALSE))







