library(text2vec)
library(tidyverse)
library(doParallel)
# register parallel backend
N_WORKERS = 4
registerDoParallel(N_WORKERS)
t1 <- Sys.time()
file_type <-"twitter"
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

it_files_par = ifiles_parallel(file_paths = 
       paste0("/data/final/en_US/en_US.",file_type,".txt"))
it_token_par = itoken_parallel(it_files_par, preprocessor = text.clean, tokenizer = word_tokenizer)
vocab = create_vocabulary(it_token_par,stopwords = as.vector(bad_words$V1))
print(difftime(Sys.time(),t1,units = 'sec'))


z<-vocab %>% arrange(desc(term_count)) %>% mutate(rank=row_number(),term_frequency=term_count/sum(term_count))
z %>% ggplot(aes(rank, term_frequency),options(scipen=999)) +
        geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) #+
        scale_x_log10(labels = function(x) format(x, scientific = FALSE)) +
        scale_y_log10(labels = function(x) format(x, scientific = FALSE))





#obtain list of profanity and extract them from the text
download.file("http://www.bannedwordlist.com/lists/swearWords.txt",destfile = "bad_words.txt")
bad_words<-read.delim("bad_words.txt",header = FALSE)

