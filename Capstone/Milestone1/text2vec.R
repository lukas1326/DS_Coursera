library(text2vec)
library(tidyverse)
library(doParallel)
# register parallel backend
N_WORKERS = 4
registerDoParallel(N_WORKERS)
t1 <- Sys.time()
it_files_par = ifiles_parallel(file_paths = 
       c("/data/final/en_US/en_US.twitter.txt"))
it_token_par = itoken_parallel(it_files_par, preprocessor = tolower, tokenizer = word_tokenizer)
vocab = create_vocabulary(it_token_par)
vocab <-vocab %>%  mutate(term = str_extract(term, "[a-z']+"))
vocab<-vocab[complete.cases(vocab),]
print(difftime(Sys.time(),t1,units = 'sec'))

# DTM vocabulary vectorization
v_vectorizer = vocab_vectorizer(vocab)
dtm_v = create_dtm(it_token_par, vectorizer = v_vectorizer)
# DTM hash vectorization
h_vectorizer = hash_vectorizer()
dtm_h = create_dtm(it_token_par, vectorizer = h_vectorizer)
# co-ocurence statistics
tcm = create_tcm(it_token_par, vectorizer = v_vectorizer, skip_grams_window = 5)

#check vulgarities, call vulgarities()
vul<-vulgarities()
z<-vocab[vocab$term %in% vul,]
length(z)


#another way to obtain list of profanity
download.file("http://www.bannedwordlist.com/lists/swearWords.txt",destfile = "bad_words.txt")
bad_words<-read.delim("bad_words.txt",header = FALSE)
z<-vocab[vocab$term %in% bad_words$V1,]
sum(z$term_count)
