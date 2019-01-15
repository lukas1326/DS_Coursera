library(text2vec)
library(Matrix)
library(magrittr)

# default approach to tcm with text2vec:
corpus = strsplit(c("here is a short document", "here is a different short document"), " ")
it = itoken(corpus) 
tcm = create_vocabulary(it)  %>% vocab_vectorizer() %>% create_tcm(it, . , skip_grams_window = 3)

# results in this:
print(as.matrix(forceSymmetric(tcm, "U")))

d