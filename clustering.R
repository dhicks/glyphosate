#' This script applies affinity propagation clustering to the fitted word2vec model. 

library(tidyverse)
library(tidytext)
library(apcluster)
library(wordVectors)

w2v_model_file = 'glyphosate_w2v.bin'

## Use tf-idf to pick out a subset of focal terms
load('tokens_df.Rdata')
token_counts_df = tokens_df %>%
	group_by(comment_id, token) %>%
	summarize(token_n = n()) %>%
	bind_tf_idf(token, comment_id, token_n) %>%
	ungroup
ggplot(token_counts_df, aes(tf_idf)) + stat_ecdf()
terms = token_counts_df %>%
	filter(tf_idf >= quantile(tf_idf, probs = (.99))) %>%
	.$token %>% unique
length(terms)
terms[1:100]

## Load the fitted word2vec model
model = read.binary.vectors(w2v_model_file)
## Submodel of focal terms
focal_model = model[[terms, average = FALSE]]
## Cosine similarity to the focal terms
focal_sim = cosineSimilarity(model, focal_model)
## Select the 2000 terms closest to any of the focal terms
model_trimmed = focal_sim[rank(-apply(focal_sim,1,max)) < 2000,]

## Cosine similarity on the submodel
cos_sim = cosineSimilarity(model_trimmed, model_trimmed)

## Cluster the terms
clusters = apcluster(cos_sim)
heatmap(clusters, cos_sim)
