#' This script applies affinity propagation clustering to the fitted word2vec model. 

library(tidyverse)
library(tidytext)
library(stringr)
library(apcluster)
library(wordVectors)

w2v_model_file = 'glyphosate_w2v.bin'

## Use tf-idf to pick out a subset of focal terms
# load('tokens_df.Rdata')
# token_counts_df = tokens_df %>%
# 	group_by(comment_id, token) %>%
# 	summarize(token_n = n()) %>%
# 	bind_tf_idf(token, comment_id, token_n) %>%
# 	ungroup
# ggplot(token_counts_df, aes(tf_idf)) + stat_ecdf()
# terms = token_counts_df %>%
# 	filter(tf_idf >= quantile(tf_idf, probs = (.99))) %>%
# 	.$token %>% unique
# length(terms)
# terms[1:100]

## --------------------
## Calculate information gain of terms
## Let's look at the distribution of negative/positive comments across commenter types
load('comments_attachments.Rdata')
comments %>%
	group_by(comment_id, 
		   commenter_type, 
		   valence) %>%
	summarize(comment_text = str_c(comment_text, 
								   collapse = ' ')) %>%
	ungroup() -> comments
with(comments, table(commenter_type, valence, useNA = 'ifany'))
## No surprise: strong correlation between advocacy/industry and neg/pos.  

## Load tokens
load('tokens_df.Rdata')
tokens_df %>%
	filter(commenter_type %in% c('advocacy', 'industry'), 
		   str_detect(token, '[a-z]+')) -> tokens_df

## Calculate information gain
base_H = tokens_df %>%
	.$commenter_type %>%
	{sum(. == 'industry') / length(.)} %>%
	{-. * log2(.)}
		
info_df = tokens_df %>%
	group_by(token) %>%
	summarize(p = sum(commenter_type == 'industry') / n()) %>%
	filter(p > 0, p < 1) %>%
	mutate(H = -p * log2(p), 
		   delta_H = base_H - H)

ggplot(info_df, aes(p, delta_H)) + geom_point() + geom_rug()

terms = info_df %>%
	filter(delta_H > .2) %>%
	.$token

terms

## Load the fitted word2vec model
model = read.binary.vectors(w2v_model_file)
## Submodel of focal terms
focal_model = model[[terms, average = FALSE]]
## Cosine similarity to the focal terms
focal_sim = cosineSimilarity(model, focal_model)
## Select the 1000 terms closest to any of the focal terms
model_trimmed = focal_sim[rank(-apply(focal_sim,1,max)) < 1000,]

## Cosine similarity on the submodel
cos_sim = cosineSimilarity(model_trimmed, model_trimmed)

## Cluster the terms
clusters = aggExCluster(cos_sim, includeSim = TRUE)
plot(clusters)
cluster_terms = cutree(clusters, h = .98) %>% 
	sort(decreasing = TRUE, sortBy = 'size') %>%
	.@clusters %>% 
	map(names)
nontrivial_clusters = cluster_terms %>% 
	map(length) %>% 
	unlist %>% 
	{. > 5} %>% 
	which()
cluster_terms[nontrivial_clusters] -> cluster_terms

TODO: need to standardize individual comments, get multigrams

## --------------------
## Cluster mapping:  Vector projection approach

## Build cluster vectors
cluster_vectors = cluster_terms %>%
	map(~ model[[.x]]) #%>%
	# map(normalize_lengths)

## Build comment vectors
comment_vectors = tokens_df %>%
	split(.$comment_id) %>%
	 # .[1:3] %>%
	map(~ .x$token) %>%
	map(~ model[[.x]]) %>%
	reduce(rbind) %>% 
	as.VectorSpaceModel()

## Project comments vectors into clusters
comment_z = map(cluster_vectors, 
				~ project(comment_vectors, .x)) %>%
	map(magnitudes) %>%
	## Arrange into a matrix
	reduce(cbind) %>% 
	as_data_frame() %>%
	`colnames<-`(str_c('cluster_', 1:ncol(.))) %>%
	## Convert to Z-score
	mutate_all(funs((. - mean(.))/sd(.))) %>%
	mutate(comment_id = {tokens_df %>% 
			split(.$comment_id) %>% 
			names})
	
comment_z %>%
	inner_join(comments) %>%
	gather(cluster, magnitude, starts_with('cluster'), 
		   factor_key = TRUE) %>%
	ggplot(aes(commenter_type, magnitude, 
			   fill = commenter_type)) +
	# geom_violin(scale = 'count') + 
	geom_dotplot(binaxis = 'y', stackdir = 'center', 
				 color = NA, dotsize = 1.2) +
	facet_wrap(~ cluster)
	

## --------------------
## Cluster mapping:  Count approach
## Count cluster occurrences in comments
cluster_terms %>%
	map(~ str_count(comments$comment_text, 
					regex(.x, ignore.case = TRUE))) %>%
	as.data.frame(col.names = str_c('cluster_', 1:length(.))) %>%
	bind_cols(comments) ->
	comment_counts
 
comment_counts %>%
	filter(commenter_type %in% c('advocacy', 'industry')) %>%
	select(-comment_text) %>%
	gather(cluster, count, starts_with('cluster'),
		   factor_key = TRUE) %>% 
	## Trim out the zero-counts
	filter(count > 0) %>%
	ggplot(aes(commenter_type, count, 
			   fill = commenter_type)) +
	geom_dotplot(binaxis = 'y', stackdir = 'center', 
				 color = NA, dotsize = 1.2) +
	facet_wrap(~ cluster, scales = 'free_y')
