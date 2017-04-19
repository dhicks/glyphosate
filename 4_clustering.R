#' This script applies affinity propagation clustering to the fitted word2vec model. 

library(tidyverse)
library(tidytext)
library(stringr)
library(apcluster)
library(wordVectors)

w2v_model_file = 'glyphosate_w2v.bin'
load('comments_attachments.Rdata')

#' First let's look at the distribution of negative/positive comments across commenter types
comments %>%
	group_by(comment_id, 
			 commenter_type, 
			 valence) %>%
	summarize(comment_text = str_c(comment_text, 
								   collapse = ' ')) %>%
	ungroup() -> comments
type_by_valence = with(comments, 
					   table(commenter_type, valence, useNA = 'ifany'))
type_by_valence
chisq.test(type_by_valence, simulate.p.value = TRUE)
#' No surprise: strong correlation between advocacy/industry and neg/pos. So we can treat industry comments as "highly informed and positive" and advoacy comments as "highly informed and negative."  

## Load tokens
load('tokens_df.Rdata')
tokens_df %>%
	filter(commenter_type %in% c('advocacy', 'industry'), 
		   str_detect(token, '[a-z]+')) -> tokens_df

tokens_df %>% 
	.$token %>%
	unique %>%
	length

## --------------------
#' ## Term selection: tf-idf ##
#' With tens of thousands of terms, the next step of analysis is to identify a set of focal terms.  One approach is to use tf-idf, treating the industry and advocacy as two bags-of-words. 
token_counts_df = tokens_df %>%
	group_by(commenter_type, token) %>%
	summarize(token_n = n()) %>%
	bind_tf_idf(token, commenter_type, token_n) %>%
	ungroup()
ggplot(token_counts_df, aes(tf_idf)) + 
	stat_ecdf() + 
	scale_x_log10()
token_counts_df %>%
	# filter(tf_idf >= quantile(tf_idf, probs = (.995))) %>%
	arrange(desc(tf_idf)) %>%
	head(60) %>%
	.$token %>% 
	unique() %>%
	sort() %>%
	matrix(ncol = 3)

#' However, note that the idf calculation, and so the tf-idf calculation, assigns a value of 0 to any term that occurs in both industry and advocacy documents, even if it is much more common in one of the two commenter types.  So the tf-idf approach discards too much data.  

token_counts_df %>%
	filter(token == 'children')

## --------------------
#' ## Term selection: Information gain ##
#' An alternative approach considers the information gain (entropy difference) for each term, relative to industry and advocacy comments. 
#' 

#' *Information gain* is calculated as *reduction in entropy*, $H(X) - H(X|token)$, where for this project the random variable $X$ is the commenter type, either advocacy or industry.  In turn, *entropy* is the expected value of *information* (in bits):
#' $$ H(X) = \sum_x p(x) I(x) = -\sum p(x) \log_2 p(x). $$ 
#' $$ H(X|T) = \sum_t p(t) H(X|T = t) = \sum_{x,t} p(x,t) \log_2 \frac{p(x)}{p(x,t)}, $$
#' where $T$ represents the token and takes the values $\{token, \lnot token\}$. 

## Calculate unconditional entropy
base_H = tokens_df %>%
	.$commenter_type %>%
	{sum(. == 'industry') / length(.)} %>%
	{-(. * log2(.)) + -((1-.) * log2(1 - .))}

## Calculate conditional entropy and information gain
info_df = token_counts_df %>%
	select(-tf, -idf, -tf_idf) %>%
	spread(commenter_type, token_n, fill = 0) %>% 
	# filter(advocacy > 0, industry > 0) %>%
	mutate(token_n = advocacy + industry, 
		   corpus_n = sum(advocacy) + sum(industry)) %>%
	mutate(p_token = token_n / corpus_n, 
		   p_ntoken = 1 - p_token, 
		   p_advocacy_token = advocacy / corpus_n, 
		   h_at = p_advocacy_token * log2(p_token / p_advocacy_token),
		   p_advocacy_ntoken = (sum(advocacy) - advocacy) / corpus_n, 
		   h_an = p_advocacy_ntoken * log2(p_ntoken / p_advocacy_ntoken),
		   p_industry_token = industry / corpus_n, 
		   h_it = p_industry_token * log2(p_token / p_industry_token),
		   p_industry_ntoken = (sum(industry) - industry) / corpus_n, 
		   h_in = p_industry_ntoken * log2(p_ntoken / p_industry_ntoken)) %>%
	## If a term doesn't occur in advocacy or industry, 
	## the corresponding h_*t is NaN
	mutate(h_at = ifelse(!is.na(h_at), h_at, 0), 
		   h_it = ifelse(!is.na(h_it), h_it, 0)) %>%
	## Calculate entropy and information gain
	mutate(H = h_at + h_an + h_it + h_in,
		   delta_H = base_H - H)

#' The funnel plot below shows information gain against the conditional probability $p(industry | token)$.  Generally, information gain increases as this conditional probably becomes more extreme; the variation at the extremes is due to points that only occur in one commenter type.   
ggplot(info_df, aes(p_industry_token / p_token, delta_H)) + 
	geom_point() +
	# geom_hex(bins = 30) +
	geom_rug() + 
	geom_smooth() +
	xlab('p(industry | token)') +
	scale_y_log10(name = 'information gain')

#' On this approach, relatively high-scoring terms are relatively frequent in one commenter type and much less frequent in the other.  

info_df %>%
	arrange(desc(delta_H)) %>%
	select(token, advocacy, industry, delta_H) %>%
	head(20)

#' Simply inspecting this list indicates a few patterns.  Agriculture-related terms — sugarbeets, seed, sugar — appear to be strongly associated with industry, while advocacy uses more personal and emotionally weighted language — I, you, my; toxic.  Industry also may make more references to formal research standards — IQA, case control studies, quality standards. Vaccines are appear on this list, due to a letter-writing campaign by the organization Moms Across America, based on laboratory test findings of glyphosate contamination in some vaccines.  
#' 
#' To proceed, we select approximately 200 focal terms with the highest information gain. 

ggplot(info_df, aes(delta_H)) + 
	stat_ecdf() + 
	scale_x_log10()

terms = info_df %>%
	filter(delta_H > 10^-4) %>%
	.$token

matrix(terms, ncol = 3)

## ------------------
#' ## Cluster construction ##
#' To cluster these terms, we first expand the termlist, identifying the 500 terms closest to the focal terms.  

## Load the fitted word2vec model
model = read.binary.vectors(w2v_model_file)
## Submodel of focal terms
focal_model = model[[terms, average = FALSE]]
## Cosine similarity to the focal terms
focal_sim = cosineSimilarity(model, focal_model)
## Select the 500 terms closest to any of the focal terms
model_trimmed = focal_sim[rank(-apply(focal_sim,1,max)) < 500,]

#' We cluster these 500 terms using cosine similarity and affinity propagation.  

## Cosine similarity on the submodel
cos_sim = cosineSimilarity(model_trimmed, model_trimmed)
## Affinity propagation
clusters = aggExCluster(cos_sim, includeSim = TRUE)

#' The dendrogram below shows that the tokens can be arranged into a cluster hierarchy with clear and distinct clusters.  
#+ fig.height = 7, fig.width = 7
library(ggraph)
clusters %>%
	as.dendrogram() %>% 
	ggraph(layout = 'dendrogram', circular = FALSE) +
	geom_edge_elbow() +
	theme_graph() + 
	coord_flip()

#' Extracting clusters requires choosing a level at which to cut the dendrogram.  Below, we plot the size of the largest cluster against the cut level $h$.  

largest_cluster = function (ap_tree, h) {
	clusters %>%
		cutree(h = h) %>%
		sort(decreasing = TRUE, sortBy = 'size') %>%
		.@clusters %>%
		.[[1]] %>%
		length
}
largest_cluster = Vectorize(largest_cluster, 
							vectorize.args = 'h')
tibble(h = seq(min(clusters@height), max(clusters@height), 
			   length.out = 50), 
	   largest_cluster = largest_cluster(clusters, h)) %>%
	ggplot(aes(h, largest_cluster)) + 
	geom_point() + 
	geom_line() +
	scale_y_log10()

#' The plot suggests that we can obtain a largest mid-size cluster around $h = .97$.  We then filter 'non-trivial clusters' that contain more than 5 terms.  
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

cluster_terms

## --------------------
#' ## Cluster mapping:  Vector projection approach ##
#' TODO: pick up documentation from here
#' looks like counts are much more informative than projections 

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
#' ## Cluster mapping:  Count approach ##
## Count cluster occurrences in comments
comment_counts = cluster_terms %>%
	# map(~ str_count(comments$comment_text, 
	# 				regex(.x, ignore.case = TRUE))) %>%
	map(~ {tokens_df %>%
			filter(token %in% .x) %>%
			group_by(comment_id) %>%
			summarize(n = n())}) %>%
	bind_rows(.id = 'cluster') %>% 
	filter(!duplicated(.)) %>% 
	mutate(cluster = {str_c('cluster_', cluster) %>%
						forcats::as_factor()}) %>% 
	right_join(comments)
	
comment_counts %>%
	filter(commenter_type %in% c('advocacy', 'industry'), 
		   !is.na(cluster)) %>%
	select(-comment_text) %>%
	# gather(cluster, count, starts_with('cluster'),
	# 	   factor_key = TRUE) %>% 
	# ## Trim out the zero-counts
	# filter(count > 0) %>%
	ggplot(aes(commenter_type, n, 
			   fill = commenter_type)) +
	geom_dotplot(binaxis = 'y', stackdir = 'center', 
				 color = NA, dotsize = 1.2) +
	facet_wrap(~ cluster, scales = 'free_y')

cluster_terms[[5]]
cluster_terms[[6]]
