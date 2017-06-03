#' The document briefly reports some findings from the cluster analysis conducted in `5_clustering.R`.  
#' 
#' # Summary #
#' Advocacy documents, taken together and compared to industry documents taken together, make greater use of personal and emotionally-loaded language rather than technical toxicological language, refer to diseases rather than toxicological studies, and connect the use of glyphosate to other prominent public scientific controversies (genetically modified foods and, to a lesser extent, vaccines).  
#' 
#' Industry documents make greater use of technical language, including discussions of both particular studies and general research quality standards. Industry connects the use of glyphosate to the agronomic benefits of genetically modified crops, especially sugarbeets.  
#' 
#' The text of public comments, by themselves, cannot tell us how they influenced EPA's decisionmaking.  However, both STS researchers and political philosophers have noted that personal narratives and emotionally-loaded language are often discounted — as anecdotal, uninformed, and irrational — in favor of the more technical, emotionally moderated language of credentialed experts.  It is at least plausible that the use of personal and emotionally-loaded language worked against advocacy in this case.  
#' 
#' The remainder of this document briefly examines the content of each of the advocacy- and industry-associated clusters.  At the end, I consider work by Guthman and Brown on a preference for consumer-based criticisms of pesticides, rather than worker-based criticisms.  

library(tidyverse)
library(stringr)
library(wordVectors)

load('5_clustering.Rdata')
names(cluster_terms) = str_c('cluster_', 1:length(cluster_terms))

#' First we re-plot the dotplots of vector projections. Note the use of a log scale.  
#+ fig.height = 7, fig.width = 7, fig.align = 'center'
ggplot(comment_counts, aes(commenter_type, n, 
					  fill = commenter_type)) +
	geom_dotplot(binaxis = 'y', stackdir = 'center', 
				 color = NA, dotsize = 1.2) +
	facet_wrap(~ cluster, scales = 'free_y') + 
	scale_y_log10()

#' Inspecting these dotplots, we identify clusters that appear to be especially associated with advocacy or industry.  
advocacy_clusters = c(6, 11, 13, 14, 17, 19, 21, 22)

industry_clusters = c(1:5, 7:10, 12, 15, 18, 20)

#' ## Advocacy clusters ##
#' ### 6 ###
cluster_terms[[6]]
#' This cluster includes both personal and emotionally-charged language.  Examples of personal language includes the use of first- and second-person pronouns; examples of emotionally-charged language include a few positive ("courage," "hope") and negative terms ("stop," "do your job," "please stop," "poisoning our," "hurting," "toxic chemicals"), as well as references to children and future generations.  
#' 
#' ### 11 ###
cluster_terms[[11]]
#' This cluster appears to refer to the work of Stephanie Seneff, a MIT computer scientist <https://en.wikipedia.org/wiki/Stephanie_Seneff#Research_on_biology_and_medical_topics>; <https://people.csail.mit.edu/seneff/>.  Seneff has published highly controversial work hypothesizing that glyphosate can interfere with the human microbiome in ways that, she suggests, might cause autism and Alzheimer's disease.
#' 
#' ### 13 ###
cluster_terms[[13]]
#' This cluster also appears to refer to the work of Stephanie Seneff. This cluster's language is more specific than cluster 11's, and perhaps for this reason it does not have the false positive industry comments. 
#' 
#' ### 14 ###
cluster_terms[[14]]
#' This cluster makes further references to health concerns associated with glyphosate.  Z-scores for clusters 13 and 14 are strongly correlated.  

comment_z %>%
	select(comment_id, cluster, magnitude) %>%
	filter(cluster %in% c('cluster_13', 'cluster_14')) %>%
	spread(cluster, magnitude) %>%
	select(-comment_id) %>%
	cor()

#' ### 17 ###
cluster_terms[[17]]
#' This cluster refers to vaccines.  Specifically, the anti-GMO advocacy organization Moms Across America <http://www.momsacrossamerica.com/> has several comments in the docket reporting findings of glyphosate contamination in vaccines.  
#' 
#' ### 19 ###
cluster_terms[[19]]
comment_counts %>%
	filter(cluster == 'cluster_19', 
		   n > 15) %>%
	arrange(desc(n))
#' This cluster appears to refer to the use of glyphosate on genetically modified corn, soy, and (on the industry side) sugarbeet.  
#' 
#' ### 21 ###
cluster_terms[[21]]
#' This cluster appears to refer to several specific kinds of cancer. 
#' 
#' ### 22 ###
cluster_terms[[22]]
comment_counts %>%
	filter(cluster == 'cluster_22')
#' Comments 0046 and 0501 come from Jennifer Sass of the Natural Resources Defense Council. Comment 0507 comes from the Center for Food Safety.  All three comments refer to a legal exchange involving Enlist Duo, an herbicide cocktail that combines glyphosate and dicamba. In particular, they each include the legal citation "Resp’ts’ Mot. for Voluntary Vacatur and Remand, NRDC v. EPA, Case Nos. 14-73353." 
#' 

#' ## Industry Clusters ##
#' ### 1 ###
cluster_terms[[1]]
#' This cluster includes technical language reporting the results of toxicological safety studies.  "Portier" refers to Christopher Portier, the lead toxicologist on the IARC review of glyphosate.  The IARC review — and a related paper, comparing IARC's methods to those of other government agencies — are cited throughout the docket.  Portier was initially appointed to the SAP for EPA's review of glyphosate.  In comment  0005, CropLife America complained that "Dr. Portier has regularly engaged in policy advocacy against glyphosate" and therefore has "disqualifying biases for the purposes of appointment to the FIFRA SAP October panel." Portier responded to these objections in comment 0371, and his work is a significant subject of contention throughout the docket.  
tokens_df %>% 
	filter(token == 'portier') %>%
	group_by(comment_id, commenter_type, valence) %>%
	summarize(n = n()) %>%
	filter(n > 1)
	
#' 
#' ### 2 ###
cluster_terms[[2]]
#' As with cluster 2, this cluster focuses on discussing particular toxicological safety studies.  Many of these particular terms are author-date references.  
#' 
#' ### 3 ###
cluster_terms[[3]]
#' This cluster focuses on the agricultural uses of glyphosate, especially its ecological-environmental benefits when paired with genetically modified crops. One of the primary arguments for this use of glyphosate is that it reduces the need to till the soil for weed control, reducing erosion and promoting carbon capture.  
#' 
#' ### 4 ###
cluster_terms[[4]]
#' This cluster discusses the agricultural benefits of glyphosate, especially on genetically modified sugarbeets. It includes references to agricultural practices to reduce the unintentional spread of GM seeds. 
#' 
#' ### 5 ###
cluster_terms[[5]]
#' This cluster focuses on the research quality standards used by regulators to determine whether or not a given study should be included in the systematic review.  
#' 
#' ### 7 ###
cluster_terms[[7]]
#' This cluster appears to discuss the use of glyphosate by the sugarbeet industry.  Most commercial sugarbeets are genetically modified to tolerate glyphosate.  
#' 
#' ### 8 ###
cluster_terms[[8]]
#' The names in this cluster are the members of an industry-sponsored expert review panel.  
#' 
#' ### 9 ###
cluster_terms[[9]]
comment_counts %>%
	filter(cluster == 'cluster_9', 
		   n > 10)
#' Acquavella et al refers to the industry-sponsored expert review panel; "biomonitoring," "cohort" and "case control" refer to different types of toxicological study.  The two industry comments contain large attachments with detailed toxicological reviews.  The two advocacy comments both contain copies of a 15-page document prepared by the Natural Resources Defense Council, which also includes a review of the toxicological literature on glyphosate. 
#' 
#' ### 10 ###
cluster_terms[[10]]
#' The terms in this cluster appear to focus on testing for DNA damage due to glyphosate.  
#' 
#' ### 12 ###
cluster_terms[[12]]
#' *[]
#' 
#' ### 15 ###
cluster_terms[[15]]
#' This cluster may originate with charge questions posed to the FIFRA SAP, asking them to comment on the process of evaluating studies for inclusion in the systematic review.  
#' 
#' ### 18 ###
cluster_terms[[18]]
#' *[]
#' 
#' ### 20 ###
cluster_terms[[20]]
#' *[]
#' 


## --------------------
#' ## Workers and consumers ##
#' Guthman and Brown examined public comments on methyl iodide registration in California, observing that references to consumer health are about twice as comment as references to worker health.  They argue that "consumer-citizenship" "loses sight of those most subject to harm [namely, workers and their children]; at worst, following Foucault, the exaltation of certain bodies is a disregard of others, effectively allowing them to die." 
#' 
#' To examine this phenomenon in the glyphosate docket, we first examine the relationship between the magnitude of a word2vec vector (the length of the vector, in the abstract space of word embeddings) and the occurrence of the corresponding term in the dataset.  
magnitudes(focal_model) %>%
	tibble(token = names(.), 
		   w2v_magnitude = .) %>%
	inner_join(token_counts_df) %>%
	group_by(token, w2v_magnitude) %>%
	summarize(token_n = sum(token_n)) %>%
	ggplot(aes(w2v_magnitude, token_n)) + 
	geom_point() +
	# geom_smooth() +
	geom_smooth(method = 'lm') +
	scale_y_log10()
#' There is a negative exponential relationship between vector magnitude and token count:  the longer the vector, the less frequent the term.  

magnitudes(focal_model) %>%
	sort() %>%
	.[1:25]
magnitudes(focal_model) %>%
	sort(decreasing = TRUE) %>%
	.[1:25]

#' We can combine these magnitudes with the `closest_to` function to examine the prevalence of concepts across the dataset. In the next code block, we first set four "base terms": worker, consumer, children, and farmer.  For each of these seed terms, we retrieve the 13 most similar terms in the word2vec model, then the magnitude for each of the resulting 80 terms.  These are plotted, with shading to indicate how similar each term is to the given base term.  Negative magnitude is used, rather than magnitude, so that higher points intuitively represent more prevalent terms.  

term_magnitudes = c('workers', 'consumers', 'children', 'farmers') %>%
	`names<-`(., .) %>%
	map(~ closest_to(model, .x, fancy_names = FALSE, n = 13)) %>%
	bind_rows(.id = 'base_term') %>%
	mutate(magnitude = {model[[word, average = FALSE]] %>% 
							magnitudes()}) %>%
	arrange(magnitude)

ggplot(term_magnitudes, 
	   aes(base_term, -magnitude, alpha = similarity)) + 
	geom_point(position = position_jitter(width = .15))
	
#' The plot suggests that "workers" and its related terms are generally more common than "consumers" and its related terms.  The next chunk calculates the mean magnitude, and its standard error, weighting each term by its similarity to the base term.  "Consumers" has the largest weighted magnitude (i.e., the lowest prevalence in the dataset), due to a few terms with especially high magnitude; "workers" has the smallest (i.e., the highest prevalence in the dataset), both on its own and due to relatively low similarity and its high-magnitude terms.  

term_magnitudes %>%
	group_by(base_term) %>%
	summarize(mean_mag = Hmisc::wtd.mean(magnitude, 
										 weights = similarity, 
										 normwt = TRUE), 
			  mean_se = sqrt(Hmisc::wtd.var(magnitude, 
			  						 weights = similarity, 
			  						 normwt = TRUE) / n()))

#' These results do not indicate the same phenomenon identified by Guthman and Brown.  However, the text mining approach taken here is not sensitive to context; in particular, it cannot distinguish the way different groups of commenters (industry vs. advocacy organizations vs. individuals) talk about children, consumers, farmers, or workers.  A more thorough analysis would use the quantiative results here as the starting point for a focused qualitative analysis.  