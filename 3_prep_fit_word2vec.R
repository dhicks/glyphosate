#' This script prepares the comments and attachments for processing by word2vec, then trains the word2vec model.  

library(tidyverse)
library(stringr)
if (!require(wordVectors)) {
	if (!(require(devtools))) {
		install.packages("devtools")
	}
	devtools::install_github("bmschmidt/wordVectors")
}

load('comments_attachments.Rdata')

## Output files
## The success ngram tokenings, and the word2vec model
outfiles = c(str_c('3_', c('unigrams', 'bigrams', 'trigrams'), 
				   '.txt'), 
				 'glyphosate_w2v.bin')

#' The wordVectors vignette (<https://github.com/bmschmidt/wordVectors/blob/master/vignettes/introduction.Rmd>) uses `prep_word2vec` to combine the texts into a single text file for training.  However, that function assumes we have a single text file or directory of text files that need to be lowercased, tokenized, etc.  Since we don't want to use all of the attachments, and need to handle comments (which aren't in files at this point), we'll use tidytext instead.  
#' In particular, according to the vignette, prep does three things: 
#' 
#' 1. Creates a single text file with the contents of every file in the original document;
#' 2. Uses the `tokenizers` package to clean and lowercase the original text, [sic]
#' 3. If [the `prep_word2vec` variable] `bundle_ngrams` is greater than 1, joins together common bigrams into a single word. For example, "olive oil" may be joined together into "olive_oil" wherever it occurs.
#' 
#' Note that prep does *not* appear to identify or remove stopwords. This may because the subsampling technique used to fit the model (cf <http://mccormickml.com/2017/01/11/word2vec-tutorial-part-2-negative-sampling/>) automatically accounts for stopwords, i.e., "so frequent they're meaningless" terms.  

library(tidytext)
## Break into tokens
tokens_df = comments %>% 
	select(-attachment_urls) %>%
	unnest_tokens(token, comment_text)

## Generate files of ngrams
tokens_df %>%
	.$token %>%
	cat(file = outfiles[1])
## Combine bigrams, trigrams, and quadgrams
word2phrase(outfiles[1], outfiles[2], force = TRUE)
word2phrase(outfiles[2], outfiles[3], force = TRUE)

#' The blocks above construct ngram-ed text files suitable for processing by `train_word2vec`.  Next we ngram-ize `tokens_df`.  We first read back in the final file and extract its vocabulary. 
ngramed = read_file(outfiles[3])
vocabulary = ngramed %>%
	str_split(' ') %>% 
	## str_split returns a list
	unlist() %>%
	unique() %>% 
	sort()
#' Next we construct all of the bigrams, trigrams, and quadgrams in tokens_df. We then filter these based on which tokens appear in the vocabulary.  
#' NB A limitation of the particular approach below is that we don't remove overlapping ngrams.  For example, the first comment contains 'our,' 'our_food,' 'our_food_supply,' 'food,' 'food_supply,' and 'supply,' rather than just 'our_food_supply.'  
tokens_df %>%
	group_by(comment_id) %>%
	mutate(token.idx = row_number()) %>% 
	ungroup() %>%
	mutate(token_p1 = lead(token), 
		   bigram = str_c(token, token_p1, sep = '_'), 
		   token_p2 = lead(token_p1), 
		   trigram = str_c(bigram, token_p2, sep = '_'), 
		   token_p3 = lead(token_p2), 
		   quadgram = str_c(trigram, token_p3, sep = '_')) %>% 
	select(-starts_with('token_')) %>% 
	gather(key = ngram, value = token, 
		   token:quadgram, -token.idx) %>% 
	filter(token %in% vocabulary) %>% 
	filter(!duplicated(.)) %>%
	arrange(comment_id, token.idx) -> tokens_df
tokens_df %>%
	filter(str_detect(comment_id, '0002')) %>%
	select(comment_id, valence, token) %>%
	head(20)
## Save this for use in later scripts
save(tokens_df, file = 'tokens_df.Rdata')

#' Now we train the word2vec model. (Or, since training can take several minutes, we detect that the file already exists and skip training.)
if (!file.exists(outfiles[4])) {
	system.time(
		model <- train_word2vec(outfiles[3],
								outfiles[4],
								vectors=100,
								threads=4,
								window=12,
								iter=10,
								negative_samples=15, 
								force = TRUE)
	)
} else {
	print('Model file found; skipping training')
}

sessionInfo()
