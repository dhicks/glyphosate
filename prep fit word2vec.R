#' This script prepares the comments and attachments for processing by word2vec, then trains the word2vec model.  

library(tidyverse)
library(stringr)
if (!require(wordVectors)) {
	if (!(require(devtools))) {
		install.packages("devtools")
	}
	devtools::install_github("bmschmidt/wordVectors")
}

load('comments and attachments.Rdata')

## Output files
## The success ngram tokenings, and the word2vec model
outfiles = c(str_c(c('unigrams', 'bigrams', 'trigrams'), '.txt'), 
				 'glyphosate_w2v.bin')

#' The wordVectors vignette (<https://github.com/bmschmidt/wordVectors/blob/master/vignettes/introduction.Rmd>) uses `prep_word2vec` to combine the texts into a single text file for training.  However, that function assumes we have a single text file or directory of text files that need to be lowercased, tokenized, etc.  Since we don't want to use all of the attachments, and need to handle comments (which aren't in files at this point), we'll use tidytext instead.  
#' In particular, according to the vignette, prep does three things: 
#' 1. Creates a single text file with the contents of every file in the original document;
#' 2. Uses the `tokenizers`` package to clean and lowercase the original text, [sic]
#' 3. If [`prep_word2vec` variable] bundle_ngrams` is greater than 1, joins together common bigrams into a single word. For example, "olive oil" may be joined together into "olive_oil" wherever it occurs.
#' 
#' Note that prep does *not* appear to identify or remove stopwords. This may because the subsampling technique used to fit the model (cf <http://mccormickml.com/2017/01/11/word2vec-tutorial-part-2-negative-sampling/>) automatically accounts for stopwords, i.e., "so frequent they're meaningless" terms.  

library(tidytext)
## Break into tokens
tokens_df = comments %>% 
	select(-attachment_urls) %>%
	unnest_tokens(token, comment_text)
## Save this for use in the clustering script
save(tokens_df, file = 'tokens_df.Rdata')

## Generate files of ngrams
outfile = file(outfiles[1])
writeLines({tokens_df %>% .$token}, outfile, sep = ' ')
close(outfile)
## Combine bigrams and trigrams
word2phrase(outfiles[1], outfiles[2], force = TRUE)
word2phrase(outfiles[2], outfiles[3], force = TRUE)

## Train the model
if (!file.exists(outfiles[4])) {
	system.time(
		model <- train_word2vec(outfiles[3],
								outfiles[4],
								vectors=100,
								threads=4,
								window=12,
								iter=10,
								negative_samples=10, 
								force = TRUE)
	)
} else {
	print('Model file found; skipping training')
}

sessionInfo()
