#' This script combines comments (scraped directly from Regulations.gov) with attachment text (downloaded and converted to plaintext). 

library(tidyverse)
library(tidytext)
library(stringr)

## Location of the attachment files
files_folder = 'files'

## Load attachments df
attachments = readxl::read_excel('attachments.xlsx')
## Parse comment ID
attachments = attachments %>%
	filter(is.na(exclude)) %>%
	mutate(comment_id = str_match(file, '(.*)-[0-9]+')[,2])
## Load attachment text
attachments = attachments %>%
	mutate(file = str_c(files_folder, '/', file, '.txt')) %>%
	mutate(comment_text = Vectorize(read_file)(file))

## Remove variables we don't need
attachments = select(attachments, file, comment_id, comment_text)

## Use mean IDF to identify attachments with OCR errors
tokens = attachments %>%
	unnest_tokens(token, comment_text) %>%
	group_by(comment_id, token) %>%
	summarize(token_n = n()) %>%
	bind_tf_idf(document_col = comment_id, 
				term_col = token, 
				n_col = token_n) %>%
	ungroup()
docs_idf = tokens %>%
	group_by(comment_id) %>%
	summarize(idf = mean(idf))
ggplot(docs_idf, aes(idf)) + geom_density() + geom_rug()

## Documents with the highest mean IDF
docs_idf %>%
	arrange(desc(idf)) %>%
	head(10)
## 0442 has lots of problems
## 0473 has lots of numbers, but looks okay
## 0217 has lots of numbers, and some errors, but also looks okay
## 0235 looks fine
## 0353 has some numbers, but is fine
## 0371 is fine
## 0135 contains several vaccine or disease names
## 0525 is fine
## 505 has some problems, but seems to be good enough to include
## 518 contains a large number of URLs

## Documents with the lowest mean IDF
docs_idf %>%
	arrange(idf) %>%
	head(5)
## These look to be just quite short (at most ~90-100 words). 

problem_attachments = c('EPA-HQ-OPP-2016-0385-0442')
attachments = attachments %>%
	filter(!(comment_id %in% problem_attachments))

## Join w/ comments
load('comments.Rdata')
comments_meta = readxl::read_excel('comment metadata.xlsx')

comments = comments_meta %>% 
	select(comment_id, commenter_type = `commenter type`, 
		   valence) %>%
	full_join(comments)

comments = full_join(comments, attachments) %>%
	## Propagate metadata
	group_by(comment_id) %>%
	mutate(date = first(date), 
		   url = first(url), 
		   commenter_type = first(commenter_type), 
		   valence = first(valence)) %>% 
	ungroup %>%
	arrange(comment_id)

str(comments, max.level = 1)

save(comments, file = 'comments_attachments.Rdata')
sessionInfo()
