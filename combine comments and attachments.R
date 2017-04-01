#' This script combines comments (scraped directly from Regulations.gov) with attachment text (downloaded and converted to plaintext). 

library(tidyverse)
library(stringr)

## Location of the attachment files
files_folder = 'files'

load('comments.Rdata')

## Load attachments df
attachments = readxl::read_excel('files.xlsx')
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

## Join w/ comments
comments = full_join(comments, attachments) %>%
	## Propagate dates
	group_by(comment_id) %>%
	mutate(date = first(date)) %>%
	ungroup

str(comments, max.level = 1)

save(comments, file = 'comments and attachments.Rdata')
sessionInfo()