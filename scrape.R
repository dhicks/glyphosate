library(tidyverse)
library(lubridate)
library(stringr)
library(xml2)
library(foreach)

#' Docket of interest
docket_id = 'EPA-HQ-OPP-2016-0385'
pdf_folder = 'pdf'
source('api_key.R')

#' Retrieve IDs for the public submissions in the docket
## https://www.regulations.gov/docket?D=EPA-HQ-OPP-2016-0385
query_url = str_c('https://api.data.gov/regulations/v3/documents.xml', 
				  '?',
				  'api_key=', api_key, '&', 
				  'dct=PS', '&', ## public submissions only
				  'rpp=1000', '&', ## get up to 1k docs
				  'dktid=', docket_id)
response = read_xml(query_url)
total_docs = response %>% xml_find_first('//recordCount') %>%
	xml_text %>% as.numeric
doc_ids = response %>% xml_find_all('//documentId') %>% xml_text

#' Retrieve the data for each document
# doc_id = doc_ids[10]
comments = foreach(doc_id = doc_ids, 
				   .verbose = FALSE, 
				   .combine = bind_rows) %do% {
		   	query_url = str_c('https://api.data.gov/regulations/v3/document.xml', 
		   					  '?', 
		   					  'api_key=', api_key, '&', 
		   					  'documentId=', doc_id)
		   	response = read_xml(query_url)
		   	
		   	num_comments = response %>% xml_find_first('//numItemsRecieved') %>% 
		   		xml_text %>% as.numeric
		   	comment_text = response %>% xml_find_first('//comment') %>%
		   		xml_text
		   	date = response %>% 
		   		xml_find_first('//receivedDate') %>%
		   		xml_text %>%
		   		ymd_hms %>%
		   		round_date(unit = 'day')
		   	
		   	attachments = response %>% xml_find_all('//attachment')
		   	attachment_urls = attachments %>% xml_find_first('fileFormats') %>%
		   		xml_text
		   	if (length(attachment_urls) == 0) {
		   		attachments = NA
		   		attachment_urls = NA
		   	}
		   	tibble(comment_id = doc_id, 
		   		   num_comments, 
		   		   comment_text, 
		   		   date, 
		   		   attachment_urls = list(attachment_urls))
		   }

#' Retrieved
{{nrow(comments)}}
#' distinct comments from docket 
{{docket_id}}
#', making 
{{sum(comments$num_comments)}}
#' total comments. There are
{{comments$attachment_urls %>% unlist %>% .[!is.na(.)] %>% length}}
#' attachments to download. 

#' Retrieve PDF attachments
attachment_urls = comments$attachment_urls %>% unlist %>% .[!is.na(.)]
# attachment_url = attachment_urls[1]
pdf_files = foreach(attachment_url = attachment_urls) %do% {
	parent_doc = str_match(attachment_url, 'documentId=([[:alnum:]\\-]+)')[,2]
	attachment_num = str_match(attachment_url, 'attachmentNumber=([0-9]+)')[,2]
	filename = str_c(parent_doc, '-', attachment_num, '.pdf')
	destfile = str_c(pdf_folder, '/', filename)
	if (!file.exists(destfile)) {
		download.file(url = str_c(attachment_url, '&', 'api_key=', api_key), 
					  destfile = destfile)
	}
	
	## pdftotext: <http://www.foolabs.com/xpdf/download.html>
	system2('pdftotext', destfile, str_replace('pdf/', filename, 'pdf', 'txt'))
}

# save(comments, file = 'comments.Rdata')
# sessionInfo()

