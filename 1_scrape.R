#' This script retrieves all comments and selected attachments for a given Regulations.gov docket.  Beyond the dependencies in the first code block, it also uses two external command-line tools, pdftotext <http://www.foolabs.com/xpdf/download.html> and pandoc <http://pandoc.org/>.  

library(tidyverse)
library(lubridate)
library(stringr)
library(xml2)

library(foreach)
library(doSNOW)

cl = parallel::makeCluster(parallel::detectCores())
registerDoSNOW(cl)

#' Top-level parameters
## Docket of interest
## https://www.regulations.gov/docket?D=EPA-HQ-OPP-2016-0385
docket_id = 'EPA-HQ-OPP-2016-0385'
## Folder to put downloads
files_folder = 'files'
if (!file.exists(files_folder)) {
	dir.create(files_folder)
}
## API key
## http://regulationsgov.github.io/developers/key/
source('api_key.R')

#' Retrieve IDs for the public submissions in the docket
query_url = str_c('https://api.data.gov/regulations/v3/documents.xml', 
				  '?',
				  'api_key=', api_key, '&', 
				  'dct=PS', '&', ## public submissions only
				  'rpp=1000', '&', ## get up to 1k docs
				  'dktid=', docket_id)
response = read_xml(query_url)
total_docs = response %>% 
	xml_find_first('//recordCount') %>%
	xml_text() %>% 
	as.numeric()
doc_ids = response %>% 
	xml_find_all('//documentId') %>% 
	xml_text()

#' Retrieve the data for each document
# doc_id = doc_ids[155]
## NB Running this in parallel will trigger rate-limiting
comments = foreach(doc_id = doc_ids, 
				   .verbose = FALSE, 
				   .combine = bind_rows, 
				   .packages = c('tidyverse', 'lubridate',
				   			  'stringr', 'xml2')) %do% {
   		  	query_url = str_c('https://api.data.gov/regulations/v3/document.xml', 
   		  					  '?', 
   		  					  'api_key=', api_key, '&', 
   		  					  'documentId=', doc_id)
   		  	response = read_xml(query_url)
   		  	
   		  	num_comments = response %>% 
   		  		xml_find_first('//numItemsRecieved') %>% 
   		  		xml_text() %>% 
   		  		as.numeric()
   		  	comment_text = response %>% 
   		  		xml_find_first('//comment') %>%
   		  		xml_text()
   		  	date = response %>% 
   		  		xml_find_first('//receivedDate') %>%
   		  		xml_text %>%
   		  		ymd_hms %>%
   		  		round_date(unit = 'day')
   		  	
   		  	attachments = response %>% 
   		  		xml_find_all('//attachment')
   		  	attachment_urls = attachments %>% 
   		  		xml_find_first('fileFormats') %>%
   		  		xml_text()
   		  	if (length(attachment_urls) == 0) {
   		  		attachments = NA
   		  		attachment_urls = as.character(NA)
   		  	}
   		  	## Don't get rate-limited
   		  	# Sys.sleep(1)
   		  	
   		  	regs_gov_url = str_c(
   		  		'https://www.regulations.gov/document?D=', 
   		  		doc_id
   		  	)
   		  	tibble(comment_id = doc_id, 
   		  		   url = regs_gov_url,
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
#' total attachments. 
#' 

#' Identify attachments to download
attachments = comments %>% 
	unnest(url = attachment_urls) %>%
	filter(!is.na(url))

attachments = attachments %>%
	mutate(type = str_match(url, 'contentType=([^&]*)')[,2], 
		   attachment_num = str_match(url, 'attachmentNumber=([0-9]+)')[,2], 
		   filename = str_c(comment_id, '-', attachment_num))

table(attachments$type)
# attachments %>% filter(type == 'msw8') %>% .$url %>% str_c('&api_key=', api_key)

attachments = attachments %>%
	filter(type %in% c('pdf', 'msw12', 'msw8', 'crtext'))

ext = function (type) switch(type, pdf = 'pdf', msw12 = 'docx', 
							 msw8 = 'doc', crtext = 'txt')
ext = Vectorize(ext)
attachments = attachments %>% mutate(ext = ext(type))

#' Download and convert to text
# attachment = attachments[48,]

pb <- txtProgressBar(max = nrow(attachments), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
foreach(attachment = iter(attachments, by = 'row'), .combine = c, 
		.packages = c('stringr'), 
		.options.snow = opts) %dopar% {
			## Download
			dl_version = str_c(files_folder, '/', 
							   attachment$filename, '.', attachment$ext)
			if (!file.exists(dl_version)) {
				url = str_c(attachment$url, '&', 
							'api_key=', api_key)
				download.file(url = url, destfile = dl_version)
			}
			
			## Convert to text
			txt_version = str_c(files_folder, '/', 
								attachment$filename, '.', 'txt')
			if (!file.exists(txt_version)) {
				if (attachment$ext == 'pdf') {
					## pdftotext: <http://www.foolabs.com/xpdf/download.html>
					system2('pdftotext', dl_version, txt_version)
				} else if (attachment$ext == 'docx') {
					## Pandoc
					system2('pandoc', c(dl_version, '-o', txt_version))
				} else if (attachment$ext == 'doc') {
					warning(str_c('Cannot convert ', attachment$filename,' from doc to txt automatically'))
				} else {
					stop('Unknown file type')
				}
			}
		}

save(comments, file = 'comments.Rdata')
sessionInfo()

