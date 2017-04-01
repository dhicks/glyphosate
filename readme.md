# Glyphosate SAP Clustering #

## Daniel J. Hicks, PhD ##

This project demonstrates the use of text mining methods to analyze public comments on government regulatory actions.  Specifically, I analyze comments on a meeting of EPA's Scientific Advisory Panel, held December 13-16, 2016, to discuss the herbicide glyphosate.  Public comments were posted in the Regulations.gov docket [EPA-HQ-OPP-2016-0385](https://www.regulations.gov/docket?D=EPA-HQ-OPP-2016-0385).  

The analysis is broken into a series of steps.  Most of these steps are completely automated, using the scripts found in this repository.  

1. The script `scrape.R` retrieves all public comments and selected attachments (PDF, Microsoft Word, and plaintext files) found in the docket.  Attachments are converted to plaintext files using two command-line tools, `pdftotext` and `pandoc`. 

2. The downloaded attachments are manually reviewed for completeness (that is, to confirm that all files were downloaded correctly), accuracy of plaintext conversation, and relevance. In this particular docket, many of the attachments were petition signature pages, submissions from letter-writing campaigns (where individuals merely signed their name to prepared letters), previous publications (such as EPA documents or peer-reviewed journal articles), bibliographies, or commentators' CVs, resumes, or biographies.  All of these attachment types were excluded from the rest of the analysis.  Attachments from comments that were not excluded were classified as coming from industry, environment or consumer advocacy organizations, academics, representatives from government or government-equivalent bodies (in particular, IARC), or individuals who did not appear to have a relevant affiliation.  

3. Comments and attachments are combined into a single dataframe using the script `combine comments and attachments.R`.  

4. 