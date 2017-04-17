# Glyphosate SAP Clustering #

## Daniel J. Hicks, PhD ##

**This demonstration project is a work in progress. Some steps below may be incomplete.** See the individual script files for details.  

This project demonstrates the use of text mining methods to analyze public comments on government regulatory actions.  Specifically, I analyze comments on a meeting of EPA's Scientific Advisory Panel, held December 13-16, 2016, to discuss the herbicide glyphosate.  Public comments were posted in the Regulations.gov docket [EPA-HQ-OPP-2016-0385](https://www.regulations.gov/docket?D=EPA-HQ-OPP-2016-0385).  

The analysis is broken into a series of steps.  Most of these steps are completely automated, using the scripts found in this repository.  

1. The script `1_scrape.R` retrieves all public comments and selected attachments (PDF, Microsoft Word, and plaintext files) found in the docket.  Attachments are converted to plaintext files using two command-line tools, `pdftotext` and `pandoc`. 

2. 
    a. The downloaded attachments are manually reviewed for completeness (that is, to confirm that all files were downloaded correctly), accuracy of plaintext conversation, and relevance. In this particular docket, many of the attachments were petition signature pages, submissions from letter-writing campaigns (where individuals merely signed their name to prepared letters), previous publications (such as EPA documents or peer-reviewed journal articles), bibliographies, or commentators' CVs, resumes, or biographies.  All of these attachment types were excluded from the rest of the analysis.  
    b. All comments were manually classified under two variables:  
        - *commenter type*:  one of academic, advocacy (consumer or environmental), government, industry (including organic farmers), or individual (anonymous or lacking a determinable affiliation)
        - *valence*: either `neg` (arguing against glyphosate, its registration, use, or that it posed health risks) or `pro` (arguing for glyphosate)

3. Comments and attachments are combined into a single dataframe by the script `2_combine_comments_attachments.R`.  

4. The script `3_prep_fit_word2vec.R` standardizes the combined comments, identifies significant multigrams (phrases comprising multiple words), and fits a [word embedding](https://en.wikipedia.org/wiki/Word2vec) model.  Word embeddings represent words in relatively low-dimensional vector spaces; the cosine similarity of a pair of vectors is proportionate to the probability of the corresponding words occurring close to each other.  The fitted word2vec model is saved in the file `glyphosate_w2v.bin`.  

5. The script `4_clustering.R` uses affinity propagation to construct clusters of terms.  "Focal terms" are identified as the terms with the highest information gain for distinguishing industry and advocacy comments.  This initial termlist is expanded by taking the 1,000 most similar terms from the fitted word embedding model.  These 1,000 terms are then clustered using affinity propagation.  After clusters are developed, they are "mapped" to individual comments, and violin plots are used to identify clusters that are more strongly associated with industry or advocacy comments.  

6. The script `5_ml.R` applies several different machine learning models to the same word embedding model.  As in the previous step, the aim is to identify word usage patterns that distinguish industry and advocacy comments.  


