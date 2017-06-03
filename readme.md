# Glyphosate SAP Clustering #

## Daniel J. Hicks, PhD ##

This project demonstrates the use of text mining methods to analyze public comments on government regulatory actions.  Specifically, I analyze comments on a meeting of EPA's Scientific Advisory Panel, held December 13-16, 2016, to discuss the herbicide glyphosate.  Public comments were posted in the Regulations.gov docket [EPA-HQ-OPP-2016-0385](https://www.regulations.gov/docket?D=EPA-HQ-OPP-2016-0385).  

The analysis is broken into a series of steps.  Most of these steps are completely automated, using the scripts found in this repository.  Each script has a corresponding literate HTML version.  Running these scripts, in numerical order, will completely reproduce the analysis.  In the steps below, filename links will take you to HTML versions of scripts (generated with `rmarkdown::render`), which include both code and output.  

Some brief findings are presented in the final script file, `7_findings.R`.  However, the focus of this demonstration project is on data-gathering and -analysis methods, rather than the development of communicable findings.  

1. The script [`1_scrape.R`](1_scrape.html) retrieves all public comments and selected attachments (PDF, Microsoft Word, and plaintext files) found in the docket.  Attachments are converted to plaintext files using two command-line tools, `pdftotext` and `pandoc`. 

2. 
    a. The downloaded attachments are manually reviewed for completeness (that is, to confirm that all files were downloaded correctly), accuracy of plaintext conversation, and relevance. In this particular docket, many of the attachments were petition signature pages, submissions from letter-writing campaigns (where individuals merely signed their name to prepared letters), previous publications (such as EPA documents or peer-reviewed journal articles), bibliographies, or commentators' CVs, resumes, or biographies.  All of these attachment types were excluded from the rest of the analysis.  This step is conducted using the file `2_attachments.xlsx`.  

    b. All comments are manually classified under two variables:  
    
    - *commenter type*:  one of academic, advocacy (consumer or environmental), government, industry (including organic farmers), or individual (anonymous or lacking a determinable affiliation)
    - *valence*: either `neg` (arguing against glyphosate, its registration, use, or that it poses health risks) or `pro` (arguing for glyphosate)
    
    For this step, the file `1_comment_metadata.csv` (generated in step 1) is opened in Excel, edited, and saved as `2_comment_metadata.xlsx`.  

3. Comments and attachments are combined into a single dataframe by the script `3_combine_comments_attachments.R`.  

4. The script `4_prep_fit_word2vec.R` standardizes the combined comments, identifies significant multigrams (phrases comprising multiple words), and fits a [word embedding](https://en.wikipedia.org/wiki/Word2vec) model.  Word embeddings represent words in relatively low-dimensional vector spaces; the cosine similarity of a pair of vectors is proportionate to the probability of the corresponding words occurring close to each other.  The fitted word2vec model is saved in the file `glyphosate_w2v.bin`.  

5. The script `5_clustering.R` uses affinity propagation to construct clusters of terms.  "Focal terms" are identified as the terms with the highest information gain for distinguishing industry and advocacy comments.  This initial termlist is expanded by taking the 500 most similar terms from the fitted word embedding model.  These 500 terms are then clustered using affinity propagation and word embedding similarity.  After clusters are developed, they are "mapped" to individual comments, and [dot plots](https://en.wikipedia.org/wiki/Dot_plot_(statistics)) are used to identify clusters that are more strongly associated with industry or advocacy comments.  

6. The script `6_ml.R` constructs a machine learning classifier to distinguish pro- and anti-glyphosate comments, extracts the most important terms for this classifier, and constructs partial dependence plots. 

7. The file `7_findings.R` draws some implications from the cluster analysis in step 5.  It discusses linguistic differences between advocacy and industry comments; suggests how these differences might have influenced EPA's decisionmaking; and examines the relative prominence of workers, consumers, children, and farmers in the comments.  


NB To view HTML views, prepend `https://htmlpreview.github.io/?` to the entire URL, including the `https`.  For example, <https://htmlpreview.github.io/?https://github.com/dhicks/glyphosate/blob/master/7_findings.html>.  