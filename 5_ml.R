#' This script constructs a machine learning model to predict comment valences (supporting or opposed to the use of glyphosate), then analyzes the terms that contribute to this prediction.  

library(caret)
library(randomForest)
library(pdp)

library(tidyverse)
library(tidytext)
library(stringr)

## Using a parallel backend speeds up fitting the random forest model
library(doSNOW)
cl = parallel::makeCluster(parallel::detectCores())
registerDoSNOW(cl)

#' First we load the data and construct a vocabulary.  In this version of the script we use tf-idf.  Future versions might use information gain.  

## Load tokens
load('tokens_df.Rdata')
table(tokens_df$valence, useNA = 'ifany')
tokens_df %>%
	filter(!is.na(valence), 
		   str_detect(token, '[a-z]+')) -> tokens_df

## Construct token counts and calculate tf-idf across comments
token_n_df = tokens_df %>%
	group_by(comment_id, valence, token) %>%
	summarize(token_n = n()) %>%
	ungroup() %>%
	bind_tf_idf(token, comment_id, token_n)
	
## Plot tf-idf
ggplot(token_n_df, aes(tf_idf)) + stat_ecdf() + scale_x_log10()

## Subset based on tf-idf
## In a real project, we'd probably do more robust feature selection here. 
terms = token_n_df %>%
	arrange(desc(tf_idf)) %>%
	slice(1:2000) %>%
	.$token %>%
	unique()

str(terms)

## Spread token counts across comments
token_n_df %>%
	filter(token %in% terms) %>%
	select(comment_id, valence, token, token_n) %>%
	spread(token, token_n, fill = 0) -> token_n_df

#' Next we fit the machine learning model.  To estimate out-of-model error, we first partition the data into training and testing sets.  

## Partition training and testing sets
set.seed(1234)
in_train = createDataPartition(token_n_df$valence, p = .5, 
							   list = TRUE)$Resample1
token_n_df %>%
	mutate(training = row_number() %in% in_train) %>%
	select(comment_id, training, valence, everything()) -> 
	token_n_df
token_n_df %>%
	filter(training) %>%
	select(-comment_id, -training) -> 
	training
token_n_df %>%
	filter(!training) %>%
	select(-comment_id, -training) ->
	testing

## Fit the model
set.seed(113355)
rf_control = trainControl(# 10-fold crossvalidation
	method = 'cv', 
	n = 10, 
	classProbs = TRUE,
	# use the doSNOW parallel backend
	allowParallel = TRUE
)
rf_grid = tibble(mtry = 2^seq(1, 9, by = 2))
system.time(
	rf_fit <- train(x = training[-1],
					y = training$valence, 
					tuneGrid = rf_grid,
					trControl = rf_control,
					method = 'rf')
)
rf_fit

## In-sample error
confusionMatrix(rf_fit)

## Out-of-sample error
predict(rf_fit, testing) %>%
	confusionMatrix(testing$valence)
## Some trouble with the positive comments, in part because there're
## so few of them.

#' Finally we extract variable importance and partial dependence plots for the 25 most important variables.  

## Variable importance
importance = rf_fit %>%
	varImp() %>% 
	.$importance %>% 
	rownames_to_column(var = 'token') %>%
	rename(importance = Overall) %>%
	arrange(desc(importance))

## Plot importance of the top 25 variables
importance %>%
	slice(1:25) %>%
	ggplot(aes(reorder(token, importance), importance)) + 
	geom_point() + 
	geom_linerange(aes(ymin = 0, ymax = importance)) +
	xlab('token') +
	coord_flip()

## Partial dependence plots
part_dep = importance %>%
	slice(c(1:25)) %>%
	.$token %>%
	## The next few lines are unperspicuous. 
	## Each list item is one token.  
	## Name that list item with the token
	`names<-`(., .) %>%
	## Calculate the range of feature values for each token
	map(~ tibble(foo = min(training[.x]):max(training[.x]))) %>%
	## Use the list item name to rename the column
	map2(names(.), ~ `names<-`(.x, .y)) %>%
	## Calculate partial dependencies, using list item name
	## to get the token
	map2(names(.), ~ pdp::partial(rf_fit, 
							 pred.var = .y, 
							 pred.grid = .x, 
							 prob = TRUE, 
							 parallel = TRUE)) %>%
	## Then replace the column names with generic names
	map(~ `names<-`(.x, c('token_count', 'prob'))) %>%
	bind_rows(.id = 'token') %>%
	mutate(token = forcats::as_factor(token), 
		   prob = 1 - prob)

#' In the partial dependence plots, the x-axis corresponds to the number of occurrences of the given token, while the y-ais corresponds to the probability that the comment is classified "positive" given the token count.  An increasing curve indicates a term that is more associated with positive comments. 

ggplot(part_dep, aes(token_count, prob, group = token)) + 
	geom_line() +
	facet_wrap(~ token, scales = 'free_x') +
	# xlim(0,5) + 
	# scale_x_continuous(breaks = scales::pretty_breaks()) +
	ylab('p(positive | token_count)')

#' This is (probably) an elaborate and somewhat obscure way to tell us what information gain or relative counts already told us in script 4.  


## --------------------
## Example code using ICEbox
## This is a cool generalization of partial dependence plots (pun intended); see <http://www.tandfonline.com/doi/abs/10.1080/10618600.2014.907095>.  However, the library doesn't include support for ggplot or broom-style dataframe output, making it annoying to loop over or incorporate into further analysis.  The example code below builds a ggplot version of the plot for a single term.  

# library(ICEbox)
# thing = ice(object = rf_fit$finalModel, 
# 			X = as.data.frame(training[-1]), 
# 			# y = training[['valence']],
# 			predictor = 'farmers', 
# 			predictfcn = function(object, newdata) {
# 				# stop('monkey')
# 				predict(object, newdata, type = 'prob')[, 2]
# 			})
# 
# curves_df = thing$ice_curves %>%
# 	as_tibble %>%
# 	mutate(curve = row_number(), 
# 		   token = thing$predictor) %>%
# 	gather(x, y, -curve, -token) %>%
# 	mutate(x = as.numeric(x))
# predictions_df = tibble(x = thing$xj,
# 						y = thing$actual_prediction, 
# 						token = thing$predictor)
# ggplot() +
# 	geom_line(data = curves_df, aes(x, y, group = curve), 
# 			  alpha = .1) +
# 	geom_point(data = predictions_df, aes(x, y)) + 
# 	xlab('token count') + ylab('p(negative | token count)') +
# 	facet_wrap(~ token)
# 
