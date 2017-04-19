TODO: document all the things

library(caret)
library(pdp)
library(tidyverse)
library(tidytext)
library(stringr)

## Load tokens
load('tokens_df.Rdata')
tokens_df %>%
	filter(commenter_type %in% c('advocacy', 'industry'), 
		   str_detect(token, '[a-z]+')) -> tokens_df

## Construct token counts and calculate tf-idf across comments
token_n_df = tokens_df %>%
	group_by(comment_id, commenter_type, token) %>%
	summarize(token_n = n()) %>%
	ungroup() %>%
	bind_tf_idf(token, comment_id, token_n)
	
## Plot tf-idf
ggplot(token_n_df, aes(tf_idf)) + stat_ecdf() + scale_x_log10()

## Subset based on tf-idf
terms = token_n_df %>%
	arrange(desc(tf_idf)) %>%
	slice(1:2000) %>%
	.$token %>%
	unique()

## Spread token counts across comments
token_n_df %>%
	filter(token %in% terms) %>%
	select(comment_id, commenter_type, token, token_n) %>%
	spread(token, token_n, fill = 0) -> token_n_df

## Partition training and testing sets
set.seed(1234)
in_train = createDataPartition(token_n_df$commenter_type, p = .5, 
							   list = TRUE)$Resample1
token_n_df %>%
	mutate(training = row_number() %in% in_train) %>%
	select(comment_id, training, commenter_type, everything()) -> 
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
rf_fit = train(x = training[-1],
			   y = training$commenter_type, 
			   verbose = TRUE,
			   method = 'rf')

## In-sample error
confusionMatrix(rf_fit)

## OOS error
predict(rf_fit, testing) %>%
	confusionMatrix(testing$commenter_type)

## Variable importance
importance = varImp(rf_fit) %>% 
	.$importance %>% 
	rownames_to_column(var = 'token') %>%
	rename(importance = Overall) %>%
	arrange(desc(importance))

## Plot importance of the top 25 variables
importance %>%
	slice(1:25) %>%
	ggplot(aes(reorder(token, importance), importance)) + 
	geom_point() + 
	xlab('token') +
	coord_flip()

## Partial dependence plots
part_dep = importance %>%
	## NB until 'in' is removed, slice has to go around it
	slice(c(1:25)) %>%
	.$token %>%
	## The next few lines are unperspicuous. 
	## Each list item is one token.  Name that list item with the token
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
							 prob = TRUE)) %>%
	## Then replace the column names with generic names
	map(~ `names<-`(.x, c('token_count', 'prob'))) %>%
	bind_rows(.id = 'token') %>%
	mutate(token = forcats::as_factor(token))
	
ggplot(part_dep, aes(token_count, prob, group = token)) + 
	geom_line() +
	facet_wrap(~ token) +
	xlim(0,5) + ylab('p(advocacy | token_count)')

## This is an elaborate and somewhat obscure way to tell us what information gain or relative counts already told us in script 4.  
