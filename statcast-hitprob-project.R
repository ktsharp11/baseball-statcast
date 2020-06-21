## Katie Sharp
## Choose Your Own Project: Hit Probability Model 
## HarvardX:PH125.9x


#############################################################
# Data Import and Preprocessing 
#############################################################

# Load the required libraries
# libraries must be loaded in this order
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(grid)) install.packages("grid", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(ROCit)) install.packages("ROCit", repos = "http://cran.us.r-project.org")
if(!require(cvms)) install.packages("cvms", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(mlr)) install.packages("mlr", repos = "http://cran.us.r-project.org")
if(!require(parallel)) install.packages("parallel", repos = "http://cran.us.r-project.org")
if(!require(parallelMap)) install.packages("parallelMap", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")

# download the dataset
# this may take a few minutes
dl <- tempfile()
download.file("https://www.dropbox.com/s/qin7ivmkch8orbo/statcast2019_bat.csv?dl=1", dl)
bat_dat <- read.csv(dl, stringsAsFactors = FALSE)

# filter dataset for only balls in play
dat_ip <- bat_dat %>% filter(type == "X")
rm(bat_dat, dl)

# remove deprecated columns (all NAs)
dat_ip <- dat_ip %>% select_if(~!all(is.na(.)))

# add outcome variable
dat_ip <- dat_ip %>% mutate(hit = ifelse(events %in% c("single", "double", "triple", "home_run"), 1, 0))
dat_ip$hit <- as.factor(dat_ip$hit)

# add horiz angle variable
dat_ip$spray_angle <- with(dat_ip, round(atan((hc_x-125.42)/(198.27-hc_y))*180/pi,1))
dat_ip$adj_spray_angle <- with(dat_ip, ifelse(stand == "L",
                                              -spray_angle, spray_angle))
# add fielding team variable
dat_ip$fieldTeam <- with(dat_ip, ifelse(inning_topbot == "bot", away_team, home_team)) %>% as.factor()

# add positioning variable
dat_ip$inf_pos <- with(dat_ip, ifelse(if_fielding_alignment %in% c("Infield shift", "Strategic"), "inf_shift", "inf_standard")) %>% as.factor()
dat_ip$of_pos <- with(dat_ip, ifelse(of_fielding_alignment %in% c("4th outfielder", "Extreme outfield shift", "Strategic"), "of_shift", "of_standard")) %>% as.factor()

# create working dataset
dat_ip$home_team <- dat_ip$home_team %>% as.factor()
dat_ip$stand <- dat_ip$stand %>% as.factor()
dat_models <- dat_ip %>% select(home_team, fieldTeam, inf_pos, of_pos, stand, hit_distance_sc, launch_speed, launch_angle, adj_spray_angle, hit) %>%
  filter(!is.na(hit_distance_sc)) %>%
  filter(!is.na(launch_speed)) %>%
  filter(!is.na(launch_angle)) %>%
  filter(!is.na(adj_spray_angle))

# working data summary
str(dat_models)

# check proportion of hits/outs
table(dat_models$hit)

# create initial train and validation sets
set.seed(123, sample.kind = "Rounding")
test_index <- createDataPartition(y = dat_models$hit, times = 1, p = 0.2, list = FALSE)
dat_train <- dat_models[-test_index,]
validation <- dat_models[test_index,]
rm(test_index)

# check balance of dat_train and validation sets
prop.table(table(dat_train$hit))
prop.table(table(validation$hit))

#############################################################
# Data Exploration and Visualization
#############################################################

# view first few lines of dat_train
head(dat_train)

# view structure
str(dat_train)

# summary statistics
summary(dat_train)

# examine frequency and percentage of outcome variable
percent <- 100*prop.table(table(dat_train$hit))
cbind(freq=table(dat_train$hit), percent=percent)

# create 'trajectory features' boxplots
boxp_dist <- dat_train %>% mutate(hit = ifelse(hit == 0, "out", "hit")) %>%
  ggplot(aes(x=hit, y=hit_distance_sc, fill=hit)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 16, size = 1.5, color = "purple") +
  theme(legend.position = "none") +
  labs(y="distance", title = "Distribution of hits/outs by distance") +
  theme(plot.title = element_text(size = 12), 
        axis.title = element_text(size = 10),
        axis.title.x = element_blank())

boxp_ev <- dat_train %>% mutate(hit = ifelse(hit == 0, "out", "hit")) %>%
  ggplot(aes(x=hit, y=launch_speed, fill=hit)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 16, size = 1.5, color = "purple") +
  theme(legend.position = "none") +
  labs(y="exit velocity", title = "Distribution of hits/outs by exit velocity") +
  theme(plot.title = element_text(size = 12), 
        axis.title = element_text(size = 10),
        axis.title.x = element_blank())

boxp_la <- dat_train %>% mutate(hit = ifelse(hit == 0, "out", "hit")) %>%
  ggplot(aes(x=hit, y=launch_angle, fill=hit)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 16, size = 1.5, color = "purple") +
  theme(legend.position = "none") +
  labs(y="launch angle", title = "Distribution of hits/outs by launch angle") +
  theme(plot.title = element_text(size = 12), 
        axis.title = element_text(size = 10),
        axis.title.x = element_blank())

boxp_sa <- dat_train %>% mutate(hit = ifelse(hit == 0, "out", "hit")) %>%
  ggplot(aes(x=hit, y=adj_spray_angle, fill=hit)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 16, size = 1.5, color = "purple") +
  theme(legend.position = "none") +
  labs(y="spray angle", title = "Distribution of hits/outs by spray angle") +
  theme(plot.title = element_text(size = 12), 
        axis.title = element_text(size = 10),
        axis.title.x = element_blank())

grid.arrange(boxp_dist, boxp_ev, boxp_la, boxp_sa, ncol = 2)

# plot launch angle + exit velo
dat_train %>% mutate(hit = ifelse(hit == 0, "out", "hit")) %>%
  ggplot(aes(x=launch_angle, y=launch_speed, color = hit)) +
  geom_point() +
  ylim(0, 125) + xlim(-90, 90) +
  theme(legend.title = element_blank()) +
  labs(y= "exit velocity", x="launch angle")

# plot spray angle + exit velo
dat_train %>% mutate(hit = ifelse(hit == 0, "out", "hit")) %>%
  ggplot(aes(x=adj_spray_angle, y=launch_speed, color = hit)) +
  geom_point() +
  ylim(0, 125) + xlim(-90, 90) +
  theme(legend.title = element_blank()) +
  labs(y= "exit velocity", x="spray angle")

# plot distance + spray angle
dat_train %>% mutate(hit = ifelse(hit == 0, "out", "hit")) %>%
  ggplot(aes(x=hit_distance_sc, y=adj_spray_angle, color = hit)) +
  geom_point() +
  ylim(-90,90) + xlim(0,500) +
  theme(legend.title = element_blank()) +
  labs(y= "spray angle", x="distance")

# calculate infield shift percentage
mean(dat_train$inf_pos == "inf_shift")

# calculate outfield shift percentage
mean(dat_train$of_pos == "of_shift")

# create table with percent of hits for: all groundballs, groundballs with shift, groundballs with no shift
all_gb <- dat_ip %>%
  filter(bb_type == "ground_ball") %>%
  select(hit)
all_gb_tab <- mean(all_gb$hit == 1) * 100

shift <- dat_ip %>%
  filter(inf_pos == "inf_shift") %>%
  filter(bb_type == "ground_ball") %>%
  select(hit)
shift_tab <- mean(shift$hit == 1) * 100

no_shift <- dat_ip %>%
  filter(inf_pos == "inf_standard") %>%
  filter(bb_type == "ground_ball") %>%
  select(hit)
no_shift_tab <- mean(no_shift$hit == 1) * 100

pos_tab <- matrix(c(all_gb_tab, shift_tab, no_shift_tab), ncol = 1, byrow = TRUE)
colnames(pos_tab) <- "Percent_Hits"
rownames(pos_tab) <- c("all", "shift", "no_shift")
pos_tab <- as.table(pos_tab)
print(pos_tab)

# plot percent of hits by fielding team
dat_train %>% mutate(hit = ifelse(hit == 0, "out", "hit")) %>%
  ggplot(aes(fieldTeam, fill=hit)) +
  geom_bar(position = "fill") +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90)) +
  ylab("percent")

# calculate park factor for hits and display data for all teams
dat_away <- dat_ip %>%
  mutate(hit = as.numeric(as.character(hit))) %>%
  group_by(team_id = away_team) %>%
  summarise(hit = mean(hit)) %>%
  mutate(type = "away")

dat_home <- dat_ip %>%
  mutate(hit = as.numeric(as.character(hit))) %>%
  group_by(team_id = home_team) %>%
  summarise(hit = mean(hit)) %>%
  mutate(type = "home")

dat_pf <- dat_away %>%
  bind_rows(dat_home) %>%
  spread(key = type, value = hit) %>%
  mutate(pf = 100 * (home / away)) %>%
  select(team_id, pf) %>%
  arrange(desc(pf))

print(dat_pf)

# plot park factor for all teams
dat_pf %>%
  ggplot(aes(x=team_id, y=pf)) +
  geom_bar(stat = "identity", fill = "light blue", color = "black") +
  geom_hline(yintercept=100, linetype="dashed", color = "red", size = 1) +
  annotate(geom="text", label="Neutral Ballpark", x=15, y=100, color = "red", size = 5, vjust = -.5) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y="park factor", x="team", title =" 2019 Hit Park Factors")

#############################################################
# Methods and Analysis
#############################################################

# create test and train sets from dat_train set
set.seed(123, sample.kind = "Rounding")
test_index <- createDataPartition(y = dat_train$hit, times = 1, p = 0.2, list = FALSE)
train <- dat_train[-test_index,]
test <- dat_train[test_index,]
rm(test_index)

# check train and test
prop.table(table(train$hit))
prop.table(table(test$hit))

### Baseline model ### 
# create baseline model and print performance metrics
set.seed(123, sample.kind = "Rounding")
baseFit <- baseline_binomial(test_data = test, dependent_col = "hit",
                             metrics = list("all" = FALSE, "Accuracy" = TRUE,
                                            "Sensitivity" = TRUE, "Specificity" = TRUE, 
                                            "Pos Pred Value" = TRUE, "F1" = TRUE))
base_model <- baseFit$summarized_metrics[c(1,5,6,9,10),c(1,2,4,5,6,3)]
print(base_model)

### Naive Bayes model ###
# create naive bayes model
set.seed(123, sample.kind = "Rounding")
nbFIT <- naiveBayes(hit~., data = train)

# make predictions
nb_preds <- predict(nbFIT, newdata = test)

# calculate performance metrics
nb_cm <- confusionMatrix(nb_preds, test$hit, positive = "1")
nb_acc <- nb_cm$overall["Accuracy"]
nb_other <- nb_cm$byClass[c("Sensitivity", "Specificity", "Precision", "F1")]
nb_metrics <- c(nb_cm$overall["Accuracy"], nb_cm$byClass[c("Sensitivity", "Specificity", "Precision", "F1")])

# print table with metrics
results_nb <- matrix(as.numeric(nb_metrics), nrow = 1, ncol = 5)
metrics <- c("Accuracy", "Sensitivity", "Specificity", "Precision", "F1")
colnames(results_nb) <- metrics
rownames(results_nb) <- "Naive Bayes"
as.data.frame(results_nb)

### rPart model ###
# make sure mlr package was loaded after caret package (should be true if libraries code chunk at top was executed)
set.seed(123, sample.kind = "Rounding")

# create tasks
traintask_rp <- makeClassifTask(data = train, target = "hit", positive = "1")
testtask_rp <- makeClassifTask(data = test,target = "hit", positive = "1")

# create learner
rp.lrn <- makeLearner("classif.rpart", predict.type = "response")

# define resample
rdesc <- makeResampleDesc("CV",iters=4)
ctrl <- makeTuneControlRandom(maxit = 30)

# set hyperparameters
rp.parms <- makeParamSet(
  makeIntegerParam("minsplit",lower = 10,upper = 50),
  makeIntegerParam("minbucket",lower = 5,upper = 50),
  makeNumericParam("cp", lower = .001, upper = .1))

# set parallel backend
# on Mac OS and Linux you may want use parallelStartMulticore() instead
parallelStartSocket(cpus = detectCores())

# tune the parameters
rp.tune <- tuneParams(learner = rp.lrn, resampling = rdesc, task = traintask_rp,
                      par.set = rp.parms, control = ctrl,
                      measures = acc)
parallelStop() #this stops the parallelization 

# show optimal parameters from tuning
rp.tune$x

# train model with optimal parameters
rp.opt_pars <- setHyperPars(rp.lrn, par.vals = rp.tune$x)
rp.train <- train(rp.opt_pars, traintask_rp)

# make predictions
rpart_preds <- predict(rp.train, testtask_rp)

# calculate performance metrics and print table with metrics
ms <- list(acc, tpr, tnr, ppv, f1)
rpart_perf <- performance(rpart_preds, measures = ms)
results_rp <- matrix(as.numeric(rpart_perf), nrow = 1, ncol = 5)
colnames(results_rp) <- metrics
rownames(results_rp) <- "rPart"
as.data.frame(results_rp)

### Random Forest model ###
set.seed(123, sample.kind = "Rounding")

# create tasks
traintask_rf <- makeClassifTask(data = train, target = "hit", positive = "1")
testtask_rf <- makeClassifTask(data = test,target = "hit", positive = "1")

# create learner
rf.lrn <- makeLearner("classif.randomForest", predict.type = "response")

# define resample
rdesc <- makeResampleDesc("CV",iters=4)
ctrl <- makeTuneControlRandom(maxit = 30)

# set hyperparameters
rf.pars <- makeParamSet(
  makeIntegerParam("mtry",lower = 1,upper = 8),
  makeIntegerParam("nodesize",lower = 10,upper = 50),
  makeIntegerParam("ntree", lower = 50, upper = 300))

# set parallel backend
# on Mac OS and Linux you may want use parallelStartMulticore() instead
parallelStartSocket(cpus = detectCores())

# tune the parameters
# this may take 10-15 minutes
rf.tune <- tuneParams(learner = rf.lrn
                   ,task = traintask_rf
                   ,resampling = rdesc
                   ,measures = acc
                   ,par.set = rf.pars
                   ,control = ctrl)
parallelStop() #this stops the parallelization

# show optimal parameters from tuning
rf.tune$x

# train model with optimal parameters
rf.opt_pars <- setHyperPars(rf.lrn, par.vals = rf.tune$x)
rf.train <- train(rf.opt_pars, traintask_rf)

# make predictions
rf_preds <- predict(rf.train, testtask_rf)

# calculate performance metrics and print table with metrics
rf_perf <- performance(rf_preds, measures = ms)
results_rf <- matrix(as.numeric(rf_perf), nrow = 1, ncol = 5)
colnames(results_rf) <- metrics
rownames(results_rf) <- "Random Forest"
as.data.frame(results_rf)

### XGBoost model
set.seed(123, sample.kind = "Rounding")

# create tasks (also create dummy variables for model)
traintask_xg <- makeClassifTask (data = train,target = "hit", positive = "1")
testtask_xg <- makeClassifTask (data = test,target = "hit", positive = "1")
traintask_xg <- createDummyFeatures (obj = traintask_xg) 
testtask_xg <- createDummyFeatures (obj = testtask_xg)

# create learner
xg.lrn <- makeLearner("classif.xgboost",predict.type = "response")
xg.lrn$par.vals <- list( objective="binary:logistic", eval_metric="error", eta = .1)

# set hyperparameters
xg.pars <- makeParamSet(makeIntegerParam("nrounds", lower = 50, upper = 300),
                       makeIntegerParam("max_depth",lower = 3, upper = 12),
                       makeNumericParam("subsample", lower = .2, upper = .8),
                       makeNumericParam("colsample_bytree",lower = .2,upper = .8))

# define resample
rdesc <- makeResampleDesc("CV",stratify = T,iters=4)
ctrl <- makeTuneControlRandom(maxit = 30)

# set parallel backend
# on Mac OS and Linux you may want use parallelStartMulticore() instead
parallelStartSocket(cpus = detectCores())

# tune the parameters
# this may take 10-15 minutes
xg.tune <- tuneParams(learner = xg.lrn, task = traintask_xg, resampling = rdesc, 
                      measures = acc, par.set = xg.pars, control = ctrl)
parallelStop() #this stops the parallelization 

# show optimal parameters from tuning
xg.tune$x

# train model with optimal parameters
xg.opt_pars <- setHyperPars(xg.lrn, par.vals =xg.tune$x)
invisible(({capture.output({
  xg.train <- train(xg.opt_pars, traintask_xg)})})) #also hides the running list of progress messages

# make predictions
xg_preds <- predict(xg.train, testtask_xg)

# calculate performance metrics and print table with metrics
xg_perf <- performance(xg_preds, measures = ms)
results_xg <- matrix(as.numeric(xg_perf), nrow = 1, ncol = 5)
colnames(results_xg) <- metrics
rownames(results_xg) <- "XGBoost"
as.data.frame(results_xg)

#############################################################
# Results
#############################################################

# Create table for performance metrics of all our models
models <- c("Baseline","Naive Bayes","rPart","Random Forest","XGBoost")
metrics <- c("Accuracy", "Sensitivity", "Specificity", "Precision", "F1")
results <- data.frame(rbind(as.numeric(base_model[1,2:6]), as.numeric(nb_metrics), as.numeric(rpart_perf), as.numeric(rf_perf), as.numeric(xg_perf)))
rownames(results) <- models
colnames(results) <- metrics
print(results)

### Final Model with Validation Set ###
# use XGBoost model on dat_train and validation sets
set.seed(123, sample.kind = "Rounding")

# create tasks and dummy variables
traintask <- makeClassifTask (data = dat_train,target = "hit", positive = "1")
testtask <- makeClassifTask (data = validation,target = "hit", positive = "1")
traintask <- createDummyFeatures (obj = traintask) 
testtask <- createDummyFeatures (obj = testtask)

# create learner
xgFinal.lrn <- makeLearner("classif.xgboost",predict.type = "response")
xgFinal.lrn$par.vals <- list(objective="binary:logistic", eval_metric="error", eta = .1)

# set hyperparameters
xgFinal.pars <- makeParamSet(makeIntegerParam("nrounds", lower = 50, upper = 300),
                       makeIntegerParam("max_depth",lower = 3, upper = 12),
                       makeNumericParam("subsample", lower = .2, upper = .8),
                       makeNumericParam("colsample_bytree",lower = .2,upper = .8))

# set resample
rdesc <- makeResampleDesc("CV",stratify = T,iters=4)
ctrl <- makeTuneControlRandom(maxit = 30)

# set parallel backend
# on Mac OS and Linux you may want use parallelStartMulticore() instead
parallelStartSocket(cpus = detectCores())

# tune the parameters
# this may take 10-15 minutes
xgFinal.tune <- tuneParams(learner = xgFinal.lrn, task = traintask, resampling = rdesc,
                           measures = acc, par.set = xgFinal.pars, control = ctrl)
parallelStop() #this stops the parallelization

# show optimal parameters from tuning
xgFinal.tune$x

# train model
xgFinal.lrn_opt <-setHyperPars(xgFinal.lrn, par.vals = xgFinal.tune$x)
invisible(({capture.output({
  xgFinal.train <- train(xgFinal.lrn_opt, traintask)})})) #also hides list of running progress messages

# make predictions
xgFinal_preds <- predict(xgFinal.train, testtask)

# calculate performance metrics and print table with metrics
xgFinal_perf <- performance(xgFinal_preds, measures = ms)
results_xgfinal <- matrix(as.numeric(xgFinal_perf), nrow = 1, ncol = 5)
colnames(results_xgfinal) <- metrics
rownames(results_xgfinal) <- "Final (XGBoost)"
as.data.frame(results_xgfinal)

# show feature importance table
imp <- getFeatureImportance(xgFinal.train)
imp$res %>% top_n(10, importance) %>% arrange(desc(importance))

