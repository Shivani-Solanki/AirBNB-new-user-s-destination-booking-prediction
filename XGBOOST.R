library(bit64)
library(data.table)
library(xgboost)
library(dummies)

## loading data

sessions <- fread("C:/Users/solan/Downloads/sessions.csv")

# Extracting predictions from LR model
train_lrpred <- fread("C:/Users/solan/Downloads/train_lr.csv")
test_lrpred <- fread("C:/Users/solan/Downloads/test_lr.csv")

## cleaning data

# keeping users from sessions data only that 2014 onwards only
XF_train <- subset(XF_train, as.integer(substr(date_account_created,1,4)) >= 2014)
XF_test <- subset(XF_test, as.integer(substr(date_account_created,1,4)) >= 2014)

# combining similar browsers into one
XF_train$first_browser[XF_train$first_browser %in% c("Arora","Avant Browser","Camino","Conkeror","CometBird","Comodo Dragon","CoolNovo","Crazy Browser","Epic","Google Earth","Flock","Googlebot","IBrowse","IceDragon","IceWeasel","Iron","Kindle Browser","Maxthon","Nintendo Browser","NetNewsWire","OmniWeb","Outlook 2007","Pale Moon","Palm Pre web browser","PS Vita browser","RockMelt","SeaMonkey","SiteKiosk","SlimBrowser","Sogou Explorer","Stainless","TenFourFox","TheWorld Browser","UC Browser","Yandex.Browser","wOSBrowser")] <- "Other"
XF_test$first_browser[XF_test$first_browser %in% c("Arora","Avant Browser","Camino","Conkeror","CometBird","Comodo Dragon","CoolNovo","Crazy Browser","Epic","Google Earth","Flock","Googlebot","IBrowse","IceDragon","IceWeasel","Iron","Kindle Browser","Maxthon","Nintendo Browser","NetNewsWire","OmniWeb","Outlook 2007","Pale Moon","Palm Pre web browser","PS Vita browser","RockMelt","SeaMonkey","SiteKiosk","SlimBrowser","Sogou Explorer","Stainless","TenFourFox","TheWorld Browser","UC Browser","Yandex.Browser","wOSBrowser")] <- "Other"

XF_train$first_browser[XF_train$first_browser %in% c("Firefox","Mobile Firefox","Mozilla")] <- "Firefox"
XF_test$first_browser[XF_test$first_browser %in% c("Firefox","Mobile Firefox","Mozilla")] <- "Firefox"

XF_train$first_browser[XF_train$first_browser %in% c("Chrome","Chrome Mobile","Chromium")] <- "Chrome"
XF_test$first_browser[XF_test$first_browser %in% c("Chrome","Chrome Mobile","Chromium")] <- "Chrome"

XF_train$first_browser[XF_train$first_browser %in% c("IE","IE Mobile")] <- "Explorer"
XF_test$first_browser[XF_test$first_browser %in% c("IE","IE Mobile")] <- "Explorer"

XF_train$first_browser[XF_train$first_browser %in% c("Opera","Opera Mini","Opera Mobile")] <- "Opera"
XF_test$first_browser[XF_test$first_browser %in% c("Opera","Opera Mini","Opera Mobile")] <- "Opera"

XF_train$first_browser[XF_train$first_browser %in% c("Mobile Safari","Safari")] <- "Safari"
XF_test$first_browser[XF_test$first_browser %in% c("Mobile Safari","Safari")] <- "Safari"

# cleaning the age
XF_train$age[is.na(XF_train$age)] <- -1
XF_test$age[is.na(XF_test$age)] <- -1

# calculating age for people who had put their Birth year instead of age
for (i in 1920:1968)
{
  print(i)
  XF_train$age[XF_train$age == i] <- 2014 - XF_train$age[XF_train$age == i]
  XF_test$age[XF_test$age == i] <- 2014 - XF_test$age[XF_test$age == i]
}

# merging logistic regression predictions as one the input attributes to XGB
XF_train <- merge(XF_train, train_lrpred, all.x=T, by="id")
XF_test <- merge(XF_test, test_lrpred, all.x=T, by="id")


# assigning negative values to missing values
XF_train$pred_lr[is.na(XF_train$pred_lr)] <- -1
XF_test$pred_lr[is.na(XF_test$pred_lr)] <- -1


# features engineering from sessions data
names(sessions)[1] <- "id"
sessions <- subset(sessions, id %in% c(unique(XF_train$id), unique(XF_test$id)))
sessions$count <- 1

# one-hot encoding 
sessions_action <- dcast(sessions, id ~ action, mean, value.var="count")
sessions_action_type <- dcast(sessions, id ~ action_type, mean, value.var="count")
sessions_action_detail <- dcast(sessions, id ~ action_detail, mean, value.var="count")

# handling missing values
sessions_action[is.na(sessions_action)] <- 0
sessions_action_type[is.na(sessions_action_type)] <- 0
sessions_action_detail[is.na(sessions_action_detail)] <- 0

# merging similar events into one event
sessions_action_detail <- sessions_action_detail[, ":="(coupons = ceiling((apply_coupon+apply_coupon_click+apply_coupon_click_success+apply_coupon_error+coupon_code_click+coupon_field_focus)/10),
                                                        cancellation = ceiling((cancellation_policies+cancellation_policy_click)/10),
                                                        guestlist = ceiling((guest_cancellation+guest_itinerary+guest_receipt)/10),
                                                        host = ceiling((host_guarantee+host_home+host_respond)/10),
                                                        listings = ceiling((listing_descriptions+listing_recommendations+listing_reviews+listing_reviews_page+manage_listing+view_listing)/10),
                                                        message = ceiling((lookup_message_thread+message_inbox+message_post+message_thread+message_to_host_change+message_to_host_focus)/10))]

sessions_action <- sessions_action[, ":="(agreements = ceiling((agree_terms_check+agree_terms_uncheck)/10),
                                          googletranslations = ceiling((ajax_google_translate_description+ajax_google_translate_reviews)/10),
                                          payouts = ceiling((ajax_payout_edit+ajax_payout_options_by_country)/10),
                                          widget = ceiling((ajax_photo_widget+ajax_photo_widget_form_iframe)/10),
                                          banners = ceiling((ajax_referral_banner_experiment_type+ajax_referral_banner_type)/10),
                                          create = ceiling((create+create_ach+create_multiple+create_paypal)/10),
                                          completion = ceiling((complete+complete_redirect+complete_status)/10),
                                          department = ceiling((department+departments)/10),
                                          edition = ceiling((edit+edit_verification)/10),
                                          email = ceiling((email_itinerary_colorbox+email_share+email_wishlist)/10),
                                          friends = ceiling((friends+friends_new)/10),
                                          home_safety = ceiling((home_safety_landing+home_safety_terms)/10),
                                          jumio = ceiling((jumio+jumio_redirect+jumio_token)/10),
                                          multimsgs = ceiling((multi+multi_message)/10),
                                          qt = ceiling((qt_reply_v2+qt_with+qt2)/10),
                                          reviews = ceiling((reviews+reviews_new)/10),
                                          similarlists = ceiling((similar_listings+similar_listings_v2)/10),
										  kba = ceiling((kba+kba_update)/10),
                                          listings = ceiling((listing+listings)/10),
                                          messagetohost = ceiling((message_to_host_change+message_to_host_focus)/10),
                                          show = ceiling((show+show_code)/10),
                                          social = ceiling((social+social_connections)/10),
                                          terms = ceiling((terms+terms_and_conditions)/10),
                                          historytransaction = ceiling((transaction_history+transaction_history_paginated)/10),
                                          travelplans = ceiling((travel_plans_current+travel_plans_previous)/10),
                                          updates = ceiling((update_cached+update_friends_display+update_hide_from_search_engines+update_notifications)/10),
                                          agree_terms_check = NULL,
                                          agree_terms_uncheck = NULL,
                                          ajax_google_translate_description = NULL,
                                          ajax_google_translate_reviews = NULL,
                                          ajax_payout_edit = NULL,
                                          ajax_payout_options_by_country = NULL,
                                          ajax_photo_widget = NULL,
                                          ajax_photo_widget_form_iframe = NULL,
                                          ajax_referral_banner_experiment_type = NULL,
                                          ajax_referral_banner_type = NULL,
                                          cancellation_policies = NULL,
                                          cancellation_policy_click = NULL,
                                          create_ach = NULL,
                                          create_multiple = NULL,
                                          create_paypal = NULL,
                                          complete_redirect = NULL,
                                          complete_status = NULL,
                                          departments = NULL,
                                          edit_verification = NULL,
                                          email_itinerary_colorbox = NULL,
                                          email_share = NULL,
                                          email_wishlist = NULL,
                                          friends_new = NULL,
                                          home_safety_landing = NULL,
                                          home_safety_terms = NULL,
                                          jumio_redirect = NULL,
                                          jumio_token = NULL,
                                          kba_update = NULL,
                                          listings = NULL,
                                          message_to_host_change = NULL,
                                          message_to_host_focus = NULL,
                                          multi_message = NULL,
                                          qt_reply_v2 = NULL,
                                          qt_with = NULL,
                                          qt2 = NULL,
                                          reviews_new = NULL,
                                          similar_listings_v2 = NULL,
                                          show_code = NULL,
                                          social_connections = NULL,
                                          terms_and_conditions = NULL,
                                          transaction_history_paginated = NULL,
                                          travel_plans_current = NULL,
                                          travel_plans_previous = NULL,
                                          update_cached = NULL,
                                          update_friends_display = NULL,
                                          update_hide_from_search_engines = NULL,
                                          update_notifications = NULL
)]

# merging attributes of sessions data with train and test data
XF_train <- merge(XF_train, sessions_action, all.x=T, by="id")
XF_test <- merge(XF_test, sessions_action, all.x=T, by="id")

XF_train <- merge(XF_train, sessions_action_type, all.x=T, by="id")
XF_test <- merge(XF_test, sessions_action_type, all.x=T, by="id")

XF_train <- merge(XF_train, sessions_action_detail, all.x=T, by="id")
XF_test <- merge(XF_test, sessions_action_detail, all.x=T, by="id")

# removing duplicate columns
XF_train <- XF_train[, colnames(unique(as.matrix(XF_train), MARGIN=2)), with=F]
XF_test <- XF_test[, names(XF_train), with=F]

# extracting ids and target
train_ids <- XF_train$id
test_ids <- XF_test$id

#extracting labels
train_DES <- XF_train$country_destination
test_DES <- XF_test$country_destination

# converting country names to numeric form
target <- as.numeric(as.factor(XF_train$country_destination)) - 1
target_test <- as.numeric(as.factor(XF_test$country_destination)) - 1

# feature engineering from dataset attributes
# segregating timestamp into year, month and weekday
XF_train <- XF_train[, ':='(year_account = as.integer(substr(date_account_created,1,4)),
                          month_account = as.integer(substr(date_account_created,6,7)),
                          day_account = as.integer(substr(date_account_created,9,10)),
                          weekday_account = weekdays(as.Date(paste0(substr(date_account_created,1,4),"-",substr(date_account_created,6,7),"-",substr(date_account_created,9,10)))),
                          year_active = as.integer(substr(timestamp_first_active,1,4)),
                          month_active = as.integer(substr(timestamp_first_active,5,6)),
                          day_active = as.integer(substr(timestamp_first_active,7,8)),
                          weekday_active = weekdays(as.Date(paste0(substr(timestamp_first_active,1,4),"-",substr(timestamp_first_active,5,6),"-",substr(timestamp_first_active,7,8)))),
                          age = ifelse(age < 15 | age > 100, -1, age),
                          id = NULL,
                          date_account_created = NULL,
                          timestamp_first_active = NULL,
                          date_first_booking = NULL,
                          country_destination = NULL)]

XF_test <- XF_test[, ':='(year_account = as.integer(substr(date_account_created,1,4)),
                        month_account = as.integer(substr(date_account_created,6,7)),
                        day_account = as.integer(substr(date_account_created,9,10)),
                        weekday_account = weekdays(as.Date(paste0(substr(date_account_created,1,4),"-",substr(date_account_created,6,7),"-",substr(date_account_created,9,10)))),
                        year_active = as.integer(substr(timestamp_first_active,1,4)),
                        month_active = as.integer(substr(timestamp_first_active,5,6)),
                        day_active = as.integer(substr(timestamp_first_active,7,8)),
                        weekday_active = weekdays(as.Date(paste0(substr(timestamp_first_active,1,4),"-",substr(timestamp_first_active,5,6),"-",substr(timestamp_first_active,7,8)))),
                        age = ifelse(age < 15 | age > 100, -1, age),
                        id = NULL,
                        date_account_created = NULL,
                        timestamp_first_active = NULL,
                        date_first_booking = NULL,
                        country_destination = NULL)]

# onehot encoding for categorical variables
source("C:/Users/solan/Downloads/categorical.R")
C_data <- encode_categories(XF_train, XF_test, onehot="all")

# preparing final datasets
XF_train <- C_data$train
XF_test <- C_data$test

# Handling missing values
XF_train[is.na(XF_train)] <- 0
XF_test[is.na(XF_test)] <- 0


## building xgboost model

# using 5 fold cross validation
set.seed(23)
model_xgb <- xgb.cv(data=as.matrix(XF_train), label=as.matrix(target), objective="multi:softprob", num_class=12, nfold=5, 
					nrounds=78, eta=0.1, max_depth=9, subsample=0.5, colsample_bytree=0.5, min_child_weight=5, eval_metric='mlogloss', 
					maximize=FALSE,verbose = T, early_stopping_rounds=10, gamma=0)

# CV: 0.90091

# plotting Train and valdation loss
e<- data.frame(model_xgb$evaluation_log)
plot(e$iter,e$train_mlogloss_mean,x_lab= 'Iterations', ylab= 'logloss', type = 'l', col='blue', title='Evaluation')
lines(e$iter,e$test_mlogloss_mean,col='green')

# model building
set.seed(23)
# defining parameters
xgb_param <- list("objective"="multi:softprob", "eval_metric"="mlogloss",
                  "num_class"=12)
# training the model with tuned parameters
model_xgb1 <- xgboost(as.matrix(XF_train), 
                     as.matrix(target), 
                     params = xgb_param,
                     nrounds=20, 
                     eta=0.5, 
                     max_depth=9, 
                     subsample=0.5, 
                     colsample_bytree=0.5, 
                     min_child_weight=10
                     #eval_metric='mlogloss',
                     )
# plotting Feature importance graph
train_names = names(XF_train)
Importance = xgb.importance(train_names, model = model_xgb1)
xgb.plot.importance(Importance[1:10,])


# predictions on test set
pred <- predict(model_xgb1, as.matrix(XF_test))
pred_matrix <- as.data.frame(matrix(pred, nrow(XF_test), 12, byrow=T))
tpred_matrix<- as.data.frame(t(pred_matrix))
rownames(tpred_matrix) <- c("AU","CA","DE","ES","FR","GB","IT","NDF","NL","other","PT","US")
predictions_top1 <- as.vector(apply(tpred_matrix, 2, function(x) names(sort(x)[12:12])))

ids <- NULL
for (i in 1:NROW(XF_test)) {
  idx <- XF_test$id[i]
  ids <- append(ids,idx)
}

submission <- NULL
submission$id <- ids
submission$country <- predictions_top1

# finding accuracy
accuracy(test_DES,predictions_top1)
# confusion matrix
table(predictions_top1,Actual)

