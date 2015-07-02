#load dependencies
library(lubridate)
library(randomForest)

train = read.csv('Data/train_set.csv', stringsAsFactors = FALSE)
test = read.csv('Data/test_set.csv', stringsAsFactors = FALSE)
tube = read.csv('Data/tube.csv', stringsAsFactors = FALSE)

# Convert to date.
train$quote_date <- as.Date(train$quote_date)
test$quote_date <- as.Date(test$quote_date)

# Extract different date components from date
train$year <- year(train$quote_date)
train$month <- month(train$quote_date)
train$dayofyear <- yday(train$quote_date)
train$dayofweek <- wday(train$quote_date)
train$day <- day(train$quote_date)

test$year <- year(test$quote_date)
test$month <- month(test$quote_date)
test$dayofyear <- yday(test$quote_date)
test$dayofweek <- wday(test$quote_date)
test$day <- day(test$quote_date)

# Convert bracket_pricing to categorical
train$bracket_pricing <- factor(train$bracket_pricing)
test$bracket_pricing <- factor(test$bracket_pricing)

# merge tube data
merged_train <- merge(train, tube, by=c("tube_assembly_id"))
merged_test <- merge(test, tube, by=c("tube_assembly_id"))

# remove unused columns
merged_train <- subset(merged_train, select = -c(tube_assembly_id, quote_date, supplier))
merged_test <- subset(merged_test, select = -c(tube_assembly_id, quote_date, id, supplier))

# Factorize some vectors
merged_train$material_id <- factor(merged_train$material_id)
merged_train$end_a_1x <- factor(merged_train$end_a_1x)
merged_train$end_a_2x <- factor(merged_train$end_a_2x)
merged_train$end_x_1x <- factor(merged_train$end_x_1x)
merged_train$end_x_2x <- factor(merged_train$end_x_2x)

merged_test$material_id <- factor(merged_test$material_id)
merged_test$end_a_1x <- factor(merged_test$end_a_1x)
merged_test$end_a_2x <- factor(merged_test$end_a_2x)
merged_test$end_x_1x <- factor(merged_test$end_x_1x)
merged_test$end_x_2x <- factor(merged_test$end_x_2x)

# numericize some vectors
merged_train$num_bends <- as.numeric(merged_train$num_bends)
merged_train$num_boss <- as.numeric(merged_train$num_boss)

merged_test$num_bends <- as.numeric(merged_test$num_bends)
merged_test$num_boss <- as.numeric(merged_test$num_boss)

#remove more columns
merged_train <- subset(merged_train, select = -c(end_a, end_x, material_id))
merged_test <- subset(merged_test, select = -c(end_a, end_x, material_id))

# Fit a RF model
fitRF = randomForest(cost ~.  , data = merged_train, ntree=500, do.trace=50, importance=TRUE)

# get predictions from the model, convert them and dump them!
# Change any below 0 to zero.
preds <- cbind(1:NROW(merged_test), predict(fitRF, merged_test))

colnames(preds) <- c('id', 'cost')

write.csv(preds, 'Output/merged_data_rf_model.csv', quote = FALSE, row.names = FALSE)
