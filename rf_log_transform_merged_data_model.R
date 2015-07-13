#load dependencies
library(lubridate)
library(randomForest)

options(scipen = 10)

###
### Build train and test db
###

### Load train and test
test = read.csv("Data/competition_data/test_set.csv")
train = read.csv("Data/competition_data/train_set.csv")

train$id = -(1:nrow(train))
test$cost = 0

train = rbind(train, test)

### Quote Date
train$quote_date <- as.Date(train$quote_date)
train$year <- year(train$quote_date)
train$month <- month(train$quote_date)
#train$week <- week(train$quote_date)
train$quote_date <- NULL

### Merge datasets if only 1 variable in common
tube = read.csv("Data/competition_data/tube.csv")
train = merge(train, tube, by = c("tube_assembly_id"), all.x = TRUE)

tube_end_form = read.csv("Data/competition_data/tube_end_form.csv")
train = merge(train, tube_end_form, by.x = c("end_a"), by.y = c("end_form_id"), all.x = TRUE)
train = merge(train, tube_end_form, by.x = c("end_x"), by.y = c("end_form_id"), all.x = TRUE)

specs = read.csv("Data/competition_data/specs.csv")
train = merge(train, specs, by = c("tube_assembly_id"), all.x = TRUE)

bill_of_materials = read.csv("Data/competition_data/bill_of_materials.csv")
train = merge(train, bill_of_materials, by = c("tube_assembly_id"), all.x = TRUE)

rbind.all.columns <- function(x, y) {
  if(length(x) == 0) {
    return(y)
  }

  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))

  x[, c(as.character(y.diff))] <- NA
  y[, c(as.character(x.diff))] <- NA

  return(rbind(x, y))
}

all_components <- data.frame()
for(f in dir("./Data/competition_data/", 'comp_.*\\.csv')) {
  print(f)
  comp <- read.csv(paste0("./Data/", f))
  all_components <- rbind.all.columns(all_components, comp)
}

train = merge(train, all_components, by.x = c("component_id_1"), by.y = c("component_id"), all.x = TRUE)

#train = merge(train, all_components, by.x = c("component_id_2"), by.y = c("component_id"), all.x = TRUE)

#train = merge(train, all_components, by.x = c("component_id_3"), by.y = c("component_id"), all.x = TRUE)

### remove useless vectors
train$tube_assembly_id <- NULL
train$component_id_1 <- NULL
#train$component_id_2 <- NULL
# train$component_id_3 <- NULL
# train$component_id_4 <- NULL
# train$component_id_5 <- NULL
# train$component_id_6 <- NULL
# train$component_id_7 <- NULL
# train$component_id_8 <- NULL

### Clean NA values
# radius of 9999 is unknown
# https://www.kaggle.com/c/caterpillar-tube-pricing/forums/t/15001/ta-04114/83231#post83231
train$bend_radius[train$bend_radius == 9999] = "NAvalue"

for(i in 1:ncol(train)){
  if(is.numeric(train[,i])){
    train[is.na(train[,i]),i] = -1
  }else{
    train[,i] = as.character(train[,i])
    train[is.na(train[,i]),i] = "NAvalue"
    train[,i] = as.factor(train[,i])
  }
}

### Clean variables with too many categories
for(i in 1:ncol(train)){
  if(!is.numeric(train[,i])){
    freq = data.frame(table(train[,i]))
    freq = freq[order(freq$Freq, decreasing = TRUE),]
    train[,i] = as.character(match(train[,i], freq$Var1[1:50]))
    train[is.na(train[,i]),i] = "rareValue"
    train[,i] = as.factor(train[,i])
  }
}

test = train[which(train$id > 0),]
train = train[which(train$id < 0),]

###
### Evaluate RF predictions by splitting the train db in 80%/20%
###

### Randomforest

dtrain_cv = train[which(train$id %% 5 > 0),]
dtest_cv = train[which(train$id %% 5 == 0),]

### Train randomForest on dtrain_cv and evaluate predictions on dtest_cv
set.seed(123)
rf1 = randomForest(dtrain_cv$cost~., dtrain_cv[,-match(c("id", "cost"), names(dtrain_cv))], ntree = 10, do.trace = 2)

pred = predict(rf1, dtest_cv)
sqrt(mean((log(dtest_cv$cost + 1) - log(pred + 1))^2)) # 0.2389104

### With log transformation trick
set.seed(123)
rf2 = randomForest(log(dtrain_cv$cost + 1)~., dtrain_cv[,-match(c("id", "cost"), names(dtrain_cv))], ntree = 10, do.trace = 2)
pred = exp(predict(rf2, dtest_cv)) - 1

sqrt(mean((log(dtest_cv$cost + 1) - log(pred + 1))^2)) # 0.2360082

### Train randomForest on the whole training set
rf = randomForest(log(train$cost + 1)~., train[,-match(c("id", "cost"), names(train))], ntree = 150, do.trace = 2)

pred = exp(predict(rf, test)) - 1

submitDb = data.frame(id = test$id, cost = pred)
submitDb = aggregate(data.frame(cost = submitDb$cost), by = list(id = submitDb$id), mean)

write.csv(submitDb, "Output/rf_log_transform_better_data_4.csv", row.names = FALSE, quote = FALSE)
