##Initialization - Library stringr
#install.packages("stringr")
library(stringr)

##Initialization - Library corrplot
#install.packages("corrplot")
library(corrplot)

##Initialization - Library mlr
#install.packages("mlr")
library(mlr)

##Initialization - Library RWeka
#install.packages("RWeka")
library(RWeka)

##Initialization - Library caret
#install.packages("caret")
library(caret)

##Initialization - Library FSelector
#install.packages("FSelector")
library(FSelector)

##Initialization - Library MLmetrics
#install.packages("MLmetrics")
library(MLmetrics)

##Initialization - Remove all objects and plots
rm(list=ls())
graphics.off()

##Load - Load combined train and test dataset, where test data starts at row 26730
pets <- read.csv("https://raw.githubusercontent.com/allanreym/ShelterAnimal/master/data_combined.tsv", header = T, sep = "\t")
test_start_row <- 26730
train_end_row <- test_start_row - 1

##Transform - Isolate month value from DateTime
pets$DateTime_Month <- as.integer(substr(pets$DateTime, 6, 7))

##Transform - Convert all AgeuponOutcome values to years
TimeUnit <- word(pets$AgeuponOutcome, -1)
TimeNum <- as.numeric(word(pets$AgeuponOutcome, 1))
pets$AgeuponOutcome_Years <- ifelse(grepl(pattern = "week", tolower(TimeUnit)), TimeNum / 52,
                             ifelse(grepl(pattern = "month", tolower(TimeUnit)), TimeNum / 12,
                             ifelse(grepl(pattern = "day", tolower(TimeUnit)), TimeNum / 365,
                             ifelse(grepl(pattern = "year", tolower(TimeUnit)), TimeNum / 1,
                             NA) ) ) )

##Transform - Isolate sex from SexuponOutcome
pets$SexuponOutcome_Sex <- as.factor(ifelse(grepl(pattern = " male", tolower(pets$SexuponOutcome)), "male",
                           ifelse(grepl(pattern = " female", tolower(pets$SexuponOutcome)), "female",
                           "unknown sex") ) )

##Transform - Isolate neutered from SexuponOutcome
pets$SexuponOutcome_Neutered <- as.factor(ifelse(grepl(pattern = "intact", tolower(pets$SexuponOutcome)), "intact",
                                ifelse(grepl(pattern = "neutered", tolower(pets$SexuponOutcome)), "neutered",
                                ifelse(grepl(pattern = "spayed", tolower(pets$SexuponOutcome)), "neutered",
                                "unknown neutered") ) ) )

##Transform - From Color values, split combination of Colors that use "/"
Color_lst <- unique(strsplit(as.character(pets$Color), "/"))
##Transform - Vector of unique Color values
Color_lvl <- sort(unique(unlist(Color_lst)))
##Transform - Add new data frame columns of unique Color values
for (color in Color_lvl){pets[make.names(paste(color, "Color"))] <- as.numeric(grepl(color, pets$Color))}
##Transform - Remove Color column
pets$Color <- NULL

# ##Transform - From Breed values, remove "mix" pattern and split combination of breeds that use "/"
# Breed_lst <- strsplit(as.character(sub(" mix", "", tolower(pets$Breed))), "/")
# ##Transform - Vector of unique Breed values
# Breed_lvl <- sort(unique(unlist(Breed_lst)))
# ##Transform - Add new data frame columns of unique Color values
# for (breed in Breed_lvl){pets[make.names(breed)] <- as.numeric(grepl(breed, pets$Breed))}
# ##Transform - Remove Breed column
pets$Breed <- NULL

##Transform - Remove columns no longer necessary
pets_kaggle_test_ID <- strtoi(pets[test_start_row:nrow(pets),1])
pets_colrm <- c("AnimalID", "Name", "DateTime", "OutcomeSubtype", "SexuponOutcome", "AgeuponOutcome", "Breed1", "Breed2",
  "BreedAKC1", "BreedAKC2", "BreedPetfinder1", "BreedPetfinder2", "Size1", "Size2", "Energy1", "Energy2",
  "Friendliness.to.Other.Pets")
for (column in pets_colrm){pets[column] <- NULL}

##Train/Test - Split pets data (from train.csv) into train and test
##test.csv cannot be used because outcome classes are not included
set.seed(555)
pets_kaggle_train <- pets[1:train_end_row,]
pets_kaggle_train$OutcomeType <- droplevels(pets_kaggle_train$OutcomeType)
pets_train_rn <- sample(nrow(pets_kaggle_train), floor(nrow(pets_kaggle_train)*0.7))
pets_train <- pets_kaggle_train[pets_train_rn,]
pets_test <- pets_kaggle_train[-pets_train_rn,]

##Train/Test - mlr Configuration
configureMlr(on.par.without.desc = "quiet")

##Train/Test - Create task for mlr
classif.task <- makeClassifTask(data = pets_train, target = "OutcomeType")
classif.task

##Train/Test - List learners available to mlr
#listLearners()

##Train/Test - Using classif.J48 -- J48 Decision Trees learner
lrn <- makeLearner("classif.J48", predict.type = "prob")  #WOW("J48")
mod <- mlr::train(lrn, classif.task)
pred <- predict(mod, newdata = pets_test)
pets_pred <- pred$data
perf_classif.J48 <- length(which(pets_pred$truth == pets_pred$response))/nrow(pets_pred)
mod$learner.model

##Train/Test - Display performance
confusionMatrix(pets_pred$response, pets_pred$truth)
listMeasures(classif.task) #performance measure suitable for task
performance(pred, measures = list(acc, ber, mmce, multiclass.auc)) #use performance measure
MultiLogLoss(y_true = as.factor(pets_pred[,7]), y_pred = as.matrix(pets_pred[,2:6]))

##Train/Test - Display information gain
infgain <- information.gain(OutcomeType ~ ., data = pets_train)
head(infgain[base::order(infgain$attr_importance, decreasing = T), , drop = F], 20)

##Visualization 1 - Create dataset for visualization
pets_numeric <- pets_kaggle_train

##Visualization 1 - Create OutcomeType indicators
OutcomeType_lvl <- make.names(sort(unique(as.character(pets_numeric$OutcomeType))))
for (outcome in OutcomeType_lvl){pets_numeric[make.names(outcome)] <- as.numeric(grepl(outcome, pets_numeric$OutcomeType))}

##Visualization 1 - Create SexuponOutcome_Neutered indicators
Neutered_lvl <- make.names(sort(unique(as.character(pets_numeric$SexuponOutcome_Neutered))))
for (neutered in Neutered_lvl){pets_numeric[make.names(neutered)] <- as.numeric(grepl(neutered, pets_numeric$SexuponOutcome_Neutered))}

##Visualization 1 - Create SexuponOutcome_Neutered indicators
Sex_lvl <- make.names(sort(unique(as.character(pets_numeric$SexuponOutcome_Sex))))
for (sex in Sex_lvl){pets_numeric[make.names(sex)] <- as.numeric(grepl(paste("\\b", sex, sep = ""), pets_numeric$SexuponOutcome_Sex))}

##Visualization 1 - Reduce pets_numeric features to those with most information gain
pets_numeric <- pets_numeric[,c(OutcomeType_lvl, Neutered_lvl, Sex_lvl,
                rownames(head(infgain[base::order(infgain$attr_importance, decreasing = T), , drop = F], 15)))]
pets_numeric$AnimalType <- NULL
pets_numeric$SexuponOutcome_Neutered <- NULL
pets_numeric$SexuponOutcome_Sex <- NULL
pets$OutcomeType <- NULL

##Visualization 1 - Correlation
pets_cor <- cor(pets_numeric, use = "pairwise.complete.obs", method = "spearman")
corrplot(pets_cor, type="upper", method="ellipse", tl.col="black", tl.srt=45)

##Visualization 2 - neutered vs adoption/return_to_owner
pets_neut <- pets_kaggle_train[which((pets_kaggle_train$OutcomeType == "Adoption" | pets_kaggle_train$OutcomeType == "Return_to_owner") &
                                       pets_kaggle_train$SexuponOutcome_Neutered != "unknown neutered"), c("OutcomeType", "SexuponOutcome_Neutered")]
pets_neut$OutcomeType <- droplevels(pets_neut$OutcomeType)
pets_neut$SexuponOutcome_Neutered <- droplevels(pets_neut$SexuponOutcome_Neutered)
#counts <- table(pets_neut$SexuponOutcome_Neutered, pets_neut$OutcomeType)
dat <- ddply(pets_neut, .(OutcomeType), function(.) {
  good <- prop.table(table(factor(.$SexuponOutcome_Neutered)))
  res <- cumsum(prop.table(table(factor(.$SexuponOutcome_Neutered))))
  data.frame(lab = names(res), y = c(res), good = good, pos = cumsum(good) - 0.5 * good)
})
ggplot(pets_neut, aes(x = OutcomeType)) + geom_bar(aes(fill = SexuponOutcome_Neutered), position = 'fill') +
  geom_text(aes(label = round(good.Freq, 2), x = OutcomeType, y = pos.Freq), data = dat) +
  theme(legend.position = "right") +
  ggtitle("Animal Outcome vs. Sterilization") + xlab("Outcome") + ylab("Frequency") +
  guides(fill = guide_legend(title = NULL))

##Visualization 3 - Histogram of Age by Outcome
pets_age <- pets_kaggle_train[which(pets_kaggle_train$AgeuponOutcome_Years > 0), c("OutcomeType", "AgeuponOutcome_Years")]
histogram(~ pets_age$AgeuponOutcome_Years | pets_age$OutcomeType,
          main = "Distrubtion of Age Upon Outcome",
          xlab = "Age Upon Outcome",
          breaks = seq(0, 20, 2))

##Visualization 4 - Decision Tree (highly pruned for visual purpose)
classif.task <- makeClassifTask(data = pets_train, target = "OutcomeType")
#lrn <- makeLearner("classif.J48", predict.type = "prob")  #WOW("J48")
lrn <- makeLearner("classif.J48", predict.type = "prob", R = TRUE, N = 10, M = 350)  #WOW("J48")
mod <- mlr::train(lrn, classif.task)
plot(as.party(mod$learner.model),
     main = "Decision Tree using J48 (C4.5) \n[highly pruned tree -- not same tree model used earlier in code]")

#####
##Submission - Create task for mlr
classif.task <- makeClassifTask(data = pets_kaggle_train, target = "OutcomeType")
classif.task
##Submission - Using classif.J48 -- J48 Decision Trees learner
pets_kaggle_test <- pets[test_start_row:nrow(pets),]  #records for submission
lrn <- makeLearner("classif.rpart", predict.type = "prob")
mod <- mlr::train(lrn, classif.task)
pred <- predict(mod, newdata = pets_kaggle_test)
pets_pred <- pred$data
##Submission - Create file
submission <- data.frame(pets_kaggle_test_ID, pets_pred[, 1:5])
names(submission)[1:6] <- c("ID", "Adoption", "Died", "Euthanasia", "Return_to_owner", "Transfer")
write.table(submission, file = ".\\submission.csv", sep = ",", quote = F, row.names = F)