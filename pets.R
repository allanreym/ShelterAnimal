##Library stringr
#install.packages("stringr")
library(stringr)

#Library corrplot
#install.packages("corrplot")
library(corrplot)

##Library mlr
#install.packages("mlr")
library(mlr)

#Library e1071
#install.packages("e1071")
library(e1071)

#Library RWeka
#install.packages("RWeka")
library(RWeka)

##Remove user defined objects from environment
rm(list=ls())

##Import training data set into pets
pets <- read.csv("https://raw.githubusercontent.com/allanreym/ShelterAnimal/master/train.csv")
pets_test_csv <- read.csv("https://raw.githubusercontent.com/allanreym/ShelterAnimal/master/test.csv")

##Summary of pets dataset
summary(pets)

##Keep original of pets dataset
pets_orig <- pets

##Make pets_test_csv same structure as pets
pets_test_csv$OutcomeType = as.factor(NA)
pets_test_csv$OutcomeSubtype = as.factor(NA)
pets_test_csv$ID <- as.factor(pets_test_csv$ID)
pets_test_csv <- pets_test_csv[,c(1, 2, 3, 9, 10, 4, 5, 6, 7, 8)]
names(pets_test_csv) <- names(pets)

##Function to transform datasets
##Two datasets need to be passed as arguments so that both datasets have same columns
trans_pets <- function(pets, pets2) {
  pets_rn <- nrow(pets)
  pets_all <- rbind(pets, pets2)
  
  ##Transform - Isolate month value from DateTime
  pets_all$DateTime_Month <- as.integer(substr(pets_all$DateTime, 6, 7))
  
  ##Transform - Convert all AgeuponOutcome values to years
  TimeUnit <- word(pets_all$AgeuponOutcome, -1)
  TimeNum <- as.numeric(word(pets_all$AgeuponOutcome, 1))
  pets_all$AgeuponOutcome_Years <- ifelse(grepl(pattern = "week", tolower(TimeUnit)), TimeNum / 52,
                                      ifelse(grepl(pattern = "month", tolower(TimeUnit)), TimeNum / 12,
                                             ifelse(grepl(pattern = "day", tolower(TimeUnit)), TimeNum / 365,
                                                    ifelse(grepl(pattern = "year", tolower(TimeUnit)), TimeNum / 1,
                                                           NA) ) ) )
  
  ##Transform - Isolate sex from SexuponOutcome
  pets_all$SexuponOutcome_Sex <- ifelse(grepl(pattern = " male", tolower(pets_all$SexuponOutcome)), "male",
                                        ifelse(grepl(pattern = " female", tolower(pets_all$SexuponOutcome)), "female",
                                               "unknown sex") )
  
  ##Transform - Vector of unique sex values
  SexuponOutcome_Sex_lvl <- sort(unique(pets_all$SexuponOutcome_Sex))
  
  ##Transform - Add new data frame columns of unique sex values
  pets_all <- data.frame(pets_all, do.call(rbind, lapply(pets_all$SexuponOutcome_Sex, function(x) table(factor(x, levels=SexuponOutcome_Sex_lvl)))), stringsAsFactors = FALSE)
  
  ##Transform - Isolate neutered from SexuponOutcome
  pets_all$SexuponOutcome_Neutered <- ifelse(grepl(pattern = "intact", tolower(pets_all$SexuponOutcome)), "intact",
                                             ifelse(grepl(pattern = "neutered", tolower(pets_all$SexuponOutcome)), "neutered",
                                                    ifelse(grepl(pattern = "spayed", tolower(pets_all$SexuponOutcome)), "neutered",
                                                           "unknown neutered") ) )
  
  ##Transform - Vector of unique neutered values
  SexuponOutcome_Neutered_lvl <- sort(unique(pets_all$SexuponOutcome_Neutered))
  
  ##Transform - Add new data frame columns of unique neutered values
  pets_all <- data.frame(pets_all, do.call(rbind, lapply(pets_all$SexuponOutcome_Neutered, function(x) table(factor(x, levels=SexuponOutcome_Neutered_lvl)))), stringsAsFactors = FALSE)
  
  ##Transform - Determine and isolate Mix or Pure breed from Breed
  ##If Breed string contains "mix" or "/", then record will be classified as Mix
  pets_all$Breed_Mix <- as.integer(grepl("mix|\\/", tolower(pets_all$Breed)))
  pets_all$Breed_Pure <- as.integer(!pets_all$Breed_Mix)
  
  ##Transform - Vector of unique AnimalType values
  AnimalType_lvl <- sort(unique(pets_all$AnimalType))
  
  ##Transform - Add new data frame columns of unique Animal values
  pets_all <- data.frame(pets_all, do.call(rbind, lapply(pets_all$AnimalType, function(x) table(factor(x, levels=AnimalType_lvl)))), stringsAsFactors = FALSE)
  
  ##Transform - From Breed values, remove "mix" pattern and split combination of breeds that use "/"
  Breed_lst <- strsplit(as.character(sub(" mix", "", tolower(pets_all$Breed))), "/")
  
  ##Transform - Vector of unique Breed values
  Breed_lvl <- sort(unique(unlist(Breed_lst)))
  
  ##Transform - Add new data frame columns of unique Breed values
  pets_all <- data.frame(pets_all, do.call(rbind, lapply(Breed_lst, function(x) table(factor(x, levels=Breed_lvl)))), stringsAsFactors = FALSE)
  
  ##Transform - From Color values, split combination of colours that use "/"
  Color_lst <- strsplit(as.character(tolower(pets_all$Color)), "/")
  
  ##Transform - Vector of unique Color values
  Color_lvl <- sort(unique(unlist(Color_lst)))
  
  ##Transform - Add new data frame columns of unique Color values
  pets_all <- data.frame(pets_all, do.call(rbind, lapply(Color_lst, function(x) table(factor(x, levels=Color_lvl)))), stringsAsFactors = FALSE)
  
  ##Transform - Fill AgeuponOutcome_Years NA values with mean value
  pets_all[is.na(pets_all$AgeuponOutcome_Years),'AgeuponOutcome_Years'] <- mean(pets_all$AgeuponOutcome_Years, na.rm = TRUE)

  pets_all <- pets_all[1:pets_rn,]

  return(pets_all)
}

##Transform datasets
pets <- trans_pets(pets_orig, pets_test_csv)
pets_test_csv <- trans_pets(pets_test_csv, pets_orig)

##Visualization
pets_corr <- pets[,c(4, 6, 11, 12, 15, 19, 22)]
pets_corr$OutcomeType <- as.integer(pets_corr$OutcomeType)
pets_corr$AnimalType <- as.integer(pets_corr$AnimalType)
pets_corr$SexuponOutcome_Sex <- as.integer(as.factor(pets_corr$SexuponOutcome_Sex))
pets_corr$SexuponOutcome_Neutered <- as.integer(as.factor(pets_corr$SexuponOutcome_Neutered))
str(pets_corr)
pets_corr_cor <- cor(pets_corr, method = "spearman")
pets_corr_cor
corrplot(pets_corr_cor, type="upper", method="ellipse", tl.col="black", tl.srt=45)

##Remove non-numeric attributes
pets$AnimalID = NULL
pets$Name = NULL
pets$DateTime = NULL
pets$OutcomeSubtype = NULL
pets$AnimalType = NULL
pets$SexuponOutcome = NULL
pets$SexuponOutcome_Sex = NULL
pets$SexuponOutcome_Neutered = NULL
pets$AgeuponOutcome = NULL
pets$Breed = NULL
pets$Color = NULL

##Summary statistics


##Split pets data (from train.csv) into train and test
##test.csv cannot be used as outcome classes are not included for competition purposes
set.seed(555)
pets_train_rn <- sample(nrow(pets), floor(nrow(pets)*0.7))
pets_train <- pets[pets_train_rn,]
pets_test <- pets[-pets_train_rn,]

##mlr Configuration
configureMlr(on.par.without.desc = "quiet")

##Create task for mlr
classif.task <- makeClassifTask(data = pets_train, target = "OutcomeType")
classif.task

##List learners available to mlr
#listLearners()

##Trial - Using classif.multinom -- Multinomial Regression learner
#rm(list = c("lrn", "mod", "pred", "pets_pred", "perf_classif.multinom"))
lrn <- makeLearner("classif.multinom", predict.type = "prob", MaxNWts = 1600)
mod <- train(lrn, classif.task)
pred <- predict(mod, newdata = pets_test)
pets_pred <- pred$data
perf_classif.multinom <- length(which(pets_pred$truth == pets_pred$response))/nrow(pets_pred)

##Trial - Using classif.naiveBayes -- Naive Bayes learner
#rm(list = c("lrn", "mod", "pred", "pets_pred", "perf_classif.naiveBayes"))
#lrn <- makeLearner("classif.naiveBayes", predict.type = "prob")
#mod <- train(lrn, classif.task)
#pred <- predict(mod, newdata = pets_test)
#pets_pred <- pred$data
#perf_classif.naiveBayes <- length(which(pets_pred$truth == pets_pred$response))/nrow(pets_pred)

##Trial - Using classif.J48 -- J48 Decision Trees learner
#rm(list = c("lrn", "mod", "pred", "pets_pred", "perf_classif.J48"))
lrn <- makeLearner("classif.J48", predict.type = "prob")
mod <- train(lrn, classif.task)
pred <- predict(mod, newdata = pets_test)
pets_pred <- pred$data
perf_classif.J48 <- length(which(pets_pred$truth == pets_pred$response))/nrow(pets_pred)

##Trial - Using classif.rpart -- Decision Tree learner
#rm(list = c("lrn", "mod", "pred", "pets_pred", "perf_classif.J48"))
lrn <- makeLearner("classif.rpart", predict.type = "prob")
mod <- train(lrn, classif.task)
pred <- predict(mod, newdata = pets_test)
pets_pred <- pred$data
perf_classif.rpart <- length(which(pets_pred$truth == pets_pred$response))/nrow(pets_pred)

##Trial - Display performance of multiclass classification learner methods on training dataset
perf_classif.multinom
#perf_classif.naiveBayes
perf_classif.J48
perf_classif.rpart

##Submission - Create task for mlr
classif.task <- makeClassifTask(data = rbind(pets_train, pets_test), target = "OutcomeType")
classif.task

##Submission - Using classif.rpart -- Decision Tree learner
lrn <- makeLearner("classif.rpart", predict.type = "prob")
mod <- train(lrn, classif.task)
pred <- predict(mod, newdata = pets_test_csv)
pets_pred <- pred$data
submission <- data.frame(pets_test_csv[,1], sapply(levels(pets_pred$response), function(x) as.integer(x == pets_pred$response)))
names(submission)[1] <- "ID"

##AKC
pets_dogs_AKC <- read.csv("https://raw.githubusercontent.com/allanreym/ShelterAnimal/master/web_scraped_AKC.csv")
pets_dogs_AKC$url <- NULL
pets_dogs_AKC$Personality.Description <- NULL
pets_dogs_AKC <- pets_dogs_AKC[pets_dogs_AKC$Group != "FSS",]

pets_dogs_AKC$Energy <- droplevels(pets_dogs_AKC$Energy)
pets_dogs_AKC$Size <- droplevels(pets_dogs_AKC$Size)
pets_dogs_AKC$Group <- droplevels(pets_dogs_AKC$Group)
pets_dogs_AKC$Breed <- droplevels(pets_dogs_AKC$Breed)

pets_dogs_AKC$Energy <- ordered(pets_dogs_AKC$Energy, levels(pets_dogs_AKC$Energy)[c(2, 3, 1)])
head(pets_dogs_AKC$Energy)

pets_dogs_AKC$Size <- ordered(pets_dogs_AKC$Size, levels(pets_dogs_AKC$Size)[c(3, 2, 1)])
head(pets_dogs_AKC$Size)

str(pets_dogs_AKC)


pets_dogs_Petfinder <- read.csv("https://raw.githubusercontent.com/allanreym/ShelterAnimal/master/web_scraped_Petfinder.csv")


#######################
##Old submission
#submission <- data.frame(pets_test_pred[,1], sapply(levels(pets_test_pred$response), function(x) as.integer(x == pets_test_pred$response)))
#                         table(factor(submission, levels = levels(submission$response)))