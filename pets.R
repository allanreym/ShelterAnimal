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

##Summary of pets data set
summary(pets)

##Keep original of pets data set
pets_orig <- pets

##Isolate month value from DateTime
pets$DateTime_Month <- as.integer(substr(pets$DateTime, 6, 7))

##Convert all AgeuponOutcome values to years
TimeUnit <- word(pets$AgeuponOutcome, -1)
TimeNum <- as.numeric(word(pets$AgeuponOutcome, 1))
pets$AgeuponOutcome_Years <- ifelse(grepl(pattern = "week", tolower(TimeUnit)), TimeNum / 52,
                                    ifelse(grepl(pattern = "month", tolower(TimeUnit)), TimeNum / 12,
                                           ifelse(grepl(pattern = "day", tolower(TimeUnit)), TimeNum / 365,
                                                  ifelse(grepl(pattern = "year", tolower(TimeUnit)), TimeNum / 1,
                                                         NA) ) ) )

##Isolate sex from SexuponOutcome
pets$SexuponOutcome_Sex <- ifelse(grepl(pattern = " male", tolower(pets$SexuponOutcome)), "male",
                                  ifelse(grepl(pattern = " female", tolower(pets$SexuponOutcome)), "female",
                                         "unknown sex") )

##Vector of unique sex values
SexuponOutcome_Sex_lvl <- unique(pets$SexuponOutcome_Sex)

##Add new data frame columns of unique sex values
pets <- data.frame(pets, do.call(rbind, lapply(pets$SexuponOutcome_Sex, function(x) table(factor(x, levels=SexuponOutcome_Sex_lvl)))), stringsAsFactors = FALSE)


##Isolate neutered from SexuponOutcome
pets$SexuponOutcome_Neutered <- ifelse(grepl(pattern = "intact", tolower(pets$SexuponOutcome)), "intact",
                                       ifelse(grepl(pattern = "neutered", tolower(pets$SexuponOutcome)), "neutered",
                                              ifelse(grepl(pattern = "spayed", tolower(pets$SexuponOutcome)), "neutered",
                                                     "unknown neutered") ) )

##Vector of unique neutered values
SexuponOutcome_Neutered_lvl <- unique(pets$SexuponOutcome_Neutered)

##Add new data frame columns of unique neutered values
pets <- data.frame(pets, do.call(rbind, lapply(pets$SexuponOutcome_Neutered, function(x) table(factor(x, levels=SexuponOutcome_Neutered_lvl)))), stringsAsFactors = FALSE)

##Determine and isolate Mix or Pure breed from Breed
##If Breed string contains "mix" or "/", then record will be classified as Mix
pets$Breed_Mix <- as.integer(grepl("mix|\\/", tolower(pets$Breed)))
pets$Breed_Pure <- as.integer(!pets$Breed_Mix)

##Determine and isolate Mix or Pure color from Color
#pets$Color_Mix <- grepl("mix|tricolor|torbie|tortie|tabby|seal point|calico|brindle|blue merle|\\/", tolower(pets$Color))
#pets$Color_Pure <- !pets$Color_Mix

##Scrap distinction of Mix or Pure color
##To classify Mix or Pure color cannot be done accurately, discretely, and absolutely
##pets$Color_Mix <- NULL
##pets$Color_Pure <- NULL

##Vector of unique OutcomeType values
##OutcomeType_lvl <- unique(pets$OutcomeType)

##Add new data frame columns of unique OutcomeType values
##pets <- data.frame(pets, do.call(rbind, lapply(pets$OutcomeType, function(x) table(factor(x, levels=OutcomeType_lvl)))), stringsAsFactors = FALSE)

##Vector of unique AnimalType values
AnimalType_lvl <- unique(pets$AnimalType)

##Add new data frame columns of unique Animal values
pets <- data.frame(pets, do.call(rbind, lapply(pets$AnimalType, function(x) table(factor(x, levels=AnimalType_lvl)))), stringsAsFactors = FALSE)

##From Breed values, remove "mix" pattern and split combination of breeds that use "/"
Breed_lst <- strsplit(as.character(sub(" mix", "", tolower(pets$Breed))), "/")

##Vector of unique Breed values
Breed_lvl <- unique(unlist(Breed_lst))

##Add new data frame columns of unique Breed values
pets <- data.frame(pets, do.call(rbind, lapply(Breed_lst, function(x) table(factor(x, levels=Breed_lvl)))), stringsAsFactors = FALSE)

##From Color values, split combination of colours that use "/"
Color_lst <- strsplit(as.character(tolower(pets$Color)), "/")

##Vector of unique Color values
Color_lvl <- unique(unlist(Color_lst))

##Add new data frame columns of unique Color values
pets <- data.frame(pets, do.call(rbind, lapply(Color_lst, function(x) table(factor(x, levels=Color_lvl)))), stringsAsFactors = FALSE)

##Fill AgeuponOutcome_Years NA values with mean value
pets[is.na(pets$AgeuponOutcome_Years),'AgeuponOutcome_Years'] <- mean(pets$AgeuponOutcome_Years, na.rm = TRUE)

##Visualization
pets_corr <- pets[,c(4,6,11,12,13,17,22)]
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


##Split pets data into train and test
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

##Trial using classif.multinom -- Multinomial Regression learner
#rm(list = c("lrn", "mod", "pred", "pets_pred", "perf_classif.multinom"))
lrn <- makeLearner("classif.multinom", predict.type = "prob", MaxNWts = 1500)
mod <- train(lrn, classif.task)
pred <- predict(mod, newdata = pets_test)
pets_pred <- pred$data
perf_classif.multinom <- length(which(pets_pred$truth == pets_pred$response))/nrow(pets_pred)

##Trial using classif.naiveBayes -- Naive Bayes learner
#rm(list = c("lrn", "mod", "pred", "pets_pred", "perf_classif.naiveBayes"))
lrn <- makeLearner("classif.naiveBayes", predict.type = "prob")
mod <- train(lrn, classif.task)
pred <- predict(mod, newdata = pets_test)
pets_pred <- pred$data
perf_classif.naiveBayes <- length(which(pets_pred$truth == pets_pred$response))/nrow(pets_pred)

##Trial using classif.J48 -- J48 Decision Trees learner
#rm(list = c("lrn", "mod", "pred", "pets_pred", "perf_classif.J48"))
lrn <- makeLearner("classif.J48", predict.type = "prob")
mod <- train(lrn, classif.task)
pred <- predict(mod, newdata = pets_test)
pets_pred <- pred$data
perf_classif.J48 <- length(which(pets_pred$truth == pets_pred$response))/nrow(pets_pred)

##Display performance of multiclass classification learner methods on training dataset
perf_classif.multinom
perf_classif.naiveBayes
perf_classif.J48




#######################

# submission <- data.frame(pets_test_pred[,1], sapply(levels(pets_test_pred$response), function(x) as.integer(x == pets_test_pred$response)))
#                          table(factor(submission, levels = levels(submission$response)))