# tech_cri_survey_nlp
---
title: "AMA Results"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Libraring packages
```{r}
library(stringr)
library(prettyR)
library(caret)
library(quanteda)
library(readtext)
library(quanteda.textmodels)
```

Try installing package
```{r}

setwd("T:/CRI_Research/telehealth_evaluation/data_codebooks/satisfaction/clinician_qual")
other_barriers_home_complete_dat = read.csv("other_barriers_home_complete_dat.csv", header = TRUE)
other_barriers = other_barriers_home_complete_dat
### Just try theme 1
other_barriers$theme = other_barriers$Theme.1
other_barriers = other_barriers[,c(1,6)]
other_barriers$theme = as.factor(other_barriers$theme)
describe.factor(other_barriers$theme)
### Try changing
other_barriers$theme = ifelse(other_barriers$theme == "home distractions", "kids", ifelse(other_barriers$theme == "clients lack tech", "clinician tech problems",ifelse(other_barriers$theme == "client engagement", "client engagement", ifelse(other_barriers$theme == "other", "other", ifelse(other_barriers$theme == "kids", "kids", ifelse(other_barriers$theme == "clinician tech problems", "clinician tech problems", "Wrong"))))))
describe.factor(other_barriers$theme)
```
Need a coprus which is a document term matrix, then maybe we can use sentence instead of a word as the token instead of the word

Try making it a corpus and see what that means
Need to make all the words characters
Token is the number of words including repeats
Types is the number of unique words

Example: https://blog.paperspace.com/intro-to-datascience/
https://tutorials.quanteda.io/basic-operations/corpus/docvars/
```{r}
head(other_barriers)

other_barriers_class = other_barriers
head(other_barriers_class)
other_barriers_class$other_barriers_home = as.character(other_barriers_class$other_barriers_home)
other_barriers_class_corp = corpus(other_barriers_class$other_barriers_home, docvars = data.frame(theme =  other_barriers_class$theme))
other_barriers_class_corp
summary(other_barriers_class_corp)
docvars(other_barriers_class_corp)
other_barriers_corp_dfm = dfm(other_barriers_class_corp, tolower = TRUE)
dim(other_barriers_corp_dfm)
summary(other_barriers_corp_dfm)
```
Create the training and testing data sets 
```{r}
#######Full
n = 239/2
raw_train <- other_barriers_class[1:n,]
raw_test <- other_barriers_class[n:nrow(other_barriers_class),]
dim(raw_train)
dim(raw_test)

other_barriers_corp_dfm = other_barriers_corp_dfm %>% dfm(remove = stopwords("english"), stem = TRUE)

dfm_train <- other_barriers_corp_dfm[1:n,]
dfm_test <- other_barriers_corp_dfm[n:nrow(other_barriers_corp_dfm),]

dfm_matached = dfm_match(dfm_test, features = featnames(dfm_train))
docvars(dfm_matached)

```
Now build the model
https://tutorials.quanteda.io/machine-learning/nb/
```{r}
model_train = textmodel_nb(dfm_train, raw_train$theme)
summary(model_train)
```
Now get predictions
```{r}
actual_class = docvars(dfm_matached)
pred_class = predict(model_train, newdata = dfm_matached)

length(pred_class)

tab =  table(pred_class, actual_class$theme)
confusionMatrix(tab, mode = "everything")
```
Now put together full education variable to put into AMA data set
```{r}

pred_full_df = data.frame(pred_full)
head(pred_full_df)
raw_full = data.frame(pred_full = raw_test_class$CodesMatt)

edu_ama = rbind(raw_full, pred_full_df)

colnames(edu_ama) = "Edu"

write.csv(edu_ama, "edu_ama.csv", row.names = FALSE)
```
pred_class_snap_
##############
Now try with Jess's codes
##################
Load data and aggregate codes into four or five
```{r}
setwd("T:/CRI_Research/telehealth_evaluation/data_codebooks/satisfaction/clinician_qual")
other_barriers_snap_dat = read.csv("other_snap_barriers_complete.csv", header = TRUE)
other_barriers_snap = other_barriers_snap_dat
### Just try theme 1
other_barriers_snap$theme = other_barriers_snap$Theme.1
other_barriers_snap = other_barriers_snap[,c(1,5)]
other_barriers_snap$theme = as.factor(other_barriers_snap$theme)
describe.factor(other_barriers_snap$theme)
### Try changing
other_barriers_snap$theme = recode(other_barriers_snap$theme, "Less user-friendly" = "No or limited use", "No  for it" = "No or limited ", "Not applicable to services" = "No or limited ", "No need" = "No or limited use", "Efficiency" =  "No or limited ", "Poor training" = "Tech and training issues", "Technology issues" = "Tech and training issues", "Unaware of platform" = "Tech and training issues", "Misinformed" = "Tech and training issues", "No access to platform" = "Tech and training issues", "Technology" = "Tech and training issues", "Clinician prefers other"= "Client / Clinician prefer / barrier", "Client barriers" = "Client / Clinician prefer / barrier", "Client prefers other" = "Client / Clinician prefer / barrier", "Clinician personal barrier" = "Client / Clinician prefer / barrier", "Clinician personal barrier" = "Client / Clinician prefer / barrier", "Time constraints" = "Client / Clinician prefer / barrier", "Client personal barrier" = "Client / Clinician prefer / barrier", "Not compensated for training" = "Client / Clinician prefer / barrier")
describe.factor(other_barriers_snap$theme)
# Following codes as :
# No or limited: Less r-friendly, No  for it, Not applicable to services, No need, Efficiency 
# tech and training issues: Poor training, Technology issues, Unaware of platform, Misinformed, No access to platform, Technology  
# Client or clincian preferences and barriers: Clinician prefers other, Client barriers, Client prefers other,  Clinician personal barrier, Time constraints, Client personal barrier, Not compensated for training
# Get these Clinican prefers other, Misinformed 
### Limit to just the codes
other_barriers_snap

other_barriers_snap_remain = other_barriers_snap[-c(1:111),]
other_barriers_snap = other_barriers_snap[1:111,]
dim(other_barriers_snap)
```
Now put together the data set together
```{r}
other_barriers_snap_class = other_barriers_snap

other_barriers_snap_class$other_snap_barriers = as.character(other_barriers_snap_class$other_snap_barriers)
other_barriers_snap_class_corp = corpus(other_barriers_snap_class$other_snap_barriers, docvars = data.frame(theme =  other_barriers_snap_class$theme))

summary(other_barriers_snap_class_corp)
docvars(other_barriers_snap_class_corp)
other_barriers_snap_corp_dfm = dfm(other_barriers_snap_class_corp, tolower = TRUE)
dim(other_barriers_snap_corp_dfm)
summary(other_barriers_snap_corp_dfm)

```
Create the training and testing data set
```{r}
n = 111/2
raw_train_snap <- other_barriers_snap_class[1:n,]
raw_test_snap <- other_barriers_snap_class[n:nrow(other_barriers_snap_class),]
dim(raw_train_snap)
dim(raw_test_snap)

other_barriers_snap_corp_dfm = other_barriers_snap_corp_dfm %>% dfm(remove = stopwords("english"), stem = TRUE)

dfm_train_snap <- other_barriers_snap_corp_dfm[1:n,]
dfm_test_snap <- other_barriers_snap_corp_dfm[n:nrow(other_barriers_snap_corp_dfm),]

dfm_matached_snap = dfm_match(dfm_test_snap, features = featnames(dfm_train_snap))
docvars(dfm_matached_snap)
dim(dfm_train_snap)
```
Now build the model
https://tutorials.quanteda.io/machine-learning/nb/
```{r}
dim(raw_test_snap)
dim(dfm_train_snap)
model_train_snap = textmodel_nb(dfm_train_snap, raw_train_snap$theme)
summary(model_train_snap)
```
Now get predictions
Precision, recall and the F1 score are frequently d to assess the classification performance. Precision is measured as TP / (TP + FP), where TP are the number of true positives and FP the false positives. Recall divides the false positives by the sum of true positives and false negatives TP / (TP + FN). Finally, the F1 score is a harmonic mean of precision and recall 2 * (Precision * Recall) / (Precision + Recall).
```{r}
actual_class_snap_ = docvars(dfm_train_snap_)
pred_class_snap_ = predict(model_train_snap_, newdata = dfm_matached_snap_)

length(pred_class)

tab_snap_ =  table(pred_class_snap_, actual_class_snap_$theme)
confusionMatrix(tab_snap_, mode = "everything")
```
Now put together full education variable to put into AMA data set
```{r}

pred_full_df = data.frame(pred_full)
head(pred_full_df)
raw_full = data.frame(pred_full = raw_test_class$CodesMatt)

edu_ama = rbind(raw_full, pred_full_df)

colnames(edu_ama) = "Edu"

write.csv(edu_ama, "edu_ama.csv", row.names = FALSE)
```

