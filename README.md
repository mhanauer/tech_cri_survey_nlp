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
Loading data
```{r}
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/AMAData")
AMAData = read.csv("AMAData.csv", header = TRUE, na.strings = c("NULL"))
```
Try installing package
```{r}

setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/AMAData")
edu_dat = read.csv("Education.csv", header = TRUE)

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
