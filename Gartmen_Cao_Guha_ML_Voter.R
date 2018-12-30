# Application of Deep Neural Networks to Political Survey Data from the 2016 U.S. Presidential Election
# Shukrit Guha, Brady Gartman and Renzhi Cao
# NYU Machine Learning in Economics Project Spring 2018

################################ Voter Data Project Begin ########################################

rm(list=ls())

setwd("C:/Users/shukr/Desktop/NYU_Classes/SEM 2/Machine_Learning_Economics/Project")
voter <- read.csv("voter_trim2.csv", header = T, sep = ',', na.strings = "")
View(voter)
str(voter)
voter.labels = voter[1,]
voter = droplevels(voter)

########################## Normalize specific variables that don't have ordered responses ################################

# for now, I remove the first row because I don't want the labels to be considered as factors, just yet
voter = voter[-1,]

levels(voter[,37])
which(colnames(voter) == "ft_black_2016")
which(colnames(voter) == "ft_altright_2016")

# These variable were scored from 0 to 100, so we normalize them to begin with, after first converting them into numeric vectors
# We use feature scale between 0 and 1 i.e. (x - min)/(max-min)
?normalizeData
for(i in 37:51){
  voter[,i] = as.numeric(voter[,i])
  voter[,i] = normalizeData(voter[,i], type = "0_1")
}
str(voter)

################################# Impute values for ordered responses #############################################

# Now we try and order all the input variables that are reasonably ordered in their values.
for(i in 8:9){
  ind = is.na(voter[,i])
  voter[,i]= as.character(voter[,i])
  voter[ind, i] <- "0"
  voter[,i]= sub("Not at all confident", "-2", voter[,i])
  voter[,i]= sub("Not too confident", "-1", voter[,i])
  voter[,i]= sub("Somewhat confident", "1", voter[,i])
  voter[,i]= sub("Very confident", "2", voter[,i])
  voter[,i]= as.numeric(voter[,i])
}

attach(voter)
levels(obamaapp_2016)
voter[,10]= sub("Don't Know", "0", voter[,10])
voter[,10]= sub("Somewhat Disapprove", "-1", voter[,10])
voter[,10]= sub("Somewhat Approve", "1", voter[,10])
voter[,10]= sub("Strongly Approve", "2", voter[,10])
voter[,10]= sub("Strongly Disapprove", "-2", voter[,10])
voter[,10]= as.numeric(voter[,10])

sum(is.na(voter$obamaapp_2016)) # This variable still has 62 NAs

# If you want to, you can just assign the NAs (which were actually blanks) to "Don't know" so they are 0
ind = is.na(voter[,10])
voter[ind, 10] <- 0
# Now, there are no more NAs

levels(voter[,11])
for(i in 11:18){
  voter[,i]= sub("Don't know", "0", voter[,i])
  voter[,i]= sub("Somewhat unfavorable", "-1", voter[,i])
  voter[,i]= sub("Somewhat favorable", "1", voter[,i])
  voter[,i]= sub("Very favorable", "2", voter[,i])
  voter[,i]= sub("Very unfavorable", "-2", voter[,i])
  voter[,i]= as.numeric(voter[,i])
}

levels(voter[,19]) 
for(i in 19:24){
  voter[,i]= sub("Agree", "3", voter[,i])
  voter[,i]= sub("Strongly agree" , "4", voter[,i])
  voter[,i]= sub("Strongly disagree", "1", voter[,i])
  voter[,i]= sub("Disagree", "2", voter[,i])
  voter[,i]= as.numeric(voter[,i])
}

levels(voter[,52])

# Now, Note this specific block. We will impute NAs for these 23 coloumns and use the imputed values.
# More on this later.
for(i in 52:74){
  voter[,i]= sub("Unimportant", "1", voter[,i])
  voter[,i]= sub("Not very important" , "2", voter[,i])
  voter[,i]= sub("Somewhat important", "3", voter[,i])
  voter[,i]= sub("Very important", "4", voter[,i])
  voter[,i]= as.numeric(voter[,i])
}

levels(voter[,76])

voter[,76]= sub("Don't know", "0", voter[,76])
voter[,76]= sub("Favor", "1", voter[,76])
voter[,76]= sub("Oppose", "-1", voter[,76])
voter[,76]= as.numeric(voter[,76])

levels(voter[,78])

voter[,78]= sub("Don't know", "0", voter[,78])
voter[,78]= sub("Somewhat favor", "1", voter[,78])
voter[,78]= sub("Somewhat oppose", "-1", voter[,78])
voter[,78]= sub("Strongly favor", "2", voter[,78])
voter[,78]= sub("Strongly oppose", "-2", voter[,78])
voter[,78]= as.numeric(voter[,78])

levels(voter[,80])

voter[,80]= sub("Don't know", "0", voter[,80])
voter[,80]= sub("Favor", "1", voter[,80])
voter[,80]= sub("Oppose", "-1", voter[,80])
voter[,80]= as.numeric(voter[,80])

levels(voter[,82])

voter[,82]= sub("Don't know", "0", voter[,82])
voter[,82]= sub("Favor the death penalty", "1", voter[,82])
voter[,82]= sub("Opposed to the death penalty", "-1", voter[,82])
voter[,82]= as.numeric(voter[,82])

levels(voter[,86])

voter[,86]= sub("Don't know", "0", voter[,86])
voter[,86]= sub("Yes", "1", voter[,86])
voter[,86]= sub("No", "-1", voter[,86])
voter[,86]= as.numeric(voter[,86])

levels(voter[,88])

voter[,88]= sub("Don't know", "0", voter[,88])
voter[,88]= sub("Probably is happening", "1", voter[,88])
voter[,88]= sub("Probably is not happening", "-1", voter[,88])
voter[,88]= sub("Definitely is happening", "1", voter[,88])
voter[,88]= sub("Definitely is not happening", "-1", voter[,88])
voter[,88]= as.numeric(voter[,88])

levels(voter[,90])

voter[,90]= sub("Don't know", "0", voter[,90])
voter[,90]= sub("Favor", "1", voter[,90])
voter[,90]= sub("Oppose", "-1", voter[,90])
voter[,90]= as.numeric(voter[,90])

levels(voter[,91])

voter[,91]= sub("Don't know", "0", voter[,91])
voter[,91]= sub("Yes", "1", voter[,91])
voter[,91]= sub("No", "-1", voter[,91])
voter[,91]= as.numeric(voter[,91])

levels(voter[,94])

voter[,94]= sub("Don't know", "0", voter[,94])
voter[,94]= sub("Favor", "1", voter[,94])
voter[,94]= sub("Oppose", "-1", voter[,94])
voter[,94]= as.numeric(voter[,94])

levels(voter[,101])

for(i in 100:102){
  voter[,i] = as.character(voter[,i])
  voter[,i]= sub("Don't know", "0", voter[,i])
  voter[,i]= sub("Disagree", "-1", voter[,i])
  voter[,i]= sub("Agree", "1", voter[,i])
  voter[,i]= sub("Agree strongly", "2", voter[,i])
  voter[,i]= sub("Disagree strongly","-2" , voter[,i])
  voter[,i]= as.factor(voter[,i])
}

voter[,100] # I don't know why it doesn't recognize first go. Maybe because I put as.character. Anyway:

for(i in 100:102){
  voter[,i] = as.character(voter[,i])
  voter[,i]= sub("Don't know", "0", voter[,i])
  voter[,i]= sub("Disagree", "-1", voter[,i])
  voter[,i]= sub("Agree", "1", voter[,i])
  voter[,i]= sub("1 strongly", "2", voter[,i])
  voter[,i]= sub("-1 strongly","-2" , voter[,i])
  voter[,i]= as.numeric(voter[,i])
}

levels(voter[,103])

for(i in 103:105){
  voter[,i]= sub("Don't know", "0", voter[,i])
  voter[,i]= sub("Not very proud" , "-1", voter[,i])
  voter[,i]= sub("Somewhat proud", "1", voter[,i])
  voter[,i]= sub("Very proud", "2", voter[,i])
  voter[,i]= sub("Not proud at all", "-2", voter[,i])
  voter[,i]= as.numeric(voter[,i])
}

levels(voter[,106])

for(i in 106:113){
  voter[,i]= sub("Don't know", "0", voter[,i])
  voter[,i]= sub("Not very important" , "-1", voter[,i])
  voter[,i]= sub("Fairly important", "1", voter[,i])
  voter[,i]= sub("Very important", "2", voter[,i])
  voter[,i]= sub("Not important at all", "-2", voter[,i])
  voter[,i]= as.numeric(voter[,i])
}

levels(voter[,114])

for(i in 114:119){
  voter[,i]= sub("Don't know", "0", voter[,i])
  voter[,i]= sub("Somewhat Disagree" , "-1", voter[,i])
  voter[,i]= sub("Somewhat Agree", "1", voter[,i])
  voter[,i]= sub("Strongly Agree", "2", voter[,i])
  voter[,i]= sub("Strongly Disagree", "-2", voter[,i])
  voter[,i]= as.numeric(voter[,i])
}

levels(voter[,120])

levels(voter[,121])

for(i in 121:124){
  voter[,i]= sub("Don't know", "0", voter[,i])
  voter[,i]= sub("Don't Know", "0", voter[,i])
  voter[,i]= sub("Disagree" , "-1", voter[,i])
  voter[,i]= sub("Agree", "1", voter[,i])
  voter[,i]= sub("Strongly Agree", "2", voter[,i])
  voter[,i]= sub("Strongly Disagree", "-2", voter[,i])
  voter[,i]= as.factor(voter[,i])
}

# Again, the same problem,
for(i in 121:124){
  voter[,i]= sub("Don't know", "0", voter[,i])
  voter[,i]= sub("Don't Know", "0", voter[,i])
  voter[,i]= sub("Disagree" , "-1", voter[,i])
  voter[,i]= sub("Agree", "1", voter[,i])
  voter[,i]= sub("Strongly 1", "2", voter[,i])
  voter[,i]= sub("Strongly -1", "-2", voter[,i])
  voter[,i]= as.numeric(voter[,i])
}

unique(voter[,121])

for(i in 126:131){
  voter[,i]= sub("Don't know", "0", voter[,i])
  voter[,i]= sub("Don't Know", "0", voter[,i])
  voter[,i]= sub("Disagree" , "-1", voter[,i])
  voter[,i]= sub("Agree", "1", voter[,i])
  voter[,i]= sub("Strongly Agree", "2", voter[,i])
  voter[,i]= sub("Strongly agree", "2", voter[,i])
  voter[,i]= sub("Strongly Disagree", "-2", voter[,i])
  voter[,i]= sub("Strongly disagree", "-2", voter[,i])
  voter[,i]= as.factor(voter[,i])
}

# And once, again
for(i in 126:131){
  voter[,i]= sub("Strongly 1", "2", voter[,i])
  voter[,i]= sub("Strongly -1", "-2", voter[,i])
  voter[,i]= as.numeric(voter[,i])
}

levels(voter[,126])

levels(voter[,142])

for(i in 142:146){
  voter[,i]= sub("Yes", "1", voter[,i])
  voter[,i]= sub("No", "0", voter[,i])
  voter[,i]= as.numeric(voter[,i])
}

levels(voter[,148])
voter[,148]

voter[,148]= sub("Never", "-3", voter[,148])
voter[,148]= sub("Not at all", "-1", voter[,148])
voter[,148]= sub("Some days", "1", voter[,148])
voter[,148]= sub("Every day", "3", voter[,148])
voter[,148]= as.numeric(voter[,148])

levels(voter[,149])

for (i in c(149,151:153)){
  voter[,i]= sub("Don't know", "0", voter[,i])
  voter[,i]= sub("Yes", "1", voter[,i])
  voter[,i]= sub("No", "-1", voter[,i])
  voter[,i]= as.numeric(voter[,i]) 
}

levels(voter[,157])

voter[,157]= sub("Yes", "1", voter[,157])
voter[,157]= sub("No", "0", voter[,157])
voter[,157]= as.numeric(voter[,157])

levels(voter[,162])

voter[,162]= sub("Don't know", "0", voter[,162])
voter[,162]= sub("Only now and then", "-1", voter[,162])
voter[,162]= sub("Some of the time", "1", voter[,162])
voter[,162]= sub("Hardly at all", "-2", voter[,162])
voter[,162]= sub("Most of the time", "2", voter[,162])
voter[,162]= as.numeric(voter[,162])

levels(voter[,163])

voter[,163]= sub("Yes", "1", voter[,163])
voter[,163]= sub("No", "0", voter[,163])
voter[,163]= as.numeric(voter[,163])

levels(voter[,164])

voter[,164]= sub("Don't know", "0", voter[,164])
voter[,164]= sub("Not too important", "-1", voter[,164])
voter[,164]= sub("Somewhat important", "1", voter[,164])
voter[,164]= sub("Not at all important", "-2", voter[,164])
voter[,164]= sub("Very important", "2", voter[,164])
voter[,164]= as.numeric(voter[,164])

# Now I must impute the NA values from the subset of coloumn 52 to 74 i.e. the imiss coloumns

voter = droplevels(voter)
str(voter)

# Proceed with the imputations for imiss coloumns

imiss.sub <- voter[,52:74]
sum(is.na(imiss.sub))
# There are quite a few NAs

library(bcv)

imiss_no_na <- na.omit(imiss.sub)
nrow(imiss.sub) - nrow(imiss_no_na)
# We lose 656 observational units by omitting NAs

# Cross validate to find optimal k for svd imputation
?cv.svd.wold
cvw <- cv.svd.wold(imiss_no_na, 5, maxrank = 10)
plot(cvw)
# therefore, optimal k = 2.

# Apply impute.svd to imiss.sub
class(imiss.sub[,1]) # They are numeric. Proceed

imp.imiss = impute.svd(imiss.sub, k = 2, maxiter = 1000)$x
imp.imiss

# To see the imputations
max(imp.imiss)
max(imiss_no_na)
min(imp.imiss)
min(imiss_no_na)
# notice that max and min is 4.57 and 0.42 whereas for the actual dataset, it was 4 and 1 respectively
# We leave is as it is since the nn will use the information flexibly.

# Now to replace the original block of voter with these new imputed values:
class(imp.imiss)
imp.imiss = as.data.frame(imp.imiss)
colnames(imp.imiss) <- colnames(voter[52:74])

# imp.imiss = rbind(rep("Importance", 23),imp.imiss) # if you want to use it.

# Therefore imp.imiss is the new imputed data for the original dataset

# Do the same for rigged
rigged <- voter[,19:24]
rigged_no_na = na.omit(rigged)
nrow(rigged) - nrow(rigged_no_na)
# we loose 398 observations

cvw <- cv.svd.wold(rigged_no_na, 5, maxrank = 6)
plot(cvw)
# Best k seems to be 6. 

# Apply impute.svd to rigged
imp.rigged = impute.svd(rigged, k = 6, maxiter = 1000)$x
imp.rigged

# To see the imputations
max(imp.rigged)
min(imp.rigged)
colnames(imp.rigged) <- colnames(voter[,19:24])

# Create the new dataset by cbinding

imp.voter <- cbind.data.frame(voter[,1:18],imp.rigged, voter[,25:51], imp.imiss, voter[,75:167])
View(imp.voter)


#################################### Setting up the model #####################################################


# First things first, divide the dataset into different subparts

Soc.labels = which(voter.labels == "SocSent")
Poli.lables = which(voter.labels == "PoliSent")
Ameri.labels = which(voter.labels == "America")
Policy.labels = which(voter.labels == "Policy")
Econ.labels = which(voter.labels == "EconSent")
Issue.labels = which(voter.labels == "Importance")
Demo.labels = which(voter.labels == "Demo")

length(Soc.labels)
length(Poli.lables)
length(Ameri.labels)
length(Policy.labels)
length(Econ.labels)
length(Issue.labels)
length(Demo.labels)

# Omit NAs from imp.voter

imp.voter2 = na.omit(imp.voter)
nrow(imp.voter2)
ncol(imp.voter2)
# We lose 301 observations


# Take out two variables that are potential responses. i.e. vote for or against and pres.vote (i.e. who they voted for)

respose.presvote = imp.voter2[,2]
response.votefor = imp.voter2[,3]

# Now divide the no NA dataset into the separate categories
# From the highest no. of coloumns to the lowest

input.Poli = imp.voter2[, Poli.lables]
input.Soc = imp.voter2[, Soc.labels]
input.Issue = imp.voter2[, Issue.labels]
input.Demo = imp.voter2[, Demo.labels]
input.Policy = imp.voter2[, Policy.labels]
input.Econ = imp.voter2[, Econ.labels]
input.Ameri = imp.voter2[, Ameri.labels]

nrow(input.Ameri)
length(response.votefor)
length(respose.presvote)
# Therefore, they have the same length

############################## Running the Models using individual explanatory variable sets ###################################################

# We now begin testing our dataset

install.packages("RSNNS")
library(RSNNS)
library(help=RSNNS)

# Start with SocSent
str(input.Soc)

# There are several categorical variables that must be converted into dummies before running nn

model.input.SocSent = model.matrix(~., input.Soc)
str(model.input.SocSent)
# This there are no more factors in the dataset

model.input.SocSent = as.data.frame(model.input.SocSent) # convert to dataframe

# lets try and run the NN of this input on votefor

Targs = response.votefor
DecTargets <- decodeClassLabels(Targs)
DecTargets

nrow(DecTargets)
nrow(Vals)
# Again lengths are same

lol <- splitForTrainingAndTest(Vals, DecTargets, ratio = 0.25)
lol <- normTrainingAndTestSet(lol)
lol

# Run NN
model <- mlp(lol$inputsTrain, lol$targetsTrain, size = 10,
             learnFuncParams = 0.01, maxit = 1000, inputsTest = lol$inputsTest,
             targetsTest = lol$targetsTest)
predictions <- predict(model, lol$inputsTest)
lol$targetsTrain
fitted.values(model)
confusionMatrix(lol$targetsTrain, fitted.values(model))

confusionMatrix(lol$targetsTest, predictions)

plotIterativeError(model)
plotRegressionError(predictions[, 2], lol$targetsTest[, 2], pch = 3)
plotROC(fitted.values(model)[, 2], lol$targetsTrain[, 2])
plotROC(predictions[, 2], lol$targetsTest[, 2])

# Not a good model.

# Try for the other response presvote

Targs = respose.presvote

model <- mlp(lol$inputsTrain, lol$targetsTrain, size = 10,
             learnFuncParams = 0.01, maxit = 1000, inputsTest = lol$inputsTest,
             targetsTest = lol$targetsTest)
predictions <- predict(model, lol$inputsTest)
lol$targetsTrain
fitted.values(model)
confusionMatrix(lol$targetsTrain, fitted.values(model))

confusionMatrix(lol$targetsTest, predictions)

plotIterativeError(model)
plotRegressionError(predictions[, 2], lol$targetsTest[, 2], pch = 3)
plotROC(fitted.values(model)[, 2], lol$targetsTrain[, 2])
plotROC(predictions[, 2], lol$targetsTest[, 2])

# Much better

# Try on Issues

str(input.Issue)
# Here no categorical variables are present

Vals = input.Issue
class(Vals)


Targs = response.votefor
DecTargets <- decodeClassLabels(Targs)
DecTargets

nrow(DecTargets)
nrow(Vals)
# Again lengths are same


lol <- splitForTrainingAndTest(Vals, DecTargets, ratio = 0.25)
lol <- normTrainingAndTestSet(lol)
lol

# Run NN
model <- mlp(lol$inputsTrain, lol$targetsTrain, size = c(10,10),
             learnFuncParams = 0.01, maxit = 1000, inputsTest = lol$inputsTest,
             targetsTest = lol$targetsTest)
predictions <- predict(model, lol$inputsTest)
lol$targetsTrain
fitted.values(model)
confusionMatrix(lol$targetsTrain, fitted.values(model))

confusionMatrix(lol$targetsTest, predictions)

plotIterativeError(model)
plotRegressionError(predictions[, 2], lol$targetsTest[, 2], pch = 3)
plotROC(fitted.values(model)[, 2], lol$targetsTrain[, 2])
plotROC(predictions[, 2], lol$targetsTest[, 2])

# Not good enough.

# Change response
Targs = respose.presvote
DecTargets <- decodeClassLabels(Targs)

Vals = input.Issue
lol <- splitForTrainingAndTest(Vals, DecTargets, ratio = 0.25)
lol <- normTrainingAndTestSet(lol)
lol

# Run NN
model <- mlp(lol$inputsTrain, lol$targetsTrain, size = 10,
             learnFuncParams = 0.01, maxit = 1000, inputsTest = lol$inputsTest,
             targetsTest = lol$targetsTest)
predictions <- predict(model, lol$inputsTest)
lol$targetsTrain
fitted.values(model)
confusionMatrix(lol$targetsTrain, fitted.values(model))

confusionMatrix(lol$targetsTest, predictions)

plotIterativeError(model)
plotRegressionError(predictions[, 2], lol$targetsTest[, 2], pch = 3)
plotROC(fitted.values(model)[, 2], lol$targetsTrain[, 2])
plotROC(predictions[, 2], lol$targetsTest[, 2])

# Again, much better.

# Run on Demographics

str(input.Demo)
model.input.Demo = model.matrix(~., input.Demo)

Vals = model.input.Demo
class(Vals)

Targs = response.votefor
DecTargets <- decodeClassLabels(Targs)
DecTargets

nrow(DecTargets)
nrow(Vals)
# Again lengths are same


lol <- splitForTrainingAndTest(Vals, DecTargets, ratio = 0.25)
lol <- normTrainingAndTestSet(lol)
lol

# Run NN
model <- mlp(lol$inputsTrain, lol$targetsTrain, size = c(10,10),
             learnFuncParams = 0.01, maxit = 1000, inputsTest = lol$inputsTest,
             targetsTest = lol$targetsTest)
predictions <- predict(model, lol$inputsTest)
predictions
lol$targetsTrain
fitted.values(model)
confusionMatrix(lol$targetsTrain, fitted.values(model))

confusionMatrix(lol$targetsTest, predictions)

plotIterativeError(model)
plotRegressionError(predictions[, 2], lol$targetsTest[, 2], pch = 3)
plotROC(fitted.values(model)[, 2], lol$targetsTrain[, 2])
plotROC(predictions[, 2], lol$targetsTest[, 2])

# Not good enough.

# Change response to presvote
Targs = response.votefor
DecTargets <- decodeClassLabels(Targs)

Vals = model.input.Demo
class(Vals)

Targs = response.votefor
DecTargets <- decodeClassLabels(Targs)
DecTargets

nrow(DecTargets)
nrow(Vals)
# Again lengths are same


lol <- splitForTrainingAndTest(Vals, DecTargets, ratio = 0.25)
lol <- normTrainingAndTestSet(lol)
lol

# Run NN
model <- mlp(lol$inputsTrain, lol$targetsTrain, size = c(10,10),
             learnFuncParams = 0.01, maxit = 1000, inputsTest = lol$inputsTest,
             targetsTest = lol$targetsTest)
predictions <- predict(model, lol$inputsTest)
predictions
lol$targetsTrain
fitted.values(model)
confusionMatrix(lol$targetsTrain, fitted.values(model))

confusionMatrix(lol$targetsTest, predictions)

plotIterativeError(model)
plotRegressionError(predictions[, 2], lol$targetsTest[, 2], pch = 3)
plotROC(fitted.values(model)[, 2], lol$targetsTrain[, 2])
plotROC(predictions[, 2], lol$targetsTest[, 2])

# Not too good either. 

# Skip Ameri and try on Political Sentiments

Targs = response.votefor
DecTargets <- decodeClassLabels(Targs)

str(input.Poli)
model.input.Poli = model.matrix(~., input.Poli)

Vals = model.input.Poli
class(Vals)

lol <- splitForTrainingAndTest(Vals, DecTargets, ratio = 0.25)
lol <- normTrainingAndTestSet(lol)
lol

# Run NN
model <- mlp(lol$inputsTrain, lol$targetsTrain, size = c(10,10),
             learnFuncParams = 0.01, maxit = 1000, inputsTest = lol$inputsTest,
             targetsTest = lol$targetsTest)
predictions <- predict(model, lol$inputsTest)
predictions
lol$targetsTrain
fitted.values(model)
confusionMatrix(lol$targetsTrain, fitted.values(model))

confusionMatrix(lol$targetsTest, predictions)

plotIterativeError(model)
plotRegressionError(predictions[, 2], lol$targetsTest[, 2], pch = 3)
plotROC(fitted.values(model)[, 2], lol$targetsTrain[, 2])
plotROC(predictions[, 2], lol$targetsTest[, 2])

# Really bad.

# Change response to presvote
Targs = respose.presvote
DecTargets <- decodeClassLabels(Targs)
DecTargets

lol <- splitForTrainingAndTest(Vals, DecTargets, ratio = 0.25)
lol <- normTrainingAndTestSet(lol)
lol

# Run NN
model <- mlp(lol$inputsTrain, lol$targetsTrain, size = c(10,10),
             learnFuncParams = 0.01, maxit = 1000, inputsTest = lol$inputsTest,
             targetsTest = lol$targetsTest)
predictions <- predict(model, lol$inputsTest)
predictions
lol$targetsTrain
fitted.values(model)
confusionMatrix(lol$targetsTrain, fitted.values(model))

confusionMatrix(lol$targetsTest, predictions)

plotIterativeError(model)
plotRegressionError(predictions[, 2], lol$targetsTest[, 2], pch = 3)
plotROC(fitted.values(model)[, 2], lol$targetsTrain[, 2])
plotROC(predictions[, 2], lol$targetsTest[, 2])

# Much better.

# Now try on Policy

Targs = response.votefor
DecTargets <- decodeClassLabels(Targs)
DecTargets

str(input.Policy)
# lots of factor, so change
model.input.Policy = model.matrix(~., input.Policy)
model.input.Policy = as.data.frame(model.input.Policy)

Vals = model.input.Policy
class(Vals)

lol <- splitForTrainingAndTest(Vals, DecTargets, ratio = 0.25)
lol <- normTrainingAndTestSet(lol)
lol

# Run NN
model <- mlp(lol$inputsTrain, lol$targetsTrain, size = c(10,10),
             learnFuncParams = 0.01, maxit = 1000, inputsTest = lol$inputsTest,
             targetsTest = lol$targetsTest)
predictions <- predict(model, lol$inputsTest)
predictions
lol$targetsTrain
fitted.values(model)
confusionMatrix(lol$targetsTrain, fitted.values(model))

confusionMatrix(lol$targetsTest, predictions)

plotIterativeError(model)
plotRegressionError(predictions[, 2], lol$targetsTest[, 2], pch = 3)
plotROC(fitted.values(model)[, 2], lol$targetsTrain[, 2])
plotROC(predictions[, 2], lol$targetsTest[, 2])

# Really bad again.

# Change response to presvote
Targs = respose.presvote
DecTargets <- decodeClassLabels(Targs)
DecTargets

lol <- splitForTrainingAndTest(Vals, DecTargets, ratio = 0.25)
lol <- normTrainingAndTestSet(lol)
lol

# Run NN
model <- mlp(lol$inputsTrain, lol$targetsTrain, size = c(10,10),
             learnFuncParams = 0.01, maxit = 1000, inputsTest = lol$inputsTest,
             targetsTest = lol$targetsTest)
predictions <- predict(model, lol$inputsTest)
predictions
lol$targetsTrain
fitted.values(model)
confusionMatrix(lol$targetsTrain, fitted.values(model))

confusionMatrix(lol$targetsTest, predictions)

plotIterativeError(model)
plotRegressionError(predictions[, 2], lol$targetsTest[, 2], pch = 3)
plotROC(fitted.values(model)[, 2], lol$targetsTrain[, 2])
plotROC(predictions[, 2], lol$targetsTest[, 2])

# Not as bad as before. Good enough

# Finally, try on Economic Issues Sentiment

Targs = response.votefor
DecTargets <- decodeClassLabels(Targs)
DecTargets

str(input.Econ)
# lots of factor, so change
model.input.Econ = model.matrix(~., input.Econ)
model.input.Econ = as.data.frame(model.input.Econ)

Vals = model.input.Econ
class(Vals)

lol <- splitForTrainingAndTest(Vals, DecTargets, ratio = 0.25)
lol <- normTrainingAndTestSet(lol)
lol

# Run NN
model <- mlp(lol$inputsTrain, lol$targetsTrain, size = c(10,10),
             learnFuncParams = 0.01, maxit = 1000, inputsTest = lol$inputsTest,
             targetsTest = lol$targetsTest)
predictions <- predict(model, lol$inputsTest)
predictions
lol$targetsTrain
fitted.values(model)
confusionMatrix(lol$targetsTrain, fitted.values(model))

confusionMatrix(lol$targetsTest, predictions)

plotIterativeError(model)
plotRegressionError(predictions[, 2], lol$targetsTest[, 2], pch = 3)
plotROC(fitted.values(model)[, 2], lol$targetsTrain[, 2])
plotROC(predictions[, 2], lol$targetsTest[, 2])

# Really bad again 

# Change response to presvote
Targs = respose.presvote
DecTargets <- decodeClassLabels(Targs)
DecTargets

lol <- splitForTrainingAndTest(Vals, DecTargets, ratio = 0.25)
lol <- normTrainingAndTestSet(lol)
lol

# Run NN
model <- mlp(lol$inputsTrain, lol$targetsTrain, size = c(10,10),
             learnFuncParams = 0.01, maxit = 1000, inputsTest = lol$inputsTest,
             targetsTest = lol$targetsTest)
predictions <- predict(model, lol$inputsTest)
predictions
lol$targetsTrain
fitted.values(model)
confusionMatrix(lol$targetsTrain, fitted.values(model))

confusionMatrix(lol$targetsTest, predictions)

plotIterativeError(model)
plotRegressionError(predictions[, 2], lol$targetsTest[, 2], pch = 3)
plotROC(fitted.values(model)[, 2], lol$targetsTrain[, 2])
plotROC(predictions[, 2], lol$targetsTest[, 2])

# Not too good but better

# So, we see that predicting the response variable presvote (i.e who they voted for) using the different subsets 
# we have individually, each being a  collection of variables representing the sentiments of the respondents towards 
# distinct topics like Politics, Society, Social Issues, Economic Issues and Policy Debates is more accurate
# than predicting the response variable votefor (i.e. whether they voter for, against their respective parties or neither.)
# It must be admitted that despite using relatively complex neural networks i.e. a 10x10 mlp is still quite limited
# in its ability to predict presvote, even though those predictions are far more accurate than trying to predict votefor.

########################## Running the Model using Combinations of explanatory variable sets #####################

# So now, using this information, let us try and predict both responses again using a combination of these variables.

# First, we try Soc.Sent, Poli.Sent and Econ.Sent

input.AllSent = cbind(input.Poli, input.Soc, input.Econ)
str(input.AllSent)
# Has categories, so change
model.input.AllSent = model.matrix(~., input.AllSent)
model.input.AllSent = as.data.frame(model.input.AllSent)

Targs = response.votefor
DecTargets <- decodeClassLabels(Targs)
DecTargets

Vals = model.input.AllSent
class(Vals)

lol <- splitForTrainingAndTest(Vals, DecTargets, ratio = 0.25)
lol <- normTrainingAndTestSet(lol)
lol

# Run NN
model <- mlp(lol$inputsTrain, lol$targetsTrain, size = c(10,10),
             learnFuncParams = 0.01, maxit = 1000, inputsTest = lol$inputsTest,
             targetsTest = lol$targetsTest)
predictions <- predict(model, lol$inputsTest)
predictions
lol$targetsTrain
fitted.values(model)
confusionMatrix(lol$targetsTrain, fitted.values(model))

confusionMatrix(lol$targetsTest, predictions)

plotIterativeError(model)
plotRegressionError(predictions[, 2], lol$targetsTest[, 2], pch = 3)
plotROC(fitted.values(model)[, 2], lol$targetsTrain[, 2])
plotROC(predictions[, 2], lol$targetsTest[, 2])

# Really bad, as expected 

# Change response to presvote
Targs = respose.presvote
DecTargets <- decodeClassLabels(Targs)
DecTargets

lol <- splitForTrainingAndTest(Vals, DecTargets, ratio = 0.25)
lol <- normTrainingAndTestSet(lol)
lol

# Run NN
model <- mlp(lol$inputsTrain, lol$targetsTrain, size = c(10,10),
             learnFuncParams = 0.01, maxit = 1000, inputsTest = lol$inputsTest,
             targetsTest = lol$targetsTest)
predictions <- predict(model, lol$inputsTest)
predictions
lol$targetsTrain
fitted.values(model)
confusionMatrix(lol$targetsTrain, fitted.values(model))

confusionMatrix(lol$targetsTest, predictions)

plotIterativeError(model)
plotRegressionError(predictions[, 2], lol$targetsTest[, 2], pch = 3)
plotROC(fitted.values(model)[, 2], lol$targetsTrain[, 2])
plotROC(predictions[, 2], lol$targetsTest[, 2])

# Not as bad, as expected.

# Now, lets try and use the other three variables i.e. Policy, Issues and Demographics.

input.rest = cbind(input.Demo, input.Policy, input.Issue)
str(input.rest)
# Has factors, so change
model.input.rest = model.matrix(~., input.rest)
model.input.rest = as.data.frame(model.input.rest)

# Try on votefor
Targs = response.votefor
DecTargets <- decodeClassLabels(Targs)
DecTargets


Vals = model.input.AllSent
class(Vals)

lol <- splitForTrainingAndTest(Vals, DecTargets, ratio = 0.25)
lol <- normTrainingAndTestSet(lol)
lol

# Run NN
model <- mlp(lol$inputsTrain, lol$targetsTrain, size = c(10,10),
             learnFuncParams = 0.01, maxit = 1000, inputsTest = lol$inputsTest,
             targetsTest = lol$targetsTest)
predictions <- predict(model, lol$inputsTest)
predictions
lol$targetsTrain
fitted.values(model)
confusionMatrix(lol$targetsTrain, fitted.values(model))

confusionMatrix(lol$targetsTest, predictions)

plotIterativeError(model)
plotRegressionError(predictions[, 2], lol$targetsTest[, 2], pch = 3)
plotROC(fitted.values(model)[, 2], lol$targetsTrain[, 2])
plotROC(predictions[, 2], lol$targetsTest[, 2])

# Really bad, as expected 

# Change response to presvote
Targs = respose.presvote
DecTargets <- decodeClassLabels(Targs)
DecTargets

lol <- splitForTrainingAndTest(Vals, DecTargets, ratio = 0.25)
lol <- normTrainingAndTestSet(lol)
lol

# Run NN
model <- mlp(lol$inputsTrain, lol$targetsTrain, size = c(10,10),
             learnFuncParams = 0.01, maxit = 1000, inputsTest = lol$inputsTest,
             targetsTest = lol$targetsTest)
predictions <- predict(model, lol$inputsTest)
predictions
lol$targetsTrain
fitted.values(model)
confusionMatrix(lol$targetsTrain, fitted.values(model))

confusionMatrix(lol$targetsTest, predictions)

plotIterativeError(model)
plotRegressionError(predictions[, 2], lol$targetsTest[, 2], pch = 3)
plotROC(fitted.values(model)[, 2], lol$targetsTrain[, 2])
plotROC(predictions[, 2], lol$targetsTest[, 2])

# Not as bad, as expected

# Finally, we use our entire dataset.

input.fullModel = cbind(input.AllSent, input.rest)
str(input.fullModel)
# Has factors, so change
model.input.fullModel = model.matrix(~., input.fullModel)
model.input.fullModel = as.data.frame(model.input.fullModel)

# Use response votefor
Targs = response.votefor
DecTargets <- decodeClassLabels(Targs)
DecTargets


Vals = model.input.fullModel
class(Vals)

lol <- splitForTrainingAndTest(Vals, DecTargets, ratio = 0.25)
lol <- normTrainingAndTestSet(lol)
lol

# Run NN
model <- mlp(lol$inputsTrain, lol$targetsTrain, size = c(10,10),
             learnFuncParams = 0.01, maxit = 1000, inputsTest = lol$inputsTest,
             targetsTest = lol$targetsTest)
predictions <- predict(model, lol$inputsTest)
predictions
lol$targetsTrain
fitted.values(model)
confusionMatrix(lol$targetsTrain, fitted.values(model))

confusionMatrix(lol$targetsTest, predictions)

plotIterativeError(model)
plotRegressionError(predictions[, 2], lol$targetsTest[, 2], pch = 3)
plotROC(fitted.values(model)[, 2], lol$targetsTrain[, 2])
plotROC(predictions[, 2], lol$targetsTest[, 2])

# Again and finally confirmed. Given our dataset, it is impossible to predict votefor accurately.  

# Change response to presvote
Targs = respose.presvote
DecTargets <- decodeClassLabels(Targs)
DecTargets

lol <- splitForTrainingAndTest(Vals, DecTargets, ratio = 0.25)
lol <- normTrainingAndTestSet(lol)
lol

# Run NN
model <- mlp(lol$inputsTrain, lol$targetsTrain, size = 7,
             learnFuncParams = 0.01, maxit = 1000, inputsTest = lol$inputsTest,
             targetsTest = lol$targetsTest)
predictions <- predict(model, lol$inputsTest)
predictions
lol$targetsTrain
fitted.values(model)
confusionMatrix(lol$targetsTrain, fitted.values(model))

confusionMatrix(lol$targetsTest, predictions)

plotIterativeError(model)
plotRegressionError(predictions[, 2], lol$targetsTest[, 2], pch = 3)
plotROC(fitted.values(model)[, 2], lol$targetsTrain[, 2])
plotROC(predictions[, 2], lol$targetsTest[, 2])

######################################## CONCLUSIONS #######################################################

# Again, it is far easier given the data we have, to predict presvote more accurately.

# Thus, by our analysis we conclude that it is much more difficult to predict which voters will remain loyal to their 
# parties and which voters will cross lines to vote against their parties using information about their views and
# opinion about the various topics considered in the study. It is however, easier to predict who one will vote for given
# this information beforehand. There may exist other underlying factors that influence decisions by voters to cross
# lines that our analysis was unable to capture. It may also be a result of a more imbalanced set of responses for votefor
# than for presvote, although we would expect a nn network to still be flexible enough to account for such imbalances.

table(imp.voter$vote_for_against_2016)
table(imp.voter$presvote16post_2016)
nrow(input.fullModel) # no. of observations used
ncol(input.fullModel) # no. of variables used


