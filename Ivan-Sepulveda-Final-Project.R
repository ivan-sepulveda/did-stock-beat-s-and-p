# Before we get started, let's import all the libraries we're are going to need.
library(caTools) # Logistic Regression
library(e1071) # Support Vector Machine
library(psych) # K Nearest Neighbor (KNN)
library(class) # K Nearest Neighbor (KNN)


# First: Import all S&P Data from Start Date - April 1st, 2018
SNP_4_1_18 <- read.csv('constituents-financials.csv')

# Cleaning Data: Deleting irrelevant columns
SNP_4_1_18 <- SNP_4_1_18[ , -which(names(SNP_4_1_18) %in% c("SEC.Filings"))]
SNP_4_1_18 <- SNP_4_1_18[ , -which(names(SNP_4_1_18) %in% c("Name"))]

# Encoding: The Stock Market has 11 sectors and our dataset current has them stored by name.
SNP_4_1_18$Sector <- factor(SNP_4_1_18$Sector, levels = 
  c("Financials", "Utilities", "Consumer Discretionary", "Consumer Staples", "Energy", "Health Care", 
    "Industrials", "Information Technology", "Telecommunication Services", "Materials", "Real Estate"), 
  labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))

colnames(SNP_4_1_18)

# Re-naming some columns for utility and for aesthetic purposes
colnames(SNP_4_1_18)[colnames(SNP_4_1_18)=="Price"] <- "Price.4.1.18"
colnames(SNP_4_1_18)[colnames(SNP_4_1_18)=="X52.Week.Low"] <- "52WeekLow.4.1.18"
colnames(SNP_4_1_18)[colnames(SNP_4_1_18)=="X52.Week.High"] <- "52WeekHigh.4.1.18"
colnames(SNP_4_1_18)[colnames(SNP_4_1_18)=="Price.Sales."] <- "52WeekHigh.4.1.18"
colnames(SNP_4_1_18)[colnames(SNP_4_1_18)=="Dividend.Yield"] <- "Dividend.Yield.4.1.18"
colnames(SNP_4_1_18)[colnames(SNP_4_1_18)=="EBITDA"] <- "EBITDA.4.1.18"
colnames(SNP_4_1_18)[colnames(SNP_4_1_18)=="Market.Cap"] <- "Market.Cap.4.1.18"
colnames(SNP_4_1_18)[colnames(SNP_4_1_18)=="Price.Book"] <- "Price.Book.4.1.18"
colnames(SNP_4_1_18)[colnames(SNP_4_1_18)=="Price.Sales"] <- "Price.Sales.4.1.18"
colnames(SNP_4_1_18)[colnames(SNP_4_1_18)=="Earnings.Share"] <- "Earnings.Share.4.1.18"

# Next: Import all S&P Data from End Date - August 24th, 2018
SNP_8_24_18 <- read.csv('Aug24_2018_SP.csv')

# We need to combine these datasets. Problem is that 8.24.18 has 497 tickers, and 4.1.18 has 505.
# The most straightforward solution is the remove the tickers in 4.1.18 that are not in 8.24.18.
SNP_4_1_18 <- SNP_4_1_18[SNP_4_1_18$Symbol %in% SNP_8_24_18$ticker, ]

# For whatever reason, now 8.24 has more rows. So now we do the inverse.
SNP_8_24_18 <- SNP_8_24_18[SNP_8_24_18$ticker %in% SNP_4_1_18$Symbol, ]

# We're going to need a list of all the tickers
ticker <- data.frame(SNP_4_1_18$Symbol)

# Now we start building our dataset from these tickers and the price differences.
stockPriceMovements <- data.frame(SNP_4_1_18)
stockPriceMovements$Price.8.24.18 <- NA
stockPriceMovements$PriceDifference <- NA
stockPriceMovements$percentChange <- NA
stockPriceMovements$OutperformedSNP <- NA

endPriceColNumber <- as.numeric(which(colnames(stockPriceMovements) =="Price.8.24.18"))
priceDifferenceColNumber <- as.numeric(which(colnames(stockPriceMovements) =="PriceDifference"))
percentChangeColNumber <- as.numeric(which(colnames(stockPriceMovements) =="percentChange"))
outperformedSNP_ColNum <- as.numeric(which(colnames(stockPriceMovements) =="OutperformedSNP"))

# According to https://dqydj.com/sp-500-return-calculator/, from April 2018 to September 2018
# the total S&P was 9.341%. If we have a stock that outperformed the S&P, it will be assigned
# a value of 1. If a stock's return was equal to or less than 9.341%, it will be assigned a
# value of 0.

SNP_ReturnPercentage <- 9.341


for (companySymbol in ticker$SNP_4_1_18.Symbol) {
  correlatedRowNumber <- which(SNP_4_1_18$Symbol  %in% c(companySymbol))
  companyFinancialsStartDate <- SNP_4_1_18[which(SNP_4_1_18$Symbol  %in% c(companySymbol)), ]
  stockMovementsEndDate <- SNP_8_24_18[which(SNP_8_24_18$ticker  %in% c(companySymbol)), ]
  stockPriceStartDate <- companyFinancialsStartDate$Price.4.1.18
  stockPriceEndDate <- round(as.numeric(stockMovementsEndDate$close), digits = 2)
  stockRiseOrDropPercentage <- 100 * ((stockPriceEndDate - stockPriceStartDate)/stockPriceStartDate)
  stockPriceMovements[correlatedRowNumber, endPriceColNumber] <- stockPriceEndDate
  stockPriceMovements[correlatedRowNumber, priceDifferenceColNumber] <- stockPriceEndDate - stockPriceStartDate
  stockPriceMovements[correlatedRowNumber, percentChangeColNumber] <- stockRiseOrDropPercentage
  if ((stockRiseOrDropPercentage*100) > SNP_ReturnPercentage){
    stockPriceMovements[correlatedRowNumber, outperformedSNP_ColNum] <- 1
  } else {
    stockPriceMovements[correlatedRowNumber, outperformedSNP_ColNum] <- 0
  }
}

# Cleaning up variables no longer needed.
rm(companyFinancialsStartDate, SNP_4_1_18, SNP_8_24_18, stockMovementsEndDate, ticker)
rm(companySymbol, correlatedRowNumber, stockPriceEndDate, stockPriceStartDate, stockRiseOrDropPercentage)

# Okay, now it's finally time to run some Classification Techniques
# Must remove columns that we don't want to benchmark and columns that are dead giveaways.
stockPriceMovements <- stockPriceMovements[ , -which(names(stockPriceMovements) %in% c("percentChange"))]
stockPriceMovements <- stockPriceMovements[ , -which(names(stockPriceMovements) %in% c("PriceDifference"))]
stockPriceMovements <- stockPriceMovements[ , -which(names(stockPriceMovements) %in% c("Price.4.1.18"))]
stockPriceMovements <- stockPriceMovements[ , -which(names(stockPriceMovements) %in% c("Price.8.24.18"))]

# I'm also chosing to completely remove rows that have any NA values
stockPriceMovements <- stockPriceMovements[complete.cases(stockPriceMovements),]

# More variable Cleaning
rm(endPriceColNumber, percentChangeColNumber, priceDifferenceColNumber, SNP_ReturnPercentage, outperformedSNP_ColNum)

# Let's start with Logistic Regression! First we pull the target variable column number.
targetVarColNum = as.numeric(which(colnames(stockPriceMovements)=='OutperformedSNP'))
set.seed(42) # Setting seed as usual.
# Now we split the dataset into training and test sets
split = sample.split(stockPriceMovements$OutperformedSNP, 0.75)
training_set = subset(stockPriceMovements, split= TRUE)
test_set = subset(stockPriceMovements, split = FALSE)
# Nest, well need out trusty glm classifier
# logRegClassifier = glm(formula = OutperformedSNP ~ ., family = binomial, data=training_set)
# Question for Prof. Shukla - The previous commented out line of code gives me the warning message below
# Warning message: glm.fit: algorithm did not converge
# Shoud I be concerned? Anyways, I looked it up. Someone said to set the parameter maxit to 100.
# https://discuss.analyticsvidhya.com/t/warning-message-glm-fit-algorithm-did-not-converge/5299/5
logRegClassifier = glm(formula = OutperformedSNP ~ ., family = binomial, data=training_set, maxit = 100)
# Doing so gave me convergance and no warning message whatsoever. 
# Anyways, moving on. As usualy, we need our prob_predict.
probPredict = predict(logRegClassifier, type = 'response', stockPriceMovements1 = test_set[-targetVarColNum])
classPredict = ifelse(probPredict > 0.5, 1, 0)
logRegConMatrix = table(test_set[, targetVarColNum], classPredict) # Confusion Matrix
logRegResultsMatrix = data.matrix(logRegConMatrix)
logRegTrueZero = as.numeric(logRegResultsMatrix[1, 1])
logRegFalseZero = as.numeric(logRegResultsMatrix[1, 2])
logRegTrueOne = as.numeric(logRegResultsMatrix[2, 2])
logRegFalseOne = as.numeric(logRegResultsMatrix[2, 1])
logRegAccuracy = (logRegTrueOne + logRegTrueZero)/(nrow(stockPriceMovements))
print(paste("Logistic Regression Accuracy: ", toString(logRegAccuracy*100) ))
# Question for Prof. Shuckla - I am getting a 100% accuracy for logistic regression...
#     I'm fairly certain that I made a mistake somewhere building this classfication model.
#     Could you possibly look through the code and see where I made a mistake?

# More variable cleanup
rm(logRegConMatrix, logRegResultsMatrix, logRegFalseOne, logRegFalseZero, logRegTrueOne, logRegTrueZero)
rm(logRegAccuracy, logRegClassifier)

# Anyways, moving on. Time for classification by Support Vector Machine (SVM)
# SVM classifier
SVM_Classifier = svm(formula = OutperformedSNP ~ .,data = training_set, type = 'C-classification', kernel = 'linear')
# SVM Prob predict
SVM_ProbPredict = predict(SVM_Classifier, type = 'response', stockPriceMovements1 = test_set[-targetVarColNum])
svmConMatrix = table(test_set[,targetVarColNum], SVM_ProbPredict)
SVM_ResultsMatrix = data.matrix(svmConMatrix)
SVM_TrueZero = as.numeric(SVM_ResultsMatrix[1, 1])
SVM_FalseZero = as.numeric(SVM_ResultsMatrix[1, 2])
SVM_TrueOne = as.numeric(SVM_ResultsMatrix[2, 2])
SVM_FalseOne = as.numeric(SVM_ResultsMatrix[2, 1])
SVM_accuracy = (SVM_TrueOne + SVM_TrueZero)/(nrow(stockPriceMovements))
print(paste("SVM Accuracy: ", toString(SVM_accuracy*100) ))


# Question for Prof. Shuckla
#     Wow, now I'm getting SVM Accuracy = 99.5833333333333. The fact that it's not 100% gives
#     me some comfort. But at the same time, accuracies this high might mean I'm over-fitting?
# Anyways, moving on. Time for classification by K Nearest Neighbor (KNN)

# More variable cleanup
rm(svmConMatrix, SVM_ResultsMatrix, SVM_TrueZero, SVM_FalseZero, SVM_TrueOne, SVM_FalseOne)
rm(SVM_accuracy, SVM_ProbPredict, SVM_Classifier)

# Due to some numbers having larger magnitudes than others (i.e. Market Cap vs Divident Yield)
#     we must scale some of out features. The lines below are me pulling column numbers for
#     the columns that I am going to scale so that knn works properly.
# priceEarningsColNum <- as.numeric(which(names(stockPriceMovements) %in% c("Price.Earnings")))
# dividendYieldColNum <- as.numeric(which(names(stockPriceMovements) %in% c("Dividend.Yield.4.1.18")))
# earningsShareColNum <- as.numeric(which(names(stockPriceMovements) %in% c("Earnings.Share.4.1.18")))
# fiftyTwoWeekLowColNum <- as.numeric(which(names(stockPriceMovements) %in% c("X52WeekLow.4.1.18")))
# fiftyTwoWeekHighColNum <- as.numeric(which(names(stockPriceMovements) %in% c("X52WeekHigh.4.1.18")))
# priceSalesColNum <- as.numeric(which(names(stockPriceMovements) %in% c("Price.Sales.4.1.18")))
# marketCapColNum <- as.numeric(which(names(stockPriceMovements) %in% c("Market.Cap.4.1.18")))
# EBITDA_ColNum <- as.numeric(which(names(stockPriceMovements) %in% c("EBITDA.4.1.18")))
# priceBookColNum <- as.numeric(which(names(stockPriceMovements) %in% c("Price.Book.4.1.18")))
# 
# scaled_features <- c(priceEarningsColNum, dividendYieldColNum, earningsShareColNum, fiftyTwoWeekLowColNum, 
#                      fiftyTwoWeekHighColNum, priceSalesColNum, marketCapColNum, EBITDA_ColNum, priceBookColNum)
# 
# # Feature Scaling (does not apply to binary values)
# training_set[scaled_features] = scale(training_set[scaled_features])
# test_set[scaled_features] = scale(training_set[scaled_features])

best_k <- 7 # Let's try an inital k of 7
# y_predict = knn(training_set[,-targetVarColNum], test_set[,-targetVarColNum], cl = training_set[,targetVarColNum], k=best_k)
# Okay, I tried running the commented out line above, and got re error below.
# Error in knn(training_set[, -targetVarColNum], test_set[, -targetVarColNum],  : 
#     NA/NaN/Inf in foreign function call (arg 6)
#   In addition: Warning messages:
#   1: In knn(training_set[, -targetVarColNum], test_set[, -targetVarColNum],  :
#     NAs introduced by coercion
#   2: Same as 1.
# I feel like this may be coming from the ticker/symbol. 

# Let me temporarily remove the ticker symbol columns and see that happens due to column altering.
stockPriceMovements <- stockPriceMovements[, -which(names(stockPriceMovements) %in% c("Symbol"))]
# Now I have to re-do training and test tests
priceEarningsColNum <- as.numeric(which(names(stockPriceMovements) %in% c("Price.Earnings")))
dividendYieldColNum <- as.numeric(which(names(stockPriceMovements) %in% c("Dividend.Yield.4.1.18")))
earningsShareColNum <- as.numeric(which(names(stockPriceMovements) %in% c("Earnings.Share.4.1.18")))
fiftyTwoWeekLowColNum <- as.numeric(which(names(stockPriceMovements) %in% c("X52WeekLow.4.1.18")))
fiftyTwoWeekHighColNum <- as.numeric(which(names(stockPriceMovements) %in% c("X52WeekHigh.4.1.18")))
priceSalesColNum <- as.numeric(which(names(stockPriceMovements) %in% c("Price.Sales.4.1.18")))
marketCapColNum <- as.numeric(which(names(stockPriceMovements) %in% c("Market.Cap.4.1.18")))
EBITDA_ColNum <- as.numeric(which(names(stockPriceMovements) %in% c("EBITDA.4.1.18")))
priceBookColNum <- as.numeric(which(names(stockPriceMovements) %in% c("Price.Book.4.1.18")))
targetVarColNum = as.numeric(which(colnames(stockPriceMovements)=='OutperformedSNP'))

scaled_features <- c(priceEarningsColNum, dividendYieldColNum, earningsShareColNum, fiftyTwoWeekLowColNum, 
                     fiftyTwoWeekHighColNum, priceSalesColNum, marketCapColNum, EBITDA_ColNum, priceBookColNum)
# Feature Scaling (does not apply to binary values)
training_set[scaled_features] = scale(training_set[scaled_features])
test_set[scaled_features] = scale(training_set[scaled_features])
# Question for Prof. Shuckla
#     Both lines above give me the following error
#     Error in colMeans(x, na.rm = TRUE) : 'x' must be numeric

y_predict = knn(training_set[,-targetVarColNum], test_set[,-targetVarColNum], cl = training_set[,targetVarColNum], k=best_k)

knn_con_matrix = table(test_set[,targetVarColNum], y_predict)
knn_con_matrix
knnTrueZero = as.numeric(knn_con_matrix[1, 1])
knnFalseZero = as.numeric(knn_con_matrix[1, 2])
knnTrueOne = as.numeric(knn_con_matrix[2, 2])
knnFalseOne = as.numeric(knn_con_matrix[2, 1])
knnAccuracy = (knn_true_one + knn_true_zero)/(knn_true_one + knn_true_zero + knn_false_one + knn_false_zero)
print(paste("knn Accuracy: ", toString(knnAccuracy*100) ))
