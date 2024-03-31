#### Task 1: Importing ####
library(readr)
appstore_games <- read.csv("appstore_games.csv")
appstore_games <- data.frame(appstore_games)


#### Task 2: Data Cleaning ####
library(dplyr)
data<-appstore_games

clean_df <- subset(data, select = -c(URL,Name,Icon.URL))

clean_df$sub_title <- ifelse(nchar(clean_df$Subtitle)>0, 1,0)
clean_df <- subset(clean_df, select=-c(Subtitle))

clean_df$In.app.Purchases <- strsplit(clean_df$In.app.Purchases, ",")

#count the number of in-app purchases per game
clean_df$IAP_count <- sapply(clean_df$In.app.Purchases,FUN = length)

#converting In.app.Purchases to numeric
clean_df$In.app.Purchases <- sapply(clean_df$In.app.Purchases,as.numeric)

#convert 0 into NA value
for(c in 1:nrow(clean_df)) {
  if (length(clean_df$In.app.Purchases[[c]])==0) {
    clean_df$In.app.Purchases[c] <- NA
  }
}

#calculating min IAP
for(d in 1:nrow(clean_df)) {
  if (clean_df$IAP_count[d]==0) {
    clean_df$IAP_min[d] <- NA
  }else{
    clean_df$IAP_min[d] <- min(clean_df$In.app.Purchases[[d]])
  }
}

#calculating max IAP
for(d in 1:nrow(clean_df)) {
  if (clean_df$IAP_count[d]==0) {
    clean_df$IAP_max[d] <- NA
  }else{
    clean_df$IAP_max[d] <- max(clean_df$In.app.Purchases[[d]])
  }
}

#calculating the sum and the mean IAP
clean_df$IAP_sum <- sapply(clean_df$In.app.Purchases,FUN =sum)
clean_df$IAP_average <- sapply(clean_df$In.app.Purchases, FUN = function(x) round(mean(x), 2))

#checking for NA and infinite values in Description
sum(is.na(clean_df$Description))
sum(is.infinite(clean_df$Desciption))
#adding a description count and removing the description column 
clean_df$Description_count <- sapply(strsplit(clean_df$Description, " "),FUN = length)
clean_df <- subset(clean_df, select=-Description)

#creating a table with all developers and the total number of games they developed
Names<-table(clean_df$Developer)
for (developer in names(Names)) {
  if(Names[developer]>3){
    clean_df$GameCount[clean_df$Developer==developer] <- "professional"
  }else{
    clean_df$GameCount[clean_df$Developer==developer] <- "newbie"
  }
}

#checking that there's nothing below 4+
table(clean_df$Age.Rating)

#replacing everything 4+ with 4+, and the rest with 9+
clean_df$age_rating<-ifelse(clean_df$Age.Rating=="4+","4+","9+")


#creating a new column for languages count
clean_df$Languages<-strsplit(clean_df$Languages, ",")

for(x in 1:nrow(clean_df)) {
  lang_length <- length(clean_df$Languages[[x]])
  if (lang_length == 0) {
    clean_df$languages_count[x] <- NA
  } else if (lang_length == 1) {
    clean_df$languages_count[x] <- "single"
  } else {
    clean_df$languages_count[x] <- "many"
  }
}

clean_df$is_available_in_english <- "No"

#note: we refer to another column for NAs
for(y in 1:nrow(clean_df)){
  if(is.na(clean_df$languages_count[[y]])){
    clean_df$is_available_in_english[y] <- NA
  } else if("EN" %in% clean_df$Languages[[y]]){ 
    clean_df$is_available_in_english[y] <- "Yes"
  }
}

#loading the extra df
load("genres_df.Rda") 

#creating a table with totals for each ID 
Genres_ID <- table(Genres_df$ID)
#printing to check
print(Genres_df$ID)
#matching the ID from the table to the df and adding the total genres to the df
for(ID in names(Genres_ID)) {
  clean_df$Genres_count[clean_df$ID==ID] <- Genres_ID[ID]
}

unique_genres <- unique(Genres_df$genre)
print(unique_genres)

#adding columns for all unique genres 
for(genre in unique_genres) {
  clean_df [ ,genre] <- 0
}

#rows: matching IDs from two data frames
#columns: matching the genre names in Genres_df to the column names in clean_df 
for (e in 1:nrow(Genres_df)) {
  clean_df[clean_df$ID==Genres_df$ID[e],Genres_df$genre[e]] <- 1
}

#calculating elapsed months since release date and describing the release month
clean_df$Current.Version.Release.Date <- as.Date(clean_df$Current.Version.Release.Date, format = "%d/%m/%Y")
clean_df$Elapsed_months<- as.numeric(difftime(Sys.Date(),clean_df$Current.Version.Release.Date,units="days"))/30 
clean_df$Release_month<- months(as.Date(clean_df$Original.Release.Date)) 

#describing whether free/non-free app
clean_df$game_free <- ifelse(is.na(clean_df$In.app.Purchases),1,0)

#finding a median for user rating count
median_rating <- median(clean_df$User.Rating.Count, na.rm = TRUE)
median_rating
clean_df$Categorical.Rating.Count <- ifelse(clean_df$User.Rating.Count < median_rating, "Low", "High")


#### Task 3: Missing Values Formatting ####

clean_df_copy <- clean_df
#From the paper, it is possible to read that games which show NA User.Rating.Count have not reached 5 total rating 
#In the paper they changed the NA values to "5", but we are changing them to "0" as this is in accordance with assignment description
clean_df_copy$User.Rating.Count <- ifelse(
  is.na(clean_df_copy$User.Rating.Count) , 0, clean_df_copy$User.Rating.Count
)

#Replacing NA Average.User.rating with 0 
clean_df_copy$Average.User.Rating <- ifelse(
  is.na(clean_df_copy$Average.User.Rating) , 0, clean_df_copy$Average.User.Rating
)

#Handling NA Price values 
sum(is.na(clean_df_copy$Price))

clean_df_copy <- clean_df_copy %>%
  mutate(Price = ifelse(is.na(Price), 0, Price))

#Replacing NA in-app-purchases with 0 
clean_df_copy$In.app.Purchases <- ifelse(
  is.na(clean_df_copy$In.app.Purchases) , 0, clean_df_copy$In.app.Purchases
)

#Handling missing languages
clean_df_copy <- clean_df_copy %>%
  filter(lengths(Languages)>0)

#To double check
sum(clean_df_copy$Languages == '0')

#Handling NA size values 
clean_df_copy <- clean_df_copy %>%
  mutate(Size = ifelse(is.na(Size), 0, Size))

#Converting IAP NA values measurements in 0 
clean_df_copy <- clean_df_copy %>%
  mutate(
    IAP_min = ifelse(is.na(IAP_min), 0, IAP_min),
    IAP_max = ifelse(is.na(IAP_max), 0, IAP_max),
    IAP_sum = ifelse(is.na(IAP_sum), 0, IAP_sum),
    IAP_average = ifelse(is.na(IAP_average), 0, IAP_average)
  )

#Dropping observations with NA categorical variables 
clean_df_copy <- clean_df_copy%>% 
  filter (!is.na(languages_count))

clean_df_copy <- clean_df_copy%>% 
  filter (!is.na(is_available_in_english))

#Checking for NA values 
sum(apply(is.na(clean_df_copy[,1:76] ), 2, sum))

#Making sure that every column is either numerical or categorical
column_types <- sapply(clean_df_copy,class)
column_types
table(column_types)

#Integers to numeric
numeric_columns <- which(column_types =='integer')
clean_df_copy[, numeric_columns] <- lapply(clean_df_copy[, numeric_columns], as.numeric)

#Dates to numeric
library(lubridate)

#Original.Release.Date to numeric
date_components <- lapply(clean_df_copy$Original.Release.Date, function(date_str) unlist(strsplit(date_str, "/")))

#Creating a function that takes each component of a date (Year, month and day), transforms them as numeric and then returns the entire date as numeric
numeric_dates <- sapply(date_components, function(components) {
  year <- as.numeric(components[1])
  month <- as.numeric(components[2])
  day <- as.numeric(components[3])
  return(year * 10000 + month * 100 + day)
})

clean_df_copy$Original.Release.Date <- numeric_dates

#Current.Version.Release.Date to numeric
clean_df_copy$Current.Version.Release.Date <- as.character(clean_df_copy$Current.Version.Release.Date)

date_components <- lapply(clean_df_copy$Current.Version.Release.Date, function(date_str) unlist(strsplit(date_str, "-")))

numeric_dates <- sapply(date_components, function(components) {
  year <- as.numeric(components[1])
  month <- as.numeric(components[2])
  day <- as.numeric(components[3])
  return(year * 10000 + month * 100 + day)
})


clean_df_copy$Current.Version.Release.Date <- numeric_dates

#Character values to categorical 
categorical_columns <- which(column_types == 'character')
categorical_columns
clean_df_copy[, categorical_columns] <- lapply(clean_df_copy[, categorical_columns], as.factor)

#Game categories to categorical
for (genres in unique_genres){
  clean_df_copy[,genres]<-as.factor(clean_df_copy[,genres])
}

#Dropping lists
clean_df_copy <- subset(clean_df_copy, select= -c(Languages,In.app.Purchases))

#Double check
column_types <- sapply(clean_df_copy, class)
table(column_types) 
str(clean_df_copy)

#Checking for duplicates 
clean_df_copy_unique <- unique(clean_df_copy)
str(clean_df_copy_unique)

#Re-calculating Dependent variable, to remove Missing Values 
clean_df_copy_unique$Categorical.Rating.Count <- ifelse(clean_df_copy_unique$User.Rating.Count < median_rating, "Low", "High")
table(clean_df_copy_unique$Categorical.Rating.Count)

#### Task 4: Data Partitioning ####
#Splitting dataset into training and testing set 
install.packages('caTools')
library(caTools)
split <- sample.split(clean_df_copy_unique, SplitRatio = 0.8)

#Creating training and est sets 
trainingSet <- subset(clean_df_copy_unique,split == TRUE)
testSet <- subset(clean_df_copy_unique,split == FALSE)


#### Task 5: Scaling Attributes ####
#Select numeric columns
numeric_columns <- sapply(clean_df_copy_unique, is.numeric)

#Scale numeric columns to the range [0, 1]
clean_df_copy_unique[, numeric_columns] <- lapply(clean_df_copy_unique[, numeric_columns], function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
})

#Repeat the same steps for data frames TrainingSet and TestSet
numeric_columns_trainingSet <- sapply(trainingSet, is.numeric)

trainingSet[, numeric_columns_trainingSet] <- lapply(trainingSet[, numeric_columns_trainingSet], function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
})

numeric_columns_testSet <- sapply(testSet, is.numeric)

testSet[, numeric_columns_testSet] <- lapply(testSet[, numeric_columns_testSet], function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
})

#### Task 6: Evaluation  ####

# PREP

# removing Games and Strategy columns 
# the values do not contribute to the outcome of the algorithms
trainingSet$Games <- NULL
trainingSet$Strategy <- NULL
testSet$Games <- NULL
testSet$Strategy <- NULL
testSet$`Sports & Activities` <- NULL
trainingSet$`Sports & Activities` <- NULL
testSet$`Comics & Cartoons` <- NULL
trainingSet$`Comics & Cartoons` <- NULL
testSet$`Animals & Nature` <- NULL
trainingSet$`Animals & Nature` <- NULL
testSet$`Places & Objects` <- NULL
trainingSet$`Places & Objects` <- NULL

# Changing the df names to match the ML code
training <- trainingSet
testing <- testSet

# Making sure that objective attribute is a factor
training$Categorical.Rating.Count <- as.factor(training$Categorical.Rating.Count)
testing$Categorical.Rating.Count <- as.factor(testing$Categorical.Rating.Count)

# Save the trainingSet dataframe
save(trainingSet, file = "training.rda")

# Save the testSet dataframe
save(testSet, file = "testing.rda")

# Installing required packages
install.packages("e1071")
install.packages("caret")
install.packages("recipes")
library(caret)
library(e1071)

load("training.Rda")
load("testing.Rda")



#-------------------------------------------------------
# START - TRAIN MODEL AND TEST

#--------------------------#
#       KNN Algorithm      #
#--------------------------#

model <- gknn(Categorical.Rating.Count ~ ., training, k = 5, method = "Manhattan")
pred<-predict(model, testing)
pred
# Confusion Matrix
# rows represent the actual categories in the testing set
# columns represent the predictions
# In a perfect prediction scenario, all values in the confusion matrix would be on the diagonal
cm <- table(testing$Categorical.Rating.Count, pred)
cm
confusionMatrix(cm)
length(testing$Categorical.Rating.Count)
length(pred)

#--------------------------#
#   Naive Bayes Algorithm  #
#--------------------------#

model <- naiveBayes(Categorical.Rating.Count ~ ., data = training)
model
pred<-predict(model, testing)
pred
# Confusion Matrix
# rows represent the actual categories in the testing set
# columns represent the predictions
# In a perfect prediction scenario, all values in the confusion matrix would be on the diagonal
cm <- table(testing$Categorical.Rating.Count, pred)
cm
confusionMatrix(cm)

#--------------------------#
#       SVM Algorithm      #
#--------------------------#

model <- svm(Categorical.Rating.Count ~ ., data = training, kernel="linear")
model <- svm(Categorical.Rating.Count ~ ., data = training, kernel="polynomial")
model <- svm(Categorical.Rating.Count ~ ., data = training, kernel="sigmoid")
model
pred<-predict(model, testing)
pred

# Confusion Matrix
# rows represent the actual categories in the testing set
# columns represent the predictions
# In a perfect prediction scenario, all values in the confusion matrix would be on the diagonal

cm <- table(testing$Categorical.Rating.Count, pred)
cm
confusionMatrix(cm)
