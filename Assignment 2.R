#Task 1: Importing
library(readr)
appstore_games <- read.csv("appstore_games.csv")
appstore_games <- data.frame(appstore_games)
sum(is.na(appstore_games$User.Rating.Count))


#Task 2: Data Cleaning
library(dplyr)
data<-appstore_games

clean_df <- subset(data, select = -c(URL,Name,Icon.URL))

clean_df$sub_title <- ifelse(nchar(clean_df$Subtitle)>0, 1,0)
clean_df <- subset(clean_df, select=-c(Subtitle))

clean_df$In.app.Purchases <- strsplit(clean_df$In.app.Purchases, ",")

#count the number of in-app purchases per game
clean_df$IAP_count <- sapply(clean_df$In.app.Purchases,FUN = length)

#converting everything to numeric
clean_df$In.app.Purchases <- sapply(clean_df$In.app.Purchases,as.numeric)

#convert 0 into NA value
for(c in 1:nrow(clean_df)) {
  if (length(clean_df$In.app.Purchases[[c]])==0) {
    clean_df$In.app.Purchases[c] <- NA
  }
}

#calcularing min IAP
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
clean_df$IAP_average <- sapply(clean_df$In.app.Purchases, FUN = function(x) round(mean(x), 2)) #Yasin - I changed it to two decimals

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

#Note: we refer to another column for NAs
for(y in 1:nrow(clean_df)){
  if(is.na(clean_df$languages_count[[y]])){
    clean_df$is_available_in_english[y] <- NA
  } else if("EN" %in% clean_df$Languages[[y]]){ 
    clean_df$is_available_in_english[y] <- "Yes"
  }
}

#load the extra df
load("genres_df.Rda") 

#create a table with totals for each ID 
Genres_ID <- table(Genres_df$ID)
#print to check
print(Genres_df$ID)
#match the ID from the table to the df and add the total genres to the df
for(ID in names(Genres_ID)) {
  clean_df$Genres_count[clean_df$ID==ID] <- Genres_ID[ID]
}

#we still have the Primary.Genres attribute which we're not using
#it's the first genre in the normal Genres column
#we can remove the attribute together with the other 3 in the very beginning of task 2
#should we do it?
#Yasin - I don't think it is hindering the rest of the code, so I don't think we should remove it

unique_genres <- unique(Genres_df$genre)
print(unique_genres)

#add coumns for all unique genres 
for(genre in unique_genres) {
  clean_df [ ,genre] <- 0
}

#rows: match IDs from two data frames
#columns: match the genre names in Genres_df to the column names in clean_df 
for (e in 1:nrow(Genres_df)) {
  clean_df[clean_df$ID==Genres_df$ID[e],Genres_df$genre[e]] <- 1
}

clean_df$Current.Version.Release.Date <- as.Date(clean_df$Current.Version.Release.Date, format = "%d/%m/%Y")
clean_df$Elapsed_months<- as.numeric(difftime(Sys.Date(),clean_df$Current.Version.Release.Date,units="days"))/30 #Yasin - I changed the name of the column to Elapsed_months and adjusted the code to have it return the correct number of months
clean_df$Release_month<- months(as.Date(clean_df$Original.Release.Date)) #Andrea - Consider rounding the elapsed months

#free/non-free app
clean_df$game_free <- ifelse(is.na(clean_df$In.app.Purchases),1,0)

#finding a median
median_rating <- median(clean_df$User.Rating.Count, na.rm = TRUE)

clean_df$Categorical.Rating.Count <- ifelse(clean_df$User.Rating.Count < median_rating, "Low", "High")


####    TASK 3: Missing Values formatting ####

clean_df_copy <- clean_df
#From the paper, it is possible to read that games which show NA User.Rating.Count have not reached 5 total rating 
#Replacing NA User.Rating.Count with 5

clean_df_copy$User.Rating.Count <- ifelse(
  is.na(clean_df_copy$User.Rating.Count) , 5, clean_df_copy$User.Rating.Count
)

#Replacing NA Average.User.rating with 0 
clean_df_copy$Average.User.Rating <- ifelse(
  is.na(clean_df_copy$Average.User.Rating) , 0, clean_df_copy$Average.User.Rating
)

#Handling NA Price values 
sum(is.na(clean_df_copy$Price))

#Yasin - I changed this to replace NA values to 0, instead of removing the rows completely
clean_df_copy <- clean_df_copy %>%
  mutate(Price = ifelse(is.na(Price), 0, Price))


#Replacing NA in-app-purchases with 0 
clean_df_copy$In.app.Purchases <- ifelse(
  is.na(clean_df_copy$In.app.Purchases) , 0, clean_df_copy$In.app.Purchases
)

#Handling missing languages
clean_df_copy$Languages <- gsub(0, "EN", clean_df_copy$Languages) 
#To double check
sum(clean_df_copy$Languages == 'EN')

#Handling NA size values 
sum(is.na(clean_df_copy$Size)) # No NA values, probably filtered together with price

#Converting IAP NA values measurements in 0 
#Yasin - I replaced the code with an easier solution for this:
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
#Re-running rating median calculation after data imputation
median_rating <- median(clean_df_copy$User.Rating.Count, na.rm = TRUE)

clean_df_copy$Categorical.Rating.Count <- ifelse(clean_df_copy$User.Rating.Count < median_rating, "Low", "High")
clean_df_copy <- clean_df_copy%>% 
  filter (!is.na(Categorical.Rating.Count))


#Making sure that every column is either numerical or categorical 
column_types <- sapply(clean_df_copy,class)
numeric_columns <- which(column_types =='integer')
clean_df_copy[, numeric_columns] <- lapply(clean_df_copy[, numeric_columns], as.numeric)
clean_df_copy$In.app.Purchases <- sapply(clean_df_copy$In.app.Purchases, function(x) as.numeric(x))#ask about the list to the professor
#clean_df_copy$Current.Version.Release.Date <- lapply(clean_df_copy$Current.Version.Release.Date, as.numeric)

categorical_columns <- which(column_types == 'character')
clean_df_copy[, categorical_columns] <- lapply(clean_df_copy[, categorical_columns], as.factor)

#Yasin - I adjusted the following code to make it easier to understand
column_types <- sapply(clean_df_copy, class)
table(column_types) #the datatypes are all either numeric or categorical despite for the Current.Version.Release.Date

#Checking for duplicates 
clean_df_copy <- clean_df_copy %>%
  filter(distinct(ID))
distinct(clean_df_copy$ID)
#Yasin - Somehow this code doesn't work for me, therefore I changed it to this:
clean_df_copy_unique <- unique(clean_df_copy)
#Yasin - I'll use this new unique data frame for the following steps as well


#### TASK 4 ####
#Splitting dataset into training and testing set 
install.packages('caTools')
library(caTools)
split <- sample.split(clean_df_copy_unique, SplitRatio = 0.8)

#Creating Training and test sets 
trainingSet <- subset(clean_df_copy_unique,split == TRUE)
testSet <- subset(clean_df_copy_unique,split == FALSE)


#### TASK 5 ####
#Select numeric columns
numeric_columns <- sapply(clean_df_copy_unique, is.numeric)

#Scale numeric columns to the range [0, 1]
clean_df_copy_unique[, numeric_columns] <- lapply(clean_df_copy_unique[, numeric_columns], function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
})


#### TASK 6 ####
