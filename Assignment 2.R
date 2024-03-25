#Task 1: Importing
library(readr)
appstore_games <- read.csv("Downloads/appstore_games.csv")
appstore_games <- data.frame(appstore_games)

#task 2
library(dplyr)
data<-appstore_games

clean_df <- subset(data, select = -c(URL,Name,Icon.URL))

clean_df$sub_title <- ifelse(nchar(clean_df$Subtitle)>0, 1,0)
clean_df <- subset(clean_df, select=-c(Subtitle))

clean_df$In.app.Purchases <- strsplit(clean_df$In.app.Purchases, ",")

#count the number of in-app purchases per game
clean_df$IAP_count <- sapply(clean_df$In.app.Purchases,FUN = length)

#converting everythign to numeric
clean_df$In.app.Purchases <- sapply(clean_df$In.app.Purchases,as.numeric)

#convert 0 into NA value
for(c in 1:nrow(clean_df)) {
  if (length(clean_df$In.app.Purchases[[c]])==0) {
    clean_df$In.app.Purchases[c] <- NA
  }
}

# calcularing min IAP
for(d in 1:nrow(clean_df)) {
  if (clean_df$IAP_count[d]==0) {
    clean_df$IAP_min[d] <- NA
  }else{
    clean_df$IAP_min[d] <- min(clean_df$In.app.Purchases[[d]])
  }
}

# calculating max IAP
for(d in 1:nrow(clean_df)) {
  if (clean_df$IAP_count[d]==0) {
    clean_df$IAP_max[d] <- NA
  }else{
    clean_df$IAP_max[d] <- max(clean_df$In.app.Purchases[[d]])
  }
}

# calculating the sum and the mean IAP
clean_df$IAP_sum <- sapply(clean_df$In.app.Purchases,FUN =sum)
clean_df$IAP_average <- sapply(clean_df$In.app.Purchases,FUN =mean)

# Adding a description count and removing the description column 
clean_df$Description_count <- sapply(strsplit(clean_df$Description, " "),FUN = length)
clean_df <- subset(clean_df, select=-Description)

# creating a table with all developers and the totla number of games they developed
Names<-table(clean_df$Developer)
for (developer in names(Names)) {
  if(Names[developer]>3){
    clean_df$GameCount[clean_df$Developer==developer] <- "professional"
  }else{
    clean_df$GameCount[clean_df$Developer==developer] <- "newbie"
  }
}

# checking that there's nothing below 4+
table(clean_df$Age.Rating)

#replacing everything 4+ with 4+, and the rest with 9+
clean_df$age_rating<-ifelse(clean_df$Age.Rating=="4+","4+","9+")


clean_df$Languages<-strsplit(clean_df$Languages, ",")
clean_df$languages_count <- rep(NA, nrow(clean_df))

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

# there's a problem with whitespaces here; remove them (preferably)
# Note: we refer to another column for NAs
for(y in 1:nrow(clean_df)){
  
  if(is.na(clean_df$languages_count[[y]])){
    clean_df$is_available_in_english[y] <- "NA"
  } else if(" EN" %in% clean_df$Languages[[y]] | "EN" %in% clean_df$Languages[[y]]){
    clean_df$is_available_in_english[y] <- "Yes"
  }
}

# load the extra df
load("/Users/arinamalcenko/Downloads/genres_df (1).Rda")

# create a table with totlas for each ID 
Genres_ID <- table(Genres_df$ID)
# print to check
print(Genres_df$ID)
# match the ID from the table to the df and add the total genres to the df
for(ID in names(Genres_ID)) {
  clean_df$Genres_count[clean_df$ID==ID] <- Genres_ID[ID]
}

# we still have the Primary.Genres attribute which we're not using
# it's the first genre in the normal Genres column
# we can remove the attribute together with the other 3 in the very beginning of task 2
# should we do it?

unique_genres <- unique(Genres_df$genre)
print(unique_genres)

# add coumns for all unique genres 
for(genre in unique_genres) {
  clean_df [ ,genre] <- 0
}

# rows: match IDs from two data frames
# columns: match the genre names in Genres_df to the column names in clean_df 
for (e in 1:nrow(Genres_df)) {
  clean_df[clean_df$ID==Genres_df$ID[e],Genres_df$genre[e]] <- 1
}

clean_df$Months_since_release<-as.numeric(difftime(Sys.Date(),clean_df$Current.Version.Release.Date,units="weeks"))/4.35
clean_df$Release_month<- months(as.Date(clean_df$Original.Release.Date))

# free/non-free app
clean_df$game_free <- ifelse(is.na(clean_df$In.app.Purchases),1,0)

# finding a median
median_rating <- median(clean_df$User.Rating.Count, na.rm = TRUE)

clean_df$Categorical.Rating.Count <- ifelse(clean_df$User.Rating.Count < median_rating, "Low", "High")
