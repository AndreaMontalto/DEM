#Task 1: Importing
library(readr)
appstore_games <- read.csv("Downloads/appstore_games.csv")
appstore_games <- data.frame(appstore_games)

#task 2
library(dplyr)
data<-appstore_games
clean_df<-subset(data, select = -c(URL,Name))

clean_df$sub_title<-ifelse(nchar(clean_df$Subtitle)>1, 1,0)
clean_df<-subset(clean_df, select=-c(Subtitle,Icon.URL))

clean_df$In.app.Purchases<-strsplit(clean_df$In.app.Purchases, ",")

clean_df$IAP_count<-sapply(clean_df$In.app.Purchases,FUN =length)
clean_df$IAP_min<-sapply(clean_df$In.app.Purchases,FUN =min)
clean_df$IAP_max<-sapply(clean_df$In.app.Purchases,FUN =max)
clean_df$IAP_sum<-sapply(sapply(clean_df$In.app.Purchases, as.numeric),FUN =sum)
clean_df$IAP_average<-sapply(sapply(clean_df$In.app.Purchases, as.numeric),FUN =mean)

clean_df$Description<-as.character(clean_df$Description)
clean_df$Decsription_count<-sapply(strsplit(clean_df$Description, " "),FUN = length)
clean_df<-subset(clean_df, select=-Description)

Names<-table(clean_df$Developer)
for (developer in names(Names)) {
  if(Names[developer]>4){
    clean_df$GameCount[clean_df$Developer==developer]<-"professional"
  }else{
    clean_df$GameCount[clean_df$Developer==developer]<-"newbie"
  }
}

table(clean_df$Age.Rating)
clean_df$age_rating<-ifelse(clean_df$Age.Rating=="4+","4+","9+")

clean_df$Languages<-strsplit(clean_df$Languages, ",")
for(x in 1:nrow(clean_df)){
  if(length(clean_df$Languages[x])==0){
    clean_df$languages_count[x]<-NA
  }else if (nchar(clean_df$Languages[x])==2){
    clean_df$languages_count[x]<-"single"
  }else{
    clean_df$languages_count[x]<-"many"
  }
}

clean_df$is_available_in_english<-"No"
for(y in 1:nrow(clean_df)){
  if(is.na(clean_df$Languages[y])){
    clean_df$is_available_in_english[y]<-"NA"
  }else if("EN" %in% clean_df$Languages[y]){
    clean_df$is_available_in_english[y]<-"Yes"
  }
}


lengths_genres <- sapply(clean_df$Genres, length)
print(lengths_genres)
clean_df$Genres<-strsplit(clean_df$Genres, ",")
clean_df$Genres_count<-sapply(clean_df$Genres, FUN=length)


unique_genres <- unique(unlist(strsplit(Genres_df$genre, ", ")))
for (b in 1:nrow(clean_df)) {
  for(genre in unique_genres){
    if(genre %in% clean_df$Genres){
      clean_df[b,genre]<-1
    }else{
      clean_df[b,genre]<-0
    }
  }
}

clean_df$Months_since_release<-as.numeric(difftime(Sys.Date(),clean_df$Current.Version.Release.Date,units="weeks"))/4
clean_df$Release_month<- months(as.Date(clean_df$Original.Release.Date))
