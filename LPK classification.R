require(data.table)                                                       # Provides a "fread" function, which heavily speeds up data opening
require(stringi)                                                          # Used to transform LT letters in to Latin letters
require(RecordLinkage)                                                    # Used to find similar text strings
require(tm)                                                               # Text mining package that simplifies classification using texts from ads
require(textstem)                                                         # Removes suffixes and such from words leaving only the root, which improves classification
require(randomForest)                                                     # A machine learning algorithm that is used to classify job ads according to LPK
require(caret)                                                            # Provides a function that allows to calculate the accuracy of the classification

#### Settings of the script --------------------------------------------------
### Folders and their locations
## Folder where all scripts and data folders are located
loc <- "C:\\Users\\zymantas.valacka\\Downloads\\Skelbimuklasifikavimoalgoritmas\\"   

## Folder where different junk words that will be removed is located
junk.words.folder <- "Junk words"

## Folder where translated words that will be used for classification are located
translation.folder <- "Translation"

## Folder where information about LPK is located
LPK.folder <- "Additional data"

### Additional settings
## If no, Levenshtein’s distance is used when looking for similar occupations and already classified job ads (Yes/No)
Exact.distance <- "Yes"

## Threshold of similarity above which two words/phrases will be interpreted as being the same (0-1)
Similarity.level <- 0.95

## If there is more than the specifid number of ads for a particular occupation that occupation will be used in the analysis
number.of.ads <- 100

## The accuracy threshold for each occupation.==========
## If the accuracy of an algorithm for a particular occupation is higher than the threshold, then the algorithm is used for classification.
use.Accuracy <- 0.9

## With what probability the algorithm has to be sure that an ad belongs to an occupation to classify it as such
class.Probability <- 0.9

## Use a predifined classification data set (in Classification df folder) or create one using the provided data
Use.classification.df <- "No"

## Should a data set that will be translated be created (if no, then the already existing data set is selected)
create.translate.words <- "No"

#### Importing data ----------------------------------------------------------
setwd(loc)
df <- as.data.frame(
  fread("Job adds without duplications.csv",
        stringsAsFactors = FALSE,
        encoding = "UTF-8"),
  stringsAsFactors = FALSE)

df$ID <- 1:nrow(df)                                                     # Changing the IDs for easier data set navigation

#### Classification  ----------------------------------------------------------
### Using LPK data ------------------------------------------------------------
## Importing LPK data
setwd(paste0(loc, LPK.folder))
LPK <- readRDS("LPK.rds")

## Cleaning LPK data, by transforming the LPK names in to a simpler form
LPK <- cbind(LPK,
             "LPK.clean" = LPK$Pavadinimas)

LPK$LPK.clean <-
  tolower(stri_trans_general(as.character(LPK$LPK.clean), "latin-ascii"))

## Finding if any non-classified job ads match any LPK level 6 name
No.LPK <- which(is.na(df$LPK.6.code))                                   # Identifying not classified job ads

## Looking for exact or similar matches between the job ad and LPK level 6 name
for (i in 1:nrow(LPK)) {
  # Using Levenshtein's distance to find job ads that are similar to LPK level 6 name
  if (tolower(Exact.distance) == "no") {
    distance <- levenshteinSim(LPK$LPK.clean[i], df$Position.clean[No.LPK])
    if (length(which(distance > Similarity.level) > 0)) {
      df$LPK.6.code[No.LPK[which(distance > Similarity.level)]] <-
        as.character(LPK$Kodas[i])
      df$LPK.6.name[No.LPK[which(distance > Similarity.level)]] <-
        as.character(LPK$Pavadinimas[i])
      No.LPK <- which(is.na(df$LPK.6.code))
    }
  } else{
    # Using exact matches
    distance <- which(LPK$LPK.clean[i] == df$Position.clean)
    if (length(distance) > 0) {
      df$LPK.6.name[distance[which(is.na(df$LPK.6.code[distance]))]] <-
        as.character(LPK$Pavadinimas[i])
      df$LPK.6.code[distance[which(is.na(df$LPK.6.code[distance]))]] <-
        as.character(LPK$Kodas[i])
      No.LPK <- which(is.na(df$LPK.6.code))
    }
  }
  rm(distance)
}

rm(No.LPK, i, LPK)

#### Using already classified data to find similar job adds ------------------
### Extracting LPK 4 information from available LPK 6 information (easier to classify)
df$LPK.4.code <- substr(df$LPK.6.code, 1, 4)

### Using and LPK level 4 data set to name the identified LPK codes
## Opening the LPK level 4 data set
LPK.4.df <- readRDS(paste0(loc, LPK.folder, "\\", "LPK level 4.rds"))
LPK.4.df$Pavadinimas <- trimws(LPK.4.df$Pavadinimas)

## Naming the codes
for (i in 1:nrow(LPK.4.df)) {
  match <- which(LPK.4.df$Kodas[i] == df$LPK.4.code)
  if (length(match) > 0) {
    df$LPK.4.name[match] <- as.character(LPK.4.df$Pavadinimas[i])
  }
  rm(match)
}

rm(i)

### Identifying job ads that should be classified
No.LPK <- which(is.na(df$LPK.4.code))

### Selecting if some prepared data set will be used or it will be created from scratch
if(tolower(Use.classification.df) == "yes") {
  setwd(paste0(loc, "Classification df"))
  LPK.df <- read.csv("Classification df.csv",
                     row.names = 1,
                     stringsAsFactors = FALSE)
  LPK.df <- df[which(!is.na(df$LPK.4.code)), ]
  }else{
    LPK.df <- df[which(!is.na(df$LPK.4.code)), ]
}

### Classifying using already classified job ads
for (i in 1:length(No.LPK)) {
  ##Using exact match or a distance measure to check if two ads are the same
  if (tolower(Exact.distance) == "No") {
    distance <- levenshteinSim(df$Position.clean[No.LPK[i]], LPK.df$Position.clean)
    to.Check <- which(distance > Similarity.level)
    } else{
      to.Check <-which(df$Position.clean[No.LPK[i]] == LPK.df$Position.clean)
    }
  
  ## If several matches are found the algorithm tries to assigned the best occupation to an ad
  if (length(to.Check) > 0) {
    ## Looking if the company that posted the ad has a similar ad
    match <- df$Company.clean[No.LPK[i]] == LPK.df$Company.clean[to.Check]
    if(any(match)) {
      high.freq <- table(LPK.df$LPK.4.code[to.Check[match]])
      df$LPK.4.code[No.LPK[i]] <- names(which.max(high.freq))
      high.freq <- table(LPK.df$LPK.4.name[to.Check[match]])
      df$LPK.4.name[No.LPK[i]] <- names(which.max(high.freq))
      rm(high.freq)
      } else{
      # If the company does not have the same ad, if there are similar ads by other companies in the same sector
      if (!is.na(df$ERVK.code[No.LPK[i]])) {
        match <-
          substr(df$ERVK.code[No.LPK[i]], 1, 4) == substr(LPK.df$ERVK.code[to.Check], 1, 4)
      } else{
        match <- FALSE
      }
      if (any(match)) {
        high.freq <- table(LPK.df$LPK.4.code[to.Check[match]])
        df$LPK.4.code[No.LPK[i]] <- names(which.max(high.freq))
        high.freq <- table(LPK.df$LPK.4.name[to.Check[match]])
        df$LPK.4.name[No.LPK[i]] <- names(which.max(high.freq))
      } else{
        # If there is no match, finding how the identified job ads classify a similar ad most frequently
        high.freq <- table(LPK.df$LPK.4.code[to.Check])
        if (any(high.freq / sum(high.freq) > 0.5)) {
          df$LPK.4.code[No.LPK[i]] <- names(which.max(high.freq))
          high.freq <- table(LPK.df$LPK.4.name[to.Check])
          df$LPK.4.name[No.LPK[i]] <- names(which.max(high.freq))
        }
      }
      rm(high.freq)
    }
    rm(match)
  }
  rm(to.Check)
}

rm(i)

#### Classifying using Machine Learning --------------------------------------
### Preparing the data set by cleaning the requirements and description texts
df$Requirments.clean <- tolower(df$Requirments)
df$Description.clean <- tolower(df$Description)

## Removing word endings
# Opening the word ending data set
setwd(paste0(loc, junk.words.folder))                                     # Specifying the location
word.endings <- as.data.frame(fread("word endings.csv",
                                    sep = ";",
                                    stringsAsFactors = FALSE,
                                    encoding = "UTF-8"),
                              stringsAsFactors = FALSE)

# Removing word endings
for (i in 1:nrow(word.endings)) {
  if(grepl("\\(", word.endings[i, 1]) |
     grepl("\\-", word.endings[i, 1])){
    df$Requirments.clean <-
      gsub(paste0(word.endings[i, 1]), " ", df$Requirments.clean, fixed = TRUE)
    df$Description.clean <-
      gsub(paste0(word.endings[i, 1]), " ", df$Description.clean, fixed = TRUE)
    }else{
      df$Requirments.clean <-
        gsub(paste0("\\b", word.endings[i, 1], "\\b"), " ", df$Requirments.clean)
    df$Description.clean <-
      gsub(paste0("\\b", word.endings[i, 1], "\\b"), " ", df$Description.clean)
  }
}

rm(word.endings)

## Removing punctuations and such from descriptions and requirements
df$Requirments.clean <- gsub('[/,:;•?!*\"\\„\\‘\`\\”\\“\\% €$\\>]', " ", df$Requirments.clean)
df$Requirments.clean <- gsub('[[:punct:]]+', " ", df$Requirments.clean)
df$Requirments.clean <- gsub('[\\\\]|[^[:print:]]', " ", df$Requirments.clean, fixed = TRUE)
df$Description.clean <- gsub('[/,:;•?!*\"\\„\\‘\`\\”\\“\\% €$\\>]', " ", df$Description.clean)
df$Description.clean <- gsub('[[:punct:]]+', " ", df$Description.clean)
df$Description.clean <- gsub('[\\\\]|[^[:print:]]', " ", df$Description.clean, fixed = TRUE)

### Creating a data set that should be translated to use later
if (tolower(create.translate.words) == "yes") {
  ## Extracting unique words and cleaning them
  words <- c(unlist(strsplit(df$Position.clean, " ")),
             unlist(strsplit(df$Requirments.clean, " ")),
             unlist(strsplit(df$Description.clean, " ")))
  words <- unique(words)
  words <- unlist(strsplit(words, "\n"))
  words <- unlist(strsplit(words, "\t"))
  words <- unique(words)
  words <- gsub("[0-9]+", " ", words)
  words <- gsub(" ", "", words)
  words <- gsub("[.]", "", words)
  words <- gsub("[(|)]", "", words)
  words <- words[grepl("[[:alpha:]]", words)]
  words <- words[!grepl("@", words)]
  words <- words[!grepl("http", words)]
  words <- sub("-", "", words)
  words <- sub("[+]", "", words)
  words <- trimws(words)
  words <- words[words != ""]
  words <- words[!duplicated(words)]
  
  #Saving the results
  setwd(paste0(loc, translation.folder))
  fwrite(as.data.frame(cbind("Words" = words)),
         'For translation.csv')
  
  rm(words)
}

#### Creating data sets for each occupation that will be used in classification --------
### Opening the translated data set
setwd(paste0(loc, translation.folder))
Translated.df <- as.data.table(fread("Translated.csv",
                                     sep = ";",
                                     stringsAsFactors = FALSE,
                                     encoding = "UTF-8",
                                     quote=""
                                     ),
                               stringsAsFactors = FALSE)
### Removing lithuanian letters
Translated.df$Words <- tolower(stri_trans_general(Translated.df$Words, "latin-ascii"))
### Only keeping unique words
Translated.df <- Translated.df[!duplicated(Translated.df$Words), ]
### Replacing + with \\+ (neccessary for some scripts)
Translated.df$Words <- gsub("+", "\\+", Translated.df$Words, fixed = TRUE)
Translated.df$Words <- gsub("[*]","",Translated.df$Words)
Translated.df$Words <- gsub("\\","",Translated.df$Words, fixed = TRUE)
### Removing unneccessary punctuations
Translated.df$Translation <- gsub('[/,:;•?!\\)\\(*\"\\„\\‘\`\\”\\“\\% €$\\>]', " ", Translated.df$Translation)
Translated.df$Translation <- gsub("[[:punct:]]+", "", Translated.df$Translation)
Translated.df$Words <- gsub("[[:punct:]]+", "", Translated.df$Words)

### Removing English words that are already translated (i.e. English words in text)
Translated.df <- Translated.df[!(Translated.df$Words == Translated.df$Translation), ]
### Removing stop words
Translated.df <- Translated.df[!grepl(paste0("\\b", paste0(stopwords("en"), collapse = "\\b|\\b"), "\\b"),Translated.df$Translation), ]
Translated.df <- Translated.df[!duplicated(Translated.df$Words)]

### Stemming translated words
Translated.df$Translation <- stem_strings(Translated.df$Translation, language = "english")
Translated.df$Translation <- tolower(stri_trans_general(Translated.df$Translation, "latin-ascii"))

### Creating vector that includes information about each job ad
Translation <- paste0(df$Position.clean, " ", 
                      tolower(stri_trans_general(df$Requirments.clean, "latin-ascii")), " ", 
                      tolower(stri_trans_general(df$Description.clean, "latin-ascii")))
### Removing punctuations
Translation <- gsub("[[:punct:]]+", " ", Translation)

### Translating the words in to english
for(i in 1:nrow(Translated.df)) {
  Translation <- gsub(paste0("\\b", Translated.df$Words[i], "\\b"),
                      Translated.df$Translation[i],
                      Translation,
                      perl = TRUE)
}

rm(i)

Translation <- tolower(stri_trans_general(Translation, "latin-ascii"))

### Creating text corpus and term frequency matrix and cleaning them
Translation <- Corpus(VectorSource(Translation))
Translation.dtm <- DocumentTermMatrix(Translation)

### Extracting occupations that have at least N job ads
### These occupations will be used for classification
Occupations <- table(df$LPK.4.code)
Occupations <- names(which(Occupations > number.of.ads))

### Building a "text matrix" which represents each occupations the best and which will be later used for classification
### Where this data set will be saved
### Selecting if data sets for classification will be created or what is created will be used
if(tolower(Use.classification.df) == "no") {
  setwd("C:/Users/zymantas.valacka/Downloads/Skelbimuklasifikavimoalgoritmas/Translation/Occupational text matrixes")
  
  for(i in 1:length(Occupations)) {
    Occupations.df <- Translation.dtm[which(df$LPK.4.code == Occupations[i]), ]
    Freq.terms <- findFreqTerms(Occupations.df,
                                lowfreq = round(0.10 * nrow(Occupations.df)),
                                highfreq = Inf)
    Occupations.df <- Occupations.df[, Freq.terms]
    Occupations.df <- as.data.table(as.matrix(Occupations.df))
    row.names(Occupations.df) <-df$ID[which(df$LPK.4.code == Occupations[i])]
    
    write.csv(Occupations.df, paste0("LPK ", Occupations[i], ".csv"))
  }
  
  rm(Occupations.df, i, Freq.terms)
}
#### Classification using machine learning algorithms ----------------------------------------------------------
### Creating a data set that needs classification
to.Classify.df <- df[is.na(df$LPK.4.code), ]

### Identifying from where the "text matrixes" will be taken
setwd("C:/Users/zymantas.valacka/Downloads/Skelbimuklasifikavimoalgoritmas/Translation/Occupational text matrixes")
#setwd(paste0(loc, translation.folder, "\\Occupational text matrixes"))
file.list <- list.files()

### Creating a data set which will hold information about the probability of each job adds belonging to each occupation
Classification.df <- as.data.table(matrix(nrow = nrow(to.Classify.df),
                                         ncol = (length(file.list) + 1)),
                                  stringsAsFactors = FALSE)
colnames(Classification.df) <- c("ID",
                                 gsub("[^0-9]", "", file.list))
Classification.df$ID <- to.Classify.df$ID

### Creating a text matrix for the non-classified job ads
To.class.dtm <- Translation.dtm[as.character(df$ID[which(is.na(df$LPK.4.code))]), ]

rm(to.Classify.df)

### Creating an accuracy matrix which will show how good is the classification algorithm for each occupation
Accuracy <- as.data.table(matrix(nrow = 0, ncol = 3),
                          stringsAsFactors = FALSE)
colnames(Accuracy) <- c("LPK.code", "LPK.name", "Accuracy")

### The classification algorithm using Machine Learning
for (i in 1:length(file.list)){
  ## Opening each occupations "text matrix"
  Classified <- read.csv(file.list[i],
                         row.names = 1,
                         stringsAsFactors = FALSE)
  
  ## Extracting the occupation
  LPK <- gsub("[^0-9]", "", file.list[i])
  ## Creating and cleaning a different "text matrix" using occupations that do not belong to the selected occupation and which will be used to train the algorithm
  Other.class <-
    Translation.dtm[as.character(df$ID[which(!is.na(df$LPK.4.code) &
                                               !grepl(LPK, df$LPK.4.code))]), ]
  Other.class <- Other.class[, intersect(colnames(Classified),
                                         Other.class$dimnames$Terms)]
  Other.class <- Other.class[sample(nrow(Other.class), round(nrow(Classified) * 0.5 / 0.5)), ]
  Other.class.names <- Other.class$dimnames$Docs
  Other.class <- as.data.table(as.matrix(Other.class))
  row.names(Other.class) <- Other.class.names
  remove.columns <- setdiff(colnames(Classified), colnames(Other.class))
  
  if(length(remove.columns) > 0){
    remove.column <-grep(paste0("\\b", paste0(remove.columns, collapse = "\\b|\\b"), "\\b"), colnames(Classified))
    Classified <- Classified[, -remove.column]
  }
  
  rm(remove.columns)
  
  ## Training the classification algorithm
  # Preparing the data
  Classification <- c(rep(1, nrow(Classified)), rep(0, nrow(Other.class)))
  Classification.training.df <- rbind(Classified,
                                      Other.class)
  

  Classification.training.df$CLASSIFIED <- c(rep(1, nrow(Classified)),
                                             rep(0, nrow(Other.class)))
  row.names(Classification.training.df) <- c(row.names(Classified),
                                             row.names(Other.class))
  
  # Making the data set smaller so that computers can run this
  if(nrow(Classification.training.df)>8000){
    Classification.training.df <- Classification.training.df[sample(1:nrow(Classification.training.df),8000)]
  }
  
  Classification.training.df$CLASSIFIED <- as.factor(Classification.training.df$CLASSIFIED)
  
  rm(Other.class, Other.class.names)
  
  division <- sample(c(1, 0),
                     nrow(Classification.training.df),
                     replace = TRUE,
                     prob = c(0.7, 0.3)
  )
  
  rf.train <- as.data.frame(Classification.training.df[division == 1, ])
  rf.test <- as.data.frame(Classification.training.df[division == 0, ])
  
  # Training on the data
  RF.tune <- try(tuneRF(rf.train[, -which(colnames(rf.train) == "CLASSIFIED")],
                        rf.train$CLASSIFIED,
                        ntreeTry = nrow(rf.train),
                        stepFactor = 1.5,
                        improve = 0.05),
                 TRUE)
  
  if(RF.tune[1] == "Error in if (Improve > improve) { : missing value where TRUE/FALSE needed\n") {
    optimal.mtry <- round(sqrt(ncol(rf.train)), 0)
  }else{
    optimal.mtry <- RF.tune[as.numeric(which.min(RF.tune[, 2])), 1]
  }
  
  LPK.rf <- randomForest(CLASSIFIED ~ .,
                         data = rf.train,
                         ntree = nrow(rf.train),
                         mty = optimal.mtry)
  
  # Checking the classification algorithms accuracy
  test.predict <- predict(LPK.rf,
                          rf.test[, -ncol(rf.test)])
  
  Accuracy.new <- confusionMatrix(table(as.numeric(rf.test$CLASSIFIED),
                                        as.numeric(test.predict)))
  
  Accuracy <- rbind(Accuracy,
                    cbind(LPK, NA, NA),
                    use.names = FALSE)
  
  Accuracy$Accuracy[nrow(Accuracy)] <- round(as.numeric(Accuracy.new$overall[1]), 4)
  
  # Checking if the accuracy of the algorithm is above the specific threshold
  # If it is, then the algorithm calculated the probability of each job ad belonging to different occupations
  if (Accuracy$Accuracy[nrow(Accuracy)] > use.Accuracy){
    
    
    LPK.rf <- randomForest(CLASSIFIED ~ .,
                           data = Classification.training.df,
                           ntree = nrow(Classification.training.df),
                           mty = optimal.mtry)
    
    
    to.Class <- To.class.dtm[, intersect(colnames(Classified),
                                         To.class.dtm$dimnames$Terms)]
    to.Class.names <- to.Class$dimnames$Docs
    to.Class <- as.data.table(as.matrix(to.Class))
    row.names(to.Class) <- to.Class.names
    
    remove.columns <- setdiff(colnames(Classified), colnames(to.Class))
    
    remove.columns <- setdiff(colnames(Classified), colnames(to.Class))
    
    if(length(remove.columns) > 0) {
      remove.column <- grep(paste0("\\b", paste0(remove.columns, collapse = "\\b|\\b"), "\\b"),
                            colnames(Classified))
      Classified <- Classified[, -remove.column]
    }
    
    rm(remove.columns)
    
    prediction <- predict(LPK.rf,
                          to.Class,
                          type = "prob")
    
    Classification.df[, i + 1] <- prediction[, which(colnames(prediction) == 1)]
    
    rm(to.Class, prediction)
  }
  
  rm(Classified, LPK, Classification, Classification.training.df, division,
    rf.train, rf.test, RF.tune, LPK.rf, test.predict, Accuracy.new)
  
}

### Using the probabilities to classify each job ad that is not classified already
## As the algorithm only provides information about the job ad code, the algorithm has to assigned the name for each occupation a new
# Opening the LPK data set that will be used to name each LPK code
setwd(paste0(loc, LPK.folder))
LPK.4 <- readRDS("LPK level 4.rds")
LPK.4$Pavadinimas <- trimws(LPK.4$Pavadinimas)

## Classifying
# Identifying which rows need classification
check <- which(is.na(df$LPK.4.code))

# Classifying if the probability of an add belonging to at least one occupation is above the use.Accuracy
for (i in check) {
  match <- which(df$ID[i] == Classification.df$ID)
  if (any(Classification.df[match, -1] > class.Probability, na.rm = TRUE)) {
    df$LPK.4.code[i] <- names(which.max(Classification.df[match, -1]))
    match <- which(df$LPK.4.code[i] == LPK.4$Kodas)
    df$LPK.4.name[i] <- as.character(LPK.4$Pavadinimas[match])
  }
  rm(match)
}

#### Saving the results ------------------------------------------------------
### Identifying a location where the results should be saved
setwd(loc)

### Saving the Accuracy
## Giving names to occupational codes
for (i in 1:nrow(Accuracy)) {
  match <- which(Accuracy$LPK.code[i] == LPK.4$Kodas)
  if (length(match) > 0) {
    Accuracy$LPK.name[i] <- as.character(LPK.4$Pavadinimas[match])
  }
  rm(match)
}


## Saving the results
fwrite(Accuracy,
       "Accuracy.csv")

### Saving the data set
## Removing unneccessary columns
if (any(colnames(df) == "Description.clean")) {
  df <- df[, -which(colnames(df) == "Description.clean")]
}

if (any(colnames(df) == "Requirments.clean")) {
  df <- df[, -which(colnames(df) == "Requirments.clean")]
}

## Saving the results
fwrite(df,
       "Classified data set.csv")
