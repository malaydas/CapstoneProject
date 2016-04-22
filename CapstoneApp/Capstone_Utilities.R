library(stringi)
library(stringr)
library(tm)
library(data.table)
library(gtools)
library(markovchain)


#--------------------------------------------------------------------------------------
#   Use :
#
#   This function helps to copy file from one location(from) to another (to)
# 
#   Arguments :   
#                1.  from - is the directory from  , 
#                2.  to - is the directory to 
# 
#   Returns : 
# 
#   Reference:
#             http://stackoverflow.com/questions/10266963/moving-files-between-folders
#--------------------------------------------------------------------------------------


capstone.file.copy <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  file.copy(from = from,  to = to , overwrite=TRUE)
}


#--------------------------------------------------------------------------------------
# Following two are custom macro , to be used for cleaning the corpus
# First one removeNonASCII is for removing all NonASCII values out of corpus
# Second one customRemovePunctuation is for removing all the punctuation except apostrophe
# Both to be used in PreprocessCorpus function
#--------------------------------------------------------------------------------------
## http://stackoverflow.com/questions/9934856/removing-non-ascii-characters-from-data-files
removeNonASCII <- content_transformer(function(x) iconv(x, "latin1", "ASCII", sub=""))

# http://stackoverflow.com/questions/8697079/remove-all-punctuation-except-apostrophes-in-r
customRemovePunctuation <- content_transformer(function(x) {
  x <- gsub("[^[:alnum:][:space:]']", "", x)
  return(x)
})

removePunct <- function(x) { x <- gsub("[^[:alnum:][:space:]']", "", x) ; return(x)}

#--------------------------------------------------------------------------------------
#   Use :
#
#   This function receives corpus vector , and produces a document term matrix 
#   consisting of Unigrams. 
# 
#   Arguments :   
#                1.  x - is the corpus vector , 
# 
#   Returns : 
#                unidtm  =_Document term matrix consists of Unigram 
# 
#--------------------------------------------------------------------------------------
CreateUnigramDTM <- function(x)
{
  ##unigram <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1,delimiters=' \r\n\t'))}
  unidtm <- DocumentTermMatrix(doc,control=list(tokenize = 'words',global = c(1, Inf)))
  #unidtm <- TermDocumentMatrix(doc)
  return(unidtm)
}




#--------------------------------------------------------------------------------------
#   Use :
#
#   This function receives corpus vector , and produces a document term matrix 
#   consisting of Bigrams. 
# 
#   Arguments :   
#                1.  x - is the corpus vector , 
# 
#   Returns : 
#                bidtm = Document term matrix consists of Bigram 
# 
#--------------------------------------------------------------------------------------
CreateBigramDTM <- function(x)
{
  bigram <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2,delimiters=' \r\n\t'))}
  bidtm <- DocumentTermMatrix(doc,control=list(tokenize = bigram))
  return(bidtm)
}




#--------------------------------------------------------------------------------------
#  Use : 
# 
#  This function receives corpus vector , and produces a document term matrix 
#  consisting of Trigrams. 
# 
#   Arguments :   
#                1.  x - is the corpus vector , 
# 
#   Returns : 
#                tridtm = Document term matrix consists of Trigram 
# 
#--------------------------------------------------------------------------------------
CreateTrigramDTM <- function(x)
{
trigram <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3,delimiters=' \r\n\t'))}
tridtm <- DocumentTermMatrix(doc,control=list(tokenize = trigram))
}




#--------------------------------------------------------------------------------------
#  Use : 
# 
#  This function receives corpus vector , and produces a document term matrix 
#  consisting of Quadrigrams. 
# 
#   Arguments :   
#                1.  x - is the corpus vector , 
# 
#   Returns : 
#                quadridtm = Document term matrix consists of Quadrigram 
#--------------------------------------------------------------------------------------
CreateQuadrigramDTM <- function(x)
{
  quadrigram <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 4, max = 4,delimiters=' \r\n\t'))}
  quadridtm <- DocumentTermMatrix(doc,control=list(tokenize = quadrigram))
  return(quadridtm)
}


#--------------------------------------------------------------------------------------
#  Use : 
# 
#  This function is meant for generating initial statistics about the data files residing 
#  in a particular directory location. 
# 
#   Arguments :   
#                1.  InputData - is the data directory of the data files , for whom we 
#                     need statistics. 
# 
#   Returns : 
#                stat = This is the data frame consisting of all the relevant statistics 
#--------------------------------------------------------------------------------------

GatherStatistics <- function(InputData)
{
 
    setwd(InputData)
    ## Reading Data Files only under en_US folder.
    ennews=readLines("en_US.news.txt")
    enblog=readLines("en_US.blogs.txt")
    entwit=readLines("en_US.twitter.txt")
    
    
    ##### Calculating Initial Statistics
    ## Calculating the file size
    ennewsfilesize <- file.info("./en_US.news.txt")$size/(1024*1024)
    enblogfilesize <- file.info("./en_US.blogs.txt")$size/(1024*1024)
    entwitfilesize <- file.info("./en_US.twitter.txt")$size/(1024*1024)
    
    ## Calculating the length of lines
    ennewslen=length(ennews)
    enbloglen=length(enblog)
    entwitlen=length(entwit)
    
    ## Calculating word count of each of the files
    ennewsnumword = sum(stri_count_words(ennews))
    enblognumword = sum(stri_count_words(enblog))
    entwitnumword = sum(stri_count_words(entwit))
    
    ## Finding Longest and shortest line in each of the files
    ennewsstrcount<-stri_count(ennews,regex="\\S+")
    enblogstrcount<-stri_count(enblog,regex="\\S+")
    entwitstrcount<-stri_count(entwit,regex="\\S+")
    
    ennewslongline=stri_length(ennews[grep(max(ennewsstrcount),ennewsstrcount)])
    enbloglongline=stri_length(enblog[grep(max(enblogstrcount),enblogstrcount)])
    entwitlongline=stri_length(entwit[grep(max(entwitstrcount),entwitstrcount)])
    
    ## Creating a data frame to collate all the stats collected erstwhile
    
    stat= data.frame ( Filename = c("news","blog","twit"),
                       FileSizeInMB = c(ennewsfilesize , enblogfilesize , entwitfilesize),
                       LinesPerFile = c(ennewslen , enbloglen , entwitlen),
                       WordsPerFile = c(ennewsnumword , enblognumword , entwitnumword),
                       LongestLine = c(ennewslongline , enbloglongline , entwitlongline)
    )
    
    ## Cleanup the memory
    rm(ennews);rm(enblog);rm(entwit)
    return(stat)
}

#--------------------------------------------------------------------------------------
#  Use : 
# 
#  This function samples out record from the input files , and create new set of sampled 
#  file corresponding it's input file in the designated directory location. 
# 
#   Arguments :   
#               1.  InputData -  is the data directory of the data files ,which will be 
#                     sampled out to create new sampled set. 
#               2.  SampleData - is the data directory where sampled file will be placed. 
#               3.  SamplePercent - is the input argument which will be used to determine 
#                                   the amount of sampling         
# 
#   Returns : 
# 
#--------------------------------------------------------------------------------------
CreateSampleFile <- function(InputData,SampleData,SamplePercent)
{
    setwd(InputData)
    ## Reading Data Files only under en_US folder.
    ennews=readLines("en_US.news.txt",skipNul=TRUE)
    enblog=readLines("en_US.blogs.txt",skipNul=TRUE)
    entwit=readLines("en_US.twitter.txt",skipNul=TRUE)
    
    ## Sampling from each file and creating sampled
    samnews = ennews[sample(1:length(ennews),SamplePercent*length(ennews))]
    samblog = enblog[sample(1:length(enblog),SamplePercent*length(enblog))]
    samtwit = entwit[sample(1:length(entwit),SamplePercent*length(entwit))]
    
    ## Setting up the path and the connection
    SampleNewsDataPath = file.path(SampleData,paste0("en_US.news.","Sample.txt"))
    SampleBlogDataPath = file.path(SampleData,paste0("en_US.blogs.","Sample.txt"))
    SampleTwitDataPath = file.path(SampleData,paste0("en_US.twitter.","Sample.txt"))
    
    SampleNewsDataConn = file(SampleNewsDataPath, "w") 
    SampleBlogDataConn = file(SampleBlogDataPath, "w")
    SampleTwitDataConn = file(SampleTwitDataPath, "w")
    
    ## Wrtting it to files
    if(!file.exists(SampleNewsDataPath)){
      file.create(SampleNewsDataPath)
    }
    write(samnews,file=SampleNewsDataConn)
    close(SampleNewsDataConn)
    
    if(!file.exists(SampleBlogDataPath)){
      file.create(SampleBlogDataPath)
    }
    write(samblog,file=SampleBlogDataConn)
    close(SampleBlogDataConn)
    
    if(!file.exists(SampleTwitDataPath)){
      file.create(SampleTwitDataPath)
    }
    write(samtwit,file=SampleTwitDataConn)
    close(SampleTwitDataConn)
    
    ## Cleanup the memory
    rm(ennews);rm(enblog);rm(entwit);rm(samnews);rm(samblog);rm(samtwit);gc()
      
}

#--------------------------------------------------------------------------------------
#  Use : 
# 
#   This function splits data files into Training,Test and validaiton set. 
#   The proportion at which this is be done is 60% , 20% , 20% respectively. 
# 
#   Arguments :   
#                1.  SampleData - is the data directory of the data files ,which will 
#                     be divided into three set of files . 
#                2.  TrainingData - is the data directory for Training data set 
#                3.  TestData - is the data directory for the Test data set 
#                4.  ValidationData - is the data directory for the Validation set 
# 
#   Returns : 
#                 
#--------------------------------------------------------------------------------------
SplitFileDataset<- function(SampleData,TrainingData,TestData,ValidationData)
{
  for(CurTextFile in dir(SampleData, pattern="(.)*.Sample.txt"))
  {
    FilePrefix = unlist(str_split(basename(CurTextFile),"\\.Sample.txt"))[1]
    TrainingDataPath = file.path(TrainingData,paste0(FilePrefix,"_TrainingData.txt"))
    TestDataPath = file.path(TestData,paste0(FilePrefix,"_TestData.txt"))
    ValidationDataPath = file.path(ValidationData,paste0(FilePrefix,"_ValidationData.txt"))
    
    TrainingDataConn = file(TrainingDataPath, "w") 
    TestDataConn = file(TestDataPath, "w")
    ValidationDataConn = file(ValidationDataPath, "w")
    
    CurFileData=readLines(paste0(SampleData,CurTextFile),skipNul=TRUE)
    TrainingData=CurFileData[sample(1:length(CurFileData),0.6*length(CurFileData))]
    TestData=CurFileData[sample(1:length(CurFileData),0.2*length(CurFileData))]
    ValidationData=CurFileData[sample(1:length(CurFileData),0.2*length(CurFileData))]
    
    ## Wrtting it to files
    if(!file.exists(TrainingDataPath)){
      file.create(TrainingDataPath)
    }
    write(TrainingData,file=TrainingDataConn)
    close(TrainingDataConn)
    
    if(!file.exists(TestDataPath)){
      file.create(TestDataPath)
    }
    write(TestData,file=TestDataConn)
    close(TestDataConn)
    
    if(!file.exists(ValidationDataPath)){
      file.create(ValidationDataPath)
    }
    write(ValidationData,file=ValidationDataConn)
    close(ValidationDataConn)
    
    ## Cleanup the memory
    rm(CurFileData);rm(TrainingData);rm(TestData);rm(ValidationData);gc()
    
  }
  
}

#--------------------------------------------------------------------------------------
#  Use : 
# 
#  This function takes a directory location as input and subsequently create a merged 
#  corpus out of all the data files residing at that input directory location 
# 
#   Arguments :   
#                1. InputDir - is the directory location , from which all the files to be 
#                    read ,merged and create a corpus out of it. 
#   Returns :                 
#                MergedCorpus - is the consolidated corpus of the data files . 
#--------------------------------------------------------------------------------------
CreateMergedCorpus <- function(InputDir)
{
  MergedVector=c() 
  for(CurTextFile in dir(InputDir, pattern="(.)*.txt"))
  {
    CurFileData=readLines(paste0(InputDir,CurTextFile),skipNul=TRUE)
    MergedVector=c(MergedVector,CurFileData)
    
    ## Cleanup the memory
    rm(CurFileData);gc()
    
  }

  ## Creating corpus out of these files
  MergedCorpus=Corpus(VectorSource(MergedVector),readerControl = list(reader=readPlain))
  
  ## Cleanup the memory
  rm(MergedVector);gc()
  
  return(MergedCorpus)
}
#--------------------------------------------------------------------------------------
#  Use : 
# 
#  This function creates a initial transition matrix , which is basically initialised 
#   with the trigram counts available in the Trigram data.frame
# 
#   Arguments :   
#               1. Trigram - is the data.frame for Trigram words with their count
#               2. Unigram - is the data.frame for Unigram words with their count
#               3. OutputData - is the location where transition matrix to be stored
#   Returns :                 
#                TransitionMatrix - is the transition matrix . 
#--------------------------------------------------------------------------------------
InitializeMarkovChainWithTrigramCounts <- function(Trigram,Unigram,OutputData)
{
  MatrixSize = length(Unigram$word)  
  TransitionMatrix = matrix(numeric(MatrixSize^2),
                            byrow=TRUE,
                            nrow=MatrixSize,
                            dimnames=list(Unigram$word,Unigram$word))
  
  TransitionMatrixPath = file.path(OutputData,"Initial_Trigram_TransitionMatrix.RData")
  
  MatchIndex = ConstructCommonTrigramElement(Unigram,Trigram)
  
  MatchTrigram = Trigram[MatchIndex,]
  
  for (m in seq_len(length(MatchTrigram$words))) {                    
    CurWords = unlist(str_split(MatchTrigram$words[m]," "))
    
    for (n in seq(2,3)) {
      ##Locate the co-ordinates of the matrix
      RowIndex = which(grepl(paste0("^",CurWords[n-1],"$"),Unigram$word))
      ColIndex = which(grepl(paste0("^",CurWords[n],"$"),Unigram$word))
      
      #print(RowIndex);print(ColIndex)
      TransitionMatrix[RowIndex,ColIndex] =
        TransitionMatrix[RowIndex,ColIndex] + MatchTrigram$count[m]
    }
  }
  
  save(file=TransitionMatrixPath, TransitionMatrix)
  
  ## Cleanup the memory 
  rm(MatchTrigram);gc()
  
  return(TransitionMatrix)
  
}
#--------------------------------------------------------------------------------------
#  Use : 
# 
# This function finds out the instersection between Trigram and Unigram. If all words
# in a trigram is part of the unigram set , then that is considered as Match.
# 
#   Arguments :   
#               1. Trigram - is the data.frame for Trigram words with their count
#               2. Unigram - is the data.frame for Unigram words with their count
#   Returns :                 
#               MatchIndex - is the vector consisting of indexes of matched Trigram . 
#--------------------------------------------------------------------------------------
ConstructCommonTrigramElement <- function(Unigram,Trigram)
{
  MatchIndex = numeric()
  
  for (n in seq_len(length(Trigram$words))) { 
    CurWords = unlist(str_split(Trigram$words[n]," "))
    
    if (sum(CurWords %in% Unigram$word) == 3) {
      MatchIndex = append(MatchIndex,n)
    }
  }
  
  return(MatchIndex)    
}

#--------------------------------------------------------------------------------------
#   Use:  
#       This function will adjust Transition Matrix by discounting method. Essentially
#       this keeps provision for the unseen words which may be required for prediction
#       It implement Kneser-Ney discouting/smoothing after calculating the conditional
#       probability for each items
#
#   Arguments:
#               1. TransitionMatrix - is the initialised transition matrix
#   Returns :
#               TransitionMatrix - is the TransitionMatrix with conditional probability
#                                 Kneser-Ney adjustment
#--------------------------------------------------------------------------------------
ConstructMarkovChainWithDiscountingKN <- function(TransitionMatrix)
{
  ZeroCount <- vector('numeric',nrow(TransitionMatrix))
  
  for (n in seq_len(length(ZeroCount))) {
    
    # Finding how many of zeros per row
    ZeroColIndex <- which(TransitionMatrix[n,] == 0)
    ZeroCount[n] <- length(ZeroColIndex)
    
    if (ZeroCount[n] != ncol(TransitionMatrix)) {
      
      ## Assign initial conditional probability
      TransitionMatrix[n,] <- TransitionMatrix[n,] / sum(TransitionMatrix[n,])
      
      ## Adjusting with minimun probability each non zero instance and 
      ## assigning it to the zero instance
      if (ZeroCount[n] > 0) {
        NonZeroColIndex <- which(TransitionMatrix[n,] > 0)
        
        MinProbability = (1 - length(NonZeroColIndex) / ncol(TransitionMatrix))/ZeroCount[n]^2
        
        NonZeroColIndex <- which(TransitionMatrix[n,] > MinProbability)
        
        ProbabilityAdjustment <- (MinProbability*ZeroCount[n])/length(NonZeroColIndex)
        
        TransitionMatrix[n,NonZeroColIndex] <- TransitionMatrix[n,NonZeroColIndex] - ProbabilityAdjustment
        
        TransitionMatrix[n,ZeroColIndex] <- MinProbability
      }    
    }
    else {
      TransitionMatrix[n,] <- 1.0/ncol(TransitionMatrix)
    }    
  }
  
  return(TransitionMatrix)
}

#--------------------------------------------------------------------------------------
# Use:
#     This function preprocesses the input corpus and returns processed corpus. It removes
#     white space,numbers, non-ascii character,punctuation,profane words and also lowar case
#     everything.
#
# Arguments:
#           1. CorpusIn - is the input corpus  
#           2. blackList - is the list of profane words
#
# Returns :
#           CoupusOut - is the output preprocessed corpus
#--------------------------------------------------------------------------------------

PreprocessCorpus <- function(CoupusIn,blackList)
{ 
  ## Removing White Space
  doc <- tm_map(CoupusIn,stripWhitespace)
  gc()
  ## Removing uppercase
  doc <- tm_map(doc,content_transformer(tolower))
  gc()
  ## Removing stopwords
  ##doc <- tm_map(doc, removeWords, stopwords("english")) 
  ##gc()
  ## Removing common word endings (e.g., “ing”, “es”, “s”)
  ##doc <- tm_map(doc, stemDocument)
  ##gc()
  ## Removing Numbers
  doc <- tm_map(doc,removeNumbers) 
  gc()
  ## Removing NonASCII
  doc <- tm_map(doc,removeNonASCII) 
  gc()
  ## Removing Punctuation
  doc <- tm_map(doc,customRemovePunctuation)  
  gc()
  ## Removing Black listed word
  CoupusOut <- tm_map(doc,removeWords,blackList) 
  gc()
 
  return(CoupusOut)  
}

#--------------------------------------------------------------------------------------
# Use:
#     This function is used to predict next word, based on the input phrase/sentence/words
#     using Markovchain Matrix.
# Arguments:
#           1. InputPhrase - is the input word/phrase/sentence for which we want find 
#                           the next word
#           2. MarkovMatrix - is the markov chain matrix 
#
# Returns :
#           WordPredicted - is the list objects consisting of the presumably the exact word 
#                           and conditional probability of the list of probable words
#--------------------------------------------------------------------------------------
PredictNextWordUsingMarkov<- function(InputPhrase,MarkovMatrix)
{ 
  NumberWords <- length(InputPhrase)
  CurState <- InputPhrase[1]
  Dictionary <- states(MarkovMatrix)
  WordPredicted <- list()
  
  if (!CurState %in% Dictionary || is.null(CurState)) 
  {
    RandomIndex <- floor(length(Dictionary) * runif(1)) + 1
    CurState <- Dictionary[RandomIndex]
  }
  
  
  for (n in seq(2,NumberWords)) {
    NextState <- InputPhrase[n]
    if (!NextState %in% Dictionary) {
      CurCondlProbability <-conditionalDistribution(MarkovMatrix, CurState)
      
      NextState <- names(which.max(CurCondlProbability))
      
      if (length(NextState) > 1) {
        RandomIndex <- floor(length(NextState) * runif(1)) + 1
        NextState <- NextState[RandomIndex]
      }
    }
    CurState <- NextState
    
  }
  
  WordPredicted$ExactWordPredicted<-names(which.max(conditionalDistribution(MarkovMatrix, CurState)))
##  WordPredicted$ExactWordPredicted<-head(sort(conditionalDistribution(MarkovMatrix, CurState),decreasing=TRUE),1)
  WordPredicted$PossibleWordConditionalProbability<-sort(conditionalDistribution(MarkovMatrix, CurState),decreasing=TRUE)
  
  return(WordPredicted)
}

#--------------------------------------------------------------------------------------
# Use:
#     This function is implementation of stupid-back off algorithm. It implements
#     back-off from quadrigram to bigram. In case of match this function returns the next
#     word.
# Arguments:
#           1. InputPhrase - is the input word/phrase/sentence for which we want find 
#                           the next word
#           2. Quadrigram - is the data.frame for Quadrigram words with their count
#           3. Trigram - is the data.frame for Trigram words with their count
#           4. Bigram - is the data.frame for Bigram words with their count
#
# Returns :
#       NextPredictedWord - is the list objects consisting of the word prediction and
#                           the source of the word predicted (Quadrigram/Trigram/Bigram)
#--------------------------------------------------------------------------------------
PredictNextWordUsingStupidBackoff<- function(InputPhrase,Quadrigram,Trigram,Bigram)
{
  CheckLength=length(InputPhrase)
  NextPredictedWord =list()
  
  ## Find the exact phrase to be searched for match
  if(CheckLength >= 3){
    
    FromWordQuadri = CheckLength-2
    FromWordTri = CheckLength-1  
    FromWordBi = CheckLength	
    ToWord = CheckLength 
    
    SearchPhraseQuadri = paste0(as.vector(InputPhrase[FromWordQuadri:ToWord]),collapse=" ")
    SearchPhraseTri = paste0(as.vector(InputPhrase[FromWordTri:ToWord]),collapse=" ")
    SearchPhraseBi = paste0(as.vector(InputPhrase[FromWordBi:ToWord]),collapse=" ")
    
    #print(1)
    
  }else { 
    
    FromWordTri = CheckLength-1
    FromWordBi = CheckLength
    ToWord = CheckLength
    
    SearchPhraseTri = paste0(as.vector(InputPhrase[FromWordTri:ToWord]),collapse=" ")
    SearchPhraseBi = paste0(as.vector(InputPhrase[FromWordBi:ToWord]),collapse=" ")
    
    #print(2)
  }  		

  
  ## Find the next Word using Stupid Back-off till Bigram
  
  if(CheckLength >= 3)
  {
    ## Match Quadrigram,Trigram and Bigram while backing off	
    
    if(length(grep(paste0("^",SearchPhraseQuadri),Quadrigram$words)) >= 1) {
      
      MatchIndex <- grep(paste0("^",SearchPhraseQuadri),Quadrigram$words)
      temp<-head(Quadrigram$words[MatchIndex],1)
      NextPredictedWord$ExactWordPredicted<- unlist(str_split(temp," "))[4]
      NextPredictedWord$MatchModel<- "Quadrigram" 
      #print(MatchIndex)
     
    }else{
      
          if(length(grep(paste0("^",SearchPhraseTri),Trigram$words)) >= 1)
          {
            MatchIndex <- grep(paste0("^",SearchPhraseTri),Trigram$words)
            temp<-head(Trigram$words[MatchIndex],1)
            NextPredictedWord$ExactWordPredicted<- unlist(str_split(temp," "))[3]
            NextPredictedWord$MatchModel<- "Trigram"
            #print(4)
            
          }else{    
            MatchIndex <- grep(paste0("^",SearchPhraseBi),Bigram$words)
            temp<-head(Bigram$words[MatchIndex],1)
            NextPredictedWord$ExactWordPredicted<- unlist(str_split(temp," "))[2]
            NextPredictedWord$MatchModel<- "Bigram"
            #print(5)
          }
    }
  }else{ 
    
    if(CheckLength ==2 ){
      
          ## Match Trigram and Bigram while backing off
          
          if(length(grep(paste0("^",SearchPhraseTri),Trigram$words)) >= 1)
          {
                MatchIndex <- grep(paste0("^",SearchPhraseTri),Trigram$words)
                temp<-head(Trigram$words[MatchIndex],1)
                NextPredictedWord$ExactWordPredicted<- unlist(str_split(temp," "))[3]
                NextPredictedWord$MatchModel<- "Trigram"
                # print(5)
          }else{
            
                MatchIndex <- grep(paste0("^",SearchPhraseBi),Bigram$words)               
                temp<-head(Bigram$words[MatchIndex],1)
                NextPredictedWord$ExactWordPredicted<- unlist(str_split(temp," "))[2]
                NextPredictedWord$MatchModel<- "Bigram"
               #print(MatchIndex)
          }
    }else{
            ## Match only Bigram
            
            MatchIndex <- grep(paste0("^",SearchPhraseBi),Bigram$words)
            temp<-head(Bigram$words[MatchIndex],1)
            NextPredictedWord$ExactWordPredicted<- unlist(str_split(temp," "))[2]
            NextPredictedWord$MatchModel<- "Bigram"
            #print(7)
    }
  }
  
  #NextPredictedWord$PossibleWordConditionalProbability= c()

  return(NextPredictedWord)

}

#--------------------------------------------------------------------------------------
# Use:
#     This function acts as wrapper for two of the function PredictNextWordUsingMarkov
#     and PredictNextWordUsingStupidBackoff. This essentially will use two of the
#     function mentioned above ,to predict the next word. Mainly to be used from
#     the App created for word prediction.
#
# Arguments:
#           1. InputPhrase - is the input word/phrase/sentence for which we want find 
#                           the next word
#           2. MarkovMatrix - is the markov chain matrix 
#           3. Quadrigram - is the data.frame for Quadrigram words with their count
#           4. Trigram - is the data.frame for Trigram words with their count
#           5. Bigram - is the data.frame for Bigram words with their count
# Returns :
#           temp = list object consists of Two set of prediction , one presumably the
#           exact word and another is set of possible words
#--------------------------------------------------------------------------------------
NextWord <- function(InputPhrase,MarkovMatrix,Quadrigram,Trigram,Bigram)
{
 
  ToMatch = removePunct(InputPhrase)
  ToMatch = unlist(str_split(as.character(tolower(str_trim(ToMatch)))," "))
  
  
  temp = list()
  
  # Check with Stupid Back off
  WordPredictedS = PredictNextWordUsingStupidBackoff(ToMatch,Quadrigram,Trigram,Bigram)
  
  # Check with Markov Matrix
  WordPredictedM = PredictNextWordUsingMarkov(ToMatch,MarkovMatrix)
  PossibleNextWordsCP =t(as.matrix(WordPredictedM$PossibleWordConditionalProbability))          
  
  if(is.null(WordPredictedS$ExactWordPredicted)){
    ##WordPredictedS$ExactWordPredicted = 0
    temp$ExactWordPredicted = WordPredictedM$ExactWordPredicted
    temp$PossibleWordPredicted = as.vector(colnames(PossibleNextWordsCP))[2:5]
  }else{
        if(WordPredictedS$ExactWordPredicted != WordPredictedM$ExactWordPredicted ){
          temp$ExactWordPredicted = WordPredictedS$ExactWordPredicted
          temp$PossibleWordPredicted = as.vector(colnames(PossibleNextWordsCP))[1:5]
        }else{
          temp$ExactWordPredicted = WordPredictedS$ExactWordPredicted
          temp$PossibleWordPredicted = as.vector(colnames(PossibleNextWordsCP))[2:5]
        }
  }
  
  

    
  return(temp)
}
