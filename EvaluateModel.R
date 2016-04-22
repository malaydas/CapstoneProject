
ProgramDir="/Users/malaydas/Documents/Data Science/Capstone Project/ProgramingAssignment/Capstone/";setwd(ProgramDir)
source("./Capstone_Utilities.R")


EvaluateModel <- function(TestData,MarkovMatrix,blackList,Quadrigram,Trigram,Bigram,Unigram)
{
  TestFileEvaluation = list()
  TestFileEvaluation[["TotalTrigramCount"]] = 0
  TestFileEvaluation[["MatchTrigramCount"]] = 0
  TestFileEvaluation[["NoClosePredictions"]] = 0
  TestFileEvaluation[["NoExactPredictions"]] = 0
  TestFileEvaluation[["NoIncorrectPredictions"]] = 0
  TestFileEvaluation[["IncorrectPredictions"]] = c()
  
  ## Create Merged corpus for the Test Data
  docTest<-CreateMergedCorpus(TestData)
  
  ## Preprocess the Test Data
  docTest<- PreprocessCorpus(docTest,blackList)
  
  ## Creating trigram document term matrix for Test Data
  tridtmTest = CreateTrigramDTM(docTest)
  
  ## Organising trigrams by their frequency 
  FreqtridtmTest <- sort(slam::col_sums(tridtmTest, na.rm = T), decreasing=TRUE) 
  TrigramTest <- data.frame(words=names(FreqtridtmTest),count=FreqtridtmTest) 
  rownames(TrigramTest)=NULL
  # Just like training we excluding trigrams whose count=1
  TrigramTest<-subset(TrigramTest,count>1)
  
  ## Finding all the states in the Markov Chain object
  UnigramTest <- data.frame( word = states(MarkovMatrix),count=c(1:length(states(MarkovMatrix))) )
  
  MatchIndexTest<-ConstructCommonTrigramElement(UnigramTest,TrigramTest)
  
  MatchTrigramTest = Trigram[MatchIndexTest,]
  rownames(MatchTrigramTest)=NULL
  
  TestFileEvaluation[["TotalTrigramCount"]] = length(Trigram$words)
  TestFileEvaluation[["MatchTrigramCount"]] = length(MatchTrigramTest$words)
  
  
  for (n in seq_len(length(MatchTrigramTest$words))) 
  {
    InputPhrase =    unlist(str_split(as.character(MatchTrigramTest$words[n])," "))
    WordPredicted=list()
    WordPredicted = PredictNextWordUsingStupidBackoff(InputPhrase[1:2],Quadrigram,Trigram,Bigram)
    
     
    
    ## Even on word match is considerd as match
    if(InputPhrase[3] == WordPredicted$ExactWordPredicted){
     
        TestFileEvaluation[["NoExactPredictions"]]  <- TestFileEvaluation[["NoExactPredictions"]]  + 1 
        #print(WordPredicted$ExactWordPredicted)
          
    }else{
      
          if(is.null(WordPredicted$ExactWordPredicted) || InputPhrase[3] != WordPredicted$ExactWordPredicted){
                
                WordPredicted = PredictNextWordUsingMarkov(InputPhrase[1:2],MarkovMatrix)
        
                ## Considering top 5 possible next word
                PossibleNextWordsCP = t(as.matrix(WordPredicted$PossibleWordConditionalProbability))          
                PossibleNextWords = as.vector(colnames(PossibleNextWordsCP))[1:5]
                #print("inside markov")
                
                if(InputPhrase[3] == WordPredicted$ExactWordPredicted){
                  TestFileEvaluation[["NoExactPredictions"]]  <- TestFileEvaluation[["NoExactPredictions"]]  + 1 
                
                }else
                {
                    if (InputPhrase[3] %in% PossibleNextWords) {
                      TestFileEvaluation[["NoClosePredictions"]]  <- TestFileEvaluation[["NoClosePredictions"]]  + 1
                      #print(InputPhrase[3])
                      
                    }
                    else{
                      TestFileEvaluation[["NoIncorrectPredictions"]] <- TestFileEvaluation[["NoIncorrectPredictions"]] + 1
                      ##TestFileEvaluation[["IncorrectPredictions"]]<-append(TestFileEvaluation[["IncorrectPredictions"]],MatchTrigramTest$words[n])
                      ##TestFileEvaluation[["IncorrectPredictions"]]<-c(TestFileEvaluation[["IncorrectPredictions"]],MatchTrigramTest$words[n])
                      ##print(TestFileEvaluation[["incorrectPredictions"]])
                    }
                }
                    
          }
    }
  }
  return(TestFileEvaluation)
}

