## Setting environment variable for consistent random variable generation
set.seed(1);options(warn=-1);options(mc.cores=1)

## Library Inclusion 
suppressPackageStartupMessages(library(tm));suppressPackageStartupMessages(library(RWeka));
suppressPackageStartupMessages(library(stringi));suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(tidyr));suppressPackageStartupMessages(library(R.utils))
suppressPackageStartupMessages(library(plyr));suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(markovchain));suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(data.table));suppressPackageStartupMessages(library(xtable))
suppressPackageStartupMessages(library(gtools))

## Setting up the processing directory 
ProgramDir="/Users/malaydas/Documents/Data Science/Capstone Project/ProgramingAssignment/Capstone/";setwd(ProgramDir)
AppDir="/Users/malaydas/Documents/Data Science/Capstone Project/ProgramingAssignment/Capstone/CapstoneApp/"
InputData="/Users/malaydas/Documents/Data Science/Capstone Project/data/InputData/en_US/"
OutputData="/Users/malaydas/Documents/Data Science/Capstone Project/data/OutputData/"
SampleData="/Users/malaydas/Documents/Data Science/Capstone Project/data/SampleData/"
TrainingData="/Users/malaydas/Documents/Data Science/Capstone Project/data/TrainingData/"
TestData="/Users/malaydas/Documents/Data Science/Capstone Project/data/TestData/"
ValidationData="/Users/malaydas/Documents/Data Science/Capstone Project/data/ValidationData/"

## Utility program inclusion
source("./Capstone_Utilities.R")
source("./EvaluateModel.R")

setwd(InputData)
## Sourcing blackListed/profane word
blackList <- readLines("./en_profanity.txt",encoding='UTF-8',skipNul=TRUE)

## Statistics about the files
stat=GatherStatistics(InputData)
#print(xtable(stat, display = c("d","s","f","f","f","f"),type="html"))

#------------------------------------------------------------------------------------------------------
#### Start of Sampling and Preprocessing 

## Assigning the sampling percent,which is 1.5% of the entire dataset

SamplePercent=0.04

## Sampling individual file
CreateSampleFile(InputData,SampleData,SamplePercent)

## Split the dataset into training,test and validation set
SplitFileDataset(SampleData,TrainingData,TestData,ValidationData)

## Create Merged Corpus of the Training data set
doc=CreateMergedCorpus(TrainingData)

## Preprocessing the corpus
doc <- PreprocessCorpus(doc,blackList)

#### End of Sampling and Preprocessing 
#------------------------------------------------------------------------------------------------------


## Creating Unigram document term matrix
unidtm=CreateUnigramDTM(doc)

## Organising unigrams by their frequency 
#Frequnidtm = sort(rowSums(as.matrix(unidtm)), decreasing=TRUE)
Frequnidtm <- sort(slam::col_sums(unidtm, na.rm = T), decreasing=TRUE)
Unigram <- data.frame(word=names(Frequnidtm), count=Frequnidtm) 
rownames(Unigram)=NULL
## Selecting only those Unigrams whose occurence is >=10
Unigram=subset(Unigram,count>19)
save(file=file.path(OutputData,"Unigram.RData"), Unigram)

#------------------------------------------------------------------------------------------------------


## Creating Bigram document term matrix
## Clean up the memory
rm(unidtm);rm(Frequnidtm);rm(Unigram);gc()
bidtm =CreateBigramDTM(doc)

## Organising the bigrams by their frequency 
Freqbidtm <- sort(slam::col_sums(bidtm, na.rm = T), decreasing=TRUE)
Bigram <- data.frame(words=names(Freqbidtm), count=Freqbidtm) 
rownames(Bigram)=NULL
save(file=file.path(OutputData,"Bigram.RData"), Bigram)

#------------------------------------------------------------------------------------------------------


## Clean up the memory
rm(bidtm);rm(Freqbidtm);rm(Bigram);gc()

## Creating trigram document term matrix
tridtm = CreateTrigramDTM(doc)

## Organising trigrams by their frequency 
Freqtridtm <- sort(slam::col_sums(tridtm, na.rm = T), decreasing=TRUE) 
Trigram <- data.frame(words=names(Freqtridtm),count=Freqtridtm) 
rownames(Trigram)=NULL
## Selecting only those Trigrams whose occurence is >=10
Trigram=subset(Trigram,count>1)
save(file=file.path(OutputData,"Trigram.RData"), Trigram)
#------------------------------------------------------------------------------------------------------

## Clean Up the memory
rm(tridtm);rm(Freqtridtm);rm(Trigram);gc()

## Creating quadrigram document term matrix
quadridtm <- CreateQuadrigramDTM(doc)

## Organising quadrigrams by their frequency 
Freqquadridtm <- sort(slam::col_sums(quadridtm, na.rm = T), decreasing=TRUE)
Quadrigram <- data.frame(words=names(Freqquadridtm), count=Freqquadridtm) 
rownames(Quadrigram)=NULL
save(file=file.path(OutputData,"Quadrigram.RData"), Quadrigram)
#------------------------------------------------------------------------------------------------------
## Clean Up the memory
rm(quadridtm);rm(Freqquadridtm);rm(Quadrigram);gc()

load(file.path(OutputData,"Unigram.RData"))
load(file.path(OutputData,"Bigram.RData"))
load(file.path(OutputData,"Trigram.RData"))
load(file.path(OutputData,"Quadrigram.RData"))


## Creating Transition Matrix for Trigram
TransitionMatrix<-InitializeMarkovChainWithTrigramCounts(Trigram,Unigram,OutputData)
##ConstructMarkovChainWithConditionalProbability(TrainsitionMatrix,Unigram,OutputData)
TransitionMatrix<-ConstructMarkovChainWithDiscountingKN(TransitionMatrix)

## Creating the markov chain 
MarkovMatrix =   new("markovchain",  transitionMatrix=TransitionMatrix)
#save(file=file.path(paste0(OutputData,"MarkovObjects.RData")), MarkovMatrix,TransitionMatrix)
save(file=file.path(paste0(OutputData,"MarkovObjects.RData")), MarkovMatrix)

rm(TransitionMatrix);gc()
## Load the Markov objects
load(file.path(OutputData,"MarkovObjects.RData"))

## Evaluate the Test Data set and determine the accuracy
TestFileEvaluation=EvaluateModel(TestData,MarkovMatrix,blackList,Quadrigram,Trigram,Bigram)


## Copy the Markov chain objects to a shiny application directory
capstone.file.copy(from = file.path(OutputData,"MarkovObjects.RData"),
                   to = file.path(AppDir,"MarkovObjects.RData"))

capstone.file.copy(from = file.path(OutputData,"Quadrigram.RData"),
                   to = file.path(AppDir,"Quadrigram.RData"))

capstone.file.copy(from = file.path(OutputData,"Trigram.RData"),
                   to = file.path(AppDir,"Trigram.RData"))

capstone.file.copy(from = file.path(OutputData,"Bigram.RData"),
                   to = file.path(AppDir,"Bigram.RData"))





