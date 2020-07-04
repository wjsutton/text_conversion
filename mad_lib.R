
#setwd('C:/Users/sutto/Documents/R/Text Converter/Mad Libs')
#source("mad_lib.R")
setwd("~/Github/text_conversion")

# Mad Lib generator
# identifies nouns, verbs and adjectives and replaces them with humourous alternatives

library(openNLP)
library(NLP)
library(dplyr)

# load in libraries of libs
NN_libs <- read.csv("NN_libs.txt",header = FALSE)
JJ_libs <- read.csv("JJ_libs.txt",header = FALSE)
VB_libs <- read.csv("VB_libs.txt",header = FALSE)

# randomising libs
NN_libs <- data.frame(id=seq.int(nrow(NN_libs)),lib=NN_libs[sample(1:nrow(NN_libs)),])
#NNS_libs <- data.frame(id=NN_libs$id,lib=paste0(NN_libs$lib,'s'))
JJ_libs <- data.frame(id=seq.int(nrow(JJ_libs)),lib=JJ_libs[sample(1:nrow(JJ_libs)),])
VB_libs <- data.frame(id=seq.int(nrow(VB_libs)),lib=VB_libs[sample(1:nrow(VB_libs)),])

# loading required function
gsub2 <- function(pattern, replacement, x, ...) {
  for(i in 1:length(pattern))
    x <- gsub(pattern[i], replacement[i], x, ...)
  x
}

# Main Function
# Just alters Nouns, Verbs and Adjectives
# passed in string, e.g.
# x <- "Gulf Applied Technologies Inc said it sold its subsidiaries engaged in pipeline and terminal operations for 12.2 mln dlrs. The company said the sale is subject to certain post closing adjustments, which it did not explain. Reuter."

mad_lib <- function(x) {
    
  x <- as.String(x)
  
  # Before POS tagging, we need to do Sentence annotation followed by word annotation
  wordAnnotation <- annotate(x, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))
  
  # POS tag the words & extract the "words" from the output
  POSAnnotation <- annotate(x, Maxent_POS_Tag_Annotator(), wordAnnotation)
  POSwords <- subset(POSAnnotation,type == "word")
  
  # Extract the tags from the words
  tags <- sapply(POSwords$features, '[[', "POS")
  
  # Create a data frame with words and tags
  tokenizedAndTagged <- data.frame(Word = x[POSwords], Tags = tags)
  
  dictionary <- data.frame(id=seq.int(length(unique(tokenizedAndTagged$Word)))
                           ,Word=unique(tokenizedAndTagged$Word))
  
  # Proper nouns
  #NNP <- ''
  # names http://deron.meranda.us/data/census-derived-all-first.txt
  
  # Nouns
  NN <- tokenizedAndTagged %>% filter(Tags=='NN')
  if(nrow(NN)>0){
    NN <- data.frame(id=seq.int(length(unique(NN$Word))),Word=unique(NN$Word)
                     ,stringsAsFactors = F)
    NN <- NN %>% left_join(NN_libs, by = 'id')
  }
  
  # Nouns plural
  #NNS <- tokenizedAndTagged %>% filter(Tags=='NNS')
  #NNS <- data.frame(id=seq.int(length(unique(NNS$Word))),Word=unique(NNS$Word))
  #NNS <- NNS %>% left_join(NNS_libs)
  
  # Verbs
  VB <- tokenizedAndTagged %>% filter(Tags=='VB')
  if(nrow(VB)>0){
    VB <- data.frame(id=seq.int(length(unique(VB$Word))),Word=unique(VB$Word)
                     ,stringsAsFactors = F)
    VB <- VB %>% left_join(VB_libs, by = 'id')
  }
  
  # Adjectives
  JJ <- tokenizedAndTagged %>% filter(Tags=='JJ')
  if(nrow(JJ)>0){
    JJ <- data.frame(id=seq.int(length(unique(JJ$Word))),Word=unique(JJ$Word)
                     ,stringsAsFactors = F)
    JJ <- JJ %>% left_join(JJ_libs, by = 'id')
  }
  
  
  if(nrow(NN)>0){
    y <- gsub2(NN[,2],NN[,3],x)
  }
  
  #y <- gsub2(NNS[,2],NNS[,3],y)
  
  if(nrow(VB)>0){
    y <- gsub2(VB[,2],VB[,3],y)
  }
  
  if(nrow(JJ)>0){
    y <- gsub2(JJ[,2],JJ[,3],y)
  }
  
  return(y)
}
