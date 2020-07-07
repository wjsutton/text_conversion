
#setwd('C:/Users/sutto/Documents/R/Text Converter/Mad Libs')
#source("mad_lib.R")
setwd("~/Github/text_conversion")

# Mad Lib generator
# identifies nouns, verbs and adjectives and replaces them with humourous alternatives

library(openNLP)
library(NLP)
library(syn)
library(dplyr)

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


myfun <- function(x){
  x <- syn(x,1)
  return(x)
}



synonyms <- function(x) {
  
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
    NN <- data.frame(id=seq.int(length(unique(NN$Word))),Word=unique(NN$Word),stringsAsFactors = F)
    new_NN <- data.frame(Word=NN$Word,new_word=unlist(lapply(NN$Word, myfun)))
    
    NN <- NN %>% left_join(new_NN, by = 'Word')
  }
  
  # Nouns plural
  NNP <- tokenizedAndTagged %>% filter(Tags=='NNP')
  if(nrow(NNP)>0){
    NNP <- data.frame(id=seq.int(length(unique(NNP$Word))),Word=unique(NNP$Word),stringsAsFactors = F)
    new_NNP <- data.frame(Word=NNP$Word,new_word=unlist(lapply(NNP$Word, myfun)))
    
    NNP <- NNP %>% left_join(new_NNP, by = 'Word')
  }
  
  # Verbs
  VB <- tokenizedAndTagged %>% filter(Tags=='VB')
  if(nrow(VB)>0){
    VB <- data.frame(id=seq.int(length(unique(VB$Word))),Word=unique(VB$Word),stringsAsFactors = F)
    new_VB <- data.frame(Word=VB$Word,new_word=unlist(lapply(VB$Word, myfun)))
    
    VB <- VB %>% left_join(new_VB, by = 'Word')
  }
  
  # Adjectives
  JJ <- tokenizedAndTagged %>% filter(Tags=='JJ')
  if(nrow(JJ)>0){
    JJ <- data.frame(id=seq.int(length(unique(JJ$Word))),Word=unique(JJ$Word),stringsAsFactors = F)
    new_JJ <- data.frame(Word=JJ$Word,new_word=unlist(lapply(JJ$Word, myfun)))
    
    JJ <- JJ %>% left_join(new_JJ, by = 'Word')
  }
  
  
  if(nrow(NN)>0){
    y <- gsub2(NN[,2],NN[,3],x)
  }
  
  if(nrow(NNP)>0){
    y <- gsub2(NNP[,2],NNP[,3],x)
  }
  
  if(nrow(VB)>0){
    y <- gsub2(VB[,2],VB[,3],y)
  }
  
  if(nrow(JJ)>0){
    y <- gsub2(JJ[,2],JJ[,3],y)
  }
  
  return(y)
}
