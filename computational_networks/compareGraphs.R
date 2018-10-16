#!/usr/local/bin//Rscript

args <- commandArgs()

library(dplyr, warn.conflicts = FALSE, quietly = TRUE)

graph1txt <- "fac_1.txt"
graph2txt <- "fac_1_predicted.txt"
directed = 0
setwd("/Users/cora/git_repos/NetworkLikelihood/computational_networks")

compareGraphs <- function(graph1txt, graph2txt, directed){
  # Compare 2 graphs using precision, recall and F-score.
  # Args:
  #   graph1 = true graph txt file name as string
  #   graph2 = predicted graph txt file name as string
  #     format of graph1.txt and graph2.txt: tab-limited file, with one line per edge.
  #     Each is tab delimited with two columns, one for each of the vertices of the edge
  #     see http://pages.discovery.wisc.edu/~sroy/teaching/network_biology/fall2018/homeworks/hw1/factors_net.txt
  #     for an example of the input graphs.
  #   directed = binary indicating if graph is directed or undirected
  #
  # Returns:
  #   precision, recall, F-score
  #
  # Dependencies: dplyr
  
  library(dplyr)
  graph1 <- read.table(graph1txt, stringsAsFactors = FALSE)
  graph2 <- read.table(graph2txt, stringsAsFactors = FALSE)
  
  predictedEdges <- nrow(graph2)
  trueEdges <- nrow(graph1)
  if (directed == 1){
    correctEdges <- trueEdges - nrow(dplyr::setdiff(graph1, graph2))
  } else {
    correctEdges <- trueEdges - nrow(dplyr::setdiff(graph1, graph2))
    diff <- nrow(dplyr::setdiff(graph1, graph2))
    diffSet <- dplyr::setdiff(graph1, graph2) #edges missing from graph2
    #check if missing's opposite is in graph2 list
    diffSet$tmp <- diffSet$V1
    diffSet$V1 <- diffSet$V2
    diffSet$V2 <- diffSet$tmp
    switchCorrect <- predictedEdges - nrow(dplyr::setdiff(graph2, diffSet[,1:2]))
    correctEdges <- correctEdges + switchCorrect
  }

  precision <- correctEdges/predictedEdges 
  
  recall <- correctEdges/trueEdges

  fscore <- (2*precision*recall)/(precision + recall)

  return(paste("Precision: ", precision, " Recall: ", recall, " F-score: ", fscore))
}

compareGraphs(args[6], args[7], args[8])

#put /u/medinfo/bmi826/2018-hw//hw1/username