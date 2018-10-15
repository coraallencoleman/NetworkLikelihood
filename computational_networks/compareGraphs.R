#!/usr/local/bin//Rscript

args <- commandArgs()

library(dplyr, warn.conflicts = FALSE, quietly = TRUE)

#setwd("/Users/cora/git_repos/NetworkLikelihood/computational_networks")
#graph1txt <- "factors_net.txt"
#graph2txt <- "factors_net_predicted.txt" #added one dir edge, lost one dir edge

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
  
  # graph1$V1 <- as.numeric(graph1$V1)
  # graph1$V2 <- as.numeric(graph1$V2)
  # 
  # graph2$V1 <- as.numeric(graph2$V1)
  # graph2$V2 <- as.numeric(graph2$V2)
  # 
  # graph1 <- graph1[order(graph1[,1], graph1[,2]), ]
  # graph2 <- graph2[order(graph2[,1], graph2[,2]), ]
  
  predictedEdges <- nrow(graph2)
  trueEdges <- nrow(graph1)
  correctEdges <- trueEdges - nrow(dplyr::setdiff(graph2, graph1))

  precision <- correctEdges/predictedEdges 
  
  recall <- correctEdges/trueEdges

  fscore <- (2*precision*recall)/(precision + recall)

  return(paste("Precision: ", precision, " Recall: ", recall, " F-score: ", fscore))
}
#args <- c("factors_net.txt","factors_net_predicted.txt")
compareGraphs(args[6], args[7], args[8])