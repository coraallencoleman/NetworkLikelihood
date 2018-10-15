#!/usr/local/bin//Rscript
#note:
#   if getting "Permisson denied" error
#   run this in command line: chmod +x compareGraphs.R

# args <- commandArgs()
# cat(args, sep = "\n")
# graph1 <- args[1]
# graph2 <- args[2]

library(dplyr)

setwd("/Users/cora/git_repos/NetworkLikelihood/computational_networks")
graph1txt <- "factors_net.txt"
graph2txt <- "factors_net_predicted.txt" #added one dir edge, lost one dir edge

compareGraphs(graph1txt, graph2txt)
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
  # Dependencies:
  graph1 <- read.table(graph1txt, stringsAsFactors = F)
  graph2 <- read.table(graph2txt, stringsAsFactors = F)
  
  #compare each line
  predictedEdges <- nrow(graph2)
  trueEdges <- nrow(graph1)
  correctEdges <- trueEdges - nrow(setdiff(graph2, graph1))

  precision <- correctEdges/predictedEdges # correct edges/ # predicted edges

  recall <- correctEdges/trueEdges # correct edges/ # true edges

  #F-score
  fscore <- (2*precision*recall)/(precision + recall)

  return(paste("Precision: ", precision, " Recall: ", recall, " F-score: ", fscore))
}
#compareGraphs(args)