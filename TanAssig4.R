## R program for determining topological properties of a network.
##
## Input requirements:   the name of the file
##   dataFile: filename of data for which to determine topological properties
##
## Josh Tan
## CSCI 479
## 11/21/13

## INPUT VARIABLES
dataFile = "networkDatasets/karate.txt"

## read data
X = as.matrix(read.table(dataFile))

## find number of nodes and create initial adjacency matrix
numNodes = max(c(X[, 1], X[, 2]))
adjMatrix = matrix(0, numNodes, numNodes)

## populate adjacency matrix
for (i in 1:nrow(X)) {
    adjMatrix[X[i, 1], X[i, 2]] = 1
    adjMatrix[X[i, 2], X[i, 1]] = 1
}

nodeDeg = sort(adjMatrix %*% matrix(1, numNodes), decreasing = TRUE)
nodeProb = nodeDeg / numNodes


plot(1, 1, xlim=c(1, numNodes), ylim=c(0, 1), type="n")
points(1:numNodes, nodeProb)

invisible(readline(prompt = "Press [enter] to display on log-log scale."))

#nodeProb = log(nodeProb)
#plot(1, 1, xlim=c(0, log(numNodes)), ylim=c(0, log(1)), type="n")
plot(log(1:numNodes), log(nodeProb))
