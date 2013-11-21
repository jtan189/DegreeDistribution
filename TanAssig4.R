## R program for determining topological properties of a network.
##
## Input requirements:   the name of the file
##   dataFile: filename of data for which to determine topological properties
##
## Josh Tan
## CSCI 479
## 11/21/13

## read Lecture 17, Slides 16-19

## INPUT VARIABLES
dataFile = "networkDatasets/HcNetwork.txt"

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

nodeDeg = adjMatrix %*% matrix(1, numNodes)

uniqueDeg = sort(unique(nodeDeg))
degDist = matrix(0, length(uniqueDeg), 2)
degDist[, 1] = uniqueDeg

degDist = t(apply(degDist, 1,
    function(row) c(
        row[1],
        length(nodeDeg[which(nodeDeg == row[1])]) / numNodes
        )
    ))
        
## index = 1
## for (i in 1:max(nodeDeg)) {
##     numWithDeg = length(which(nodeDeg == i))
##     if (numWithDeg != 0) {
##         degDist[index, 2] = numWithDeg
##         index = index + 1
##     }
## }


#degDist = data.matrix(data.frame(table(nodeDeg)))
#degDist[ , 2] = degDist[ , 2] / numNodes
#nodeProb = nodeDeg / numNodes

# maybe useful
#data.frame(table(nodeDeg))



plot(1, 1, xlim=c(min(degDist[, 1]), max(degDist[, 1])), ylim=c(min(degDist[, 2]), max(degDist[, 2])), type="n")
points(degDist)

invisible(readline(prompt = "Press [enter] to display on log-log scale."))
degDist = log(degDist)
plot(1, 1, xlim=c(min(degDist[, 1]), max(degDist[, 1])), ylim=c(min(degDist[, 2]), max(degDist[, 2])), type="n")
points(degDist)

#plot(1, 1, xlim=c(1, length(uniqueDeg)), ylim=c(0, max(degDist[, 2])), type="n")
#points(degDist)

##nodeProb = log(nodeProb)
##plot(1, 1, xlim=c(0, log(numNodes)), ylim=c(0, log(1)), type="n")
#plot(log(1:numNodes), log(nodeProb))
