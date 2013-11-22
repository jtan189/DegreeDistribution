## Plot degree distribution for an undirected network.
##
## Input requirements:
##   dataFile: filename of data for which to plot degree distribution
##
## Josh Tan
## CSCI 479
## 11/21/13

## input variables
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

## calculate degree of each node
nodeDeg = adjMatrix %*% matrix(1, numNodes)

## calculate number of nodes having each degree
degDist = t(sapply(0:max(nodeDeg), function(x) c(x, length(which(nodeDeg == x)))))

## plot degree distribution
plot(NULL, NULL, xlim = c(min(degDist[, 1]), max(degDist[, 1])),
     ylim = c(min(degDist[, 2]), max(degDist[, 2])),
     xlab = "Degree: k", ylab = "f(k)", main = "Degree Distribution", type = "n")
points(degDist)

invisible(readline(prompt = "Press [enter] to display on log-log scale."))

## calculate P(f(k)) and take log2 of both k and P(f(k))
degDist[, 2] = degDist[, 2] / numNodes
degDist = log2(degDist[which(degDist[, 2] > 0), ])

## fit degree distribution probabilities according to a linear model
fit = lm(degDist[, 2]~degDist[, 1])

## plot degree distribution probabilities on log-log scale
plot(NULL, NULL, xlim = c(min(degDist[, 1]), max(degDist[, 1])),
     ylim = c(min(degDist[, 2]), max(degDist[, 2])),
     xlab = bquote(paste("Degree: ", log[2], k)),
     ylab = bquote(paste("Probability: ", log[2], f(k))),
     main = "Degree Distribution (log-log scale)", type = "n")
points(degDist)
abline(fit)
