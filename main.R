getMidPoints <- function(vector) {
  ret <- sort(unique(vector))
  return(0.5 * (ret[-length(ret)] + ret[-1]))
}

getSetosa <- function(data) {
  return(subset(data, Species == "setosa"))
}

getVersicolor <- function(data) {
  return(subset(data, Species == "versicolor"))
}

gini <- function(data, splitter) {
  N_all <- nrow(data)
  N_true <- nrow(splitter(data))
  N_false <- N_all - N_true
  P <- c(N_true / N_all, N_false / N_all)
  return(sum(P * (1-P)))
}

getLeftNodes <- function(data, label, threshold) {
  return(data[data[[label]] < threshold, ])
}

getRightNodes <- function(data, label, threshold) {
  return(data[data[[label]] > threshold, ])
}

splitNodeForBestGini <- function(nodes, inputLabel, midPoints) {
  minimumGini <- 1.0
  for(midPoint in midPoints) {
    leftNodes <- getLeftNodes(nodes, inputLabel, midPoint)
    rightNodes <- getRightNodes(nodes, inputLabel, midPoint)
    gini <- gini(leftNodes, getVersicolor) + gini(rightNodes, getVersicolor)
    if (gini < minimumGini) {
      minimumGini <- gini
      bestPointwiseResult <- list(
        gini = gini,
        midPoint = midPoint,
        leftNodes = leftNodes,
        rightNodes = rightNodes,
        inputLabel = inputLabel
      )
    }
  }
  return(bestPointwiseResult)
}

split <- function(nodes, inputLabels) {
  bestInputLabelWiseGini <- 1.0
  bestResult <- NULL
  for(inputLabel in inputLabels) {
    midPoints <- getMidPoints(nodes[[inputLabel]])
    if (length(midPoints) < 2) {
      next
    }
    bestLabelwiseResult <- splitNodeForBestGini(nodes, inputLabel, midPoints)
    if (bestLabelwiseResult[["gini"]] < bestInputLabelWiseGini) {
      bestInputLabelWiseGini <- bestLabelwiseResult[["gini"]]
      bestResult <- bestLabelwiseResult
    }
  }
  if (is.null(bestResult)) {
    return(list(
      left = NULL,
      right = NULL,
      nodes = nodes,
      isClassTrue = nrow(getVersicolor(nodes)) / nrow(nodes) > 0.5
    ))
  } else {
    return(list(
      left = split(bestResult[["leftNodes"]], inputLabels),
      right = split(bestResult[["rightNodes"]], inputLabels),
      threshold = bestResult[["midPoint"]],
      inputLabel = bestResult[["inputLabel"]],
      nodes = nodes
    ))
  }
}

traverseTree <- function(tree, thresholds) {
  if (!is.null(tree[["inputLabel"]])) {
    newIndex = length(thresholds$inputLabel) + 1
    thresholds$inputLabel[newIndex] = tree[["inputLabel"]]
    thresholds$threshold[newIndex] = tree[["threshold"]]
    thresholds <- traverseTree(tree[["left"]], thresholds)
    thresholds <- traverseTree(tree[["right"]], thresholds)
  }
  return(thresholds)
}

classifyByTree <- function(tree, sample) {
  inputLabel <- tree[["inputLabel"]]
  threshold <- tree[["threshold"]]

  if (is.null(inputLabel)) {
    return(tree[["isClassTrue"]])
  } else {
    isLeft <- sample[[inputLabel]] < threshold
    if (isLeft == TRUE) {
      return(classifyByTree(tree[["left"]], sample))
    } else {
      return(classifyByTree(tree[["right"]], sample))
    }
  }
}

main <- function() {
  inputLabels <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  tree = split(iris, inputLabels)
  inferred <- apply(iris, 1, function(row) classifyByTree(tree, row))
  actual <- iris[["Species"]] == "versicolor"
  print(data.frame(inferred, actual))

  # return(traverseTree(tree, list(inputLabel = c(), threshold = c())))
}
