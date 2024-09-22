#' Add together two numbers
#' 
#' @param x A number.
#' @param y A number.
#' @returns A numeric vector.
#' @examples
#' add(1, 1)
#' add(10, 1)
dijkstra <-
function(graph, init_node){
    expectedNames <- c("v1", "v2", "w")
    stopifnot(identical(names(graph), expectedNames))
    stopifnot(sapply(expectedNames,function(name) is.vector(graph[[name]])))
    stopifnot(is.numeric(init_node))

    priorityQueue <- getPriorityQueue(graph,init_node)
    savedData <- priorityQueue

    while(length(priorityQueue) > 0){
        u <- getMinDistVertice(priorityQueue)
        savedData[[u]] <- priorityQueue[[u]]
        priorityQueue[[u]] <- NULL

        neighbors <- getNeighbours(graph, priorityQueue, u)
        
        for(neighbor in neighbors){
            neighborData <- priorityQueue[[neighbor]]
            alt <- savedData[[u]]$dist + getWeightFromName(u,neighbor,graph)    
            if(alt < neighborData$dist){
                priorityQueue[[neighbor]]$dist <- alt
                priorityQueue[[neighbor]]$prev <- u
            }
        }
    }

    outputVector <- unname(sapply(savedData, function(x) x$dist))
    return(outputVector)
}


getPriorityQueue <- function(dataFrame,initNode){
    vertices <- unique(c(dataFrame$v1, dataFrame$v2))

    # Throw error if initNode not in provided data
    stopifnot(initNode %in% vertices)

    priorityQueue <- lapply(vertices, function(x) createProperties())

    # Have to apply the names explicitly in case of scrambled vertice numbers 
    names(priorityQueue) <- vertices
    
    priorityQueue[[initNode]]$dist <- 0
    return(priorityQueue)
}

createProperties <- function() {
  list(dist = Inf, prev = NA)
}

getMinDistVertice <-function(priorityQueue){
    minVertice <- list()
    verticeName <- c()

    for(name in names(priorityQueue)){
        evaluatedVertice <- priorityQueue[[name]]

        if(length(minVertice)==0){
            minVertice = evaluatedVertice
            verticeName <- name
        }

        if(minVertice$dist > evaluatedVertice$dist){
            minVertice <- evaluatedVertice
            verticeName <- name
        }
    }
    return(verticeName)
}

getNeighbours <-function(graph, priorityQueue, currentNode){
    allNodesConnectedToCurrentNode <- graph[graph$v1 == currentNode,]$v2
    allNodesInPriorityQueue <- as.numeric(names(priorityQueue))
    # take the intersection of the two vectors to get the nodes still in the queue and is connected
    # return as a string since we will filter on that later
    return(as.character(intersect(allNodesConnectedToCurrentNode, allNodesInPriorityQueue)))
}

getWeightFromName <-function(current,neighbor,graph){
    sort1 <- graph[graph$v1 == current,,]
    sort2 <- sort1[sort1$v2 == neighbor,]
    return(sort2$w)
}