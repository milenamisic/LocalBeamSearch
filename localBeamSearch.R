library(igraph)

generateTestGraph <- function(){
  graph <- make_graph("Frucht");
  return(graph)
}

initializeState <- function(graph){
  numberOfNodes <- sample(1:gsize(graph), 1)
  return(sample(1:gsize(graph), numberOfNodes))
}

initializeStates <- function(graph, k){
  states <- list();
  for(i in 1:k){
    states[[i]] <- initializeState(graph)  
  }
  return(states)
}

sgenerateSuccessors <- function(graphSize, states, i){
  tmp <- states[[i]];
  
  result <- list();
  resultCount <- 1;
  count <- 1;
  
  # generate states when 1 element is removed
  
  if(length(tmp) > 1)
  
    while(count <= length(tmp)){
      result[[resultCount]] <- tmp[-c(count)];
      resultCount <- resultCount + 1;
      count <- count + 1;
    }
  
  count <- 1;
  
  # generate states when 1 element is added
  
  if(length(tmp) < graphSize){
  
    elementsTmp <- 1:graphSize;
    elementsTmp <- elementsTmp[!(elementsTmp %in% tmp)];
    
    while(count <= length(elementsTmp)){
      result[[resultCount]] <- c(tmp, elementsTmp[count]);
      resultCount <- resultCount + 1;
      count <- count + 1;
    }
  
  }
  
  # generate states when 1 element is changed
  
  if(length(tmp) < graphSize){
    
    count <- 1;
    count2 <- 1;
    
    while(count <= length(elementsTmp)){
      while(count2 <= length(tmp)){
        result[[resultCount]] <- tmp;
        result[[resultCount]][count2] <- elementsTmp[count];
        count2 <- count2 + 1;
        resultCount <- resultCount + 1;
      }
      count <- count + 1;
      count2 <- 1;
    }
    
  }
  
  return(result)
}

generateSuccessors <- function(graphSize, states){
  result <- list();
  count <- 1;
  for(i in 1:length(states)){
    result <- append(result, sgenerateSuccessors(graphSize, states, i));
  }
  return(result)
}

evaluate <- function(successor, graph){
  cover <- 1;
  edgesMatrix <- get.edges(graph, E(graph))
  
  # for each edge check if there is at least one node in successor:
  # if there is, it is vertex cover of the graph

  for(i in 1:dim(edgesMatrix)[1]){
    
    if(edgesMatrix[i] %in% successor){
      tmp1 = 1;
    }else{
      tmp1 = 0;
    }
    
    if(edgesMatrix[,2][i] %in% successor){
      tmp2 = 1;
    }else{
      tmp2 = 0;
    }
    
    if(tmp1+tmp2 == 0)
      cover <- 0;
  }

  # calculates goodness of potential cover to minimize the number of nodes: number of nodes in graph - number of nodes in successor
  # to ensure it is a cover: cover * number of nodes in graph
  # cover has value 0 or 1 (false or true)
  
  return(gsize(graph) - length(successor) + cover * gsize(graph))
}

selectBestSuccessors <- function(successors, k, graph){
  bestSuccessors <- list();
  
  # evaluates each successor state

  for(i in 1:length(successors)){
    bestSuccessors[[i]] <- c(evaluate(successors[[i]], graph), successors[i]);
  }

  # sorts successor states based on evaluations
  
  bestSuccessors = bestSuccessors[order(sapply(bestSuccessors, "[[", 1), na.last = TRUE, decreasing = TRUE)];
  
  if(length(bestSuccessors) > k)
    bestSuccessors <- bestSuccessors[1:k];
 
  return(sapply(bestSuccessors, "[", 2));
}

# local beam search for finding vertex cover of a graph

# params:
# graph
# k - number of states to be taken into account simultaneously
# stepLimit - max number of steps while searching
# sameStateLimit - number of iterations where the same state is picked as the best successor

localBeamSearch <- function(graph, k, stepLimit, sameStateLimit){
  states <- initializeStates(graph, k);
  previousState <- NULL;
  previousStateCount <- 1;
  step <- 0;

  while(step < stepLimit && previousStateCount < sameStateLimit){
  
    step <- step + 1;
    successors <- generateSuccessors(gsize(graph), states);
    states <- selectBestSuccessors(successors, k, graph);
  
    if(is.null(previousState) == FALSE && length(previousState) == length(states[[1]]) && setequal(previousState, states[[1]])){
      previousStateCount <- previousStateCount + 1;
    }else{
      previousStateCount <- 1;
      previousState <- states[[1]];
    }
  
    print(step);
    print(previousState);
  }
  return(states[[1]]);
}
