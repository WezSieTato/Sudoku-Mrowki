source('mrowki.r')
source('sort.r')

mrowki.sort.build_sons <- function(ID) {
  mrowki.vertices[[ID]]$sons <<- list()
  
  board = mrowki.vertices[[ID]]$board
  neighbours <- sort.neighbours(board)
  
  if(length(neighbours) > 0){
    for(i in 1: length(neighbours)){  
            newboard <- neighbours[[i]]
            newID <- mrowki.getID(newboard)
            mrowki.vertices[[ID]]$sons[[length(mrowki.vertices[[ID]]$sons)+1]] <<- newID
    }      
  }
  return(mrowki.vertices[[ID]]$sons)
}

mrowki.stop_criterion<-function(XS, M){
  path <- XS[[length(XS)]]
  lenght_path <- length(path)
  vertex <- mrowki.vertices[[path[[lenght_path]]]]
  mrowki.solution <<- vertex$board
  return(sort.is_complete(vertex$board) && ((lenght_path - 1) <= length(mrowki.task)))
  return(sort.is_complete(vertex$board))
}

mrowki.sort <-function(ants = 1){
  sort.task <<- c(2,-6,1,-2, 1, 0)
mrowki.sort <-function(task){
  sort.task <<- task
  mrowki.task <<- sort.task
  mrowki.trail <<- function(delta){
    return (1 - (delta / length(mrowki.task)))
  }
  
  mrowki.stop_ant <<- function(ID){
    return (sort.is_complete(mrowki.vertices[[ID]]$board))
  mrowki.stop_ant <<- function(ID, YS){
    if(sort.is_complete(mrowki.vertices[[ID]]$board) || length(YS) >= length(mrowki.task))
      return(TRUE)
    
    return(FALSE)
  }
  
  mrowki.first_vertex <<-function(){
    vector <- c()
    for (i in 1 : length(sort.task)){
      vector[[i]] <- i
    }
    return(list(board = vector, sons = NULL ))
  }
  
  mrowki.build_sons <<- mrowki.sort.build_sons
  
  return (mrowki.search())
}

print('Sortujemy')
mrowki.sort()
print('Posortowane')
print(sort.task[mrowki.solution])