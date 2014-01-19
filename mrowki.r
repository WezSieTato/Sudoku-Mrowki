source('mht.r')

mrowki.antNumber <- 1
mrowki.pheromonDegradation <- 0.81
mrowki.startPheromon <- 0.5
mrowki.task <- NULL
mrowki.vertices <- NULL
mrowki.solution <- NULL

mrowki.op_init<-function(UG)
{
  
  s = list(1)
  return(s)
}

mrowki.model_init<-function(UG)
{
  firstVertex = list(board = mrowki.task, sons = NULL )
  M <- list(pheromons = list())
  mrowki.vertices <<- list(firstVertex)
  mrowki.solution <<- NULL
  print(mrowki.task)
  return(M)
}

mrowki.stop_criterion<-function(XS, M)
{
  stop('Brak implementacji funkcji stopu')
}


mrowki.op_select<-function(XS,M, UG)
{
  if(length(XS) == 1)
    return(list())
  if ((length(XS) - 1) %% mrowki.antNumber != 0 )
    return(list())
  return(pop(mrowki.antNumber))
}

mrowki.model_update<-function(XS,M)
{
  if(length(XS) == 0){
    return(M)
  }
  
  #######
  for(i in 1 : length(XS)){
    path <- XS[[i]]
    trail <- length(path)
    percent <- trail / 81
    value <- percent
    for(j in 1 : (trail - 1)){
      string <- paste(path[[j]],"->",path[[j+1]])
      if( is.null(M$pheromons[[string]])){
        M$pheromons[[string]] <- mrowki.startPheromon
      }
      M$pheromons[[string]] <- M$pheromons[[string]] + value
    }
  }
  
  for(k in 1 : length(M$pheromons)){
    M$pheromons[[k]] <- M$pheromons[[k]] * mrowki.pheromonDegradation
  }
  
  return(M)
}

mrowki.search<-function(){
  search(mrowki.model_init, mrowki.model_update, mrowki.op_init, mrowki.op_select, mrowki.op_generate, mrowki.stop_criterion, UG)
  
  return(mrowki.solution)
}

#################################
##Dzialania na rzecz
##dodania kolonii
##do algorytmu sortowania
#################################

mrowki.build_sons <- function(ID){
  stop('Brak implementacji')
}

mrowki.is_complete <-function(board){
  stop('Brak implementacji')
}

mrowki.there_is_move <- function(sons) {
  if(length(sons) == 0)
    return (FALSE)
  return (TRUE)
}

mrowki.rand_move <- function(Elements,P) {
  return ((sample(x=Elements,size=1,replace=FALSE,prob=P))[[1]])
}


mrowki.get_sons <- function(ID) {
  if(is.null(mrowki.vertices[[ID]]$sons)) {
    return(mrowki.build_sons(ID))
  }
  
  return(mrowki.vertices[[ID]]$sons)
}

mrowki.add_son <- function(B) {
  newVertex = list(board = B, sons = NULL)
  
  mrowki.vertices[[length(mrowki.vertices)+1]] <<- newVertex
  
  return(length(mrowki.vertices))
}

mrowki.update_state <- function(YS,e) {
  YS[length(YS)+1] <- e
  return(YS)
}

mrowki.get_pheromons <- function(ID, sons, pheromons) {
  prob = list()
  for(i in 1:length(sons)) {
    if(is.null(pheromons[[paste(ID,"->",sons[[i]])]])) {
      prob[[i]] = mrowki.startPheromon
    } else {
      prob[[i]] = pheromons[[paste(ID,"->",sons[[i]])]]
    }
    
    
  }
  ### ten blok umozliwia podlaczenie atrakcyjnosci
  #  max = which.max(prob)
  #  for(i in 1:length(prob)) {
  #    prob[[1]] <- prob[[1]] / max
  #  }
  #  attr <- mrowki.sudoku.set_attractivity(mrowki.vertices[[ID]]$board)
  #  for(i in 1:length(sons)) {
  #    prob[[i]] <- prob[[i]] + mrowki.sudoku.get_attractivity(mrowki.vertices[[ID]]$board,mrowki.vertices[[sons[[i]]]]$board,attr)
  #  }
  ### /ten blok umozliwia podlaczenie atrakcyjnosci
  return(prob)
}

mrowki.op_generate <- function(XS,M,UG, WA=4) {
  YS <- mrowki.op_init()
  
  ID <- YS[[1]]
  
  X <- mrowki.get_sons(ID)
  
  while(mrowki.there_is_move(X)) {
    P <- mrowki.get_pheromons(ID,X,M$pheromons)
    
    ID <- mrowki.rand_move(X,P)
    
    YS <- mrowki.update_state(YS,ID)
    
    X <- mrowki.get_sons(ID)
  }
  
  return(YS)
}

mrowki.stop_criterion<-function(XS, M){
  
  path <- XS[[length(XS)]]
  lastId <- path[[length(path)]]
  vertex <- mrowki.vertices[[lastId]]
  
  if(mrowki.is_complete(vertex$board)){
    mrowki.solution <<- vertex$board
    return(TRUE)
  } else{
    return(FALSE)
  }
  
}

mrowki.check_boards<- function(f_board,s_board) {
  return(all(f_board == s_board))
}
