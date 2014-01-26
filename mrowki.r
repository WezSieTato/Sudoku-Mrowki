source('mht.r')

mrowki.antNumber <- 1
mrowki.pheromonDegradation <- 0.81
mrowki.startPheromon <- 0.5
mrowki.task <- NULL
mrowki.vertices <- NULL
mrowki.solution <- NULL

mrowki.fathers <- NULL
mrowki.deep <- 1

mrowki.op_init<-function(UG)
{
  
  s = list(1)
  return(s)
  return(list(1))
}

mrowki.first_vertex <- function(){
  stop('Brak implementacji')
}

mrowki.model_init<-function(UG)
{
  firstVertex <- mrowki.first_vertex() 
  M <- list(pheromons = list())
  mrowki.vertices <<- list(firstVertex)
  mrowki.solution <<- NULL
  mrowki.fathers <<- list()
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

mrowki.trail <- function(delta){
  stop('Brak implementacji funkcji stopu')
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
    value <- mrowki.trail(trail)
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
    if(M$pheromons[[k]] < 0.05)
      M$pheromons[[k]] <- 0.05
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
  return (!(length(sons) == 0))
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
  YS[[length(YS)+1]] <- e
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
  #  attr <- mrowki.sudoku.set_attractivity( sudoku.board(mrowki.task, mrowki.vertices[[ID]]$board) )
  #  attr <- 20* attr
  #  thisBoard <- sudoku.board(mrowki.task, mrowki.vertices[[ID]]$board)
  #  for(i in 1:length(sons)) {
  #    sonBoard <- sudoku.board(mrowki.task, mrowki.vertices[[sons[[i]]]]$board)
  #    prob[[i]] <- prob[[i]] + mrowki.sudoku.get_attractivity( thisBoard, sonBoard, attr)
  #  }
  #  
  #  # wsp <- table(mrowki.vertices[[ID]]$board)
    
  ### /ten blok umozliwia podlaczenie atrakcyjnosci
  return(prob)
}

mrowki.stop_ant <- function(ID){
  stop('Brak implementacji')
}

mrowki.add_to_fathers <- function(ID,deep) {
  
  if(length(mrowki.fathers[[deep]]) == 0) {
    mrowki.fathers[[deep]][[1]] <<- ID
    return (TRUE)
  }
  mrowki.fathers[[deep]][[length(mrowki.fathers[[deep]])+1]] <<- ID
}

mrowki.op_generate <- function(XS,M,UG, WA=4) {
  YS <- mrowki.op_init()
  mrowki.deep <<- 1
  ID <- YS[[1]]
    
  while(!mrowki.stop_ant(ID, YS)) {
    X <- mrowki.get_sons(ID)
    XT <- X
    for(i in 1 : length(X)){
      if(is.element(X[[i]], YS))
        XT <- XT[-which(XT == X[[i]])]
    }
    P <- mrowki.get_pheromons(ID,X,M$pheromons)
  
    ID <- mrowki.rand_move(X,P)
    
#    while(is.element(ID, YS))
#      ID <- mrowki.rand_move(X,P)
    
    YS <- mrowki.update_state(YS,ID)
  
    mrowki.deep <<- mrowki.deep + 1
    
  }
  
  return(YS)
}



mrowki.check_boards<- function(f_board,s_board) {
  return(all(f_board == s_board))
}

mrowki.is_son <- function(son,father) {
  found <- FALSE
  for(i in father) {
    found <- FALSE
    
    for(j in son) {
      if(i == j) {
        found <- TRUE
      }
    }
    
    if(found == FALSE) {
      return (FALSE)
    }
  }
  
  return (TRUE)
}

mrowki.getID <- function(board,deep){
  newID <- -1
  father <- -1
  
  if(length(mrowki.fathers) < mrowki.deep) {
    mrowki.fathers[[mrowki.deep]] <<- list()
    
    newID = mrowki.add_son(board)
    return(newID)
  }
 
  for(i in mrowki.fathers[[mrowki.deep]]) {
    if(mrowki.is_son(board,mrowki.vertices[[i]]$board)) {
      father <- i
      break
    }
  }
  
  if(father != -1) {
    for(m in mrowki.vertices[[father]]$sons) {
      if(mrowki.check_boards(board,mrowki.vertices[[m]]$board)) {
        newID = m
        break
      }
    }
  }
  
  if(newID == -1) {
    newID = mrowki.add_son(board)
  }
  return(newID)
}
