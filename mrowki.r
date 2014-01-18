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

mrowki.op_generate<-function(XS,M,UG)
{
  stop('Brak implementacji funkcji generujacej stany')
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