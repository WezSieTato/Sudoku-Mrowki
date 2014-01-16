source('mrowki.r')
source('sudoku.r')

#model init jest w mrowkach, model init czesciowo korzysta z funkcji
# ktore sa w mrowki.sudoku, ale sama funkcja jest w mrowkach

mrowki.sudoku.init_state <- function() {

  s = list(1)
  return(s)
}

mrowki.sudoku.there_is_move <- function(sons) {
  if(length(sons) == 0)
    return (FALSE)
  
  return (TRUE)
}


mrowki.sudoku.rand_move <- function(Elements,P) {
  return (sample(x=Elements,size=1,replace=FALSE,prob=P)[[1]])
}

mrowki.sudoku.update_state <- function(YS,n_column,n_raw,n_value) {
  YS[n_raw,n_column] <- n_value
  return(YS)
}

mrowki.sudoku.get_sons <- function(ID) {
  if(is.null(mrowki.vertices)) {
      return(mrowki.vertices[[ID]]$sons)
  }
  
  # if M$vertices[[ID]] == NULL
  return(mrowki.sudoku.build_sons(ID))
    
}

mrowki.sudoku.add_son <- function(B) {
  newVertex = list(board = B, sons = NULL)
  
  mrowki.vertices[[length(mrowki.vertices)+1]] <<- newVertex
  
  return(length(mrowki.vertices)+1)
}

mrowki.sudoku.build_sons <- function(ID) {
  board = mrowki.vertices[[ID]]$board
  for(i in 1:9)
    for(j in 1:9)
      if(board[i,j] == 0) {
        seq = sudoku.set_numb_seq(i,j,board)
        for(k in seq) {
          newboard = sudoku.new_board(board,i,j,k)
          newID = -1
          for(m in 1:length(mrowki.vertices))
            if(sudoku.check_boards(newboard,mrowki.vertices[[m]]$board)) {
              newID = m
              break
            }
          
          if(newID == -1) {
            newID = mrowki.sudoku.add_son(newboard)
          }
          
          mrowki.vertices[[ID]]$sons[[length(mrowki.vertices[[ID]]$sons)+1]] <<- newID
          
        }
      }
  
  return(mrowki.vertices[[ID]]$sons)
      
    
}

mrowki.sudoku.get_pheromons <- function(ID, sons, pheromons) {
  prob = list()
  for(i in 1:length(sons)) {
    if(is.null(pheromons[[paste(ID,"->",sons[[i]])]])) {
      prob[i] = mrowki.startPheromon
    } else {
      prob[i] = pheromons[[paste(ID,"->",sons[[i]])]]
    }
  }
  return(prob)
}


mrowki.sudoku.op_generate <- function(XS,M,UG, WA=4) {
  YS <- mrowki.sudoku.init_state()
  

  ID <- YS[[1]]
  
  X <- mrowki.sudoku.get_sons(ID)
    
  while(mrowki.sudoku.there_is_move(X)) {
    P <- mrowki.sudoku.get_pheromons(ID,X,M$pheromons)
    
    ID <- mrowki.sudoku.rand_move(X,P)
    
    YS <- mrowki.sudoku.update_state(YS,ID)
    
    X <- mrowki.sudoku.get_sons(ID)
  }
  
  return(YS)
}

mrowki.sudoku.stop_criterion<-function(XS, M){
 # print('asdasd')
  if(XS == 0)
    return(FALSE)
  path <- XS[[length(XS)]]
  lastId <- path[[length(path)]]
  vertex <- mrowki.vertices[[lastId]]
  
  if(sudoku.is_complete(vertex$board)){
    mrowki.solution(vertex$board)
    return(TRUE)
  } else{
    return(FALSE)
  }
  
}

mrowki.sudoku <-function(){
  s = c()
  s[1:9] =   c(1,2,3,4,5,6,7,8,9)
  s[10:18] = c(4,5,6,7,8,9,1,2,3)
  s[19:27] = c(7,8,9,1,2,3,4,5,6)
  s[28:36] = c(2,1,4,3,6,5,8,9,7)
  s[37:45] = c(3,6,5,8,9,7,2,1,4)
  s[46:54] = c(8,9,7,2,1,4,3,6,5)
  s[55:63] = c(5,3,1,6,4,2,9,7,8)
  s[64:72] = c(6,4,2,9,7,8,5,3,1)
  s[73:81] = c(9,7,8,5,3,1,6,4,0)
  dim(s) = c(9,9)
  
  sudoku.task <<- s
  mrowki.task <<- s
  mrowki.op_generate <<- mrowki.sudoku.op_generate
  mrowki.stop_criterion <<- mrowki.sudoku.stop_criterion
  
  mrowki.op_init<<-function(UG)
  {
    return (sudoku.task)
  }
  
  return (mrowki.search())
}


print('Rozwiazujemy sudoku')
mrowki.sudoku()
print('Rozwiazane sudoku')
