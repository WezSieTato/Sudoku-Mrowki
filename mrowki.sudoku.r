source('mrowki.r')
source('sudoku.r')

#model init jest w mrowkach, model init czesciowo korzysta z funkcji
# ktore sa w mrowki.sudoku, ale sama funkcja jest w mrowkach

mrowki.sudoku.init_state <- function() {
#  s = c()
#  s[1:9] =   c(0,0,0,0,0,0,3,1,0)
#  s[10:18] = c(0,0,0,3,0,9,0,0,5)
#  s[19:27] = c(9,3,0,6,0,0,2,0,8)
#  s[28:36] = c(1,0,4,0,0,0,5,8,0)
#  s[37:45] = c(0,6,3,7,4,5,1,9,0)
#  s[46:54] = c(0,7,2,0,0,0,4,0,3)
#  s[55:63] = c(7,0,1,0,0,8,0,2,6)
#  s[64:72] = c(6,0,0,1,0,7,0,0,0)
#  s[73:81] = c(0,4,8,0,0,0,0,0,0)
#  dim(s) = c(9,9)
  s = list(1)
  return(s)
}

mrowki.sudoku.there_is_move <- function(id_sons) {
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
  if(M$vertices[[ID]] != NULL) {
      return(mrowki.vertices[[ID]]$sons)
    
  # if M$vertices[[ID]] == NULL
  return(mrowki.sudoku.build_sons(ID))
    
}

mrowki.sudoku.add_son <- function(board) {
  mrowki.vertices[[length(mrowki.vertices)+1]]$board <<- board
  mrowki.vertices[[length(mrowki.vertices)+1]]$sons <<- NULL
}

mrowki.sudoku.build_sons <- function(ID) {
  board = mrowki.vertices[[ID]]$board
  for(i in 1:9)
    for(j in 1:9)
      if(board[i,j] == 0) {
        seq = sudoku.set_numb_seq(i,j,board)
        for(k in seq) {
          newboard = sudoku.new_board(i,j,k)
          newID = -1
          for(m in 1:length(vertices))
            if(sudoku.check_boards(newboard,mrowki.vertices[[m]]$board)) {
              newID = m
              toCreate = false
              break
            }
          
          if(newID == -1) {
            mrowki.sudoku.add_son(newboard)
          }
          
        }
      }
  
  return(mrowki.vertices[[ID]]$sons)
      
    
}

mrowki.sudoku.get_pheromons <- function(ID, sons, pheromons) {
  prop = list()
  for(i in 1:length(sons)) {
    prop[i] = pheromons[[paste(ID,"->",sons[[i]])]]
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
  return(sudoku.is_complete(XS[[length(XS)]]))
}



mrowki.sudoku <-function(task){
  sudoku.task <<- task
  mrowki.zaznacz_odwiedzone <<- mrowki.sudoku.zaznacz_odwiedzone
  mrowki.feromony_init <<- mrowki.sudoku.feromony_init
  mrowki.op_generate <<- mrowki.sudoku.op_generate
  mrowki.stop_criterion <<- mrowki.sudoku.stop_criterion
  
  mrowki.op_init<<-function(UG)
  {
    return (sudoku.task)
  }
  
  return (mrowki.search())
}
