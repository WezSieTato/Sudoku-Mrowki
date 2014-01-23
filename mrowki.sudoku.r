source('mrowki.r')
source('sudoku.r')

#model init jest w mrowkach, model init czesciowo korzysta z funkcji
# ktore sa w mrowki.sudoku, ale sama funkcja jest w mrowkach

mrowki.sudoku.extend_state <- function(s) {
  seq = array(data=list(), dim = c(9,9))
  for(i in 1:9) {
    for(j in 1:9) {
      if(s[i,j] == 0) {
        seq[i,j][[1]] <- sudoku.set_numb_seq(i,j,s)
      } 
    }
  }
  return(seq)
}



mrowki.sudoku.set_attractivity <- function(board,WA=4) {
  attr = array(data=0, dim = c(9,9)) 
  
  pn = mrowki.sudoku.extend_state(board)
  
  for(i in 1:9) {
    for(j in 1:9) {
      if(length(pn[i,j][[1]]) != 0)
        attr[i,j] = 1/(length(pn[i,j][[1]]))^WA
    }
  }
  
  return (attr)
}

mrowki.sudoku.get_attractivity <- function(boardFirst,boardSecond,attr) {
  board = boardSecond - boardFirst
  for( i in 1:9) {
    for( j in 1:9) {
      if(board[i,j] != 0) {
        return (attr[i,j])
      }
        
    }
  }
  stop("Cos sie zlego stalo")
}

mrowki.sudoku.build_sons <- function(ID) {
  mrowki.vertices[[ID]]$sons <<- list()
  
  board = mrowki.vertices[[ID]]$board
  for(i in 1:9) {
    for(j in 1:9) {
      if(board[i,j] == 0) {
        seq = sudoku.set_numb_seq(i,j,board)
        for(k in seq) {
          newboard <- sudoku.new_board(board,i,j,k)
          newID <- mrowki.getID(newboard)
          mrowki.vertices[[ID]]$sons[[length(mrowki.vertices[[ID]]$sons)+1]] <<- newID
        }
      }
    }
  }
  return(mrowki.vertices[[ID]]$sons)
}

mrowki.stop_criterion<-function(XS, M){
  
  path <- XS[[length(XS)]]
  lastId <- path[[length(path)]]
  vertex <- mrowki.vertices[[lastId]]
  
  if(sudoku.is_complete(vertex$board)){
    mrowki.solution <<- vertex$board
    return(TRUE)
  } else{
    return(FALSE)
  }
  
}

mrowki.sudoku <-function(ants = 1){
  s = c()
# Z jednym  
#  s[1:9] =   c(1,2,3,4,5,6,7,8,9)
#  s[10:18] = c(4,5,6,7,8,9,1,2,3)
#  s[19:27] = c(7,8,9,1,2,3,4,5,6)
#  s[28:36] = c(2,1,4,3,6,5,8,9,7)
#  s[37:45] = c(3,6,5,8,9,7,2,1,4)
#  s[46:54] = c(8,9,7,2,1,4,3,6,5)
#  s[55:63] = c(5,3,1,6,4,2,9,7,8)
#  s[64:72] = c(6,4,2,9,7,8,5,3,1)
#  s[73:81] = c(9,7,8,5,3,1,6,4,0)

# z dwoma
#  s[1:9] =   c(1,2,3,4,5,6,7,8,9)
#  s[10:18] = c(4,5,6,7,8,9,1,2,3)
#  s[19:27] = c(7,8,9,1,2,3,4,5,6)
#  s[28:36] = c(2,1,4,3,6,5,8,9,7)
#  s[37:45] = c(3,6,5,8,9,7,2,1,4)
#  s[46:54] = c(8,9,7,2,1,4,3,6,5)
#  s[55:63] = c(5,3,1,6,4,2,9,7,8)
#  s[64:72] = c(6,4,2,9,7,8,5,3,1)
#  s[73:81] = c(9,7,8,5,3,1,0,0,0)

# bez kolumny
#  s[1:9] =   c(1,2,3,4,5,6,7,8,9)
#  s[10:18] = c(4,5,6,7,8,9,1,2,3)
#  s[19:27] = c(7,8,9,1,2,3,4,5,6)
#  s[28:36] = c(2,1,4,3,6,5,8,9,7)
#  s[37:45] = c(3,6,5,8,9,7,2,1,4)
#  s[46:54] = c(8,9,7,2,1,4,3,6,5)
#  s[55:63] = c(5,3,1,6,4,2,9,7,8)
#  s[64:72] = c(6,4,2,9,7,8,5,3,1)
#  s[73:81] = c(0,0,0,0,0,0,0,0,0)

# dwie puste kolumny
#  s[1:9] =   c(1,2,3,4,5,6,7,8,9)
#  s[10:18] = c(4,5,6,7,8,9,1,2,3)
#  s[19:27] = c(7,8,9,1,2,3,4,5,6)
#  s[28:36] = c(2,1,4,3,6,5,8,9,7)
#  s[37:45] = c(3,6,5,8,9,7,2,1,4)
#  s[46:54] = c(8,9,7,2,1,4,3,6,5)
#  s[55:63] = c(5,3,1,6,4,2,9,7,8)
#  s[64:72] = c(0,0,0,0,0,0,0,0,0)
#  s[73:81] = c(0,0,0,0,0,0,0,0,0)

# pusta
s[1:9] =   c(1,2,3,4,5,6,7,8,9)
s[10:18] = c(4,5,6,7,8,9,1,2,3)
s[19:27] = c(7,8,9,1,2,3,4,5,6)
s[28:36] = c(2,1,4,3,6,5,8,9,7)
s[37:45] = c(3,6,5,8,9,7,2,1,4)
s[46:54] = c(8,9,7,2,1,4,3,6,5)
s[55:63] = c(5,3,1,6,0,2,0,0,0)
s[64:72] = c(6,4,2,9,7,8,0,0,0)
s[73:81] = c(9,7,8,5,3,1,0,0,0)
  dim(s) = c(9,9)
  
  sudoku.task <<- s
  mrowki.task <<- s
  mrowki.trail <<- function(delta){
    return (delta / 81)
  }

  mrowki.stop_ant <<- function(ID){
    return (!mrowki.there_is_move(mrowki.get_sons(ID)))
  }

  mrowki.first_vertex <<-function(){
    return(list(board = mrowki.task, sons = NULL ))
  }

  mrowki.build_sons <<- mrowki.sudoku.build_sons
#  mrowki.is_complete <<- sudoku.is_complete
  
  
  return (mrowki.search())
}
