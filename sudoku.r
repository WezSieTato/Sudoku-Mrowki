sudoku.task <- c()

#przyjmuje tablice 9x9 i sprawdza czy sudoku jest wypelnione
# TRUE - jesli wypelnione (nie wazne czy poprawnie)
sudoku.is_complete<- function(sudoku){
   return (!is.element(0, sudoku) )
}

# i - rzad polozenia punktu
# j - kolumna polozenia punktu
# s - macierz stanu
sudoku.set_numb_seq <- function(i,j,s) {
  prob_seq <- 1:9
  prob_seq <- sudoku.check_raw(prob_seq,i,s)
  prob_seq <- sudoku.check_column(prob_seq,j,s)
  prob_seq <- sudoku.check_square(prob_seq,i,j,s)
  return(prob_seq)
}

#prob_seq - sekwencja mozliwych ruchow dla danego pola
#i - rzad polozenia pola
#j - kolumna polozenia pola
#s - macierz stanu zawierajaca pola
sudoku.check_raw <- function(prob_seq,j,s) {
  for(k in 1:9) {
    if(s[j,k] != 0) {
      prob_seq <- prob_seq[prob_seq!=s[j,k]]
    }
  }    
  
  return(prob_seq)
}

sudoku.check_column <- function(prob_seq,i,s) {
  for(k in 1:9) {
    if(s[k,i] != 0) {
      prob_seq <- prob_seq[prob_seq!=s[k,i]]
    }
  }
  
  return(prob_seq)
}

sudoku.check_square <- function(prob_seq,i,j,s) {
  m_seq <- sudoku.set_square_iter_seq(i)
  n_seq <- sudoku.set_square_iter_seq(j)
  for(m in m_seq) {
    for(n in n_seq) {
      if(s[m,n] != 0) {
        prob_seq <- prob_seq[prob_seq!=s[m,n]]
      }
    }
  }
  return(prob_seq)
}

sudoku.set_square_iter_seq <- function(position) {
  temp <- position %% 3 
  if(temp == 0) { 
    return((position-2):position) 
  }
  if(temp == 1) {
    return(position:(position+2) )
  }
  if(temp == 2) {
    return((position-1):(position+1))
  }
  stop("Wystapil blad!")
  return(c())
}


sudoku.new_board <- function(board,raw,column,value) {
  board[raw,column] = value;
  return (board)
}

sudoku.board <- function(startBoard, path){
  if(length(path) == 0){
    return(startBoard)
  }
  for(i in 1 : length(path)){
    vert <- path[[i]]
    startBoard[vert[[1]], vert[[2]]] <- vert[[3]]
  }
  return(startBoard)
}

sudoku.path <- function(startBoard, board){
  path <- list()
  delta = board - startBoard
  for(i in 1:9){
    for(k in 1:9){
      if(delta[i, k] != 0)
      path[[length(path) + 1]] <- c(i, k, delta[i, k])
    }
  }
  return(path)
}
