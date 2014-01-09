sudoku.task <- c()

#przyjmuje tablice 9x9 i sprawdza czy sudoku jest wypelnione
# TRUE - jesli wypelnione (nie wazne czy poprawnie)
sudoku.is_complete<- function(sudoku){
  tablica <- table(sudoku)
   return (tablica[names(tablica) == 0][[1]] == 0)
}

# i - rzad polozenia punktu
# j - kolumna polozenia punktu
# s - macierz stanu
sudoku.set_numb_seq <- function(i,j,s) {
  prob_seq <- 1:9
  prob_seq <- mrowki.sudoku.check_raw(prob_seq,j,s)
  prob_seq <- mrowki.sudoku.check_column(prob_seq,i,s)
  prob_seq <- mrowki.sudoku.check_square(prob_seq,i,j,s)
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
  m_seq <- mrowki.sudoku.set_square_iter_seq(i)
  n_seq <- mrowki.sudoku.set_square_iter_seq(j)
  for(m in m_seq) {
    for(n in n_seq) {
      if(s[n,m] != 0) {
        prob_seq <- prob_seq[prob_seq!=s[n,m]]
      }
    }
  }
  return(prob_seq)
}