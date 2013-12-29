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
sudoku.check_raw(prob_seq,j,s) {
  for(k in 1:9) {
    if(s[k,j] != 0) {
      prob_seq <- prob_seq[prob_seq!=s[k,j]]
    }
  }
  return(prob_seq)
}

sudoku.check_column(prob_seq,i,s) {
  for(k in 1:9) {
    if(s[i,k] != 0) {
      prob_seq <- prob_seq[prob_seq!=s[i,k]]
    }
  }
  return(prob_seq)
}

#mrowki.sudoku.check_square(prob_seq,i,j,s) {
#nalezy ustalic w ktorym kwadracie znajduje sie dana liczba
#  if(k<4) {
# k nalezy do przedzialu {0..3}
#  } else {

#  }

#}