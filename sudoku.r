sudoku.task <- c()

#przyjmuje tablice 9x9 i sprawdza czy sudoku jest wypelnione
# TRUE - jesli wypelnione (nie wazne czy poprawnie)
sudoku.is_complete<- function(sudoku){
  tablica <- table(sudoku)
   return (tablica[names(tablica) == 0][[1]] == 0)
}