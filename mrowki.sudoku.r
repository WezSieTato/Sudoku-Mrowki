source('mrowki.r')
source('sudoku.r')

mrowki.sudoku.model_init <- function(UG) {
  
}

mrowki.sudoku.init_state <- function() {
  s = c()
  s[1:9] =   c(0,0,0,0,0,0,3,1,0)
  s[10:18] = c(0,0,0,3,0,9,0,0,5)
  s[19:27] = c(9,3,0,6,0,0,2,0,8)
  s[28:36] = c(1,0,4,0,0,0,5,8,0)
  s[37:45] = c(0,6,3,7,4,5,1,9,0)
  s[46:54] = c(0,7,2,0,0,0,4,0,3)
  s[55:63] = c(7,0,1,0,0,8,0,2,6)
  s[64:72] = c(6,0,0,1,0,7,0,0,0)
  s[73:81] = c(0,4,8,0,0,0,0,0,0)
  dim(s) = c(9,9)
  return(s)
}

mrowki.sudoku.extend_state <- function(s) {
  seq = c()
  for(i in 1:9) {
    for(j in 1:9) {
        if(s[i,j] == 0) {
          seq[i,j] <- mrowki.sudoku.set_numb_seq(i,j,s)
        } else {
          seq[i,j] = c(0)
        }
    }
  }
  return(seq)
}



# i - rzad polozenia punktu
# j - kolumna polozenia punktu
# s - macierz stanu
mrowki.sudoku.set_numb_seq <- function(i,j,s) {
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
mrowki.sudoku.check_raw(prob_seq,j,s) {
  for(k in 1:9) {
      if(s[k,j] != 0) {
        prob_seq <- prob_seq[prob_seq!=s[k,j]]
    }
  }
  return(prob_seq)
}

mrowki.sudoku.check_column(prob_seq,i,s) {
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

# M - model zawierajacy feromony
# XS - wybrany ostatni stan
mrowki.sudoku.op_generate <- function(XS,M,UG) {
  YS <- mrowki.sudoku.init_state()
  
  return(YS)
}
