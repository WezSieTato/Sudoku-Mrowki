source('mrowki.r')
source('sudoku.r')

#model init jest w mrowkach, model init czesciowo korzysta z funkcji
# ktore sa w mrowki.sudoku, ale sama funkcja jest w mrowkach

mrowki.sudoku.identity_number <- function() {
  return(1:729)
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

mrowki.sudoku.set_numb_seq <<- sudoku.set_numb_seq
mrowki.sudoku.check_raw <<- sudoku.check_raw
mrowki.sudoku.check_column <<- sudoku.check_column
mrowki.sudoku.check_square <<- sudoku.check_square

mrowki.sudoku.extend_state <- function(s) {
  seq = array(data=list(), dim = c(9,9))
  for(i in 1:9) {
    for(j in 1:9) {
      if(s[j,i] == 0) {
        seq[j,i][[1]] <- mrowki.sudoku.set_numb_seq(i,j,s)
      } 
    }
  }
  return(seq)
}

mrowki.sudoku.set_square_iter_seq <- function(position) {
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

# funkcja uzyta do wyznaczania atrakcyjnosci to 1/x^WA gdzie x to ilosc
# mozliwych podstawien 
mrowki.sudoku.set_attractivity <- function(pn,WA=4) {
  attr = array(data=0, dim = c(9,9)) 
  
  for(i in 1:9) {
    for(j in 1:9) {
      if(length(pn[i,j][[1]]) != 0)
        attr[i,j] = 1/(length(pn[i,j][[1]]))^WA
    }
  }
  
  return (attr)
}

mrowki.sudoku.there_is_move <- function(pn) {
  for(i in 1:9) {
    for(j in 1:9){
      if(length(pn[i,j][[1]]) != 0)
        return (TRUE) 
    }
  }
  
  return (FALSE)
}

# 
mrowki.sudoku.set_propability_matrix <- function(attr, pn, pheromons) { 
  # pher = array(data=0, dim=c(9,9,9)) # stub chwilowy
  prop_array = array(data=0, dim=c(9,9,9))
  
  pheromons <- pheromons/sum(pheromons) # normalizacja
  attr <- attr/sum(attr)

  
  for(i in 1:9) {
    for(j in 1:9) {
      for(k in pn[i,j][[1]]) {
        prop_array[i,j,k] = pheromons[i,j,k] + attr[i,j]
      }
    }
  }
  
  return (prop_array)
}

mrowki.sudoku.rand_move <- function(P) {
  return (sample(1:729,1,replace=FALSE,P))
}

mrowki.sudoku.get_rand_column <- function(numb) {
  return (ifelse(ceiling(numb/9)%%9 == 0, 9, ceiling(numb/9)%%9))
}

mrowki.sudoku.get_rand_raw <- function(numb) {
  return (ifelse((numb %% 9) == 0, 9, numb%%9))
}

mrowki.sudoku.get_rand_value <- function(numb) {
  return (ceiling(numb/81))
}

## blok funkcji dokonujπcych zmian w iteracji nad macierzπ wspÛ≥czynnikÛw atrakcyjnoúci

mrowki.sudoku.update_attractivity <- function(attr,pn, j_column, i_raw, value) {
  attr <- mrowki.sudoku.update_attr_for_raw(attr,pn,i_raw,value)
  attr <- mrowki.sudoku.update_attr_for_column(attr,pn,j_column,value)
  attr <- mrowki.sudoku.update_attr_for_square(attr,pn,j_column,i_raw,value)
  return(attr)
}

mrowki.sudoku.update_attr_for_raw <- function(attr,pn,raw,value,WA=4) {
  for(i in 1:9) {
    if(length(pn[raw,i][[1]] != 0))
      attr[raw,i] = 1/(length(pn[raw,i][[1]]))^WA
    if(length(pn[raw,i][[1]]) == 0)
      attr[raw,i] = 0
  }
  return(attr)
}

mrowki.sudoku.update_attr_for_column <- function(attr,pn,column,value,WA=4) {
  for(j in 1:9) {
    if(length(pn[j,column][[1]] != 0)) {
      attr[j,column] = 1/(length(pn[j,column][[1]]))^WA
      if(length(pn[j,column][[1]]) == 0)
        attr[j,column] = 0
    }
  }
  return(attr)
}

mrowki.sudoku.update_attr_for_square <- function(attr,pn,j,i,value,WA=4) {
  m_seq <- mrowki.sudoku.set_square_iter_seq(i)
  n_seq <- mrowki.sudoku.set_square_iter_seq(j)
  for(m in m_seq) {
    for(n in n_seq) {
      if(length(pn[n,m][[1]] != 0))
        attr[n,m] = 1/(length(pn[n,m][[1]]))^WA
      if(length(pn[n,m][[1]]) == 0)
        attr[n,m] = 0
    }
  }
  return(attr)
}

## blok funkcji dokonujπcych zmian nad macierzπ moøliwych ruchÛw dla pÛl

mrowki.sudoku.update_prop_number <- function(pn,j_column,i_raw,value) {
  pn <- mrowki.sudoku.delete_value_from_raw(pn,i_raw,value)
  pn <- mrowki.sudoku.delete_value_from_column(pn,j_column,value)
  pn <- mrowki.sudoku.delete_value_from_square(pn,j_column,i_raw,value)
  return(pn)
}

mrowki.sudoku.delete_value_from_raw <- function(pn,raw,value) {
  for(i in 1:9) {
    if(length(pn[raw,i][[1]] != 0))
      pn[raw,i][[1]] <- pn[raw,i][[1]][pn[raw,i][[1]]!=value]
    if(length(pn[raw,i][[1]]) == 0)
      pn[raw,i][1] <- list(NULL)
  }
  return(pn)
}

mrowki.sudoku.delete_value_from_column <- function(pn,column,value) {
  for(j in 1:9) {
    if(length(pn[j,column][[1]] != 0)) {
      pn[j,column][[1]] <- pn[j,column][[1]][pn[j,column][[1]]!=value]
      if(length(pn[j,column][[1]]) == 0)
        pn[j,column][1] <- list(NULL)
    }
  }
  return(pn)
}

mrowki.sudoku.delete_value_from_square <- function(pn,j,i,value) {
  m_seq <- mrowki.sudoku.set_square_iter_seq(i)
  n_seq <- mrowki.sudoku.set_square_iter_seq(j)
  for(m in m_seq) {
    for(n in n_seq) {
      if(length(pn[n,m][[1]] != 0))
        pn[n,m][[1]] <- pn[n,m][[1]][pn[n,m][[1]]!=value]
      if(length(pn[n,m][[1]]) == 0)
        pn[n,m][1] <- list(NULL)
    }
  }
  return(pn)
}

mrowki.sudoku.update_state <- function(YS,n_column,n_raw,n_value) {
  YS[n_raw,n_column] <- n_value
  return(YS)
}

# M - model zawierajacy feromony
# XS - wybrany ostatni stan
# PN - ciagi mozliwych liczb do wstawienia w pola
# AS - stopien atrakcyjnosci
# WA - wskaznik atrakcyjnosci
mrowki.sudoku.op_generate <- function(XS,M,UG, WA=4) {
  YS <- mrowki.sudoku.init_state()
  PN <- mrowki.sudoku.extend_state(YS)
  AS <- mrowki.sudoku.set_attractivity(PN,WA)
  
  
  while(mrowki.sudoku.there_is_move(PN)) {
    # zakladam, ze M to macierz 9x9x9 
    P <- mrowki.sudoku.set_propability_matrix(AS,PN,M$feromony) # zalozony stub, powinno byc jeszcze M w liscie argumentow
    N <- mrowki.sudoku.rand_move(P)
    
    n_column <- mrowki.sudoku.get_rand_column(N)
    n_raw <- mrowki.sudoku.get_rand_raw(N)
    n_value <- mrowki.sudoku.get_rand_value(N)
    
    PN <- mrowki.sudoku.update_prop_number(PN,n_column,n_raw,n_value)
    AS <- mrowki.sudoku.update_attractivity(AS,PN,n_column,n_raw,n_value)
    YS <- mrowki.sudoku.update_state(YS,n_column,n_raw,n_value)
  }
  
  return(YS)
}


mrowki.sudoku.stop_criterion<-function(XS, M){
  path <- XS[[length(XS)]]
  lastId <- path[[length(path)]]
  vertex <- mrowki.vertices[[lastId]]
  return(sudoku.is_complete(vertex$board))
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

print('Rozwiazujemy sudoku')
mrowki.sudoku.init_state()
print('Rozwiazane sudoku')
mrowki.sudoku(mrowki.sudoku.init_state())

mrowki.sudoku.op_generate(s,c(),c())
mrowki.sudoku.extend_state(mrowki.sudoku.init_state())
