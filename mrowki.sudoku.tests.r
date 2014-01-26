source('mrowki.sudoku.r')

#print('Rozwiazujemy sudoku')
# mrowki.sudoku()
#print('Rozwiazane sudoku')
# print(mrowki.solution)

mrowki.sudoku.tests.fill_levels <- c()
mrowki.sudoku.tests.max_fill <- -1
mrowki.sudoku.tests.current_fill_max <- -1

mrowki.sudoku.tests.origin_stop_criterion <<- mrowki.stop_criterion

mrowki.sudoku.tests <-function(option,ants = 1){
  s = c()
  
  if(option == 1) {
  # Z jednym  
    s[1:9] =   c(1,2,3,4,5,6,7,8,9)
    s[10:18] = c(4,5,6,7,8,9,1,2,3)
    s[19:27] = c(7,8,9,1,2,3,4,5,6)
    s[28:36] = c(2,1,4,3,6,5,8,9,7)
    s[37:45] = c(3,6,5,8,9,7,2,1,4)
    s[46:54] = c(8,9,7,2,1,4,3,6,5)
    s[55:63] = c(5,3,1,6,4,2,9,7,8)
    s[64:72] = c(6,4,2,9,7,8,5,3,1)
    s[73:81] = c(9,7,8,5,3,1,6,4,0)
  }
  if(option == 2) {
  # z dwoma
    s[1:9] =   c(1,2,3,4,5,6,7,8,9)
    s[10:18] = c(4,5,6,7,8,9,1,2,3)
    s[19:27] = c(7,8,9,1,2,3,4,5,6)
    s[28:36] = c(2,1,4,3,6,5,8,9,7)
    s[37:45] = c(3,6,5,8,9,7,2,1,4)
    s[46:54] = c(8,9,7,2,1,4,3,6,5)
    s[55:63] = c(5,3,1,6,4,2,9,7,8)
    s[64:72] = c(6,4,2,9,7,8,5,3,1)
    s[73:81] = c(9,7,8,5,3,1,0,0,0)
  }
  if(option == 3) {
  # bez kolumny
    s[1:9] =   c(1,2,3,4,5,6,7,8,9)
    s[10:18] = c(4,5,6,7,8,9,1,2,3)
    s[19:27] = c(7,8,9,1,2,3,4,5,6)
    s[28:36] = c(2,1,4,3,6,5,8,9,7)
    s[37:45] = c(3,6,5,8,9,7,2,1,4)
    s[46:54] = c(8,9,7,2,1,4,3,6,5)
    s[55:63] = c(5,3,1,6,4,2,9,7,8)
    s[64:72] = c(6,4,2,9,7,8,5,3,1)
    s[73:81] = c(0,0,0,0,0,0,0,0,0)
  }
  if(option == 4) {
  # dwie puste kolumny
    s[1:9] =   c(1,2,3,4,5,6,7,8,9)
    s[10:18] = c(4,5,6,7,8,9,1,2,3)
    s[19:27] = c(7,8,9,1,2,3,4,5,6)
    s[28:36] = c(2,1,4,3,6,5,8,9,7)
    s[37:45] = c(3,6,5,8,9,7,2,1,4)
    s[46:54] = c(8,9,7,2,1,4,3,6,5)
    s[55:63] = c(5,3,1,6,4,2,9,7,8)
    s[64:72] = c(0,0,0,0,0,0,0,0,0)
    s[73:81] = c(0,0,0,0,0,0,0,0,0)
  }
  if(option == 5) {
  # prawie wype?niona
    s[1:9] =   c(1,2,3,4,5,6,7,8,9)
    s[10:18] = c(4,5,0,7,8,0,1,2,3)
    s[19:27] = c(7,8,0,1,2,3,4,5,6)
    s[28:36] = c(2,1,4,3,6,0,8,9,7)
    s[37:45] = c(0,6,5,0,9,7,2,1,4)
    s[46:54] = c(8,9,0,2,1,4,3,6,5)
    s[55:63] = c(5,3,1,6,4,2,9,7,8)
    s[64:72] = c(0,0,0,0,0,0,0,0,0)
    s[73:81] = c(0,0,8,5,3,1,0,0,0)
  }
  if(option == 6) {
    #prawie niewype?niona
    s[1:9] =   c(0,5,0,0,0,6,8,0,0)
    s[10:18] = c(0,0,3,0,8,1,7,0,0)
    s[19:27] = c(0,0,7,4,3,0,0,1,0)
    s[28:36] = c(7,0,0,0,0,4,1,5,8)
    s[37:45] = c(3,0,0,0,7,0,0,0,2)
    s[46:54] = c(2,1,5,9,0,0,0,0,7)
    s[55:63] = c(0,7,0,0,4,9,3,0,0)
    s[64:72] = c(0,0,9,8,5,0,2,0,0)
    s[73:81] = c(0,0,8,2,0,0,0,9,0)
  }
  if(option == 7) {
    #pusta
    s[1:9] =   c(0,0,0,0,0,0,0,0,0)
    s[10:18] = c(0,0,0,0,0,0,0,0,0)
    s[19:27] = c(0,0,0,0,0,0,0,0,0)
    s[28:36] = c(0,0,0,0,0,0,0,0,0)
    s[37:45] = c(0,0,0,0,0,0,0,0,0)
    s[46:54] = c(0,0,0,0,0,0,0,0,0)
    s[55:63] = c(0,0,0,0,0,0,0,0,0)
    s[64:72] = c(0,0,0,0,0,0,0,0,0)
    s[73:81] = c(0,0,0,0,0,0,0,0,0)
  }
  if(is.null(s)) {
    stop("Podano bledny parametr option")
  }
  dim(s) = c(9,9)
  
  mrowki.sudoku.tests.max_fill <<- mrowki.sudoku.tests.set_max_fill(s)
  mrowki.sudoku.tests.fill_levels <<- c()
  mrowki.sudoku.tests.current_fill_max <<- -1
  
  sudoku.task <<- s
  mrowki.task <<- s
  
  
  mrowki.trail <<- function(delta){
    return (delta / 81)
  }
  
  mrowki.stop_ant <<- function(ID, YS){
    return (!mrowki.there_is_move(mrowki.get_sons(ID)))
  }
  
  mrowki.first_vertex <<-function(){
    # return(list(board = mrowki.task, sons = NULL ))
    return(list(board = list(), sons = NULL))
  }
  
  mrowki.build_sons <<- mrowki.sudoku.build_sons
  
  
  return (mrowki.sudoku.tests.search(mrowki.sudoku.tests.stop_criterion))
}

mrowki.sudoku.tests.search<-function(mrowki.sudoku.tests.stop_criterion){
  search(mrowki.model_init, mrowki.model_update, mrowki.op_init, mrowki.op_select, mrowki.op_generate, mrowki.sudoku.tests.stop_criterion, UG )
  
  return(mrowki.solution)
}

mrowki.sudoku.tests.stop_criterion <- function(XS,M) {
  if(length(XS) != 1) {
    mrowki.sudoku.tests.current_fill_max <<- max(mrowki.sudoku.tests.current_fill_max,length(XS[[length(XS)]]) / mrowki.sudoku.tests.max_fill)
    mrowki.sudoku.tests.fill_levels[length(mrowki.sudoku.tests.fill_levels) + 1] <<- mrowki.sudoku.tests.current_fill_max
  }
  
   return(mrowki.sudoku.tests.origin_stop_criterion(XS,M))
}
  
mrowki.sudoku.tests.set_max_fill <- function(s) {
  fill <- 1
  for(i in 1:9) {
    for(j in 1:9) {
      if(s[i,j] == 0){
        fill <- fill + 1
      }
    }
  }
  return(fill)
}

mrowki.sudoku.tests.test_1 <- function(option=4) {
  mrowki.sudoku.tests(option)
  
  plot(1:length(mrowki.sudoku.tests.fill_levels),mrowki.sudoku.tests.fill_levels,type="l",col="blue",xlab="Iteracja",ylab="Wypelnienie",main="Wykres wypelnienia sudoku w zaleznosci od iteracji")
  
}

mrowki.sudoku.tests.test_2 <- function(option=4) {
  
  mrowki.sudoku.tests.origin_stop_criterion <<- mrowki.stop_criterion
  results <- c()
  
  for(i in 1:15) {
    
    mrowki.sudoku.tests(option)
    results[length(results)+1] = length(mrowki.sudoku.tests.fill_levels)
  }
  
  plot(1:length(results),results,type="l",col="blue",xlab="Kolejne uruchomienia",ylab="Ilosc krokow",main="Wykres wykonanych ilosci krokow algorytmu w kolejnych uruchomieniach programu")
  
}

mrowki.sudoku.tests.test_3 <- function(option=4,antnumber=2) {
  if(antnumber > 4) {
    stop("Testy nie przewiduja przypadku wiekszego niz 4 mrowki")
  }

   
  resultslist <- list()
  
  for(i in 1:antnumber) {
    resultslist[[i]] <- list()
    mrowki.antNumber <<- i
    mrowki.sudoku.tests(option)
    
    resultslist[[i]] <- mrowki.sudoku.tests.fill_levels
  }
  
  maxLength <- -1
  maxIndex <- -1
  for(i in 1:length(resultslist)) {
    if(length(resultslist[[i]]) > maxLength) {
      maxLength <- length(resultslist[[i]])
      maxIndex <- i
    }
  }
  
  plot(1:length(resultslist[[maxIndex]]),resultslist[[maxIndex]],type="l",xlab="Iteracja",ylab="Wypelnienie",main="Wykres wypelnienia sudoku w zaleznosci od iteracji")
  
  
  
  for(i in 1:antnumber) {
    if(i == 1 && i != maxIndex) {
      lines(1:length(resultslist[[i]]),resultslist[[i]],col="red")
    }
    if(i == 2 && i != maxIndex) {
      lines(1:length(resultslist[[i]]),resultslist[[i]],col="green")
    }
    if(i == 3 && i != maxIndex) {
      lines(1:length(resultslist[[i]]),resultslist[[i]],col="blue")
    }
    if(i == 4 && i != maxIndex) {
      lines(1:length(resultslist[[i]]),resultslist[[i]],col="yellow")
    }
    
  }
  
  
  #
  mrowki.antNumber <<- 1
  #
}

