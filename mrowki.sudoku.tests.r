source('mrowki.sudoku.r')

#print('Rozwiazujemy sudoku')
# mrowki.sudoku()
#print('Rozwiazane sudoku')
# print(mrowki.solution)

mrowki.sudoku.tests.fill_levels <- c()
mrowki.sudoku.tests.max_fill <- -1
mrowki.sudoku.tests.current_fill_max <- -1

mrowki.sudoku.tests.origin_stop_criterion <<- mrowki.stop_criterion


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

mrowki.sudoku.tests <-function(s,ants = 1,degrad=0.81,start=0.5){
  
  
  mrowki.sudoku.tests.max_fill <<- mrowki.sudoku.tests.set_max_fill(s)
  mrowki.sudoku.tests.fill_levels <<- c()
  mrowki.sudoku.tests.current_fill_max <<- -1
  
  sudoku.task <<- s
  mrowki.task <<- s
  
  mrowki.antNumber <<- ants
  
  mrowki.pheromonDegradation <<-degrad
  
  mrowki.startPheromon <<- start
  
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


mrowki.sudoku.tests.test_main <- function(n=20,antnumber=2,degrad=0.81,start=0.5) {
  results <- list()
  
  
  
  for(i in 1:15) {
    print('Sudoku do rozwiazania:')
    mrowki.sudoku.tests(mrowki.sudoku.tests.generate_option(n),antnumber,degrad,start)
    print("Rozwiazanie:")
    print(mrowki.solution)
    results[[i]] = mrowki.sudoku.tests.fill_levels
    print('=========================')
    
   
  }
  
  maxLength <- -1
  maxIndex <- -1
  for(i in 1:length(results)) {
    if(length(results[[i]]) > maxLength) {
      maxLength <- length(results[[i]])
      maxIndex <- i
    }
  }
  
  plot(1:length(results[[maxIndex]]),results[[maxIndex]],type="l",col="blue",xlab="Numer uruchomienia",ylab="Funkcja celu (wypełnienie sudoku)",ylim=c(0.5,1), xlim=c(1,length(results[[maxIndex]])),main="Wykres wykonanych ilosci krokow algorytmu w kolejnych uruchomieniach programu")
  
  for(i in 1:15) {
    if(i != maxIndex) {
      lines(1:length(results[[i]]),results[[i]],col=sample(colours(),1))
    }
  }
}


mrowki.sudoku.tests.generate_option <- function(n) {
  if(n<1) {
    stop("Bledna wartosc n!")
  }
  if(n > 81) {
    stop("Bledna wartosc n!")
  }
  
  s = c()
  s[1:9] =   c(1,2,3,4,5,6,7,8,9)
  s[10:18] = c(4,5,6,7,8,9,1,2,3)
  s[19:27] = c(7,8,9,1,2,3,4,5,6)
  s[28:36] = c(2,1,4,3,6,5,8,9,7)
  s[37:45] = c(3,6,5,8,9,7,2,1,4)
  s[46:54] = c(8,9,7,2,1,4,3,6,5)
  s[55:63] = c(5,3,1,6,4,2,9,7,8)
  s[64:72] = c(6,4,2,9,7,8,5,3,1)
  s[73:81] = c(9,7,8,5,3,1,6,4,2)
  
  dim(s) = c(9,9)
  
  indexes <- sample(x=1:81,size=n,replace=FALSE)
  
  for(i in indexes) {
    s[i] <- 0
  }
  
  return (s)
}
