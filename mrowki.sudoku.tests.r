source('mrowki.sudoku.r')

print('Rozwiazujemy sudoku')
# mrowki.sudoku()
print('Rozwiazane sudoku')
# print(mrowki.solution)

mrowki.sudoku.tests.fill_levels <- c()
mrowki.sudoku.tests.max_fill <- 0

mrowki.sudoku.tests.origin_stop_criterion <- function() {
  stop("Brak implementacji funkcji mrowki.sudoku.tests.origin_stop_criterion!")
}

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
  # pusta
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
  if(is.null(s)) {
    stop("Podano bledny parametr option")
  }
  dim(s) = c(9,9)
  
  mrowki.sudoku.tests.max_fill <<- mrowki.sudoku.tests.set_max_fill(s)
  
  sudoku.task <<- s
  mrowki.task <<- s
  
  mrowki.stop_criterion <<- mrowki.sudoku.tests.stop_criterion
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

mrowki.sudoku.tests.stop_criterion <- function(XS,M) {
  if(length(XS) != 0) {
    mrowki.sudoku.tests.fill_levels[length(mrowki.sudoku.tests.fill_levels) + 1] <<- length(XS[[length(XS)]]) / mrowki.sudoku.tests.max_fill
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

mrowki.sudoku.tests.test_1 <- function() {
  
  mrowki.sudoku.tests.origin_stop_criterion <<- mrowki.stop_criterion
  
  mrowki.sudoku.tests(option=4)
  
  plot(1:length(mrowki.sudoku.tests.fill_levels),mrowki.sudoku.tests.fill_levels,type="l",col="blue",xlab="Iteracja",ylab="Wypelnienie",main="Wykres wypelnienia sudoku w zaleznosci od iteracji")
  
  # czyszczenie po testach
  mrowki.stop_criterion <<- mrowki.sudoku.tests.origin_stop_criterion
  mrowki.sudoku.tests.fill_levels <<- c()
  mrowki.sudoku.tests.iterations <<- -1
  #lines(x,y,col="red")
  #legend("bottomleft", legend = c("1 mrowka","2 mrowki"), col = 1:2, lty = c(1,1))
}
