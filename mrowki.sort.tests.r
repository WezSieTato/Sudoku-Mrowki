source("mrowki.sort.r")

mrowki.sort.test <- function(problem_size, ant_number, pheromon_degradation, start_pheromon){
  task <- runif(problem_size, -10.0, 10.0)
  mrowki.antNumber <<- ant_number
  mrowki.pheromonDegradation <<- pheromon_degradation
  mrowki.startPheromon <<- start_pheromon
  print('Sortujemy!')
  mrowki.sort(task)
  print('==============')
  print(mrowki.task[mrowki.solution])
  
  costs <- c()
  
  for(i in mrowki.vertices){
    costs[[length(costs)+1]] <- sort.cost(i$board)
  }
  
  plot(costs)
  
}

mrowki.sort.test(10, 2, 0.9, 0.5)