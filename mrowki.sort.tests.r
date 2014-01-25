source("mrowki.sort.r")

mrowki.sort.test <- function(problem_size, ant_number, pheromon_degradation){
  task <- runif(problem_size, -10.0, 10.0)
  mrowki.antNumber <<- ant_number
  mrowki.pheromonDegradation <<- pheromon_degradation
  print('Sortujemy!')
  mrowki.sort(task)
  print('==============')
  print(mrowki.task[mrowki.solution])
  
}

mrowki.sort.test(5, 1, 0.3)