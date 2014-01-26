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
  
}

mrowki.sort.test(10, 1, 0.8, 0.5)