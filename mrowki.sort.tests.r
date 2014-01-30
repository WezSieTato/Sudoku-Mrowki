source("mrowki.sort.r")

mrowki.sort.test <- function(problem_size, ant_number, pheromon_degradation, start_pheromon){
 
  ilosc_powtorzen <- 3
  results <- list()
  for(i in 1 : ilosc_powtorzen){
    task <- runif(problem_size, -10.0, 10.0)
    mrowki.antNumber <<- ant_number
    mrowki.pheromonDegradation <<- pheromon_degradation
    mrowki.startPheromon <<- start_pheromon
    print('Sortujemy!')
    mrowki.sort(task)
    print('==============')
    print(mrowki.task[mrowki.solution])
    
    costs <- c()
    
    for(k in mrowki.vertices){
      costs[length(costs)+1] <- sort.cost(k$board)
    }
    results[[i]] <- costs
  }
  
  maxLength <- -1
  maxIndex <- -1
  for(i in 1:length(results)) {
    if(length(results[[i]]) > maxLength) {
      maxLength <- length(results[[i]])
      maxIndex <- i
    }
  }
  
  colorki <- sample(colours(), ilosc_powtorzen)
  colorki[[1]] <- "blue"
  
  plot(1:length(results[[maxIndex]]),results[[maxIndex]],type="l",col="blue",xlab="Numer wierzcholka",ylab="Funkcja celu",ylim=c(0,20), xlim=c(1,length(results[[maxIndex]])),main="Funkcja celow w generowanych wierzcholkach")
  for(i in 1: ilosc_powtorzen) {
    if(i != maxIndex) {
      lines(1:length(results[[i]]),results[[i]],col=colorki[[i]])
    }
  }
  
  plot(1:length(results[[maxIndex]]),results[[maxIndex]],type="b",col="blue",xlab="Numer wierzcholka",ylab="Funkcja celu",ylim=c(0,20), xlim=c(1,100),main="Funkcja celow w wierzcholkach [1, 100]")
  for(i in 1: ilosc_powtorzen) {
    if(i != maxIndex) {
      lines(1:length(results[[i]]),results[[i]],type="b",col=colorki[[i]])
    }
  }
  
  plot(results[[maxIndex]], type = "o", col = colorki[[1]], xlab="Numer wierzcholka",ylab="Funkcja celu",ylim=c(0,20), xlim=c(1,100),main="Funkcja celow w wierzcholkach najdluzszego przebiegu [1, 100]")
}
