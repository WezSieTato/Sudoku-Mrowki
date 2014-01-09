
source('mht.r')

mrowki.iloscMrowek <- 1
mrowki.startoweFeromony <- 0.5

## funkcja zalezna od problemu
# na podstawie stanow odwiedzonych przez mrowki
# zaznacza w tablicy rownej wymiarom tej do feromonow
# przebyte wierzcholki
mrowki.zaznacz_odwiedzone<-function(XS, stanZero){
  stop('Brak implementacji funkcji zaznaczajacej odwiedzone wierzcholki')
}

## funkcja zalezna od problemu
# inicjuje tablice z feromonami
mrowki.feromony_init<-function(startoweFeromony){
  stop('Brak implementacji funkcji inicjujacej feromony')
}

mrowki.op_init<-function(UG)
{
  stop('Brak implementacji funkcji inicjujacej stan poczatkowy')
}


mrowki.op_generate<-function(XS,M,UG)
{
  stop('Brak implementacji funkcji generujacej stany')
}

mrowki.stop_criterion<-function(XS, M)
{
  stop('Brak implementacji funkcji stopu')
}

mrowki.model_init<-function(UG){
  model <- list(feromony = mrowki.feromony_init(mrowki.startoweFeromony),trwalosc_feromonu = 0.81)
  return(model)
}

mrowki.op_select<-function(XS,M, UG)
{
  if ((length(XS) - 1) %% mrowki.iloscMrowek != 0 )
    return(list())
  return(pop(mrowki.iloscMrowek))
}

mrowki.model_update<-function(XS,M)
{
  if(length(XS) == 0){
    return(M)
  }
  
  #lista tablic z przebytymi sciezkami
  przebyte <- mrowki.zaznacz_odwiedzone(XS, mrowki.op_init() )
  wielkosc_tablicy <- length(przebyte[[1]])
  for (i in 1: length(przebyte)){
    tablica <- table(przebyte[[i]])
    dlugosc_drogi <- wielkosc_tablicy - tablica[names(tablica) == 0][[1]]
    procent <-  dlugosc_drogi / wielkosc_tablicy
    przebyte[[i]] <- replace (przebyte[[i]], przebyte[[i]] == 1, procent)
    M$feromony <- M$feromony + przebyte[[i]]
  }
  
  M$feromony <- M$feromony * M$trwalosc_feromonu
  M$feromony <- replace(M$feromony, M$feromony < 0.1, 0.1)
  return(M)
}

mrowki.search<-function(){
  search(mrowki.model_init, mrowki.model_update, mrowki.op_init, mrowki.op_select, mrowki.op_generate, mrowki.stop_criterion, UG)
  
  return(pop(1)[[1]])
}