
source('mht.r')

mrowki.iloscMrowek <- 3
mrowki.startoweFeromony <- 0.5

## funkcja zalezna od problemu
# na podstawie stanow odwiedzonych przez mrowki
# zaznacza w tablicy rownej wymiarom tej do feromonow
# przebyte wierzcholki
mrowki.zaznacz_odwiedzone()<-function(XS, stanZero){
  stop('Brak implementacji funkcji zaznaczajacej odwiedzone wierzcholki')
}

## funkcja zalezna od problemu
# inicjuje tablice z feromonami
mrowki.feromony_init<-function(startoweFeromony){
  stop('Brak implementacji funkcji inicjujacej feromony')
}

mrowki.model_init<-function(UG){
  model <- list(feromony = mrowki.feromony_init(mrowki.startoweFeromony),trwalosc_feromonu = 0.81)
  for (i in 1: length(model$feromony)){
    model%feromony[[i]] <- mrowki.startoweFeromony
  }
  
  return(model)
}

op_select<-function(XS,M, UG)
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
  przebyte <- mrowki.zaznacz_odwiedzone(XS, mrowki.init_state() )
  wielkosc_tablicy <- length(przebyte[[1]])
  for (i in 1: length(przebyte)){
    tablica <- table(przebyte[[i]])
    dlugosc_drogi <- tablica[names(tablica) == 1][[1]]
    procent <- wielkosc_tablicy / dlugosc_drogi
    przebyte[[i]] <- replace (przebyte[[i]], przebyte[[i]] == 1, procent)
    M$feromony <- M$feromony + przebyte[[i]]
  }
  
  M$feromony <- M$feromony * M$trwalosc_feromonu
  return(M)
}