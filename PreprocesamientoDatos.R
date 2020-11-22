limpiar_violencia<-function(){
  datos_violencia<-datos_violencia[,-c(16,17,18,19,42,43,44,30,28)]
  colnames(datos_violencia)
  View(datos_violencia)
}

#BORRAR ID NIVEL EDUCATIVO,nivel educativo,ocupación,CUADRA
#ID MATERIA Y MATERIA ID TIPO, TIPO,VIA,