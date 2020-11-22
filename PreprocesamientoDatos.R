limpiar_violencia<-function(dv){
  dv<-dv[,-c(16,17,18,19,42,43,44,30,28)]
  return (dv)
}
datos_violencia<-limpiar_violencia(datos_violencia)
#BORRAR ID NIVEL EDUCATIVO,nivel educativo,ocupación,CUADRA
#ID MATERIA Y MATERIA ID TIPO, TIPO,VIA,