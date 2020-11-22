library(lubridate)

limpiar_violencia<-function(df)
{
  return(df[,-c(16,17,18,19,42,43,44,30,28)])
}

conversion_fecha<-function(df)
{
  #Modificando las fechas para poder trabajar con ellas
  
  df$FECHA_HORA_HECHO<-strptime(df$FECHA_HORA_HECHO,"%m/%d/%Y")
  
  df$fec_registro<-parse_date_time2(df$fec_registro,orders="dmy")
  
  return(df)
}

#BORRAR ID NIVEL EDUCATIVO,nivel educativo,ocupación,CUADRA
#ID MATERIA Y MATERIA ID TIPO, TIPO,VIA,