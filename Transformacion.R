source("RecoleccionDatos.R")
source("PreprocesamientoDatos.R")

library(dplyr)

datos_violencia<-limpiar_violencia(datos_violencia)
datos_violencia<-conversion_fecha(datos_violencia)

glimpse(datos_violencia)

#1)Cantidad de casos registrados por año y que sean de modalidad violencia fisica#
temporal<-datos_violencia

casos_anhos_modalidad<-function(df,anho,modalidad,totdatos)
{
  #Se le podria poner un if si quieres que se guarde con todos los datos o no.
  if(totdatos==TRUE)
  {
  return(q1<-df%>%filter(year(df$fec_registro)==anho & df$MODALIDAD ==modalidad))
  }
  else
  {
  return(q1<-df%>%filter(year(df$fec_registro)==anho & df$MODALIDAD ==modalidad)%>%select(c(3,20,22,28,14,19)))

  }
}  
q1<-casos_anhos_modalidad(datos_violencia,2017,"VIOLENCIA FISICA",FALSE)

#Cantidad de casos por cada tipo de modalidad #
casos_totales_por_modalidad<-function(df)
{
  return(q2<-df%>%group_by(Modalidad=df$MODALIDAD)%>%summarise(Cantidad_Casos=n()))  
}
q2<-casos_totales_por_modalidad(datos_violencia)
