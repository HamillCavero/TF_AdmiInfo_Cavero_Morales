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
  return(q1<-df%>%filter(year(df$fec_registro)==anho & df$MODALIDAD ==modalidad)%>%summarise(Total=n()))
  }
  else
  {
  return(q1<-df%>%filter(year(df$fec_registro)==anho & df$MODALIDAD ==modalidad)%>%select(c(3,20,22,28,14,19))%>%summarise(Total=n()))
  }
}  
q1<-casos_anhos_modalidad(datos_violencia,2017,"VIOLENCIA FISICA",FALSE)

#2) Cantidad de casos por cada tipo de modalidad #
casos_totales_por_modalidad<-function(df)
{
  return(q2<-df%>%group_by(Modalidad=df$MODALIDAD)%>%summarise(Cantidad_Casos=n()))  
}
q2<-casos_totales_por_modalidad(datos_violencia)

#3)Conteo de tipos de denuncias dado en cada comisaria

group_by_denuncias<-function(df)
{
    return(df%>%group_by(Denuncia=TIPO_DENUNCIA,Region_Policial=REGION,Comisaria=COMISARIA)%>%summarise(Total=n()))
}

q3<-group_by_denuncias(temporal)

#Modificando el pais natal de forma correcta#  #REMOVER AYCUCHO#

PAISES<-temporal$pais_natal <- gsub(x=temporal$pais_natal,"ALEMANIAIA","ALEMANIA")
PAISES<-temporal$pais_natal <- gsub(x=temporal$pais_natal,"ALEMAN","ALEMANIA")
PAISES<-temporal$pais_natal <- gsub(x=temporal$pais_natal,"GRECIA   ALEMAN","ALEMANIA")

PAISES<-temporal$pais_natal <- gsub(x=temporal$pais_natal,"ARGENTINO","ARGENTINA")
PAISES<-temporal$pais_natal <- gsub(x=temporal$pais_natal,"RGENTINA Y PERUANA","ARGENTINA")
PAISES<-temporal$pais_natal <- gsub(x=temporal$pais_natal,"AARGENTINA","ARGENTINA")

PAISES<-temporal$pais_natal <- gsub(x=temporal$pais_natal,"ASTURIA  ESPAÃ‘A","ESPANHA")
PAISES<-temporal$pais_natal <- gsub(x=temporal$pais_natal,"ESPAÃ‘A","ESPANHA")
PAISES<-temporal$pais_natal <- gsub(x=temporal$pais_natal,"MADRID ESPANHA","ESPANHA")
PAISES<-temporal$pais_natal <- gsub(x=temporal$pais_natal,"ESPAÃ‘OL","ESPANHA")
PAISES<-temporal$pais_natal <- gsub(x=temporal$pais_natal,"ESPANHA VILLARREAL","ESPANHA")


PAISES<-temporal$pais_natal <- gsub(x=temporal$pais_natal,"BELGA","BELGICA")

PAISES<-temporal$pais_natal <- gsub(x=temporal$pais_natal,"COLOMBIANO","COLOMBIA")
PAISES<-temporal$pais_natal <- gsub(x=temporal$pais_natal,"COLOMBIANA","COLOMBIA")
PAISES<-temporal$pais_natal <- gsub(x=temporal$pais_natal,"COLOMBIA BOGOTA","COLOMBIA")
PAISES<-temporal$pais_natal <- gsub(x=temporal$pais_natal,"BOGOTA COLOMBIA","COLOMBIA")





View(PAISES)
#4)Cantidad de personas según la relación con la denuncia#






