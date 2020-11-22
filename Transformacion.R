source("RecoleccionDatos.R")
source("PreprocesamientoDatos.R")

library(dplyr)

datos_violencia<-limpiar_violencia(datos_violencia)
datos_violencia<-conversion_fecha(datos_violencia)
datos_violencia<-limpieza_paises(datos_violencia)

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
  return(q1<-df%>%filter(year(df$fec_registro)==anho & df$MODALIDAD ==modalidad)%>%group_by(MODALIDAD,fec_registro)%>%summarise(Total=n()))
    #%>%select(c(3,20,22,28,14,19))
  }
}  
q1<-casos_anhos_modalidad(datos_violencia,2017,"VIOLENCIA FISICA",FALSE)

#2) Cantidad de casos por cada tipo de modalidad #
casos_totales_por_modalidad<-function(df)
{
  return(q2<-df%>%group_by(Modalidad=df$MODALIDAD)%>%summarise(Cantidad_Casos=n()))  
}
q2<-casos_totales_por_modalidad(datos_violencia)

#3)Conteo de tipos de denuncias por departamento

group_by_denuncias<-function(df)
{
    return(df%>%group_by(DPTO_CIA)%>%summarise(Total=n()))%>%arrange(desc(Total))
}

q3<-group_by_denuncias(temporal)


#4)Cantidad de personas según la relación con la denuncia#

relacion_denuncia<-function(df,cant_top)
{
  q<-df%>%group_by(SIT_PERSONA)%>%summarise(Total=n())%>%arrange(desc(Total))
  if(cant_top==0)
  {
    return(q[,c(1,2)])
  }
  else{
    return(q[c(1:cant_top),c(1,2)])
    
  }
}

q4<-relacion_denuncia(temporal,0)

#5 Cantidad de casos segun paises

cant_paises<-function(df,cant_top)
{
  q<-df%>%group_by(pais_natal)%>%summarise(Total=n())%>%arrange(desc(Total))
  if(cant_top==0)
  {
    return(q[,c(1,2)])
  }
  else{
  return(q[c(1:cant_top),c(1,2)])

  }
}
q5<-cant_paises(temporal,0)

