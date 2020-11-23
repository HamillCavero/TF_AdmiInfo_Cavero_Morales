source("RecoleccionDatos.R")
source("PreprocesamientoDatos.R")

library(dplyr)

datos_violencia<-limpiar_violencia(datos_violencia)
datos_violencia<-conversion_fecha(datos_violencia)
datos_violencia<-limpieza_paises(datos_violencia)
#ELIMINANDO LOS VALORES NA
datos_violencia<-borrar_na(datos_violencia)
datos_violencia<-limpiar_edades(datos_violencia)

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
  return(df%>%group_by(DPTO_CIA)%>%summarise(Total=n())%>%arrange(desc(Total)))
}

q3<-group_by_denuncias(datos_violencia)


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

q4<-relacion_denuncia(datos_violencia,0)

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

q5<-cant_paises(datos_violencia,0)


#6 Cantidad de casos por edad
casos_Edad<-function(df)
{
  return(df%>%group_by(EDAD)%>%summarise(Total=n())%>%arrange(desc(Total)))
}

q6<-casos_Edad(datos_violencia)

#7 Cantidad de casos por sexo

casos_sexo<-function(df)
{
  return(df%>%group_by(SEXO)%>%summarise(Total=n())%>%arrange(desc(Total)))
}
q7<-casos_sexo(datos_violencia)


#8 Cantidad de casos registrados por Distrito top
casos_distritos<-function(df,cant_top)
{
  q<-df%>%group_by(DIST_CIA)%>%summarise(Total=n())%>%arrange(desc(Total))
  
  if(cant_top==0)
  {
    return(q[,c(1,2)])
  }
  else{
    return(q[c(1:cant_top),c(1,2)])
    
  }
}

q8<-casos_distritos(datos_violencia,5)

#9 Fiscalia Derivadas
fiscalias<-function(df)
{
  return(df%>%group_by(DERIVADA_FISCALIA)%>%filter(DERIVADA_FISCALIA!="NULL")%>%summarise(Total=n())%>%arrange(desc(Total)))
}
q9<-fiscalias(datos_violencia)

#10 cantidad de denunciantes y denuncoados separados por sexos

situcion_persona_por_Sexo<-function(df)
{
  return(df%>%group_by(SIT_PERSONA,SEXO)%>%filter(SIT_PERSONA=="DENUNCIANTE" |SIT_PERSONA=="DENUNCIADO")%>%summarise(Total=n())%>%arrange(desc(Total)))
}
q10<-situcion_persona_por_Sexo(datos_violencia)

#11 Mes en la que hubo mayor cantidad de violencia,denuncia

mayor_mes<-function(df)
{
  return(df%>%group_by(MES)%>%summarise(Total=n())%>%arrange(desc(Total)))
}

q11<-mayor_mes(datos_violencia)

#12 Cantidad casos por estado civil

estado_civil<-function(df)
{
  return(df%>%group_by(EST_CIVIL)%>%filter(EST_CIVIL!="NULL")%>%summarise(Total=n())%>%arrange(desc(Total)))
}

q12<-estado_civil(datos_violencia)

#13 Busqueda por tipo de semilla, con mayor area

tipo_semilla<-function(df,area)
{
  dv<-df%>%group_by(Tipo,Area)%>%filter(Area>area)%>%select(c(1,8))
  return(dv)
}
q13<-tipo_semilla(semillas,8)




