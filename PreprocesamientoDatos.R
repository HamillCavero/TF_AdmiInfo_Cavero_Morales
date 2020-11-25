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
borrar_na<-function(df)
{
  dv<-na.omit(df)
  return(dv)  
}
limpiar_edades<-function(df)
{
  temporal<-df%>%filter(EDAD>8 & EDAD<101 & PERSONA>9999 & ID_PERSONA>9999)  
  return(temporal)
}

#Modificando el pais natal de forma correcta#  #REMOVER AYCUCHO# REMOVER REVENTANDOTE,SOLTERO(A),SUDAFRICANA,BORRAR VACIAS

limpieza_paises<-function(df){
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"ALEMANIAIA","ALEMANIA")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"ALEMAN","ALEMANIA")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"GRECIA   ALEMAN","ALEMANIA")
  
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"ARGENTINO","ARGENTINA")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"RGENTINA Y PERUANA","ARGENTINA")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"AARGENTINA","ARGENTINA")
  
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"ASTURIA  ESPAÃ‘A","ESPANHA")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"ESPAÃ‘A","ESPANHA")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"MADRID ESPANHA","ESPANHA")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"ESPAÃ‘OL","ESPANHA")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"ESPANHA VILLARREAL","ESPANHA")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"MALTA","ESPANHA")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"MARRUECOS","ESPANHA")
  
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"BELGA","BELGICA")
  
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"COLOMBIANO","COLOMBIA")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"COLONBIA","COLOMBIA")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"COOMBIA","COLOMBIA")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"COLOMBIANO","COLOMBIA")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"COLOMBIANA","COLOMBIA")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"COLOMBIA BOGOTA","COLOMBIA")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"BOGOTA COLOMBIA","COLOMBIA")
  
  
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"BOLIVIANO","BOLIVIA")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"BOLIVIANA","BOLIVIA")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"NACIONALIDADA BOLIVIA","BOLIVIA")
  
  
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"BRASIL","BRAZIL")
  
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"BRITANICO","REINO UNIDO")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"INGLATERRA","REINO UNIDO")
  
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"CANADIENSE","CANADA")
  
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"CHACHAPOYAS","PERU")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"AYCUCHO","PERU")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"LIMA","PERU")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"PERUANO","PERU")
  
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"CHILENA","CHILE")
  
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"DOMICANO","REPUBLICA DOMICANA")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"DOMICANA","REPUBLICA DOMICANA")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"RPUBLICA REPUBLICA DENOMICANA","REPUBLICA DOMICANA")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"REPUBLICA DOMENICANA","REPUBLICA DOMICANA")
  
  
  
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"ECUATORIANA","ECUADOR")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"ECUATORIANO","ECUADOR")
  
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"EE UU","ESTADOS UNIDOS")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"EEUU","ESTADOS UNIDOS")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"FLORIDA  ESTADOS UNIDOS","ESTADOS UNIDOS")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"ESTADOS UNIDOS DE NORTE A","ESTADOS UNIDOS")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"ESTADO UNIDOS","ESTADOS UNIDOS")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"USA","ESTADOS UNIDOS")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"RESIDE EN USA","ESTADOS UNIDOS")
  
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"FRANCES","FRANCIA")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"FRANCESA","FRANCIA")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"FRANCIAA","FRANCIA")
  
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"ISRAELI","ISRAEL")
  
  
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"ITALIANO","ITALIA")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"ITALIANA","ITALIA")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"ITALIA  MILAN","ITALIA")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"VILLARICCA   ITALIA","ITALIA")
  
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"RUMANA","ROMA")
  
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"VENEZOLANO","VENEZUELA")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"VENEZOLANA","VENEZUELA")
  PAISES<-df$pais_natal <- gsub(x=df$pais_natal,"VENUEZUELA","VENEZUELA")


  PAISES<-df[-c(10533,16253,20100,41550,42717,60593,65142,71992,74298,92126,110947,117632,129027,137612,42333,152052,154612,162271,30076),]
  
  return(PAISES)
}
