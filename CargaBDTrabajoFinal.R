library(RMySQL)
library(DBI)
library(wakefield)
source("Credenciales.R")
source("RecoleccionDatos.R")

if (dbCanConnect(driver=driver,host=host,port=port,user=user,password=password,dbname=dbname)) 
{
  conexion<-dbConnect(drv=driver,host=host,port=port,user=user,
                      password=password,dbname=dbname)
}
conexion

#Creación de Tablas para la Base de Datos
personas<-data.frame(ID_PERSONAS=datos_violencia$ID_PERSONA,DNI_PERSONA=datos_violencia$PERSONA,PAIS=datos_violencia$pais_natal,ESTADO_CIVIL=datos_violencia$EST_CIVIL,SEXO=datos_violencia$SEXO,EDAD=datos_violencia$EDAD)
dbWriteTable(conexion,"Personas",personas)


#Creación de Tablas COMISARIA
comisaria<-data.frame(ID_COMISARIAS=datos_violencia$ID_COMISARIA,NOMBRE_COMISARIA=datos_violencia$COMISARIA,REGION_COMISARIA=datos_violencia$REGION,DEPARTAMENTO_COMISARIA=datos_violencia$DPTO_CIA,PROVINCIA_COMISARIA=datos_violencia$PROV_CIA
                      ,DISTRITO_COMISARIA=datos_violencia$DIST_CIA,UBICACION=datos_violencia$DIRECCION)

dbWriteTable(conexion,"Comisarias",comisaria)

#Creación de Tablas LIBRO
libro<-data.frame(ID_LIBROS=datos_violencia$ID_LIBRO,NOMBRE_LIBRO=datos_violencia$LIBRO)
dbWriteTable(conexion,"Libros",libro)

#Creación de Tablas MODALIDAD
modalidad<-data.frame(ID_MODALIDADES=datos_violencia$ID_MODALIDAD,NOMBRE_MODALIDAD=datos_violencia$MODALIDAD)
modalidad<-modalidad%>%group_by(ID_MODALIDADES,NOMBRE_MODALIDAD)%>%summarise(n())%>%select(c(1,2))

dbWriteTable(conexion,"Modalidad",modalidad)

#Creación de Tablas TIPO DENUNCIA
tipo_denuncia<-data.frame(ID_TIPO_DENUNCIA=datos_violencia$ID_TIPO_DENUNCIA,TIPO_DENUNCIA=datos_violencia$TIPO_DENUNCIA)
tipo_denuncia<-tipo_denuncia%>%group_by(ID_TIPO_DENUNCIA,TIPO_DENUNCIA)%>%summarise(n())%>%select(c(1,2))

dbWriteTable(conexion,"Tipo_Denuncia",tipo_denuncia)

#Creación de Tablas DENUNCIA
denuncia<-data.frame(ID_DENUNCIA=datos_violencia$ID_DENUNCIA,FECHA_REGISTRO=datos_violencia$fec_registro,FECHA_HECHO=datos_violencia$FECHA_HORA_HECHO,DEPARTAMENTO_HECHO=datos_violencia$DPTO_HECHO,PROVINCIA_HECHO=datos_violencia$PROV_HECHO,DISTRITO_HECHO=datos_violencia$DIST_HECHO,UBICACION=datos_violencia$UBICACION,SIT_PERSONA=datos_violencia$SIT_PERSONA)

dbWriteTable(conexion,"Denuncia",denuncia)



