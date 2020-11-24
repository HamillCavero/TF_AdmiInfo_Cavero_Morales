library(RMySQL)
library(DBI)
library(wakefield)
source("Credenciales.R")

if (dbCanConnect(driver=driver,host=host,port=port,user=user,password=password,dbname=dbname)) 
{
  conexion<-dbConnect(drv=driver,host=host,port=port,user=user,
                      password=password,dbname=dbname)
}
dbIsValid(conexion)

#Creación de Tablas para la Base de Datos
personas_unicas<-datos_violencia[!duplicated(datos_violencia$ID_PERSONA),]

personas<-data.frame(ID_PERSONAS=personas_unicas$ID_PERSONA,DNI_PERSONA=personas_unicas$PERSONA,PAIS=personas_unicas$pais_natal,ESTADO_CIVIL=personas_unicas$EST_CIVIL,SEXO=personas_unicas$SEXO,EDAD=personas_unicas$EDAD)
dbWriteTable(conexion,"Personas",personas)

#Creación de Tablas COMISARIA
comisarias_unicas<-datos_violencia[!duplicated(datos_violencia$ID_COMISARIA),]
comisaria<-data.frame(ID_COMISARIAS=comisarias_unicas$ID_COMISARIA,NOMBRE_COMISARIA=comisarias_unicas$COMISARIA,REGION_COMISARIA=comisarias_unicas$REGION,DEPARTAMENTO_COMISARIA=comisarias_unicas$DPTO_CIA,PROVINCIA_COMISARIA=comisarias_unicas$PROV_CIA
                      ,DISTRITO_COMISARIA=comisarias_unicas$DIST_CIA,UBICACION=comisarias_unicas$DIRECCION)

dbWriteTable(conexion,"Comisarias",comisaria)


#Creación de Tablas LIBRO

libros_unicos<-datos_violencia[!duplicated(datos_violencia$ID_LIBRO),]
libro<-data.frame(ID_LIBROS=libros_unicos$ID_LIBRO,NOMBRE_LIBRO=libros_unicos$LIBRO)
libro<-libro%>%group_by(ID_LIBROS,NOMBRE_LIBRO)%>%summarise(n())%>%select(c(1,2))
dbWriteTable(conexion,"Libros",libro)


#Creación de Tablas MODALIDAD
modalidad_unicos<-datos_violencia[!duplicated(datos_violencia$ID_MODALIDAD),]

modalidad<-data.frame(ID_MODALIDADES=modalidad_unicos$ID_MODALIDAD,NOMBRE_MODALIDAD=modalidad_unicos$MODALIDAD)
modalidad<-modalidad%>%group_by(ID_MODALIDADES,NOMBRE_MODALIDAD)%>%summarise(n())%>%select(c(1,2))

dbWriteTable(conexion,"Modalidad",modalidad)

#Creación de Tablas TIPO DENUNCIA
tipos_denun_unicos<-datos_violencia[!duplicated(datos_violencia$ID_TIPO_DENUNCIA),]
tipo_denuncia<-data.frame(ID_TIPO_DENUNCIA=tipos_denun_unicos$ID_TIPO_DENUNCIA,TIPO_DENUNCIA=tipos_denun_unicos$TIPO_DENUNCIA)
tipo_denuncia<-tipo_denuncia%>%group_by(ID_TIPO_DENUNCIA,TIPO_DENUNCIA)%>%summarise(n())%>%select(c(1,2))
tipo_denuncia$IDENTIFICADOR=c(1:NROW(tipo_denuncia))
dbWriteTable(conexion,"Tipo_Denuncia",tipo_denuncia)

#Creación de Tablas DENUNCIA
denuncia_unicas<-datos_violencia[!duplicated(datos_violencia$ID_DENUNCIA),]
denuncia<-data.frame(ID_DENUNCIA=denuncia_unicas$ID_DENUNCIA,FECHA_REGISTRO=denuncia_unicas$fec_registro,FECHA_HECHO=denuncia_unicas$FECHA_HORA_HECHO,DEPARTAMENTO_HECHO=denuncia_unicas$DPTO_HECHO,PROVINCIA_HECHO=denuncia_unicas$PROV_HECHO,DISTRITO_HECHO=denuncia_unicas$DIST_HECHO,UBICACION=denuncia_unicas$UBICACION,SIT_PERSONA=denuncia_unicas$SIT_PERSONA
                     ,FK_ID_PERSONAS=denuncia_unicas$ID_PERSONA,FK_ID_COMISARIA=denuncia_unicas$ID_COMISARIA,FK_ID_LIBROS=denuncia_unicas$ID_LIBRO,FK_ID_MODALIDAD=denuncia_unicas$ID_MODALIDAD,FK_ID_TIPO_DENUNCIA=tipo_denuncia$IDENTIFICADOR)

dbWriteTable(conexion,"Denuncia",denuncia)


#ASOCIANDO PKS AND  FKS #

dbExecute(conexion,sprintf("ALTER TABLE Personas ADD PRIMARY KEY(ID_PERSONAS)"))
dbExecute(conexion,sprintf("ALTER TABLE Comisarias ADD PRIMARY KEY(ID_COMISARIAS)"))
dbExecute(conexion,sprintf("ALTER TABLE Denuncia ADD PRIMARY KEY(ID_DENUNCIA)"))
dbExecute(conexion,sprintf("ALTER TABLE Libros ADD PRIMARY KEY(ID_LIBROS)"))
dbExecute(conexion,sprintf("ALTER TABLE Modalidad ADD PRIMARY KEY(ID_MODALIDADES)"))
dbExecute(conexion,sprintf("ALTER TABLE Tipo_Denuncia ADD PRIMARY KEY(IDENTIFICADOR)"))


dbExecute(conexion,sprintf("ALTER TABLE Denuncia ADD FOREIGN KEY (FK_ID_PERSONAS) REFERENCES Personas(ID_PERSONAS)"))


dbExecute(conexion,sprintf("ALTER TABLE Denuncia ADD FOREIGN KEY (FK_ID_COMISARIA) REFERENCES Comisarias(ID_COMISARIAS)"))


dbExecute(conexion,sprintf("ALTER TABLE Denuncia ADD FOREIGN KEY (FK_ID_LIBROS) REFERENCES Libros(ID_LIBROS)"))


dbExecute(conexion,sprintf("ALTER TABLE Denuncia ADD FOREIGN KEY (FK_ID_MODALIDAD) REFERENCES Modalidad(ID_MODALIDADES)"))

dbExecute(conexion,sprintf("ALTER TABLE Denuncia ADD FOREIGN KEY (FK_ID_TIPO_DENUNCIA) REFERENCES Tipo_Denuncia(IDENTIFICADOR)"))




