library(corrplot)
library(ggplot2)
source("Transformacion.R")
#grafico del q2
qplot(x=Modalidad,y=Cantidad_Casos,data = q2,geom = "col")+aes(fill= Modalidad)

#grafico del q3
qplot(x=Total,y=DPTO_CIA,data = q3,geom = "col")+aes(fill= DPTO_CIA)

#grafico del q4
qplot(x=SIT_PERSONA,y=Total,data = q4%>%slice(1:5),geom = "col")+aes(fill= SIT_PERSONA)

#grafico del q5
qplot(x=pais_natal  ,y=Total,data = q5%>%slice(1:5),geom = "col")+aes(fill= pais_natal)

#grafico del q6
qplot(x=EDAD   ,y=Total,data = q6,geom = "line")+aes(fill= EDAD )

#grafico del q7
 ggplot(data = q7, aes(x="", y=q7$Total, fill=SEXO)) +
   geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+theme_void()

#grafico del q8
qplot(x=DIST_CIA   ,y=Total,data = q8,geom = "col")+aes(fill= DIST_CIA )

#grafico del q9
ggplot(data = q9, aes(x="", y= Total, fill= DERIVADA_FISCALIA)) +
  geom_bar(stat="identity", width=1,color="white") +
  coord_polar("y", start=0)+theme_void()

#grafico del q10
qplot(x=SIT_PERSONA  ,y=Total,data = q10,geom = "col")+aes(fill= SEXO)

#grafico del q11
qplot(x= MES  ,y=Total,data = q11,geom = "col")+aes(fill= MES)+theme_minimal()

#grafico del q12
qplot(x=EST_CIVIL,y=Total,data = q12)+aes(fill= EST_CIVIL ) +
  geom_point(size=4, color="#aaaaaa") +
  theme_minimal()+
  theme(legend.position = "none") +
  geom_text(aes(label=q12$Total),hjust=0, vjust=0)

#grafico del q13

qplot(x=Tipo   ,y=Area,data = q13,geom = "boxplot")+theme_classic()+xlab(label = "")

#grafico del q15
qplot(x= Edad  ,y= total,data = q15,geom = "line")+aes(fill= total)

#grafico del q17
ggplot(data = q17, aes(x="", y= tot, fill= TIPO_DENUNCIA)) +
  geom_bar(stat="identity", width=1,color="white") +
  coord_polar("y", start=0)+theme_void()+
   geom_text(aes(label = paste(round(tot, digits = 2)
, "%"), x = 1.3),
            position = position_stack(vjust = 0.5))

