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
ggplot(data = q7, aes(x="", y=q7$Total, fill=q7$SEXO)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

#grafico del q8
qplot(x=q8$DIST_CIA   ,y=Total,data = q8,geom = "col")+aes(fill= q8$DIST_CIA )


RColorBrewer::display.brewer.all() 

