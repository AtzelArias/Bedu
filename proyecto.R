#Leer Data Frames
data.crm <- read.csv("Tabla-CRM.csv")
data.fb <- read.csv("Tabla-FACEBOOK.csv")
head (data.crm)
str(data.crm)
summary(data.crm)
str(data.fb)

#Seleccionar las columnas que me interesaban
status.motivo <- select(data.crm, Estatus, Motivo_Compra)
head(status.motivo)

status.asesor <- select(data.crm, Fecha_Creacion, Estatus, Asesor_Asignado)
head(tatus.asesor)

#Obtener los % de cada Estatus en el Motivo "Habitar"
n.status <- status.motivo %>% group_by(Motivo_Compra) %>% count(n=Estatus)
habitar <- n.status [n.status$Motivo_Compra=="Habitar",]
habitar$Porc <- prop.table(habitar$nn,2)
habitar <- rename(habitar, Estatus = n, Leads = nn)
habitar

#Obtener leads calificados por cada asesor
n.asesores <- status.asesor %>% group_by(Asesor_Asignado) %>% count(n=Estatus)
n.asesores <- rename(n.asesores, Estatus = n, Cantidad_Leads = nn)
calif.asesores <- n.asesores [n.asesores$Estatus=="Calificado",]
calif.asesores.lipm <- select(calif.asesores, Asesor_Asignado, Cantidad_Leads)

summary(calif.asesores.lipm)

#Graficar los leads calificados por asesor
plot(calif.asesores.lipm$Cantidad_Leads,
     ylab = "Leads Calificados",
     xlab = "Asesores")

grap.ase.calif <- ggplot(data = calif.asesores.lipm) +
  geom_point(aes(calif.asesores.lipm$Cantidad_Leads, calif.asesores.lipm$Asesor_Asignado))

install.packages("plotly")
library(plotly)

ggplotly(grap.ase.calif)

#Grafica treemap
install.packages("treemapify")
library(treemapify)

grap.ase.calif.tree <- ggplot(data = calif.asesores.lipm) +
  geom_treemap(aes(area = calif.asesores.lipm$Cantidad_Leads, fill = calif.asesores.lipm$Asesor_Asignado))

ggplotly(grap.ase.calif.tree)








#Fallidos

dt_utc <- ymd_hm("2010-08-03 00:50")
exp.status.asesor <- mutate(status.asesor, Fecha_Creacion = as.POSIXct(Fecha_Creacion, "%d/%m/%Y %H:%M"))










