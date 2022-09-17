library(sqldf)
library(tidyverse)
library(ggplot2)
library(priceR)

## 10 ref mas rechazadas 

data_1 <- sqldf('SELECT A.* FROM 
(SELECT Desc_Referencia as [Descripción Referencia] , SUM([Cantidad Lote]) AS counter, Resultado
FROM Consolidado_Base_de_datos_verificadores_2021 
WHERE Resultado = "No Conforme"
GROUP BY [Desc_Referencia]) A
ORDER BY A.counter DESC
LIMIT 10')
##View(data)

Grafica_1 <- ggplot(data_1, aes(x = reorder(`Descripción Referencia`, -counter), y=counter, fill =`Descripción Referencia`))+
  scale_y_continuous(limit = c(0,200000))+
  
  geom_col(position='dodge' ) +
  labs( title = "Diez Referencias más Rechazadas\n", x= "Referencia" , y ="Cantidad de lote" )+
  
  geom_text(aes(y = counter, label = counter), 
            position = position_dodge(width = 0.9), size=5.5, vjust=-1, hjust=0.5 ,col="black")+
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8), axis.title.x = element_text(size = 12),
    axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 10),
    plot.title = element_text(size = 10, face = "bold", color = "black",hjust = 0.5)
  )




##Cantidades de lotes , conformes y no conformes

data_2 <- sqldf('SELECT A.Resultado, ROUND( (Cantidad_lote / Total *1.0)*100,2) Porcentaje  FROM 
(SELECT Resultado , SUM ([Cantidad Lote] *1.0) AS Cantidad_lote
,(
SELECT SUM ([Cantidad Lote] *1.0) 
FROM Consolidado_Base_de_datos_verificadores_2021 
) AS Total

FROM Consolidado_Base_de_datos_verificadores_2021 
GROUP BY [Resultado]) A
')

#View (data_2)

#Pie
Grafica_2 <- ggplot(data_2,aes(x="",y=Porcentaje, fill=Resultado))+
  geom_bar(stat = "identity",
           color="white")+
  ggtitle("Cantidades de lotes conformes y no conformes")+
  geom_text(aes(label=paste(Porcentaje, "%")),
            position=position_stack(vjust=0.73),color="white",size=5)+
  coord_polar(theta = "y")
#--------------------------



##Disposicion de producto no conforme

data_3 <- sqldf('SELECT "Destruir (Moler o Fundir) " as Proceso, ROUND((A.[Destruir (Moler o Fundir)]/A.total*1.0)*100,2) as Porcentaje
FROM 
(SELECT SUM ([Destruir (Moler o Fundir)]) as [Destruir (Moler o Fundir)],
SUM( Reproceso) as Reproceso ,SUM( Liberación) as Liberación
, (    SUM ([Destruir (Moler o Fundir)]) +SUM( Reproceso) +SUM( Liberación)  ) as total

FROM Consolidado_Base_de_datos_verificadores_2021 ) A
UNION ALL
SELECT "Reproceso " as Proceso, ROUND((A.Reproceso / A.total*1.0)*100 ,2) as Porcentaje
FROM 
(SELECT SUM ([Destruir (Moler o Fundir)]) as [Destruir (Moler o Fundir)],
SUM( Reproceso) as Reproceso ,SUM( Liberación) as Liberación
, (    SUM ([Destruir (Moler o Fundir)]) +SUM( Reproceso) +SUM( Liberación)  ) as total

FROM Consolidado_Base_de_datos_verificadores_2021 ) A
UNION ALL
SELECT 
"Liberación " as Proceso, ROUND((A.Liberación/ A.total*1.0)*100,2) as Porcentaje
FROM 
(SELECT SUM ([Destruir (Moler o Fundir)]) as [Destruir (Moler o Fundir)],
SUM( Reproceso) as Reproceso ,SUM( Liberación) as Liberación
, (    SUM ([Destruir (Moler o Fundir)]) +SUM( Reproceso) +SUM( Liberación)  ) as total

FROM Consolidado_Base_de_datos_verificadores_2021 ) A')


#View(data_3)


#Pie
Grafica_3 <- ggplot(data_3,aes(x="",y=Porcentaje, fill=Proceso))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=paste(Porcentaje, "%")),
            position=position_stack(vjust=0.73),color="white",size=5)+
  coord_polar(theta = "y")
#---Cantidad de lotes Rechazados por mes

data_4 <- sqldf('
SELECT A.Mes, A.Cantidad_lote FROM (
 SELECT  [Fecha (dd-mm-año)] as Fecha ,Mes, COUNT([Cantidad Lote]) as Cantidad_lote FROM Consolidado_Base_de_datos_verificadores_2021
where Resultado = "No Conforme"
GROUP by MES
)A
order by A.Fecha
                
                ')
##View(data_4)


Grafica_4 <- ggplot(data_4, aes(x=fct_inorder(Mes), y=Cantidad_lote, group=1,label=Cantidad_lote)) +
  geom_line(color="skyblue",size=1.5)+
  ylab("Cantidad") +
  xlab("Mes") +
  ggtitle("Cantidad de lotes rechazados por mes")+
  theme(
    axis.text.x = element_text( vjust = 0.5, hjust=1, size = 8), axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 10),
    plot.title = element_text(size = 10, face = "bold", color = "black",hjust = 0.5)
  )+
  geom_point()+
  geom_text(nudge_y = 2)



#----------Cantidad de unidades de lotes rechazados por mes
data_5 <- sqldf('
SELECT A.Mes, A.Cantidad_lote FROM (
 SELECT  [Fecha (dd-mm-año)] as Fecha ,Mes, SUM ([Cantidad Lote]) as Cantidad_lote FROM Consolidado_Base_de_datos_verificadores_2021
where Resultado = "No Conforme"
GROUP by MES
)A
order by A.Fecha')
#View(data_5)


Grafica_5 <- ggplot(data_5, aes(x=fct_inorder(Mes), y=Cantidad_lote, group=1,label=Cantidad_lote)) +
  geom_line(color="#FF9C04",size=1.5)+
  ylab("Cantidad") +
  xlab("Mes") +
  ggtitle("Cantidad de unidades de lotes rechazados por mes")+
  theme(
    axis.text.x = element_text( vjust = 0.5, hjust=1, size = 8), axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 10),
    plot.title = element_text(size = 10, face = "bold", color = "black",hjust = 0.5)
  )+
  geom_point()+
  geom_text(nudge_x = 0.25, nudge_y = 0.25, 
            check_overlap = T)


#-------------COSTO POR DESTRUCCION DEL MATERIAL Y	COSTO POR REPROCESO DEL MATERIAL (MO)	-------------


data_6 <- sqldf('SELECT  "Perdida Destruccion " as "Perdida",   ROUND (SUM ([Perdida Destruccion])) AS "Total" from Consolidado_Base_de_datos_verificadores_2021
WHERE Resultado="No Conforme"
UNION ALL
SELECT "Perdida por reproceso " as "Perdida", ROUND ( SUM ([Perdida por reproceso])) AS "Total" from Consolidado_Base_de_datos_verificadores_2021
WHERE Resultado="No Conforme"')
#View(data_6)




Grafica_6 <- ggplot(data_6, aes(x = reorder(Perdida, -Total), y= Total, fill =Perdida))+
  scale_y_continuous(limit = c(0,44000000))+
  
  geom_col(position='dodge',width = .5 ) +
  labs( title = "Costo por destrucción del material y perdida por reproceso\n", x= "Perdida" , y ="Costo" )+
  
  geom_text(aes(y =  Total, label = format_dollars(Total,1)), 
            position = position_dodge(width = 0.9), size=5.5, vjust=-1, hjust=0.5 ,col="black")+
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8), axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 10),
    plot.title = element_text(size = 10, face = "bold", color = "black",hjust = 0.5)
  )


data_7 <- sqldf('SELECT A.Defecto, ROUND( (A.counter / A.total *1.0)*100,2) AS Porcentaje FROM
(SELECT Defecto, SUM([Cantidad Lote]) AS counter,
(SELECT SUM([Cantidad Lote]) AS counter
FROM Consolidado_Base_de_datos_verificadores_2021
WHERE Resultado = "No Conforme") total
FROM Consolidado_Base_de_datos_verificadores_2021
WHERE Resultado = "No Conforme"
GROUP BY Defecto)A
ORDER BY A.counter DESC
LIMIT 10')
View(data_7)

Grafica_7 <- ggplot(data_7,aes(x="",y=Porcentaje, fill=Defecto))+
  geom_bar(stat = "identity",
           color="white")+
  
  ggtitle("Diez causas principales de rechazo")+
  geom_text(aes(label=paste(Porcentaje, "%")),
            position=position_stack(vjust=0.5),color="white",size=5)+
  coord_polar(theta = "y")
#-----------------------

Grafica_1

Grafica_2

Grafica_3

Grafica_4

Grafica_5

Grafica_6

Grafica_7
#--------


