library(sqldf)
library(ggplot2)

data <- sqldf('SELECT  *  FROM Consolidado_Base_de_datos_verificadores_2021')
View(data)


## 10 ref mas rechazadas 

data_1 <- sqldf('SELECT A.* FROM 
(SELECT Desc_Referencia, SUM([Cantidad Lote]) AS counter, Resultado
FROM Consolidado_Base_de_datos_verificadores_2021 
WHERE Resultado = "No Conforme"
GROUP BY [Desc_Referencia]) A
ORDER BY A.counter DESC
LIMIT 10')
View(data)

ggplot(data_1, aes(x = reorder(Desc_Referencia, -counter), y=counter, fill =Desc_Referencia))+
  scale_y_continuous(limit = c(0,200000))+
  
  geom_col(position='dodge' ) +
  labs( title = "10 Referencias mas Rechazadas\n", x= "Referencia" , y ="Cantidad de lote" )+
  
  geom_text(aes(y = counter, label = counter), 
            position = position_dodge(width = 0.9), size=5.5, vjust=-1, hjust=0.5 ,col="black")+
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8), axis.title.x = element_text(size = 12),
    axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 8),
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
##View (data_2)

#Pie
ggplot(data_2,aes(x="",y=Porcentaje, fill=Resultado))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=paste(Porcentaje, "%")),
            position=position_stack(vjust=0.73),color="white",size=5)+
  coord_polar(theta = "y")
#--------------------------

##Barras

ggplot(data_2, aes(x = reorder(Resultado, -Cantidad_lote), y=Cantidad_lote, fill =Resultado))+
  scale_y_continuous(limit = c(0,6700000))+
  
  geom_col(position='dodge' ) +
  labs( title = "Cantidad de Lotes Revisados\n", x= "Resultado" , y ="Cantidad de lote" )+
  
  geom_text(aes(y = Cantidad_lote, label = Cantidad_lote), 
            position = position_dodge(width = 0.9), size=5.5, vjust=-1, hjust=0.5 ,col="black")+
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8), axis.title.x = element_text(size = 12),
    axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 8),
    plot.title = element_text(size = 10, face = "bold", color = "black",hjust = 0.5)
  )

#------------------
