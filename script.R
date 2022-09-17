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
