install.packages("NbClust")
install.packages("cluster")
install.packages("e1071")
install.packages("mclust")
install.packages("fpc")
install.packages("factoextra")

library(cluster) 
library(e1071)
library(mclust) 
library(fpc) 
library(NbClust) 
library(factoextra)
library("dplyr")
library(readxl)
library(lattice)
library(broom)
library(ggpubr)
library(ggplot2)
library(tidyverse)
library(reshape2)




X2010 <- read_excel("db mineria/2010.xls", 
                    sheet = "Mes de registro y departamento", 
                    skip = 1)
X2011 <- read_excel("db mineria/2011.xls", 
                    sheet = "Mes de registro y departamento", 
                    skip = 1)
X2012 <- read_excel("db mineria/2012.xls", 
                    sheet = "Mes de registro y departamento", 
                    skip = 1)
X2013 <- read_excel("db mineria/2013.xls", 
                    sheet = "Mes de registro y departamento", 
                    skip = 1)
X2014 <- read_excel("db mineria/2014.xls", 
                    sheet = "Mes de registro y departamento", 
                    skip = 1)
X2015 <- read_excel("db mineria/2015.xls", 
                    sheet = "Mes de registro y departamento", 
                    skip = 1)
X2016 <- read_excel("db mineria/2016.xls", 
                    sheet = "Mes de registro y departamento", 
                    skip = 1)
X2017 <- read_excel("db mineria/2017.xls", 
                    sheet = "Mes de registro y departamento", 
                    skip = 1)
X2018 <- read_excel("db mineria/2018.xlsx", 
                    sheet = "Mes de registro y departamento", 
                    skip = 1)
X2019 <- read_excel("db mineria/2019.xlsx", 
                    sheet = "Mes de registro y departamento", 
                    skip = 1)
X2020 <- read_excel("db mineria/2020.xlsx", 
                    sheet = "Mes de registro y departamento", 
                    skip = 1)
X2021 <- read_excel("db mineria/2021.xlsx", 
                    sheet = "Mes de registro y departamento", 
                    skip = 1)

#############################################################################################
XX2009 <- read_excel("nacimientos/2009.xls", 
                    sheet = "Estado civil y depto. resi.", 
                    skip = 1)
XX2010 <- read_excel("nacimientos/2010.xls", 
                     sheet = "Estado civil y depto. resid.", 
                     skip = 1)
XX2011 <- read_excel("nacimientos/2011.xls", 
                     sheet = "Estado civil y depto. resi.", 
                     skip = 1)
XX2012 <- read_excel("nacimientos/2012.xls", 
                     sheet = "Estado civil y depto. resi.", 
                     skip = 1)
XX2013 <- read_excel("nacimientos/2013.xls", 
                     sheet = "Estado civil y depto. resi.", 
                     skip = 1)
XX2014 <- read_excel("nacimientos/2014.xls", 
                     sheet = "Estado civil y depto. resi.", 
                     skip = 1)
XX2015 <- read_excel("nacimientos/2015.xls", 
                     sheet = "Estado civil y depto. resi.", 
                     skip = 1)
XX2016 <- read_excel("nacimientos/2016.xlsx", 
                     sheet = "Estado civil y depto. resi.", 
                     skip = 1)
XX2017 <- read_excel("nacimientos/2017.xlsx", 
                     sheet = "Estado civil y depto. resi.", 
                     skip = 1)
XX2018 <- read_excel("nacimientos/2018.xlsx", 
                     sheet = "Estado civil y depto. resi.", 
                     skip = 1)
XX2019 <- read_excel("nacimientos/2019.xlsx", 
                     sheet = "Estado civil y depto. resi.", 
                     skip = 1)
XX2020 <- read_excel("nacimientos/2020.xlsx", 
                     sheet = "Estado civil y depto. resi.", 
                     skip = 1)

#########################PA TOTAL DE NACIMIENTOS###########################################

total_nacimientos<-total_nacimientos[!(total_nacimientos$Mes=="Total"),]

barchart(total_nacimientos$Mes~total_nacimientos$`Cantidad de nacimientos` , xlab="Cantidad de nacimientos de 2009 a 2020",ylab="Mes")


################################################################################################
# PA DIVORCIOS #################################################################################
df1$rn <- rownames(df1)
df2$rn <- rownames(df2)
res1 <- rbind(X2012,X2013,X2014,X2015,X2016,X2017,X2018,X2019,X2020,X2021)
res2 <- rbind(X2010,X2011)

data_sum3 <- res1 %>%                                     # Consolidate duplicates
  group_by(res1$`Departamento de registro`) %>%
  dplyr::summarise_if(is.numeric,sum) %>% 
  as.data.frame()

data_sum3    

names(data_sum3)[1]<-"Departamento"
data_sum3$Departamento[data_sum3$Departamento=="Petén"]<-"Peten"
data_sum3$Departamento[data_sum3$Departamento=="Quiché"]<-"Quiche"
data_sum3$Departamento[data_sum3$Departamento=="Sacatepéquez"]<-"Sacatepequez"
data_sum3$Departamento[data_sum3$Departamento=="Sololá"]<-"Solola"
data_sum3$Departamento[data_sum3$Departamento=="Suchitepéquez"]<-"Suchitepequez"
data_sum3$Departamento[data_sum3$Departamento=="Totonicapán"]<-"Totonicapan"

data_sumFF <- data_sum3 %>%                                     # Consolidate duplicates
  group_by(data_sum3$Departamento) %>%
  dplyr::summarise_if(is.numeric,sum) %>% 
  as.data.frame()
names(data_sumFF)[1]<-"Departamento"
res2$Departamento[res2$Departamento=="Petén"]<-"Peten"
res2$Departamento[res2$Departamento=="Quiché"]<-"Quiche"
res2$Departamento[res2$Departamento=="Sacatepéquez"]<-"Sacatepequez"
res2$Departamento[res2$Departamento=="Sololá"]<-"Solola"
res2$Departamento[res2$Departamento=="Suchitepéquez"]<-"Suchitepequez"
res2$Departamento[res2$Departamento=="Totonicapán"]<-"Totonicapan"




resF<- rbind(data_sumFF,res2)
data_sum4 <- resF %>%                                     # Consolidate duplicates
  group_by(resF$Departamento) %>%
  dplyr::summarise_if(is.numeric,sum) %>% 
  as.data.frame()

summary(data_sum4)

dff<- data_sum4[order(-data_sum4$Total),]
names(dff)[1]<-"Departamento"
df_divorcios<-dff %>% select(1, 2) %>% as.data.frame()
names(df_divorcios)[2]<-"Divorcios"
dff3<-as.data.frame(dff[-1,])
dff2<-head(dff3,23)
barchart(dff2$Total~dff2$`resF$Departamento`, xlab="departamentos",ylab="divorcios desde el 2009")

############################ PA NACIMIENTOS ####################################################################################

res2_nacimientos <- rbind(XX2009,XX2010,XX2011,XX2012,XX2013,XX2014,XX2015,XX2016,XX2017,XX2018,XX2019,XX2020)
data_sum_nacimientos <- res2_nacimientos %>%                                     # Consolidate duplicates
  group_by(res2_nacimientos$`Departamento de residencia`) %>%
  dplyr::summarise_if(is.numeric,sum) %>% 
  as.data.frame()
names(data_sum_nacimientos)[1]<-"Departamento"
df2<-data_sum_nacimientos[!(data_sum_nacimientos$Departamento=="Extranjero" | data_sum_nacimientos$Departamento=="Ignorado"),]
df_nacimientos<- as.data.frame(df2[1,4])
rm(df_nacimientos)
df_nacimientos<-df2 %>% select(1, 4) %>% as.data.frame()
barchart(df_nacimientos$Casada~df_nacimientos$Departamento, xlab="departamentos",ylab="Nacimientos de madre casa desde 2009")
###############################################################################################################
df_divorcios<-df_divorcios[!(df_divorcios$Departamento=="Todos los departamentos"),]
df_nacimientos<-df_nacimientos[!(df_nacimientos$Departamento=="Todos los departamentos"),]
df_divorcios<- df_divorcios[order(df_divorcios$Departamento),]
df_nacimientos<- df_nacimientos[order(df_nacimientos$Departamento),]
df_final<- data.frame(df_divorcios$Departamento,df_divorcios$Divorcios,df_nacimientos$Casada)
grr<-ggplot(df_final, aes(x=df_nacimientos.Casada, y=df_divorcios.Divorcios))+geom_point()
grr<- grr+geom_smooth(method = "lm",col="black")
grr<- grr+theme_bw()+labs(title="regresion lineal", x= "Nacimientos de mujeres casadas", y="Divorcios")
grr

heig<-as.matrix(df_final)
dd<-df_final %>% 
  melt %>% 
  as.data.frame() 
options(scipen = 100000)
ggplot(dd, aes(df_divorcios.Departamento, value, fill = variable))+geom_col(position = "dodge")+coord_cartesian(ylim = c(0, 100000))

df_final2<-data.frame(ada$Mes,ada$Cantidad_Divorcios,total_nacimientos$`Cantidad de nacimientos`)
dd2<-df_final2 %>% 
  melt %>% 
  as.data.frame() 

ggplot(dd2, aes(dd2$ada.Mes, value, fill = variable))+geom_col(position = "dodge")+coord_cartesian(ylim = c(0, 15000))


wss <- (nrow(df_final[,c(2,3)])-1)*sum(apply(df_final[,c(2,3)],2,var))

for (i in 2:3) 
  wss[i] <- sum(kmeans(df_final[,c(2,3)], centers=i)$withinss)

plot(1:3, wss, type="b", xlab="Numero de clusters",  ylab="Dentro de los grupos suma de cuadrados")

#k means
conjunto<-df_final
datosCompleto<-df_final[complete.cases(conjunto),]
km<-kmeans(df_final[,c(2,3)],2,iter.max =100)
conjunto$grupo<-km$cluster

g1<- conjunto[conjunto$grupo==1,]
prop.table(table(g1$budget))*100
nrow(g1)
summary(g1)

g2<- conjunto[conjunto$grupo==2,]
prop.table(table(g2$budget))*100

plotcluster(df_final[,c(2,3)],km$cluster)


hc<-hclust(dist(df_final[,c(2,3)])) 
plot(hc)
rect.hclust(hc,k=3) 
groups<-cutree(hc,k=3) 
conjunto$gruposHC<-groups


g1HC<-conjunto[conjunto$gruposHC==1,]
g2HC<-conjunto[conjunto$gruposHC==2,]
g3HC<-conjunto[conjunto$gruposHC==3,]
