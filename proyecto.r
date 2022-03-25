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
