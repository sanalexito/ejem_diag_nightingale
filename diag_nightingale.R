
library(viridis)
library(ggplot2)
library(tidyverse)
library(circlize)


#Partimos de un conjunto de datos grande... muy grande!!!

directorio <- "Tu/ruta/donde_esta_el_dataset/"
arch <- "delitos.xlsx"

load(paste0(directorio, arch))
bd_a <- TAB[[2]]
noms_bd <- names(bd_a)
bd_a = merge(bd_a, TAB[[1]][ ,c("CONSEC", "ESTRATO")])
df_a = bd_a[,c("ID_DELITO", "ESTRATO", "TOT_MODULO")]
df_a$TOT_MODULO = as.character(df_a$TOT_MODULO)
df_a

# Análisis descriptivo de los datos
# Gráfica que indica el número de módulos abiertos para cada delito ------------

ggplot(df_a) +  
  # Pone las barras apiladas
  geom_bar(aes(x=ID_DELITO, y=TOT_MODULO, fill=TOT_MODULO), stat="identity") 

# Gráfica que indica el número de módulos abiertos para cada delito ------------

ggplot(df_a) +  
  # Pone las barras apiladas
  geom_bar(aes(x=ID_DELITO, y=TOT_MODULO, fill=TOT_MODULO), stat="identity") +
  # Usa otra paleta de colores
  scale_fill_viridis(discrete=TRUE, direction = 1, option = "C", name="Módulos")

# Gráfica que indica el número de módulos abiertos para cada delito ------------

ggplot(df_a) +  
  # Pone las barras apiladas
  geom_bar(aes(x=ID_DELITO, y=TOT_MODULO, fill=TOT_MODULO), stat="identity") +
  # Usa otra paleta de colores
  scale_fill_viridis(discrete=TRUE, direction = 1, option = "C", name="Módulos")+
  # Acomoda sobre un pay
  coord_polar()


n_reng = c("Robo total de vehículo", "Robo de accesorios", "Robo de mercancía en tránsito", "Robo hormiga",
           "Robo de insumos o dinero", "Fraude bancario", "Fraude al establecimiento",
           "Delito informático", "Extorsión", "Secuestro", "Daño a instalaciones",
           "Vandalismo",  "Corrupción") 
df_a$ID_DELITO2 = ifelse(df_a$ID_DELITO%in%'01', n_reng[1],
                         ifelse(df_a$ID_DELITO%in%'02', n_reng[2],
                                ifelse(df_a$ID_DELITO%in%'03', n_reng[3],
                                       ifelse(df_a$ID_DELITO%in%'04', n_reng[4],
                                              ifelse(df_a$ID_DELITO%in%'05', n_reng[5],
                                                     ifelse(df_a$ID_DELITO%in%'07', n_reng[6],
                                                            ifelse(df_a$ID_DELITO%in%'08', n_reng[7],
                                                                   ifelse(df_a$ID_DELITO%in%'09', n_reng[8],
                                                                          ifelse(df_a$ID_DELITO%in%'10', n_reng[9],
                                                                                 ifelse(df_a$ID_DELITO%in%'11', n_reng[10],
                                                                                        ifelse(df_a$ID_DELITO%in%'12', n_reng[11], 
                                                                                               ifelse(df_a$ID_DELITO%in%'13', n_reng[12],
                                                                                                      ifelse(df_a$ID_DELITO%in%'15', n_reng[13],NA)))))))))))))


p <- ggplot(df_a) +  
  geom_bar(aes(x=reorder(str_wrap(ID_DELITO2, 5), TOT_MODULO), y=TOT_MODULO, fill=TOT_MODULO), stat="identity") +
  scale_fill_viridis(discrete=TRUE, direction = 1, option = "C", name="Módulos")+
  coord_polar() 
p
