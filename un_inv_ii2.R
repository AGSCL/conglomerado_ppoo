
#dtX2023_12_05_DatosEgresosHosp_encrip<-rio::import("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_ii/20231205_hosp_mod20240404.parquet.gzip")

## LOAD LIBRARY 

# 0.Fechas y paquetes --------------------------------------------------

# remover objetos y memoria utilizada
rm(list=ls());gc()

# Austin, P. C. (2009). The Relative Ability of Different Propensity 
# Score Methods to Balance Measured Covariates Between 
# Treated and Untreated Subjects in Observational Studies. Medical 
# Decision Making. https://doi.org/10.1177/0272989X09341755
smd_bin <- function(x,y){
  z <- x*(1-x)
  t <- y*(1-y)
  k <- sum(z,t)
  l <- k/2
  
  return((x-y)/sqrt(l))
  
}

theme_custom_sjplot2 <- function(base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Text elements
      text = element_text(size = base_size, family = base_family),
      plot.title = element_text(face = "bold", hjust = 0.5, size = base_size * 1.2),
      plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 10)),
      axis.title = element_text(size = base_size, face = "bold"),
      axis.text = element_text(size = base_size * 0.8),
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      
      # Plot layout
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      legend.text = element_text(size = base_size * 0.8),
      legend.title = element_text(size = base_size, face = "bold"),
      legend.background = element_rect(fill = "white", colour = NA),
      legend.box.background = element_rect(colour = "grey80", linetype = "solid"),
      legend.key = element_rect(fill = "white", colour = "white")
    )
}


copiar_nombres2 <- function(x,row.names=FALSE,col.names=TRUE,dec=",",...) {
  if(class(try(dplyr::ungroup(x)))[1]=="tbl_df"){
    if(options()$OutDec=="."){
      options(OutDec = dec)
      write.table(format(data.frame(x)),"clipboard",sep="\t",row.names=FALSE,col.names=col.names,...)
      options(OutDec = ".")
      return(x)
    } else {
      options(OutDec = ",")
      write.table(format(data.frame(x)),"clipboard",sep="\t",row.names=FALSE,col.names=col.names,...)
      options(OutDec = ",")
      return(x)    
    }
  } else {
    if(options()$OutDec=="."){
      options(OutDec = dec)
      write.table(format(x),"clipboard",sep="\t",row.names=FALSE,col.names=col.names,...)
      options(OutDec = ".")
      return(x)
    } else {
      options(OutDec = ",")
      write.table(format(x),"clipboard",sep="\t",row.names=FALSE,col.names=col.names,...)
      options(OutDec = ",")
      return(x)       
    }
  }
}  

# paquetes en R
if(!require(openxlsx)){install.packages("openxlsx");require(openxlsx)}
if(!require(TraMineR)){install.packages("TraMineR");require(TraMineR)}
if(!require(TraMineRextras)){install.packages("TraMineRextras");require(TraMineRextras)}
if(!require(cluster)){install.packages("cluster");require(cluster)}
if(!require(RColorBrewer)){install.packages("RColorBrewer");require(RColorBrewer)}
if(!require(WeightedCluster)){install.packages("WeightedCluster");require(WeightedCluster)}
if(!require(NbClust)){install.packages("NbClust");require(NbClust)}
if(!require(foreach)){install.packages("foreach");require(foreach)}
if(!require(doParallel)){install.packages("doParallel");require(doParallel)}
if(!require(dtplyr)){install.packages("dtplyr");require(dtplyr)}
if(!require(tidyverse)){install.packages("tidyverse");require(tidyverse)}
if(!require(tidylog)){install.packages("tidylog");require(tidylog)}
if(!require(missRanger)){install.packages("missRanger");require(missRanger)}
if(!require(rpart)){install.packages("rpart");require(rpart)}
if(!require(caret)){install.packages("caret");require(caret)}
if(!require(rpart.plot)){install.packages("rpart.plot");require(rpart.plot)}
if(!require(factoextra)){install.packages("factoextra");require(factoextra)}
if(!require(partykit)){install.packages("partykit");require(partykit)}
if(!require(devtools)){install.packages("devtools");require(devtools)}
if(!require(bpmn)){devtools::install_github("bergant/bpmn");require(bpmn)}
if(!require(ROSE)){install.packages("ROSE");require(ROSE)}
if(!require(geosphere)){install.packages("geosphere");require(geosphere)}
if(!require(igraph)){install.packages("igraph");require(igraph)}
if(!require(data.table)){install.packages("data.table");require(data.table)}
if(!require(parallel)){install.packages("parallel")}; library(parallel)
if(!require(polars)){install.packages("polars", repos = "https://rpolars.r-universe.dev")}; library(polars)
if(!require(car)){install.packages("car")}; library(car)

invisible("set  windows, para plots")
options(device="windows")


num_cores <- detectCores() -1
setDTthreads(threads = num_cores)#restore_after_fork = NULL, throttle = NULL)

# Create a new environment to load the data into
env <- new.env()

# Load all data into the environment
#load("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/20240320.RData", envir = env)
load("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/20240405.RData", envir = env)

# Update 2024-05-10: eliminated people that experienced no SM or SUD related hosp before 2017, but in 2018 still had SM or SUD issues before the first at 15-29
dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl_2 <- dplyr::filter(env$dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl, !run %in% (env$dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl %>% dplyr::filter(edad_anos<15) %>% pull(run)))

#Update 2024-05-11: Eliminate events of people with duplicated rows in run, area of admission, date of admission and discharge, and the first ICD-10 codes
vector_duplicates<- c("run", "estab_homo",  "areaf_egr", "fecha_ingreso", "fecha_egreso", "glosa_prevision", "benef", "glosa_sexo", "edad_anos", "diag1", "diag2", "diag3", "diag4", "diag5")

dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl_3 <-
  dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl_2%>%
  dplyr::group_by(across(all_of(vector_duplicates))) %>% 
  dplyr::mutate(n_duplicated=dplyr::row_number()) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(n_duplicated<2)

# Extract only the desired object
data_long <- dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl_3



## 0a. SF: Coincidencia fechas --------------------------------------------------

invisible("Solicitud Sandra Flores: ver si coinciden las fechas de egreso con las de defunción en mortalidad intrahosp")
data_long %>% 
       dplyr::left_join(env$def_enc17_21, by=c("run"="RUN")) %>% 
       dplyr::filter(!is.na(GLOSA_FUND_CAUSA) & cond_egr==2) %>% 
       dplyr::select(fecha_egreso_rec24, FECHA_DEF)
# A tibble: 12 x 2
#    fecha_egreso_rec24 FECHA_DEF 
#    <date>             <chr>     
#  1 2018-12-28         28-12-2018
#  2 2021-08-08         08-08-2021
#  3 2020-03-31         31-03-2020
#  4 2019-08-26         26-08-2019
#  5 2020-06-29         29-06-2020
#  6 2019-01-27         27-01-2019
#  7 2020-11-29         29-11-2020
#  8 2019-07-09         08-07-2019
#  9 2020-03-13         13-03-2020
# 10 2021-09-21         21-09-2021
# 11 2018-03-02         02-03-2018
# 12 2018-06-19         19-06-2018

env$def_enc17_21$ing_year<- lubridate::epiyear(as.Date.character(env$def_enc17_21$FECHA_DEF, format="%d-%m-%Y"))

barplot(table(env$def_enc17_21$ing_year), main = "Muertes por año", xlab = "Año", ylim=c(0,max(table(env$def_enc17_21$ing_year))+1e4))
text(x = barplot(table(env$def_enc17_21$ing_year), add = TRUE),  # Trick: Reuse barplot to get coordinates
     y = table(env$def_enc17_21$ing_year) + .2,  # Offset slightly above
     labels = table(env$def_enc17_21$ing_year), 
     pos = 3)  # Position labels above
recorded_plot <- recordPlot() 
png("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/_figs/muertes_ano.png", height=6, width=8, res=500, units="in") 
recorded_plot
dev.off()

#delete environment; free up memory
rm(env);gc()


## 0b. Códigos Z --------------------------------------------------

codigos_z<-rio::import("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_ii/codigos_z_filtered_df.csv.gz")

codigos_z$fecha_egreso_rec24 <- 
  readr::parse_date(as.character(codigos_z$fecha_egreso_rec), format = "%Y-%m-%d")

codigos_z$fecha_ingreso_rec24 <- 
  readr::parse_date(as.character(codigos_z$fecha_ingreso_rec), format = "%Y-%m-%d")

invisible("filtra aquellos que tienen RUNs en la última muestra y que tienen códigos Z")
codigos_z2<-
  dplyr::filter(codigos_z,run %in%unique(data_long$run),recoded_zs==1)
nrow(codigos_z2)
#[1] 144

codigos_z_filt<-
  codigos_z2 %>% 
  dplyr::filter(fecha_ingreso_rec24<"2018-01-01")
nrow(codigos_z_filt)
# 13
warning(paste0("Sólo ",length(unique(codigos_z_filt$run))," personas tienen zs antes del primer dg SUD/SM"))
#Sólo 11 personas tienen zs antes del primer dg SUD/SM 

invisible("AHora por persona, de los que tienen Zs antes del 2018")
data_long %>% 
  dplyr::mutate(zs_antes= ifelse(run %in% unique(codigos_z_filt$run), 1, 0)) %>% 
  janitor::tabyl(zs_antes, ppo_conadi)
 # zs_antes     0   1
 #        0 10843 991
 #        1    17   2

message(paste0("Solo ",
data_long %>% 
  dplyr::mutate(zs_antes= ifelse(run %in% unique(codigos_z_filt$run), 1, 0)) %>% 
  dplyr::filter(zs_antes==1, ppo_conadi==1) %>% 
  distinct(run) %>% nrow(),
" personas CONADI con zs antes"))
#Solo 2 personas CONADI con zs antes


invisible("Al final se decidió no considerar por su bajo número")

#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_
invisible("AHora sólo filtrando antes del primer dg.")

codigos_z2$fecha_ingreso_rec24<-
  as.Date.character(as.character(substr(codigos_z2$fecha_ingreso_rec24, 1, 10)))
codigos_z2$fecha_egreso_rec24<-
  as.Date.character(as.character(substr(codigos_z2$fecha_egreso_rec24, 1, 10)))
codigos_z2$fecha_ingreso_rec24_num<-
  as.numeric(unclass(as.Date.character(as.character(substr(codigos_z2$fecha_ingreso_rec24, 1, 10)))))
codigos_z2$fecha_egreso_rec24_num<-
  as.numeric(unclass(as.Date.character(as.character(substr(codigos_z2$fecha_egreso_rec24, 1, 10)))))

data_long <- dplyr::group_by(data_long, run) %>% 
  dplyr::mutate(min_fecha_egreso_rec24_num= min(fecha_egreso_rec24_num)) %>% 
  dplyr::ungroup()
#11,853

library(sqldf)

result2 <- sqldf("SELECT d.*, c.*
              FROM data_long d
              LEFT JOIN codigos_z2 c
              ON d.run = c.run AND d.min_fecha_egreso_rec24_num > c.fecha_egreso_rec24_num
              ")
nrow(result2)
#[1] 11859

result2 %>% 
  janitor::clean_names() %>% 
  dplyr::filter(!is.na(recoded_zs )) %>% nrow()
#[1] 25

result2 %>% 
  janitor::clean_names() %>% 
  dplyr::filter(!is.na(recoded_zs )) %>% group_by(run) %>% distinct(run, .keep_all = T) %>% 
  janitor::tabyl(ppo_conadi)
 # ppo_conadi n   percent
 #          0 9 0.8181818
 #          1 2 0.1818182

result2 %>% 
  janitor::clean_names() %>% 
  dplyr::filter(!is.na(recoded_zs )) %>% group_by(run) %>% distinct(run, .keep_all = T) %>% 
  janitor::tabyl(inclusivo_real_historico, ppo_conadi)
 # inclusivo_real_historico 0 1
 #                        0 8 0
 #                        1 1 2

invisible("Al final se decidió no considerar por su bajo número")


## 0c. Serie de tiempo --------------------------------------------------

df <- data.frame(date = as.Date(codigos_z2$fecha_egreso_rec)) 

df_group_trimester <- 
  df %>%  dplyr::mutate(fech_trimester = as.character(lubridate::quarter(date, with_year = TRUE))) %>% 
  dplyr::group_by(fech_trimester) %>% summarise(n=n())

df_group_yearmonth <-
  df %>% dplyr::mutate(yearmonth = format(date, "%Y-%m")) %>% 
dplyr::group_by(yearmonth) %>% summarise(n=n())

all_years <- 2010:2022
quarters <- paste(rep(all_years, each=4), ".", rep(1:4, times=length(all_years)), sep="")
all_quarters <- data.frame(
  quarter = quarters
)
start_date <- as.Date("2010-01-01") ; end_date <- as.Date("2022-12-31")
all_months <- seq(from = start_date, to = end_date, by = "month")
all_months_df <- data.frame(
  month = format(all_months, "%Y-%m")
)


check_full_trimestre <- full_join(all_quarters, df_group_trimester, by = c("quarter"="fech_trimester")) %>%
  group_by(quarter) %>%
  summarise(count = sum(n,na.rm=T), .groups = 'drop') %>%
  mutate(count = ifelse(is.na(count), 0, count))
check_full_meses <- full_join(all_months_df, df_group_yearmonth, by = c("month"="yearmonth")) %>%
  group_by(month) %>%
  summarise(count = sum(n,na.rm=T), .groups = 'drop') %>%
  mutate(count = ifelse(is.na(count), 0, count))

#_#_#_#_#_#_#_#_
#Hacer las figuras
plot_timeseries<-
check_full_meses %>% 
  dplyr::mutate(month_fmt=as.Date(paste0(month, "-01"))) %>% 
ggplot(aes(x = month_fmt, y = count)) +
  geom_line() +  # Adds a line graph
  geom_point() + # Adds points to each data entry
  labs(title = "Recuento de ingresos mensuales", x = "Trimestre de egreso", y = "Recuento") +
  theme_minimal() + # Minimal theme for a clean look
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +# Formatting the x-axis
  theme_custom_sjplot2()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=.5,size=10),
        plot.caption = element_text(hjust = 0, face= "italic"))+
  ylim(c(50000,200000))

plot_timeseries

ggsave(plot= plot_timeseries, filename= "H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/_figs/serietiempo_total_hosp.png", width=8, height=5,dpi=600)


## 0d. Combinar base con código hospitales ------------------------------------------------------------------

EH_2010_2022_Pasantes_v2_encrip <- readr::read_delim("_data/EH_2010_2022_Pasantes_v2_encrip.csv", 
                                              delim = ";", escape_double = FALSE, trim_ws = TRUE)
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
invisible("Unir con bases de referencia de establecimientos")
X717508813_Establecimientos_ChileDEIS_MINSAL_29_04_2022 <- readxl::read_excel("_antecedentes/717508813-Establecimientos-ChileDEIS-MINSAL-29-04-2022.xlsx", 
                                                                              sheet = "Establecimientos Vig", skip = 1) %>% janitor::clean_names()

X717508813_Establecimientos_ChileDEIS_MINSAL_cerrado_29_04_2022 <- readxl::read_excel("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/_antecedentes/717508813-Establecimientos-ChileDEIS-MINSAL-29-04-2022.xlsx", 
                                                                                      sheet = "Establecimientos Cerrados", skip = 1) %>% janitor::clean_names() %>% 
  dplyr::mutate(codigo_vigente=as.numeric(codigo_vigente))

vector_duplicates<- c("RUN", "ESTAB_HOMO",  "AREAF_EGR", "FECHA_INGRESO", "FECHA_EGRESO", "GLOSA_PREVISION", "BENEF", "GLOSA_SEXO", "EDAD_ANOS", "DIAG1", "DIAG2", "DIAG3", "DIAG4", "DIAG5")

EH_2010_2022_Pasantes_v2_encrip <-
  EH_2010_2022_Pasantes_v2_encrip%>%
  dplyr::group_by(across(all_of(vector_duplicates))) %>% 
  dplyr::mutate(n_duplicated=dplyr::row_number()) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(n_duplicated<2)
#from 20957004 to 20952449= 4,555

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
invisible("Unión con la base de datos corregida")
data_long_establecimiento<-
  data_long %>%
  dplyr::mutate(fecha_ingreso_rec_fmt= as.Date(fecha_ingreso_rec), fecha_egreso_rec_fmt= as.Date(fecha_egreso_rec)) %>%
  #dplyr::select(run, estab, fecha_ingreso_rec, fecha_egreso_rec, estab, estab_homo) %>%
  tidylog::left_join(dplyr::mutate(EH_2010_2022_Pasantes_v2_encrip[,c("RUN", "FECHA_INGRESO_FMT_DEIS", "FECHA_EGRESO_FMT_DEIS", "ESTAB", "ESTAB_HOMO", "DIAG1", "DIAG2", "DIAG3")], estab_homo_five=as.numeric(stringr::str_sub(ESTAB_HOMO, 1, 5))),
                     by=c("run"="RUN", "fecha_ingreso_rec_fmt"="FECHA_INGRESO_FMT_DEIS", "fecha_egreso_rec_fmt"="FECHA_EGRESO_FMT_DEIS", "diag1"="DIAG1", "diag2"="DIAG2", "diag3"="DIAG3", "estab_homo"="estab_homo_five"))
#nrow() #169494}
#left_join: added 2 columns (ESTAB, ESTAB_HOMO)
# > rows only in x            0
# > rows only in y  (20,945,149)
# > matched rows         11,869    (includes duplicates)
# > rows total           11,869

invisible("Al final hice inner join")
# inner_join: added 2 columns (ESTAB, ESTAB_HOMO)
#             > rows only in x  (         0)
#             > rows only in y  (20,945,149)
#             > matched rows         11,867    (includes duplicates)
#             > rows total           11,867

invisible("Probé con ")
# inner_join: added 2 columns (ESTAB, ESTAB_HOMO)
# > rows only in x  (         0)
# > rows only in y  (20,945,149)
# > matched rows         11,857    (includes duplicates)
# > rows total           11,857

invisible("Ver si se generan más filas producto de la unión")
if(
  nrow(data_long_establecimiento) >nrow(data_long)){warning("Producto de la unión se añaden filas (hay un pareamiento 1 a >1)")}

paste0("Pacientes duplicados: ",
data_long_establecimiento %>% 
  janitor::get_dupes(run, fecha_ingreso_rec_fmt, fecha_egreso_rec_fmt) %>% 
    dplyr::distinct(run) %>% nrow(),
"; eventos: ",
data_long_establecimiento %>% 
  janitor::get_dupes(run, fecha_ingreso_rec_fmt, fecha_egreso_rec_fmt) %>% 
  nrow()
)
#[1] "Pacientes duplicados: 6; eventos: 16"
#

data_long_establecimiento<-
  data_long %>%
  dplyr::mutate(fecha_ingreso_rec_fmt= as.Date(fecha_ingreso_rec), fecha_egreso_rec_fmt= as.Date(fecha_egreso_rec)) %>%
  #dplyr::select(run, estab, fecha_ingreso_rec, fecha_egreso_rec, estab, estab_homo) %>%
  tidylog::left_join(dplyr::mutate(EH_2010_2022_Pasantes_v2_encrip[,c("RUN", "FECHA_INGRESO_FMT_DEIS", "FECHA_EGRESO_FMT_DEIS", "ESTAB", "ESTAB_HOMO", "DIAG1", "DIAG2", "DIAG3")], estab_homo_five=as.numeric(stringr::str_sub(ESTAB_HOMO, 1, 5))),
                     by=c("run"="RUN", "fecha_ingreso_rec_fmt"="FECHA_INGRESO_FMT_DEIS", "fecha_egreso_rec_fmt"="FECHA_EGRESO_FMT_DEIS", "diag1"="DIAG1", "diag2"="DIAG2", "diag3"="DIAG3", "estab_homo"="estab_homo_five"), multiple="first")

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
invisible("Unimos por separado con las planillas de directorio de cada tipo de establecimiento")
data_long_establecimiento_2024_std<-
  data_long_establecimiento %>% 
  tidylog::left_join(janitor::clean_names(X717508813_Establecimientos_ChileDEIS_MINSAL_29_04_2022)[,c("codigo_antiguo", "codigo_vigente", 
                      "codigo_comuna", "codigo_region", 
                      "tiene_servicio_de_urgencia", "nombre_oficial",
                      "tipo_establecimiento", "nivel_de_atencion", "tipo_de_prestador_sistema_de_salud", 
                      "longitud_grados_decimales", "latitud_grados_decimales", 
                      "nivel_de_complejidad", "modalidad_de_atencion")], 
                     by= c("ESTAB_HOMO"="codigo_vigente")) %>% 
  tidylog::left_join(janitor::clean_names(X717508813_Establecimientos_ChileDEIS_MINSAL_cerrado_29_04_2022)[,c("codigo_antiguo", "codigo_vigente", 
                      "codigo_comuna", "codigo_region", 
                      "tiene_servicio_de_urgencia", "nombre_oficial",
                      "tipo_establecimiento", "nivel_de_atencion", "tipo_de_prestador_sistema_de_salud", 
                      "longitud_grados_decimales", "latitud_grados_decimales", 
                      "nivel_de_complejidad", "modalidad_de_atencion")], 
                     by= c("ESTAB_HOMO"="codigo_vigente")) %>% 
  dplyr::mutate(codigo_antiguo= ifelse(is.na(codigo_antiguo.x),codigo_antiguo.y, codigo_antiguo.x),       
                codigo_comuna= ifelse(is.na(codigo_comuna.x),codigo_comuna.y, codigo_comuna.x),
                codigo_region= ifelse(is.na(codigo_region.x),codigo_region.y, codigo_region.x),
                tiene_servicio_de_urgencia= ifelse(is.na(tiene_servicio_de_urgencia.x),tiene_servicio_de_urgencia.y, tiene_servicio_de_urgencia.x),
                nombre_oficial= ifelse(is.na(nombre_oficial.x),nombre_oficial.y, nombre_oficial.x),
                tipo_establecimiento= ifelse(is.na(tipo_establecimiento.x),tipo_establecimiento.y, tipo_establecimiento.x),
                nivel_de_atencion= ifelse(is.na(nivel_de_atencion.x),nivel_de_atencion.y, nivel_de_atencion.x),
                tipo_de_prestador_sistema_de_salud= ifelse(is.na(tipo_de_prestador_sistema_de_salud.x),tipo_de_prestador_sistema_de_salud.y, tipo_de_prestador_sistema_de_salud.x),
                longitud_grados_decimales= ifelse(is.na(longitud_grados_decimales.x),longitud_grados_decimales.y, longitud_grados_decimales.x),
                latitud_grados_decimales= ifelse(is.na(latitud_grados_decimales.x),latitud_grados_decimales.y, latitud_grados_decimales.x),
                nivel_de_complejidad= ifelse(is.na(nivel_de_complejidad.x),nivel_de_complejidad.y, nivel_de_complejidad.x),
                modalidad_de_atencion= ifelse(is.na(modalidad_de_atencion.x),modalidad_de_atencion.y, modalidad_de_atencion.x)) %>% 
  dplyr::select(-matches("\\.x$|\\.y$"))
# left_join: added 12 columns (codigo_antiguo, codigo_comuna, codigo_region, tiene_servicio_de_urgencia, nombre_oficial, …)
#            > rows only in x       92
#            > rows only in y  ( 4,017)
#            > matched rows     11,777
#            > rows total       11,869
# left_join: added 24 columns (codigo_antiguo.x, codigo_comuna.x, codigo_region.x, tiene_servicio_de_urgencia.x, nombre_oficial.x, …)
#            > rows only in x   11,777
#            > rows only in y  (   253)
#            > matched rows         92
#            > rows total       11,869

# left_join: added 12 columns (codigo_antiguo, codigo_comuna, codigo_region, tiene_servicio_de_urgencia, nombre_oficial, …)
#            > rows only in x       91
#            > rows only in y  ( 4,017)
#            > matched rows     11,764
#            > rows total       11,855
# left_join: added 24 columns (codigo_antiguo.x, codigo_comuna.x, codigo_region.x, tiene_servicio_de_urgencia.x, nombre_oficial.x, …)
#            > rows only in x   11,764
#            > rows only in y  (   253)
#            > matched rows         91
#            > rows total       11,855
invisible("Ver si hay perdidos en establecimiento")
table(is.na(data_long_establecimiento_2024_std$ESTAB_HOMO))
# FALSE 
# 11855 
table(is.na(data_long_establecimiento_2024_std$ESTAB))
# FALSE 
# 11855 
# 

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
invisible("Ver si se generan más filas producto de la unión")
if(
  data_long_establecimiento_2024_std %>% nrow() >data_long_establecimiento %>% nrow()){stop("Producto de la unión se añaden filas (hay un pareamiento 1 a >1)")}

# 1.Formatear base de datos --------------------------------------------------

#Eliminar variables que no existen
data_long_establecimiento_2024_std$nac_chile<-NULL
data_long_establecimiento_2024_std$nac_chile<-NULL
data_long_establecimiento_2024_std$perc_nac_cl<-NULL

invisible("Para ver cómo clasificar")
#https://www.perplexity.ai/search/crea-Mdica-Adulto-Lj.RWBVtQliYcDUggLbi8g#0

## 1.2. Recodificar previsión y tramo --------------------------------------------------

# La clasificación se dividió en dos grupos principales: (i) quienes están afiliados 
# a un sistema privado de salud o a sistemas de las Fuerzas Armadas (CAPREDENA,
# DIPRECA y SISA) (102), o (ii) quienes están afiliados a Fonasa. A su vez, el segundo 
# grupo fue subdividido en base al nivel de ingresos: Fonasa A y B para quienes no 
# tienen recursos suficientes para cotizar en salud (en situación de indigencia o con 
# ingresos menores a aproximadamente 400 dólares mensuales) y Fonasa C y D para aquellos 
# con ingresos superiores a dicho umbral (103). Dichas variables en la base de datos se 
# originan de los campos originalmente llamados “PREVI” (previsión usuario) y “BENEF” 
# (tramo de beneficiario FONASA). Aquellas personas sin cobertura o afiliación 
# (96= Ninguna)  fueron clasificadas en “NINGUNA”. Las personas con afiliación o 
# cobertura de salud desconocida (99= Desconocida) fueron imputadas para el análisis (104).

table(data_long_establecimiento_2024_std$glosa_prevision, exclude=NULL)
  # CAPREDENA DESCONOCIDO     DIPRECA      FONASA      ISAPRE     NINGUNA        SISA 
  #       139          71         136        8349        2870         121         169 

message(paste0("Número de pacientes con previsión desconocida: ",
data_long_establecimiento_2024_std %>% 
  dplyr::group_by(run) %>% 
  summarise(sum_desc= sum(glosa_prevision=="DESCONOCIDO"), 
            sum_capr=sum(glosa_prevision=="CAPREDENA"),
            sum_dipr=sum(glosa_prevision=="DIPRECA"),
            sum_fonas=sum(glosa_prevision=="FONASA"),
            sum_isapr=sum(glosa_prevision=="ISAPRE"),
            sum_ning=sum(glosa_prevision=="NINGUNA"),
            sum_sisa=sum(glosa_prevision=="SISA")) %>% 
  dplyr::filter(sum_desc>0) %>% 
  nrow()
))
#[1] "Número de pacientes con previsión desconocida: 62"

message(paste0("Número de pacientes con previsión desconocida: ",
data_long_establecimiento_2024_std %>% 
         dplyr::group_by(run) %>% 
         summarise(sum_desc= sum(glosa_prevision=="DESCONOCIDO"), 
                   sum_capr=sum(glosa_prevision=="CAPREDENA"),
                   sum_dipr=sum(glosa_prevision=="DIPRECA"),
                   sum_fonas=sum(glosa_prevision=="FONASA"),
                   sum_isapr=sum(glosa_prevision=="ISAPRE"),
                   sum_ning=sum(glosa_prevision=="NINGUNA"),
                   sum_sisa=sum(glosa_prevision=="SISA")) %>% 
         dplyr::filter(sum_desc>1) %>% 
         nrow()
))

#[1] "Número de pacientes con previsión desconocida: 6"

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
# - Si sum_desc==1, defina en la columna de reemplazo la columna de sum_* que no es sum_desc que tiene mayor frecuencia. 
# - Si hay empates con el resto de columnas sum_*, no elija sum_isapr. Si hay empates y no hay recuento de sum_isapr, 
# - indique la opción "IMPUTAR" en la columna de reemplazo
# - Si sum_desc>1, ver si hay un recuento de otra sum_* mayor o igual a sum_desc,
# en la columna de reemplazo indique la columna que cumpla esta condición. Para el resto de valores si sum_desc>1, 
# indique en la columna de reemplazo el valor "IMPUTAR"

library(bpmn)
bpmn_fig<-
bpmn("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/_figs/diagram_imputacion_prev_isapre.bpmn")



htmlwidgets::saveWidget(bpmn_fig, 
                  "H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/_figs/_diagram_imputacion_prev_isapre.html")
webshot::webshot("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/_figs/_diagram_imputacion_prev_isapre.html", 
                 "H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/_figs/_diagram_imputacion_prev_isapre.png",
                 vwidth = 300*1.2, vheight = 300,  zoom=10, expand=100)  # Prueba con diferentes coordenadas top, left, width, and height.


data_long_est_24_std_imp_prev <-
  data_long_establecimiento_2024_std %>%
  group_by(run) %>%
  summarise(
    sum_desc = sum(glosa_prevision == "DESCONOCIDO"),
    sum_capr = sum(glosa_prevision == "CAPREDENA"),
    sum_dipr = sum(glosa_prevision == "DIPRECA"),
    sum_fonas = sum(glosa_prevision == "FONASA"),
    sum_isapr = sum(glosa_prevision == "ISAPRE"),
    sum_ning = sum(glosa_prevision == "NINGUNA"),
    sum_sisa = sum(glosa_prevision == "SISA")
  ) %>%
  dplyr::filter(sum_desc>0) %>% 
  rowwise() %>%
  mutate(reemplazo = {
    # Get all sum_ values excluding sum_desc
    sums <- c(sum_capr, sum_dipr, sum_fonas, sum_isapr, sum_ning, sum_sisa)
    names_sums <- c("sum_capr", "sum_dipr", "sum_fonas", "sum_isapr", "sum_ning", "sum_sisa")
    
    if (sum_desc == 1) {
      max_val <- max(sums)
      indices_max <- which(sums == max_val)
      names_max <- names_sums[indices_max]
      
      # Exclude sum_isapr if possible
      if ("sum_isapr" %in% names_max && length(names_max) > 1) {
        names_max <- names_max[names_max != "sum_isapr"]
      }
      if (length(names_max) == 1) {
        names_max
      } else {
        "IMPUTAR"
      }
    } else if (sum_desc > 1) {
      # Find columns with values >= sum_desc
      valid_cols <- names_sums[sums >= sum_desc]
      if (length(valid_cols) == 1) {
        valid_cols
      } else if (length(valid_cols) > 1) {
        # More than one valid column, choose the one with max value or any if tied
        max_val_cols <- max(sums[sums >= sum_desc])
        max_cols <- names_sums[which(sums == max_val_cols)]
        max_cols[1]  # Choose the first one if tied
      } else {
        "IMPUTAR"
      }
    } else {
      "IMPUTAR"
    }
  }) %>%
  ungroup() %>% 
  dplyr::mutate(reemplazo_rec= dplyr::case_when(reemplazo=="sum_capr"~ "CAPREDENA",
                                  grepl("sum_dipr",reemplazo)~ "DIPRECA",
                                  grepl("sum_fonas",reemplazo)~ "FONASA",
                                  grepl("sum_sisa",reemplazo)~ "SISA",
                                  grepl("sum_isapr",reemplazo)~ "ISAPRE",
                                  grepl("sum_capr",reemplazo)~ "CAPREDENA",
                                  grepl("sum_ning",reemplazo)~ "NINGUNA",
                                  T~reemplazo)) 

data_long_est_24_std_imp_prev %>% janitor::tabyl(reemplazo_rec)
# reemplazo_rec  n    percent
#        DIPRECA  2 0.03225806
#         FONASA 27 0.43548387
#        IMPUTAR 18 0.29032258
#         ISAPRE 10 0.16129032
#           SISA  5 0.08064516


data_long_est_24_std_imp_rec_prev <-
  data_long_establecimiento_2024_std %>%
  dplyr::left_join(data_long_est_24_std_imp_prev[, c("run", "reemplazo_rec")], by = "run") %>%
  {if (nrow(.) > nrow(data_long_establecimiento_2024_std)) stop("left join added rows") else .} %>% 
  dplyr::mutate(glosa_prevision_rec= ifelse(!is.na(reemplazo_rec),reemplazo_rec,glosa_prevision),
                glosa_prevision_rec= ifelse(glosa_prevision_rec=="IMPUTAR",NA,glosa_prevision_rec)) #%>% 
  #janitor::tabyl(glosa_prevision_rec)
  #glimpse() 

data_long_est_24_std_imp_rec_prev %>% 
  janitor::tabyl(glosa_prevision_rec)
invisible("Añadimos más n")
 # glosa_prevision_rec    n     percent valid_percent
 #           CAPREDENA  138 0.011640658    0.01166526
 #             DIPRECA  139 0.011725011    0.01174979
 #              FONASA 8378 0.706706031    0.70819949
 #              ISAPRE 2880 0.242935470    0.24344886
 #             NINGUNA  121 0.010206664    0.01022823
 #                SISA  174 0.014677351    0.01470837
 #                <NA>   25 0.002108815            NA

data_long_est_24_std_imp_rec_prev$reemplazo_rec<-NULL

invisible("2024-05-10: Volver a traer la base pero ya no filtrar")


invisible("Dejo los que corresponden a fechas menores al 2018")
codigos_z_filt2<-
  codigos_z %>% 
  dplyr::filter(run %in% unique(data_long_est_24_std_imp_rec_prev$run)) %>% 
  dplyr::filter(fecha_ingreso_rec24<"2018-01-01") %>% 
  dplyr::arrange(run, fecha_ingreso_rec24) %>% 
  dplyr::mutate(dias_en_tto=fecha_egreso_rec24-fecha_ingreso_rec24) %>% 
  dplyr::mutate(dias_fuera_tto=lead(fecha_ingreso_rec24)-fecha_egreso_rec24) %>% 
  dplyr::group_by(run) %>% 
  dplyr::summarise(promedio_dias_fuera_tto= mean(dias_fuera_tto, na.rm=T), 
                   mediana_dias_fuera_tto= quantile(dias_fuera_tto, .5, na.rm=T),
                   promedio_dias_tto= mean(dias_en_tto), 
                   mediana_dias_tto= quantile(dias_en_tto, .5),
                   ttos= n()) %>% 
  dplyr::ungroup()

invisible("Los días fuera de tto funcionan PÉSIMO. PARECE HABER SUPERPOSICIÓN")

rm(codigos_z);gc()

data_long_est_24_std_imp_rec_prev2 <- 
  dplyr::left_join(data_long_est_24_std_imp_rec_prev, codigos_z_filt2[,c("run", "promedio_dias_tto", "ttos")], by="run")
if(nrow(data_long_est_24_std_imp_rec_prev2)> 
   nrow(data_long_est_24_std_imp_rec_prev)){
     stop("Se generaron filas")
     }else {
       data_long_est_24_std_imp_rec_prev<-data_long_est_24_std_imp_rec_prev2; rm(data_long_est_24_std_imp_rec_prev2)
     }

set.seed(2125)
data_long_est_24_std_imp_rec_imp<-
  missRanger::missRanger(
    formula =  glosa_prevision_rec ~ min_fecha_egreso_rec24_num+ factor(inclusivo_real_historico)+ min_edad_anos+ factor(glosa_sexo)+ factor(ESTAB_HOMO),#+ ttos+ promedio_dias_tto,
    data= data_long_est_24_std_imp_rec_prev %>% 
      dplyr::group_by(run) %>% 
      dplyr::mutate( min_edad_anos=min(edad_anos, na.rm=T)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(ttos=ifelse(!is.na(ttos),ttos, 0),  promedio_dias_tto= ifelse(!is.na( promedio_dias_tto),as.numeric( promedio_dias_tto),0)),
    num.trees = 900, 
    pmm.k = 100,                
    returnOOB=T,
    maxiter= 500,
    verbose = 2, 
    seed = 2125)
# Missing value imputation by random forests
# 
# Variables to impute:		glosa_prevision_rec
# Variables used to impute:	min_fecha_egreso_rec24_num, inclusivo_real_historico, min_edad_anos, glosa_sexo, ESTAB_HOMO
# 
# gls_p_
# iter 1:	0.0962 	
# iter 2:	0.0943 	
# iter 3:	0.0981 

# Missing value imputation by random forests
# 
# Variables to impute:		glosa_prevision_rec
# Variables used to impute:	min_fecha_egreso_rec24_num, inclusivo_real_historico, min_edad_anos, glosa_sexo, ESTAB_HOMO
# 
# gls_p_
# iter 1:	0.0963 	
# iter 2:	0.0967
invisible("Se generó un oob mayor, y hubo más error de clasificación si controlaba por numero de ttos. previos y días en tto.")

dput(round(attr(data_long_est_24_std_imp_rec_imp,"oob"),3))
#c(glosa_prevision_rec = 0.1)
#c(glosa_prevision_rec = 0.094)

invisible("Recodificar Previsión y Tramo de beneficios")
data_long_est_24_std_imp_rec_imp$prev_benef<-
  glue::glue("{factor(data_long_est_24_std_imp_rec_imp$glosa_prevision_rec)} {factor(data_long_est_24_std_imp_rec_imp$benef)}")

data_long_est_24_std_imp_rec_imp$prev_benef_rec <-
  car::recode(data_long_est_24_std_imp_rec_imp$prev_benef,
              "'CAPREDENA NA'='ISAPRE/FFAA';'DIPRECA B'='ISAPRE/FFAA';'DIPRECA NA'='ISAPRE/FFAA';'FONASA NA'='FONASA AB';'FONASA A'='FONASA AB';'FONASA B'='FONASA AB';'FONASA C'='FONASA CD';'FONASA D'='FONASA CD';'FONASA NA'='FONASA AB';'NINGUNA NA'='FONASA AB';'ISAPRE B'='ISAPRE/FFAA';'ISAPRE NA'='ISAPRE/FFAA';'SISA NA'='ISAPRE/FFAA';'SISA NA'='ISAPRE/FFAA'")
table(data_long_est_24_std_imp_rec_imp$prev_benef_rec, exclude=NULL)
#  FONASA AB   FONASA CD ISAPRE/FFAA 
#       6631        1892        3332 
invisible("Recodificación después de retroalimentación")
data_long_est_24_std_imp_rec_imp$prev_benef_rec_post <-
  car::recode(data_long_est_24_std_imp_rec_imp$prev_benef,
              "'CAPREDENA NA'='FFAA';'DIPRECA B'='FFAA';'DIPRECA NA'='FFAA';'FONASA NA'='FONASA A';'FONASA A'='FONASA A';'FONASA B'='FONASA BC';'FONASA C'='FONASA BC';'FONASA D'='FONASA D';'FONASA NA'='FONASA A';'NINGUNA NA'='FONASA A';'ISAPRE B'='ISAPRE';'ISAPRE NA'='ISAPRE';'SISA NA'='FFAA';'SISA NA'='FFAA'")
table(data_long_est_24_std_imp_rec_imp$prev_benef_rec_post, exclude=NULL)
# FFAA  FONASA A FONASA BC  FONASA D    ISAPRE 
#  451      3599      3847      1077      2881 


## 1.3. Especialidad de los cuidados entregados --------------------------------------------------


#areaf_egr= Nivel de Cuidado del cual egresó el paciente  (Niveles de Cuidado) 
#https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Frepositoriodeis.minsal.cl%2FEgresos%2Fegr2018%2FNiveles_Cuidado.xlsx&wdOrigin=BROWSELINK
niv_cuidado<-
  cbind.data.frame(Código = c(401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 412, 413, 414, 415, 416, 330, 418, 419, 420, 421, 422, 423, 424, 425, 426, 427, 428, 429), 
                   Descripción = c("Área Médica Adulto Cuidados Básicos", "Área Médica Adulto Cuidados Medios", "Área Médico-Quirúrgico Cuidados Básicos", "Área Médico-Quirúrgico Cuidados Medios", "Área Cuidados Intensivos Adultos", "Área Cuidados Intermedios Adultos", "Área Médica Pediátrica Cuidados Básicos", "Área Médica Pediátrica Cuidados Medios", "Área Médico-Quirúrgico Pediátrica Cuidados Básicos", "Área Médico-Quirúrgico Pediátrica Cuidados Medios", "Área Cuidados Intensivos Pediátricos", "Área Cuidados Intermedios Pediátricos", "Área Neonatología Cuidados Básicos", "Área Neonatología Cuidados Intensivos", "Área Neonatología Cuidados Intermedios", "Área Obstetricia", "Área Pensionado", "Área Psiquiatría Adulto Corta estadía", "Área Psiquiatría Adulto Mediana estadía", "Área Psiquiatría Adulto Larga estadía", "Área Psiquiatría Infanto-adolescente corta estadía", "Área Psiquiatría Infanto-adolescente mediana estadía", "Área Psiquiatría Forense Adulto evaluación e inicio tto.", "Área Psiquiatría Forense Adulto tratamiento", "Área Psiquiatría Forense Infanto Adolescente evaluación e inicio tto.", "Área Psiquiatría Forense Infanto Adolescente tratamiento", "Área Sociosanitaria Adulto", "Área de Hospitalización de Cuidados Intensivos en Psiquiatría Adulto", "Área de Hospitalización de Cuidados Intensivos en Psiquiatría Infanto Adolescente"))

niv_cuidado %>% 
  dplyr::filter(Código %in% unique(data_long_est_24_std_imp_rec_imp$areaf_egr))
# 1     401                                               Área Médica Adulto Cuidados Básicos
# 2     402                                                Área Médica Adulto Cuidados Medios
# 3     403                                           Área Médico-Quirúrgico Cuidados Básicos
# 4     404                                            Área Médico-Quirúrgico Cuidados Medios
# 5     405                                                  Área Cuidados Intensivos Adultos
# 6     406                                                 Área Cuidados Intermedios Adultos
# 7     407                                           Área Médica Pediátrica Cuidados Básicos
# 8     408                                            Área Médica Pediátrica Cuidados Medios
# 9     409                                Área Médico-Quirúrgico Pediátrica Cuidados Básicos
# 10    410                                 Área Médico-Quirúrgico Pediátrica Cuidados Medios
# 11    411                                              Área Cuidados Intensivos Pediátricos
# 12    412                                             Área Cuidados Intermedios Pediátricos
# 13    413                                                Área Neonatología Cuidados Básicos
# 14    415                                            Área Neonatología Cuidados Intermedios
# 15    416                                                                  Área Obstetricia
# 16    330                                                                   Área Pensionado
# 17    418                                             Área Psiquiatría Adulto Corta estadía
# 18    419                                           Área Psiquiatría Adulto Mediana estadía
# 19    420                                             Área Psiquiatría Adulto Larga estadía
# 20    421                                Área Psiquiatría Infanto-adolescente corta estadía
# 21    422                              Área Psiquiatría Infanto-adolescente mediana estadía
# 22    423                          Área Psiquiatría Forense Adulto evaluación e inicio tto.
# 23    424                                       Área Psiquiatría Forense Adulto tratamiento
# 24    426                          Área Psiquiatría Forense Infanto Adolescente tratamiento
# 25    427                                                        Área Sociosanitaria Adulto
# 26    428              Área de Hospitalización de Cuidados Intensivos en Psiquiatría Adulto
# 27    429 Área de Hospitalización de Cuidados Intensivos en Psiquiatría Infanto Adolescente

data_long_est_24_std_imp_rec_imp_areaf <-
  data_long_est_24_std_imp_rec_imp %>% 
    dplyr::left_join(niv_cuidado, by=c("areaf_egr"="Código"))%>%
    {if (nrow(.) > nrow(data_long_est_24_std_imp_rec_imp)) stop("left join added rows") else .} %>% 
  dplyr::rename("areaf_egr_glosa"="Descripción")



data.frame(table(data_long_est_24_std_imp_rec_imp_areaf$areaf_egr_glosa)) %>% arrange(desc(Freq))
#                                                                                 Var1 Freq
# 1                                              Área Psiquiatría Adulto Corta estadía 3635
# 2                                            Área Médico-Quirúrgico Cuidados Básicos 2431
# 3                                                Área Médica Adulto Cuidados Básicos 1308
# 4                                                                   Área Obstetricia  817
# 5                                 Área Psiquiatría Infanto-adolescente corta estadía  802
# 6                                             Área Médico-Quirúrgico Cuidados Medios  630
# 7                                              Área Psiquiatría Adulto Larga estadía  581
# 8                                                  Área Cuidados Intermedios Adultos  400
# 9                                                 Área Médica Adulto Cuidados Medios  286
# 10                                                                   Área Pensionado  180
# 11                                           Área Psiquiatría Adulto Mediana estadía  169
# 12                                                  Área Cuidados Intensivos Adultos  150
# 13              Área de Hospitalización de Cuidados Intensivos en Psiquiatría Adulto  112
# 14                          Área Psiquiatría Forense Infanto Adolescente tratamiento   83
# 15                                           Área Médica Pediátrica Cuidados Básicos   64
# 16                                Área Médico-Quirúrgico Pediátrica Cuidados Básicos   62
# 17                                             Área Cuidados Intermedios Pediátricos   30
# 18                                 Área Médico-Quirúrgico Pediátrica Cuidados Medios   30
# 19                          Área Psiquiatría Forense Adulto evaluación e inicio tto.   25
# 20                                            Área Médica Pediátrica Cuidados Medios   20
# 21                                              Área Cuidados Intensivos Pediátricos   15
# 22 Área de Hospitalización de Cuidados Intensivos en Psiquiatría Infanto Adolescente    7
# 23                                                Área Neonatología Cuidados Básicos    5
# 24                                       Área Psiquiatría Forense Adulto tratamiento    5
# 25                              Área Psiquiatría Infanto-adolescente mediana estadía    5
# 26                                            Área Neonatología Cuidados Intermedios    2
# 27                                                        Área Sociosanitaria Adulto    1

## 1.4. aclarar a qué pueblo originario pertenece cada sujeto --------------------------------------------------


invisible("Crear alguna regla de decisión. Hay casos que no tienen solución como ")
invisible("d2e0036004876e30530e94e902c667698aeef2522760310c90bedee14900d559 que tiene un valor con COLLA y otro con MAPUCHE")
invisible("o a01646ab8546d2a11a13fd873abec9d75e62afaf0e54a9173ae8ad204e92e575, lo mismo")
invisible("o a01646ab8546d2a11a13fd873abec9d75e62afaf0e54a9173ae8ad204e92e575, lo mismo")



## 1.5. hice ppoo por 3 categorias --------------------------------------------------

table(data_long_est_24_std_imp_rec_imp_areaf$inclusivo_real_historico, 
      data_long_est_24_std_imp_rec_imp_areaf$ppo_conadi2)
#      0    1
# 0 9415    0
# 1 1447  993


data_long_est_24_std_imp_rec_imp_areaf$factor_inclusivo_real_hist_mas_autperc<-
factor(paste0(data_long_est_24_std_imp_rec_imp_areaf$inclusivo_real_historico, 
              data_long_est_24_std_imp_rec_imp_areaf$ppo_conadi2))

table(data_long_est_24_std_imp_rec_imp_areaf$factor_inclusivo_real_hist_mas_autperc)
#   00   10   11 
# 9415 1447  993 

## 1.6. ver area_ref con nacimientos --------------------------------------------------

nac_enc17_22 <- read_delim("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/_output/nac_enc.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% janitor::clean_names()


invisible("Ver si hay gente entre los nacidos")
nac_enc17_22 %>% 
  dplyr::filter(run %in% (dplyr::filter(data_long_est_24_std_imp_rec_imp_areaf, grepl("Neonat", areaf_egr_glosa)) %>% distinct(run) %>% pull())
                  ) %>% nrow()
#[1] 0
nac_enc17_22 %>% 
  dplyr::filter(run %in% (data_long_est_24_std_imp_rec_imp_areaf %>% distinct(run) %>% pull())
  ) %>% nrow()
# 0
invisible("Resultó no habber gente en nacimientos: se elimina")
rm(nac_enc17_22);gc()


nacimientos <- read_delim("_data/nac_20240610140022_runmenc.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

nacimientos %>% 
  dplyr::filter(RUN %in% (dplyr::filter(data_long_est_24_std_imp_rec_imp_areaf, grepl("Neonat", areaf_egr_glosa)) %>% distinct(run) %>% pull())
  ) %>% nrow()
#[1] 0
nacimientos %>% 
  dplyr::filter(RUN %in% (data_long_est_24_std_imp_rec_imp_areaf %>% distinct(run) %>% pull())
  ) %>% nrow()
# 0
rm(nacimientos);gc()

invisible("Cuántos episodios hospitalarios son de ignresos de mayores de 18 años en área adolescente o pediátrico??")

invisible("Cuántos episodios hospitalarios son de ignresos de menores de 18 años en área adulto??")
dplyr::filter(data_long_est_24_std_imp_rec_imp_areaf, grepl("Adult", areaf_egr_glosa), edad_anos<18) %>% nrow()
#1421
dplyr::filter(data_long_est_24_std_imp_rec_imp_areaf, grepl("Adult", areaf_egr_glosa), edad_anos<18) %>% distinct(run) %>% nrow()
#1020

message(paste0("Menos de 18 en área de adultos: ",
               scales::percent(dplyr::filter(data_long_est_24_std_imp_rec_imp_areaf, 
                grepl("Adult", areaf_egr_glosa), edad_anos<18) %>% nrow()/nrow(data_long_est_24_std_imp_rec_imp_areaf))
               ))
#Menos de 18 en área de adultos: 12%


## Hay adultos o adolescentes que ingresen como adolescentes?
dplyr::filter(data_long_est_24_std_imp_rec_imp_areaf, grepl("Adoles", areaf_egr_glosa), edad_anos>=18) %>% nrow()
#8 
dplyr::filter(data_long_est_24_std_imp_rec_imp_areaf, grepl("Adoles", areaf_egr_glosa), edad_anos>=18) %>% distinct(run) %>% nrow()
#7

invisible("Ver gente menor a 15 años, por qué!!?!?!?!; actualización 2024-05-11= Ya no me debería salir. LA SAQUÉ")
data_long_est_24_std_imp_rec_imp_areaf %>% dplyr::filter(edad_anos<15) %>% pull(edad_anos)
#0

data_long_est_24_std_imp_rec_imp_areaf %>% dplyr::filter(run %in%
(data_long_est_24_std_imp_rec_imp_areaf %>% dplyr::filter(edad_anos<15) %>% pull(run))) %>% nrow()
#0

#save.image(paste0("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/20240508.RData"))
save.image(paste0("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/20240702.RData"))

## 1.7. ver area_ref con regression trees --------------------------------------------------
#load(paste0("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/20240702.RData"))

#https://www.statmethods.net/advstats/cart.html 

### 1.7.a clasificacion preliminar --------------------------------------------------

# Hay adultos o adolescentes que ingresen como adolescentes?
#   A veces por disponibilidad
# Hay legislación por DDHH
# Deberían estar separados, pero no ocurre porque es muy menor la disponibilidad las camas
# Nivel de cuidados o intensidad; corta o larga, dos paradigmas para mirar la situación
# La especialidad no es tan informativa
# Pero creo que se debe dejar el adultos/jóvenes
# Exploratoriamente, trabaja como dummies? Como un decision trees, para ver la clasificación. 
# Formas de clasificar que puedes categorizar. Ver si un grupo no es tan importante y sacarlo. Que no separe entre árboles


# médico-quirúrgicas: Son las que habitualmente usan tanto técnicas invasivas (quirúrgicas) como no invasivas (farmacológicas, etc).

threshold <- 50  # Dejar un umbral para clasificación "Otros"

data_long_est_24_std_imp_rec_imp_areaf_rec <- data_long_est_24_std_imp_rec_imp_areaf %>%
  dplyr::mutate(areaf_egr_glosa_rec= dplyr::case_when(areaf_egr_glosa=="Área Psiquiatría Adulto Corta estadía"~"alta_complejidad", #dudas
                areaf_egr_glosa=="Área Médico-Quirúrgico Cuidados Básicos"~"med_baj_complejidad", #generalista
                areaf_egr_glosa=="Área Médica Adulto Cuidados Básicos"~"med_baj_complejidad", #generalista
                areaf_egr_glosa=="Área Obstetricia"~ "med_baj_complejidad", # menor complejidad, art. 1
                areaf_egr_glosa=="Área Psiquiatría Infanto-adolescente corta estadía"~ "alta_complejidad", #dudas
                areaf_egr_glosa=="Área Médico-Quirúrgico Cuidados Medios"~ "med_baj_complejidad", #generalista
                areaf_egr_glosa=="Área Psiquiatría Adulto Larga estadía"~ "alta_complejidad", #dudas: tal vez requieran altos niveles de supervisión, junto con presentar inestabilidad psicopatológica crónica
                areaf_egr_glosa=="Área Cuidados Intermedios Adultos"~ "med_baj_complejidad", 
                areaf_egr_glosa=="Área Médica Adulto Cuidados Medios"~ "med_baj_complejidad",
                areaf_egr_glosa=="Área Pensionado"~ "med_baj_complejidad", #residuo histórico, copago mayor, más cercano a médico-quirúrgico cuidados básicos
                areaf_egr_glosa=="Área Psiquiatría Adulto Mediana estadía"~ "alta_complejidad",
                areaf_egr_glosa=="Área Cuidados Intensivos Adultos"~ "alta_complejidad",
                areaf_egr_glosa=="Área de Hospitalización de Cuidados Intensivos en Psiquiatría Adulto"~ "alta_complejidad",
                areaf_egr_glosa=="Área Psiquiatría Forense Infanto Adolescente tratamiento"~ "alta_complejidad", #dudas
                areaf_egr_glosa=="Área Médica Pediátrica Cuidados Básicos"~ "med_baj_complejidad", #+areas basicas, generalista
                areaf_egr_glosa=="Área Médico-Quirúrgico Pediátrica Cuidados Básicos"~ "med_baj_complejidad", #+areas basicas, generalista
                areaf_egr_glosa=="Área Cuidados Intermedios Pediátricos"~ "med_baj_complejidad",
                areaf_egr_glosa=="Área Médico-Quirúrgico Pediátrica Cuidados Medios"~ "med_baj_complejidad",
                areaf_egr_glosa=="Área Psiquiatría Forense Adulto evaluación e inicio tto."~ "alta_complejidad", #dudas
                areaf_egr_glosa=="Área Médica Pediátrica Cuidados Medios"~ "med_baj_complejidad",
                areaf_egr_glosa=="Área Cuidados Intensivos Pediátricos"~ "alta_complejidad",
                areaf_egr_glosa=="Área de Hospitalización de Cuidados Intensivos en Psiquiatría Infanto Adolescente"~ "alta_complejidad",
                areaf_egr_glosa=="Área Neonatología Cuidados Básicos"~ "descartar",
                areaf_egr_glosa=="Área Psiquiatría Forense Adulto tratamiento"~ "alta_complejidad",
                areaf_egr_glosa=="Área Psiquiatría Infanto-adolescente mediana estadía"~ "alta_complejidad", #dudas
                areaf_egr_glosa=="Área Neonatología Cuidados Intermedios"~ "med_baj_complejidad", #dudas
                areaf_egr_glosa=="Área Sociosanitaria Adulto"~ "med_baj_complejidad", #ej., demencia señil; más parecido a una cama básica
                T~""
                )) %>% 
  dplyr::filter(areaf_egr_glosa_rec!="descartar") #Neonatología Cuidados Básicos
table(data_long_est_24_std_imp_rec_imp_areaf_rec$areaf_egr_glosa_rec, exclude=NULL)

# alta_complejidad           descartar med_baj_complejidad 
#             5587                   5                6261 
   # alta_complejidad med_baj_complejidad 
   #             5589                6261 

#table(data_long_est_24_std_imp_rec_imp_areaf_rec$areaf_egr_glosa_rec, exclude=NULL)
#



table(data_long_est_24_std_imp_rec_imp_areaf_rec$tiene_servicio_de_urgencia, 
      data_long_est_24_std_imp_rec_imp_areaf_rec$nivel_de_complejidad)
#    Alta Complejidad Baja Complejidad Mediana Complejidad Pendiente Sin dato
# NO              758              118                1082       213       91
# SI             6936             1573                1074         5        0

#El demediana complejidadd es el que tiene menos servicios de orgencia en terminos relativos. Los sin dato no tienen servicios de urgencia y los pendiente tienen menos del 2%


table(data_long_est_24_std_imp_rec_imp_areaf_rec$tipo_de_prestador_sistema_de_salud, 
      data_long_est_24_std_imp_rec_imp_areaf_rec$nivel_de_complejidad)
#                            Alta Complejidad Baja Complejidad Mediana Complejidad Pendiente Sin dato
# Fuerzas Armadas y de Orden              217               92                  35         4        0
# Pendiente                                 6               54                   0         0        0
# Privado                                2150               65                 970       213       91
# Público                                5321             1480                1151         1        0
#La mayoría de las instituciones son públicas de alta complejidad, segudia por privadas de alta compeljidad, y de ahi públicas de mediana y baja complejidad
#Las unicas instituciones que tienen complejidad pendiente o sin dato son las privadas

table(data_long_est_24_std_imp_rec_imp_areaf_rec$areaf_egr_glosa, 
      data_long_est_24_std_imp_rec_imp_areaf_rec$nivel_de_complejidad)
#                                                                                 Alta Complejidad Baja Complejidad Mediana Complejidad Pendiente Sin dato
# Área Cuidados Intensivos Adultos                                                               141                1                   6         2        0
# Área Cuidados Intensivos Pediátricos                                                            14                0                   0         1        0
# Área Cuidados Intermedios Adultos                                                              371                3                  26         0        0
# Área Cuidados Intermedios Pediátricos                                                           30                0                   0         0        0
# Área de Hospitalización de Cuidados Intensivos en Psiquiatría Adulto                           112                0                   0         0        0
# Área de Hospitalización de Cuidados Intensivos en Psiquiatría Infanto Adolescente                7                0                   0         0        0
# Área Médica Adulto Cuidados Básicos                                                            408              817                  78         5        0
# Área Médica Adulto Cuidados Medios                                                             219               19                  48         0        0
# Área Médica Pediátrica Cuidados Básicos                                                         43               19                   2         0        0
# Área Médica Pediátrica Cuidados Medios                                                          17                0                   3         0        0
# Área Médico-Quirúrgico Cuidados Básicos                                                       1469              607                 352         2        1
# Área Médico-Quirúrgico Cuidados Medios                                                         445               24                 161         0        0
# Área Médico-Quirúrgico Pediátrica Cuidados Básicos                                              59                0                   3         0        0
# Área Médico-Quirúrgico Pediátrica Cuidados Medios                                               30                0                   0         0        0
# Área Neonatología Cuidados Intermedios                                                           2                0                   0         0        0
# Área Obstetricia                                                                               670               52                  92         1        2
# Área Pensionado                                                                                147                9                  24         0        0
# Área Psiquiatría Adulto Corta estadía                                                         2207               64                1078       198       88
# Área Psiquiatría Adulto Larga estadía                                                          574                0                   5         2        0
# Área Psiquiatría Adulto Mediana estadía                                                         69                0                  93         7        0
# Área Psiquiatría Forense Adulto evaluación e inicio tto.                                        12                2                  11         0        0
# Área Psiquiatría Forense Adulto tratamiento                                                      5                0                   0         0        0
# Área Psiquiatría Forense Infanto Adolescente tratamiento                                        83                0                   0         0        0
# Área Psiquiatría Infanto-adolescente corta estadía                                             555               73                 174         0        0
# Área Psiquiatría Infanto-adolescente mediana estadía                                             5                0                   0         0        0
# Área Sociosanitaria Adulto                                                                       0                1                   0         0        0

prop.table(table(data_long_est_24_std_imp_rec_imp_areaf_rec$areaf_egr_glosa, 
                 data_long_est_24_std_imp_rec_imp_areaf_rec$nivel_de_complejidad),1) %>% 
  data.frame() %>% 
  dplyr::filter(Var2!="Alta Complejidad") %>% 
  group_by(Var1) %>% summarise(sum= sum(Freq)) %>% 
  dplyr::arrange(desc(sum)) %>%  head(15)
# Las que no remiten a establecimientos de alta complejidad en mayor porcentaje son sociosanitaria (0%), area médica adulto cuidados básicos (31%, más en estab. de Baja),
# Área Psiquiatría Adulto Mediana estadía (0.41), y en  Área Psiquiatría Forense Adulto evaluación e inicio tto, (48%)

# Más en mediana= Área Psiquiatría Adulto Mediana estadía (55%) y Área Psiquiatría Forense Adulto evaluación e inicio tto. (44%)
prop.table(table(data_long_est_24_std_imp_rec_imp_areaf_rec$areaf_egr_glosa, 
                 data_long_est_24_std_imp_rec_imp_areaf_rec$nivel_de_complejidad),1) %>% 
  data.frame() %>% 
  dplyr::filter(Var2=="Alta Complejidad") %>% 
  group_by(Var1) %>% summarise(sum= sum(Freq)) %>% 
  dplyr::arrange(desc(sum)) %>%  head(20)

prop.table(table(data_long_est_24_std_imp_rec_imp_areaf_rec$areaf_egr_glosa, 
                 data_long_est_24_std_imp_rec_imp_areaf_rec$nivel_de_complejidad),1) %>% 
  data.frame() %>% 
  dplyr::filter(Var2=="Baja Complejidad") %>% 
  group_by(Var1) %>% summarise(sum= sum(Freq)) %>% 
  dplyr::arrange(desc(sum)) %>%  head(20)

table(data_long_est_24_std_imp_rec_imp_areaf_rec$areaf_egr_glosa, 
      data_long_est_24_std_imp_rec_imp_areaf_rec$modalidad_de_atencion)
#                                                                                   Atención Abierta-Ambulatoria Atención Cerrada-Hospitalaria Sin dato
# Área Cuidados Intensivos Adultos                                                                             0                           150        0
# Área Cuidados Intensivos Pediátricos                                                                         0                            15        0
# Área Cuidados Intermedios Adultos                                                                            0                           400        0
# Área Cuidados Intermedios Pediátricos                                                                        0                            30        0
# Área de Hospitalización de Cuidados Intensivos en Psiquiatría Adulto                                         0                           112        0
# Área de Hospitalización de Cuidados Intensivos en Psiquiatría Infanto Adolescente                            0                             7        0
# Área Médica Adulto Cuidados Básicos                                                                          0                          1308        0
# Área Médica Adulto Cuidados Medios                                                                           0                           286        0
# Área Médica Pediátrica Cuidados Básicos                                                                      0                            64        0
# Área Médica Pediátrica Cuidados Medios                                                                       0                            20        0
# Área Médico-Quirúrgico Cuidados Básicos                                                                      0                          2430        1
# Área Médico-Quirúrgico Cuidados Medios                                                                       0                           630        0
# Área Médico-Quirúrgico Pediátrica Cuidados Básicos                                                           0                            62        0
# Área Médico-Quirúrgico Pediátrica Cuidados Medios                                                            0                            30        0
# Área Neonatología Cuidados Intermedios                                                                       0                             2        0
# Área Obstetricia                                                                                             0                           815        2
# Área Pensionado                                                                                              0                           180        0
# Área Psiquiatría Adulto Corta estadía                                                                        0                          3547       88
# Área Psiquiatría Adulto Larga estadía                                                                        2                           579        0
# Área Psiquiatría Adulto Mediana estadía                                                                      0                           169        0
# Área Psiquiatría Forense Adulto evaluación e inicio tto.                                                     0                            25        0
# Área Psiquiatría Forense Adulto tratamiento                                                                  0                             5        0
# Área Psiquiatría Forense Infanto Adolescente tratamiento                                                     0                            83        0
# Área Psiquiatría Infanto-adolescente corta estadía                                                           0                           802        0
# Área Psiquiatría Infanto-adolescente mediana estadía                                                         0                             5        0
# Área Sociosanitaria Adulto                                                                                   0                             1        0
#Área Psiquiatría Adulto Corta estadía ES EL UNICO  que dice algo de la modalidad de atención: sin dato; y solo tiene información de 112222
#Sólo hay ambulatoria abierta en 2 casos de  Área Psiquiatría Adulto Larga estadía   table(data_long_rec_prev_imp_cod_areaf_establecimiento_2024$areaf_egr_glosa, 

table(data_long_est_24_std_imp_rec_imp_areaf_rec$areaf_egr_glosa, 
      data_long_est_24_std_imp_rec_imp_areaf_rec$tipo_de_prestador_sistema_de_salud)
#                                                                                   Fuerzas Armadas y de Orden Pendiente Privado Público
# Área Cuidados Intensivos Adultos                                                                           2         0     122      26
# Área Cuidados Intensivos Pediátricos                                                                       0         0      14       1
# Área Cuidados Intermedios Adultos                                                                          7         0     328      65
# Área Cuidados Intermedios Pediátricos                                                                      0         0      26       4
# Área de Hospitalización de Cuidados Intensivos en Psiquiatría Adulto                                       0         0       0     112
# Área de Hospitalización de Cuidados Intensivos en Psiquiatría Infanto Adolescente                          0         0       0       7
# Área Médica Adulto Cuidados Básicos                                                                       65         9      17    1217
# Área Médica Adulto Cuidados Medios                                                                        30         0      33     223
# Área Médica Pediátrica Cuidados Básicos                                                                    0         0       1      63
# Área Médica Pediátrica Cuidados Medios                                                                     0         0       3      17
# Área Médico-Quirúrgico Cuidados Básicos                                                                   55        36     882    1458
# Área Médico-Quirúrgico Cuidados Medios                                                                     1         0     124     505
# Área Médico-Quirúrgico Pediátrica Cuidados Básicos                                                         0         0      44      18
# Área Médico-Quirúrgico Pediátrica Cuidados Medios                                                          0         0       4      26
# Área Neonatología Cuidados Intermedios                                                                     0         0       2       0
# Área Obstetricia                                                                                          16        11     138     652
# Área Pensionado                                                                                           15         4      97      64
# Área Psiquiatría Adulto Corta estadía                                                                      0         0     987    2648
# Área Psiquiatría Adulto Larga estadía                                                                     88         0     492       1
# Área Psiquiatría Adulto Mediana estadía                                                                   69         0      84      16
# Área Psiquiatría Forense Adulto evaluación e inicio tto.                                                   0         0       1      24
# Área Psiquiatría Forense Adulto tratamiento                                                                0         0       0       5
# Área Psiquiatría Forense Infanto Adolescente tratamiento                                                   0         0       0      83
# Área Psiquiatría Infanto-adolescente corta estadía                                                         0         0      90     712
# Área Psiquiatría Infanto-adolescente mediana estadía                                                       0         0       0       5
# Área Sociosanitaria Adulto                                                                                 0         0       0       1

#Área Psiquiatría Adulto Corta estadía ES EL UNICO  que dice algo de la modalidad de atención: sin dato; y solo tiene información de 112222
#si veo el area por tipo de prestador, me aparece que el privado tiene más preponderancia en Área Cuidados Intensivos Adultos;  Área Pensionado ; Área Cuidados Intermedios Adultos; Área Psiquiatría Adulto Corta estadía ; Área Psiquiatría Adulto Larga estadía;  Área Psiquiatría Adulto Mediana estadía 
#tambien el privado tiene harta pero menos que publico en Área Médico-Quirúrgico Cuidados Básicos;   Área Médico-Quirúrgico Cuidados Medios;  Área Psiquiatría Adulto Corta estadía; Área Obstetricia;  Área Psiquiatría Infanto-adolescente corta estadía
#Privado no tiene presencia en   Área Psiquiatría Forense Infanto Adolescente tratamiento; Área de Hospitalización de Cuidados Intensivos en Psiquiatría Adulto    
#Algo de presencia de FFAA en Área Médica Adulto Cuidados Básicos; Área Médica Adulto Cuidados Medios ;  Área Médico-Quirúrgico Cuidados Básicos; Área Psiquiatría Adulto Larga estadía ; Área Psiquiatría Adulto Mediana estadía 
#


# Crear el gráfico de pie
  data.frame(Complejidad = c("Alta", "Mediana/Baja"), Count = c(5587, 6261)) %>% 
ggplot(aes(x = "", y = Count, fill = Complejidad)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_minimal() +
  labs(title = "Distribución de la Complejidad",
       x = NULL,
       y = NULL) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank()) +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.title = element_blank())

ggsave(filename= "H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/_figs/complejidad.png", width=8/2, height=5/2,dpi=600)

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
# 2024-07-22
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
table(data_long_est_24_std_imp_rec_imp_areaf_rec$ESTAB_HOMO,
      data_long_est_24_std_imp_rec_imp_areaf_rec$nivel_de_atencion) %>% 
        data.frame() %>%  
        dplyr::filter(Var2=="No Aplica", Freq>0) %>% 
        dplyr::arrange(desc(Freq))

#table(data_long_est_24_std_imp_rec_imp_areaf_rec$ESTAB_HOMO,data_long_est_24_std_imp_rec_imp_areaf_rec$nivel_de_atencion) %>% data.frame() %>%  dplyr::filter(Var2=="No Aplica", Freq>0) %>% arrange(desc(Freq)) %>% pull(Var1) %>% as.character() %>% dput()
nivel_atencion_no_aplica<-
c("200091", "112252", "112229", "112241", "112207", "123203", 
  "103215", "112205", "112215", "112258", "112260", "112264", "118202", 
  "111294", "112224", "112259", "111277", "200566", "110201", "200234", 
  "124274", "111276", "112217", "102221", "112510", "111219", "120205", 
  "116262", "112242", "112278", "200053", "118210", "200193", "106205", 
  "111211", "111221", "112218", "112276", "112507")

X717508813_Establecimientos_ChileDEIS_MINSAL_29_04_2022 %>% dplyr::filter(codigo_vigente %in% nivel_atencion_no_aplica) %>% pull(nombre_oficial)
# [1] "Clínica Complejo Penitenciario de Puerto Montt"                     "Clínica Monteverde SpA"                                            
# [3] "Clínica Alemana de Osorno"                                          "Clínica Adventista"                                                
# [5] "Servicios Clínicos Neuropsiquiátricos y Geriátricos R y G Limitada" "Clínica de la Mujer Sanatorio Alemán"                              
# [7] "Clínica Regional Lircay"                                            "Clínica San Antonio"                                               
# [9] "Clínica Universidad de Chile Quilín"                                "Clínica del Carmen"                                                
# [11] "Establecimiento Penitenciario Santiago 1"                           "Clínica Macul"                                                     
# [13] "Clínica Santa Rosa"                                                 "Clínica Juan Pablo II"                                             
# [15] "Clínica Psicoterapia los Tiempos"                                   "Clínica Pedro Montt"                                               
# [17] "Clínica Oriente"                                                    "Clínica Psiquiatrica Renacer"                                      
# [19] "Clínica Recuperación Alcohol Alfa"                                  "Clínica Central"                                                   
# [21] "Clínica Instituto El Cedro"                                         "Clínica UC Red de Salud UC CHRISTUS"                               
# [23] "Clínica Psiquiátrica Santa Cecilia"                                 "Mirandes S.P.A."                                                   
# [25] "Clínica Miguel Claro"                                               "Clínica Psiquiatrica Bretaña"                                      
# [27] "Clínica Quantumm"                                                   "Clínica WLK"                                                       
# [29] "Clínica Ensenada"                                                   "Pensionado San José"                                               
# [31] "Clínica Fundación Médica San Cristóbal"                             "Clínica IST Vitacura"                                              
# [33] "Clínica Colonial"                                                   "Clínica Psiquiatrica Pocuro"                                       
# [35] "Clínica MEDS La Dehesa"                                             "Instituto Médico el Arrayán"                                       
# [37] "Clínica Monteblanco"                                                "Clínica Oriente"                                                   
# [39] "Clínica Establecimiento Penitenciario Alto Hospicio" 
X717508813_Establecimientos_ChileDEIS_MINSAL_29_04_2022 %>% dplyr::filter(codigo_vigente %in% nivel_atencion_no_aplica) %>% pull(pertenencia_al_snss)
#Sólo no pertenece

data_long_est_24_std_imp_rec_imp_areaf_rec_cod<-
data_long_est_24_std_imp_rec_imp_areaf_rec %>% 
  dplyr::mutate(code_niv_at_mod_area_compl= dplyr::case_when(
    grepl("Terciario", nivel_de_atencion)& 
    grepl("Alta",nivel_de_complejidad) &
    areaf_egr_glosa_rec=="alta_complejidad"~ "Terciario-inst alta-area alta",
    grepl("Terciario", nivel_de_atencion)& 
    grepl("Alta",nivel_de_complejidad) &
    areaf_egr_glosa_rec=="med_baj_complejidad"~ "Terciario-inst alta-area med/baja",
    
    grepl("Terciario", nivel_de_atencion)& 
    !grepl("Alta",nivel_de_complejidad) &
    areaf_egr_glosa_rec=="alta_complejidad"~ "Terciario-inst med/baja-area alta",
    grepl("Terciario", nivel_de_atencion)& 
    !grepl("Alta",nivel_de_complejidad) &
    areaf_egr_glosa_rec=="med_baj_complejidad"~ "Terciario-inst med/baja-area med/baja",   

    
    grepl("Secundario", nivel_de_atencion)& 
    grepl("Alta",nivel_de_complejidad) &
    areaf_egr_glosa_rec=="alta_complejidad"~ "Primario/secundario-inst alta-area alta",
    grepl("Secundario", nivel_de_atencion)& 
    grepl("Alta",nivel_de_complejidad) &
    areaf_egr_glosa_rec=="med_baj_complejidad"~ "Primario/secundario-inst alta-area med/baja",
    
    grepl("Secundario", nivel_de_atencion)& 
    !grepl("Alta",nivel_de_complejidad) &
    areaf_egr_glosa_rec=="alta_complejidad"~ "Primario/secundario-inst med/baja-area alta",
    grepl("Secundario", nivel_de_atencion)& 
    !grepl("Alta",nivel_de_complejidad) &
    areaf_egr_glosa_rec=="med_baj_complejidad"~ "Primario/secundario-inst alta-area med/baja",   
    
    
    grepl("No Aplica|Pendiente", nivel_de_atencion)& 
      grepl("Alta",nivel_de_complejidad) &
      areaf_egr_glosa_rec=="alta_complejidad"~ "No aplica/pendiente-inst alta-area alta",
    grepl("No Aplica|Pendiente", nivel_de_atencion)& 
      grepl("Alta",nivel_de_complejidad) &
      areaf_egr_glosa_rec=="med_baj_complejidad"~ "No aplica/pendiente-inst alta-area med/baja",
    
    grepl("No Aplica|Pendiente", nivel_de_atencion)& 
      !grepl("Alta",nivel_de_complejidad) &
      areaf_egr_glosa_rec=="alta_complejidad"~ "No aplica/pendiente-inst med/baja-area alta",
    grepl("No Aplica|Pendiente", nivel_de_atencion)& 
      !grepl("Alta",nivel_de_complejidad) &
      areaf_egr_glosa_rec=="med_baj_complejidad"~ "No aplica/pendiente-inst alta-area med/baja"
                   )) %>% 
  
    dplyr::mutate(nivel_de_atencion_rec= dplyr::case_when(
    grepl("Terciario", nivel_de_atencion)~ "3. Terciario",
    grepl("Secundario", nivel_de_atencion)~ "2. Secundario",
    grepl("No Aplica|Pendiente", nivel_de_atencion)~ "1. No aplica/pendiente",
    T~ nivel_de_atencion)) %>% 
    dplyr::mutate(nivel_de_complejidad_rec= dplyr::case_when(
    grepl("Alta", nivel_de_complejidad)~ "Alta",
    !grepl("Alta", nivel_de_complejidad)~ "Media-baja"))
    

table(data_long_est_24_std_imp_rec_imp_areaf_rec_cod$code_niv_at_mod_area_compl, exclude=NULL) %>% data.frame()
#                                          Var1 Freq
# 1     No aplica/pendiente-inst alta-area alta    1
# 2 No aplica/pendiente-inst alta-area med/baja  239
# 3 No aplica/pendiente-inst med/baja-area alta 1122
# 4 Primario/secundario-inst alta-area med/baja 1444
# 5 Primario/secundario-inst med/baja-area alta  126
# 6               Terciario-inst alta-area alta 3783
# 7           Terciario-inst alta-area med/baja 3770
# 8           Terciario-inst med/baja-area alta  555
# 9       Terciario-inst med/baja-area med/baja  808
# 

table(data_long_est_24_std_imp_rec_imp_areaf_rec_cod$code_niv_at_mod_area_compl, 
      data_long_est_24_std_imp_rec_imp_areaf_rec_cod$codigo_region==13)
#                                             FALSE TRUE
# No aplica/pendiente-inst alta-area alta         1    0
# No aplica/pendiente-inst alta-area med/baja   151   88
# No aplica/pendiente-inst med/baja-area alta   136  986
# Primario/secundario-inst alta-area med/baja  1409   35
# Primario/secundario-inst med/baja-area alta    53   73
# Terciario-inst alta-area alta                1847 1936
# Terciario-inst alta-area med/baja            1920 1850
# Terciario-inst med/baja-area alta             453  102
# Terciario-inst med/baja-area med/baja         733   75

# 2024-07-27
  table(data_long_est_24_std_imp_rec_imp_areaf_rec_cod$code_niv_at_mod_area_compl,
data_long_est_24_std_imp_rec_imp_areaf_rec_cod$tiene_servicio_de_urgencia,#tipo_de_prestador_sistema_de_salud,
      exclude=NULL)
#tipo_de_prestador_sistema_de_salud     

round(prop.table(table(data_long_est_24_std_imp_rec_imp_areaf_rec_cod$code_niv_at_mod_area_compl, 
                       data_long_est_24_std_imp_rec_imp_areaf_rec_cod$tipo_establecimiento,#tiene_servicio_de_urgencia,#tipo_de_prestador_sistema_de_salud,
                       exclude=NULL),1),2)
#Los no aplica/pendiente son en su mayoria clínicas (>=80%), el resto son >=69% son hospital

round(prop.table(table(data_long_est_24_std_imp_rec_imp_areaf_rec_cod$code_niv_at_mod_area_compl, 
                       data_long_est_24_std_imp_rec_imp_areaf_rec_cod$tipo_de_prestador_sistema_de_salud,
                       exclude=NULL),1),2)
#>=75% son establecimientos privados, vs. el >=61% son públicos

#Ver gente en complejos penitenciarios
data_long_est_24_std_imp_rec_imp_areaf_rec_cod %>% 
  dplyr::filter(ESTAB_HOMO %in% c(124274, 111294, 102221)) %>% nrow()
data_long_est_24_std_imp_rec_imp_areaf_rec_cod %>% 
  dplyr::filter(ESTAB_HOMO %in% c(124274, 111294, 102221)) %>% distinct(run)

### 1.7.b concordancia --------------------------------------------------

# médico-quirúrgicas: Son las que habitualmente usan tanto técnicas invasivas (quirúrgicas) como no invasivas (farmacológicas, etc).

## Antes de 2024-07-22

data_long_est_24_std_imp_rec_imp_areaf_rec[, c("areaf_egr_glosa", "areaf_egr_glosa_rec","tiene_servicio_de_urgencia", "tipo_establecimiento", "nivel_de_atencion", 
                                                          "tipo_de_prestador_sistema_de_salud", "nivel_de_complejidad", "modalidad_de_atencion")] %>% 
  rio::export(file="H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/_output/unido_std_2024.xlsx")

### 1.7.c  Exploración --------------------------------------------------

#### 1.7.c.1.  Duplicados --------------------------------------------------

#2024-07-27

paste0("Pacientes duplicados: ",
       data_long_est_24_std_imp_rec_imp_areaf_rec_cod %>% 
         janitor::get_dupes(run, fecha_ingreso_rec_fmt, fecha_egreso_rec_fmt) %>% 
         dplyr::distinct(run) %>% nrow(),
       "; eventos: ",
       data_long_est_24_std_imp_rec_imp_areaf_rec_cod %>% 
         janitor::get_dupes(run, fecha_ingreso_rec_fmt, fecha_egreso_rec_fmt) %>% 
         nrow()
)
data_long_est_24_std_imp_rec_imp_areaf_rec_cod %>% 
  janitor::get_dupes(run, fecha_ingreso_rec_fmt, fecha_egreso_rec_fmt) %>%
  dplyr::group_by(run, fecha_ingreso_rec_fmt, fecha_egreso_rec_fmt) %>% 
  dplyr::summarise(n_dis_alph= n_distinct(alphabet), 
                   n_dis_estab= n_distinct(ESTAB_HOMO),
                   n_dis_diag1= n_distinct(diag1),
                   n_dis_diag2= n_distinct(diag2),
                   n_dis_diag3= n_distinct(diag3),
                   n_dis_prev_benef= n_distinct(prev_benef_rec_post))
#   run                           fecha_ingreso_rec_fmt fecha_egreso_rec_fmt n_dis_alph n_dis_estab n_dis_diag1 n_dis_diag2 n_dis_diag3 n_dis_prev_benef
#   <chr>                         <date>                <date>                    <int>       <int>       <int>       <int>       <int>            <int>
# 1 06460087980bfe59db9a6ae25773… 2018-11-09            2018-12-24                    1           2           1           1           1                1
# 2 4496b95ebd8df45e3b7e0e16a635… 2018-11-25            2018-12-12                    1           2           1           1           1                1
# 3 4a42993fa3c7fc53771cae035bd5… 2018-05-29            2018-05-30                    1           2           1           1           1                2
# 4 4a42993fa3c7fc53771cae035bd5… 2019-10-01            2019-10-02                    2           2           2           1           2                2
# 5 584dcdbde33318ef6fbf9f41f201… 2018-08-14            2018-09-03                    1           1           1           1           1                2
# 6 ab07663c076a17d45aea92438789… 2018-11-22            2018-12-06                    1           2           1           1           1                1
# 7 acb305b617affa25e93280e0bce5… 2018-12-06            2018-12-22                    1           2           1           1           1                1
  
invisible("Por lo visto, están en instituciones distintas.")
invisible("Hubo un caso que tiene sólo distinto la previsión")
invisible("NO ERAN VERDADEROS DUPLICADOS")

paste0("Pacientes duplicados, AHORA INCLUYENDO INSTITUCION: ",
       data_long_est_24_std_imp_rec_imp_areaf_rec_cod %>% 
         janitor::get_dupes(run, fecha_ingreso_rec_fmt, fecha_egreso_rec_fmt, ESTAB_HOMO) %>% 
         dplyr::distinct(run) %>% nrow(),
       "; eventos: ",
       data_long_est_24_std_imp_rec_imp_areaf_rec_cod %>% 
         janitor::get_dupes(run, fecha_ingreso_rec_fmt, fecha_egreso_rec_fmt, ESTAB_HOMO) %>% 
         nrow()
)

invisible("POR LO VISTO, NINGUNO DE ESOS ES DUPLICADO")


### 1.7.d  Red --------------------------------------------------

invisible("Pares de registros de tratamiento (episodios hospitalarios) secuenciadas o transiciones que tienen un establecimiento posterior y que no provienen de la RM")
data_long_est_24_std_imp_rec_imp_areaf_rec_filt<-
  data_long_est_24_std_imp_rec_imp_areaf_rec_cod %>% 
  dplyr::group_by(run) %>% 
  dplyr::mutate(lag_ESTAB_HOMO= lag(ESTAB_HOMO), lag_codigo_region= lag(codigo_region), lag_codigo_comuna= lag(codigo_comuna), lag_longitud_grados_decimales= lag(longitud_grados_decimales), lag_latitud_grados_decimales= lag(latitud_grados_decimales)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(lag_codigo_region!=13)

nrow(data_long_est_24_std_imp_rec_imp_areaf_rec_filt)
#3329

invisible("Personas?")
length(unique(data_long_est_24_std_imp_rec_imp_areaf_rec_filt$run))
# 1632
invisible("Del total de personas?")
length(unique(data_long_est_24_std_imp_rec_imp_areaf_rec$run))
#6040
#
data_long_est_24_std_imp_rec_imp_areaf_rec_filt %>% 
  dplyr::group_by(run) %>% 
  count() %>% pull() %>% {
    hist(.,main="N de últimos tratamientos por paciente", breaks=15)
    print(paste0("p95= ",round(quantile(.,p=.95),2),"; p99= ",round(quantile(.,p=.99),2)))
    summary(.)
  }

invisible("Pares de registros de tratamiento (episodios hospitalarios) secuenciadas o transiciones que tienen un establecimiento posterior y que no provienen de la RM. Además, que no permanecen en su hospital")
data_long_est_24_std_imp_rec_imp_areaf_rec_filt2<-
  data_long_est_24_std_imp_rec_imp_areaf_rec_cod %>% 
  dplyr::group_by(run) %>% 
  dplyr::mutate(lag_ESTAB_HOMO= lag(ESTAB_HOMO), lag_codigo_region= lag(codigo_region), lag_codigo_comuna= lag(codigo_comuna), lag_longitud_grados_decimales= lag(longitud_grados_decimales), lag_latitud_grados_decimales= lag(latitud_grados_decimales)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(lag_codigo_region!=13) %>% 
    dplyr::filter(dplyr::case_when(ESTAB_HOMO==lag_ESTAB_HOMO~F,T~T))

nrow(data_long_est_24_std_imp_rec_imp_areaf_rec_filt2)
#1634
invisible("Personas?")
length(unique(data_long_est_24_std_imp_rec_imp_areaf_rec_filt2$run))
#924
data_long_est_24_std_imp_rec_imp_areaf_rec_filt2 %>% 
  dplyr::group_by(run) %>% 
  count() %>% pull() %>% {
    hist(.,main="N de últimos tratamientos por paciente", breaks=10)
    print(paste0("p95= ",round(quantile(.,p=.95),2),"; p99= ",round(quantile(.,p=.99),2)))
    summary(.)
  }

data_long_est_24_std_imp_rec_imp_areaf_rec_filt[,c("ESTAB_HOMO", "lag_ESTAB_HOMO", "codigo_comuna", "lag_codigo_comuna", "longitud_grados_decimales", "lag_longitud_grados_decimales", "latitud_grados_decimales", "lag_latitud_grados_decimales")] %>% head(20)

data_long_est_24_std_imp_rec_imp_areaf_rec_filt2[,c("ESTAB_HOMO", "lag_ESTAB_HOMO", "codigo_comuna", "lag_codigo_comuna", "longitud_grados_decimales", "lag_longitud_grados_decimales", "latitud_grados_decimales", "lag_latitud_grados_decimales")]%>% head(20)

data_long_est_24_std_imp_rec_imp_areaf_rec_filt2$longitud_grados_decimales <- as.numeric(data_long_est_24_std_imp_rec_imp_areaf_rec_filt2$longitud_grados_decimales)
data_long_est_24_std_imp_rec_imp_areaf_rec_filt2$lag_longitud_grados_decimales <- as.numeric(data_long_est_24_std_imp_rec_imp_areaf_rec_filt2$lag_longitud_grados_decimales)
data_long_est_24_std_imp_rec_imp_areaf_rec_filt2$latitud_grados_decimales <- as.numeric(data_long_est_24_std_imp_rec_imp_areaf_rec_filt2$latitud_grados_decimales)
data_long_est_24_std_imp_rec_imp_areaf_rec_filt2$lag_latitud_grados_decimales <- as.numeric(data_long_est_24_std_imp_rec_imp_areaf_rec_filt2$lag_latitud_grados_decimales)
#matriz de distancias geográficas
dist_matrix <- geosphere::distm(cbind(data_long_est_24_std_imp_rec_imp_areaf_rec_filt2$longitud_grados_decimales, 
                           data_long_est_24_std_imp_rec_imp_areaf_rec_filt2$latitud_grados_decimales), 
                     cbind(data_long_est_24_std_imp_rec_imp_areaf_rec_filt2$lag_longitud_grados_decimales, 
                           data_long_est_24_std_imp_rec_imp_areaf_rec_filt2$lag_latitud_grados_decimales), 
                     fun = distHaversine)

#ejemplo que ya no sirve
#data_long_est_24_std_imp_rec_imp_areaf_rec %>% dplyr::filter(run=="ffeede987e4dd22d5f675338df475269763e45eccdd51e7b416a807e121451cc") %>% View()

invisible("parece que antes de 2024-07-20 habia problemas por perdidos ")
invisible("pero ahora están todos los datos completos por lo visto; ACTUALIZACION: persisten ")

data_long_est_24_std_imp_rec_imp_areaf_rec_filt2$lag_longitud_grados_decimales[which(is.na(data_long_est_24_std_imp_rec_imp_areaf_rec_filt2$lag_longitud_grados_decimales))]<--71.653025
data_long_est_24_std_imp_rec_imp_areaf_rec_filt2$lag_latitud_grados_decimales[which(is.na(data_long_est_24_std_imp_rec_imp_areaf_rec_filt2$lag_latitud_grados_decimales))]<--35.423312
data_long_est_24_std_imp_rec_imp_areaf_rec_filt2$longitud_grados_decimales[which(is.na(data_long_est_24_std_imp_rec_imp_areaf_rec_filt2$longitud_grados_decimales))]<--71.653025
data_long_est_24_std_imp_rec_imp_areaf_rec_filt2$latitud_grados_decimales[which(is.na(data_long_est_24_std_imp_rec_imp_areaf_rec_filt2$latitud_grados_decimales))]<--35.423312

dist_matrix <- geosphere::distm(cbind(data_long_est_24_std_imp_rec_imp_areaf_rec_filt2$longitud_grados_decimales, 
                           data_long_est_24_std_imp_rec_imp_areaf_rec_filt2$latitud_grados_decimales), 
                     cbind(data_long_est_24_std_imp_rec_imp_areaf_rec_filt2$lag_longitud_grados_decimales, 
                           data_long_est_24_std_imp_rec_imp_areaf_rec_filt2$lag_latitud_grados_decimales), 
                     fun = distHaversine)
#distancia en metros entre dos puntos especificados por sus coordenadas de latitud y longitud, utilizando la fórmula del Haversine.

edges <- data.frame(from = data_long_est_24_std_imp_rec_imp_areaf_rec_filt2$lag_ESTAB_HOMO, 
                    to = data_long_est_24_std_imp_rec_imp_areaf_rec_filt2$ESTAB_HOMO)
g <- graph_from_data_frame(edges, directed = TRUE)
# Agrupar por 'from' y 'to' y sumar los pesos
edges$weight <- 1
weighted_edges <- aggregate(weight ~ from + to, data = edges, FUN = sum)
weighted_edges$width <- weighted_edges$weight / max(weighted_edges$weight) * 10  # Normalizar el grosor para que sea más visible
degree_centrality <- degree(g, mode = "in")

data.table::as.data.table(degree_centrality, 
                          keep.rowname=T) %>% 
  dplyr::arrange(desc(degree_centrality)) %>% 
  head(10) %>% 
  tidylog::left_join(dplyr::mutate(X717508813_Establecimientos_ChileDEIS_MINSAL_29_04_2022[,c("codigo_vigente","nombre_oficial")], 
    codigo_vigente=as.character(codigo_vigente)), by=c("rn"="codigo_vigente"))
#         rn degree_centrality                                                       nombre_oficial
#     <char>             <num>                                                               <char>
#  1: 124105                43                                             Hospital de Puerto Montt
#  2: 118100                42 Hospital Clínico Regional Dr. Guillermo Grant Benavente (Concepción)
#  3: 106104                41                                  Hospital Del Salvador de Valparaíso
#  4: 121109                40                       Hospital Dr. Hernán Henríquez Aravena (Temuco)
#  5: 115100                39                                        Hospital Regional de Rancagua
#  6: 108105                36                  Hospital Psiquiátrico Dr. Philippe Pinel (Putaendo)
#  7: 122100                34                                 Hospital Clínico Regional (Valdivia)
#  8: 119100                30                                   Hospital Las Higueras (Talcahuano)
#  9: 107109                27           Hospital Juana Ross de Edwards (Peñablanca, Villa Alemana)
# 10: 116105                25                         Hospital Dr. César Garavagno Burotto (Talca)

#Los que tienen centralidad más alta son 


g_aggr <- graph_from_data_frame(weighted_edges, directed = TRUE)

#Grado de Centralidad (Degree Centrality)
#El grado de centralidad mide el número de conexiones directas que tiene un nodo. En términos simples, cuenta cuántos enlaces (aristas) entran o salen de un nodo. Esta métrica puede calcularse de dos formas:
  
#Modo entrante (mode = "in"): Cuenta solo los enlaces que llegan al nodo.
#Modo saliente (mode = "out"): Cuenta solo los enlaces que salen del nodo.
#Modo total (mode = "all"): Cuenta todos los enlaces, tanto entrantes como salientes.

num_aristas <- ecount(g)
# Extraer las distancias para cada arista
distances <- numeric(num_aristas)
to_vec <- as.numeric()

#tomamos donde aparezcan en la base original los IDs que aparecen en edges, tanto para ejes X como Y
for (i in 1:num_aristas) {
  from <- which(data_long_est_24_std_imp_rec_imp_areaf_rec_filt2$lag_ESTAB_HOMO == edges$from[i])[1]
  to <- which(data_long_est_24_std_imp_rec_imp_areaf_rec_filt2$ESTAB_HOMO == edges$to[i])[1]
  to_vec[i]<-edges$to[i]
  distances[i] <- dist_matrix[from, to]
}

# Añadir distancias como atributos de las aristas
E(g)$distance <- distances
log_distances <- log(ifelse(E(g)$distance==0,1,E(g)$distance))
color_palette <- colorRampPalette(c("blue", "red"))(8)
edge_colors <- color_palette[cut(log_distances, breaks=8, labels=FALSE)]


l <- layout_with_fr(g, niter=500000) # debiese agregarle otro 0 o dejarlo en 200000

plot(g, 
     #  vertex.color=c("salmon", "lightblue"),#"orange", #fondo de los círculos #Node color
     #  vertex.shape="circle", #One of “none”, “circle”, “square”, “csquare”, “rectangle”“crectangle”, “vrectangle”, “pie”, “raster”, or “sphere”
     edge.color=edge_colors, # direcciones NO FUNCIONA
     vertex.label.color="black", 
     vertex.frame.color=NA,#"#ffffff",
     #vertex.label.family	="Times",#Font family of the label
     vertex.size=log(degree_centrality),  #Size of the node (default is 15)
     #vertex.label=	"ju",# Character vector used to label the nodes
     #ETIQUETA
     vertex.label.font=1, #Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
     vertex.label.cex=.3, # Font size (multiplication factor, device-dependent)
     vertex.label.dist=0,#Distance between the label and the vertex
     vertex.label.color="black", 
     edge.label.font=5,	 #Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
     vertex.label.degree= 0,#The position of the label in relation to the vertex, where 0 right, “pi” is left, “pi/2” is below, and “-pi/2” is above
     #vertex.label.dist=1,
     arrow.mode= 2, #Vector specifying whether edges should have arrows,possible values: 0 no arrow, 1 back, 2 forward, 3 both
     edge.arrow.size=.2,
     margin=-0.07,
     main="Redes de transiciones que tienen un establecimiento posterior y que\nno provienen de la RM. Además, que no permanecen en su establecimiento\n(Tamaño del nodo: log-grado de centralidad)",
     layout=l,
     asp =0 #aspect ratio defaults to 1, but often setting it to 0 is more useful so that we can exploit the maximum available plot area.
)
legend("bottomleft", 
       legend = c("Baja", "Mediana ", "Alta"), 
       col = color_palette[c(1, 4, 8)], 
       lwd = 2, 
       title = "Log-distancia\nentre aristas",
       cex = 0.8,
       bty = "n")


jpeg("_figs/redes.jpg", height=21, width=24, res=500, units="in")
# Plot code goes here, for example:


plot(g, 
     #  vertex.color=c("salmon", "lightblue"),#"orange", #fondo de los círculos #Node color
     #  vertex.shape="circle", #One of “none”, “circle”, “square”, “csquare”, “rectangle”“crectangle”, “vrectangle”, “pie”, “raster”, or “sphere”
     edge.color=edge_colors, # direcciones NO FUNCIONA
     vertex.label.color="black", 
     vertex.frame.color=NA,#"#ffffff",
     #vertex.label.family	="Times",#Font family of the label
     vertex.size=log(degree_centrality),  #Size of the node (default is 15)
     #vertex.label=	"ju",# Character vector used to label the nodes
     #ETIQUETA
     vertex.label.font=1, #Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
     vertex.label.cex=.3, # Font size (multiplication factor, device-dependent)
     vertex.label.dist=0,#Distance between the label and the vertex
     vertex.label.color="black", 
     edge.label.font=5,	 #Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
     vertex.label.degree= 0,#The position of the label in relation to the vertex, where 0 right, “pi” is left, “pi/2” is below, and “-pi/2” is above
     #vertex.label.dist=1,
     arrow.mode= 2, #Vector specifying whether edges should have arrows,possible values: 0 no arrow, 1 back, 2 forward, 3 both
     edge.arrow.size=.2,
     margin=-0.07,
     main="Redes de transiciones que tienen un establecimiento posterior y que\nno provienen de la RM. Además, que no permanecen en su establecimiento\n(Tamaño del nodo: log-grado de centralidad)",
     layout=l,
     asp =0 #aspect ratio defaults to 1, but often setting it to 0 is more useful so that we can exploit the maximum available plot area.
)
legend("bottomleft", 
       legend = c("Baja", "Mediana ", "Alta"), 
       col = color_palette[c(1, 4, 8)], 
       lwd = 2, 
       title = "Log-distancia\nentre aristas",
       cex = 0.8,
       bty = "n")
dev.off()


invisible("Saqué el promedio de distancias por cada ID recepetor, junto con una medida de centralidad. Luego saco la correlación")
db_centrality_distances<-
cbind.data.frame(to_vec, distances) %>% 
  dplyr::group_by(to_vec) %>% 
  dplyr::summarise(mean_distances= mean(distances, na.rm=T), mdn_distances= median(distances, na.rm=T)) %>% 
  dplyr::mutate(to_vec=as.character(to_vec)) %>% 
  dplyr::left_join(cbind.data.frame(to= attr(degree_centrality,"names"), centrality= degree_centrality), by=c("to_vec"="to")) 

cor(db_centrality_distances$centrality, db_centrality_distances$mean_distances)
#-0.1425991
cor(db_centrality_distances$centrality, db_centrality_distances$mdn_distances)
#-0.1745588
#
#
invisible("Descartada la matriz de distancias general.R = demasiado grande")

### 1.7.e  Red, sólo 90 días entre --------------------------------------------------


# dt_EH_2010_2022_Pasantes_v2_encrip_filt_est2 <- dt_EH_2010_2022_Pasantes_v2_encrip_filt_est2[!is.na(lag_FECHA_EGRESO_FMT_DEIS) & 
#                                                                                                (FECHA_INGRESO_FMT_DEIS - lag_FECHA_EGRESO_FMT_DEIS) < 90]
                
invisible("Personas que tienen un establecimiento posterior y transiciones menores o iguales a 90 días entre egreso anterior e ingreso")
data_long_est_24_std_imp_rec_imp_areaf_rec_filt3<-
  data_long_est_24_std_imp_rec_imp_areaf_rec_cod %>% 
  dplyr::group_by(run) %>% 
  dplyr::mutate(lag_ESTAB_HOMO= lag(ESTAB_HOMO), lag_codigo_region= lag(codigo_region), lag_codigo_comuna= lag(codigo_comuna), lag_longitud_grados_decimales= lag(longitud_grados_decimales), lag_latitud_grados_decimales= lag(latitud_grados_decimales), lag_fecha_egreso_rec24_num=lag(fecha_egreso_rec24_num)) %>% 
  dplyr::ungroup() %>% 
#  dplyr::filter(lag_codigo_region!=13) %>% 
  dplyr::filter(!is.na(lag_ESTAB_HOMO)) %>% 
#  dplyr::filter(dplyr::case_when(ESTAB_HOMO==lag_ESTAB_HOMO~F,T~T)) %>% 
  dplyr::filter(dplyr::case_when((fecha_ingreso_rec24_num-lag_fecha_egreso_rec24_num)>90~F,T~T))

nrow(data_long_est_24_std_imp_rec_imp_areaf_rec_filt3)
#2663
invisible("Personas?")
length(unique(data_long_est_24_std_imp_rec_imp_areaf_rec_filt3$run))
#[1] 1434

data_long_est_24_std_imp_rec_imp_areaf_rec_filt3 %>% 
  dplyr::group_by(run) %>% 
  count() %>% pull() %>% {
    hist(.,main="N de últimos tratamientos por paciente", breaks=40)
      print(paste0("p95= ",round(quantile(.,p=.95),2),"; p99= ",round(quantile(.,p=.99),2)))
      summary(.)
      }
# [1] "p95= 5; p99= 9"
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   1.000   1.857   2.000  33.000 
# 
data_long_est_24_std_imp_rec_imp_areaf_rec_filt3$longitud_grados_decimales <- as.numeric(data_long_est_24_std_imp_rec_imp_areaf_rec_filt3$longitud_grados_decimales)
data_long_est_24_std_imp_rec_imp_areaf_rec_filt3$lag_longitud_grados_decimales <- as.numeric(data_long_est_24_std_imp_rec_imp_areaf_rec_filt3$lag_longitud_grados_decimales)
data_long_est_24_std_imp_rec_imp_areaf_rec_filt3$latitud_grados_decimales <- as.numeric(data_long_est_24_std_imp_rec_imp_areaf_rec_filt3$latitud_grados_decimales)
data_long_est_24_std_imp_rec_imp_areaf_rec_filt3$lag_latitud_grados_decimales <- as.numeric(data_long_est_24_std_imp_rec_imp_areaf_rec_filt3$lag_latitud_grados_decimales)
#matriz de distancias geográficas
dist_matrix2 <- geosphere::distm(cbind(data_long_est_24_std_imp_rec_imp_areaf_rec_filt3$longitud_grados_decimales, 
                                      data_long_est_24_std_imp_rec_imp_areaf_rec_filt3$latitud_grados_decimales), 
                                cbind(data_long_est_24_std_imp_rec_imp_areaf_rec_filt3$lag_longitud_grados_decimales, 
                                      data_long_est_24_std_imp_rec_imp_areaf_rec_filt3$lag_latitud_grados_decimales), 
                                fun = distHaversine)
#distancia en metros entre dos puntos especificados por sus coordenadas de latitud y longitud, utilizando la fórmula del Haversine.

data_long_est_24_std_imp_rec_imp_areaf_rec_filt3$lag_longitud_grados_decimales[which(is.na(data_long_est_24_std_imp_rec_imp_areaf_rec_filt3$lag_longitud_grados_decimales))]<--71.653025
data_long_est_24_std_imp_rec_imp_areaf_rec_filt3$lag_latitud_grados_decimales[which(is.na(data_long_est_24_std_imp_rec_imp_areaf_rec_filt3$lag_latitud_grados_decimales))]<--35.423312
data_long_est_24_std_imp_rec_imp_areaf_rec_filt3$longitud_grados_decimales[which(is.na(data_long_est_24_std_imp_rec_imp_areaf_rec_filt3$longitud_grados_decimales))]<--71.653025
data_long_est_24_std_imp_rec_imp_areaf_rec_filt3$latitud_grados_decimales[which(is.na(data_long_est_24_std_imp_rec_imp_areaf_rec_filt3$latitud_grados_decimales))]<--35.423312
invisible("Ahora sin los problemas por NAs, que persisten")
dist_matrix2 <- geosphere::distm(cbind(data_long_est_24_std_imp_rec_imp_areaf_rec_filt3$longitud_grados_decimales, 
                                       data_long_est_24_std_imp_rec_imp_areaf_rec_filt3$latitud_grados_decimales), 
                                 cbind(data_long_est_24_std_imp_rec_imp_areaf_rec_filt3$lag_longitud_grados_decimales, 
                                       data_long_est_24_std_imp_rec_imp_areaf_rec_filt3$lag_latitud_grados_decimales), 
                                 fun = distHaversine)

edges2 <- data.frame(from = data_long_est_24_std_imp_rec_imp_areaf_rec_filt3$lag_ESTAB_HOMO, 
                    to = data_long_est_24_std_imp_rec_imp_areaf_rec_filt3$ESTAB_HOMO)
g2 <- graph_from_data_frame(edges2, directed = TRUE)

g2_filt <- graph_from_data_frame(dplyr::filter(edges2, from!=to), directed = TRUE)
# Agrupar por 'from' y 'to' y sumar los pesos
edges2$weight <- 1
weighted_edges2 <- aggregate(weight ~ from + to, data = edges2, FUN = sum)

weighted_edges2$width <- weighted_edges2$weight / max(weighted_edges2$weight) * 10  # Normalizar el grosor para que sea más visible
degree_centrality2 <- degree(g2, mode = "in")
degree_centrality2_filt <- degree(g2_filt, mode = "in")

data.table::as.data.table(degree_centrality2, 
                          keep.rowname=T) %>% 
  dplyr::arrange(desc(degree_centrality2)) %>% 
  head(10) %>% 
  tidylog::left_join(dplyr::mutate(X717508813_Establecimientos_ChileDEIS_MINSAL_29_04_2022[,c("codigo_vigente","nombre_oficial")], 
                                   codigo_vigente=as.character(codigo_vigente)), by=c("rn"="codigo_vigente"))
#         rn degree_centrality2                                                     nombre_oficial
#     <char>              <num>                                                             <char>
#  1: 112200                 97                                                    Clínica Alemana
#  2: 111100                 96                                Hospital Clínico San Borja Arriarán
#  3: 102100                 88                     Hospital Dr. Ernesto Torres Galdames (Iquique)
#  4: 108105                 85                Hospital Psiquiátrico Dr. Philippe Pinel (Putaendo)
#  5: 109102                 76 Instituto Psiquiátrico Dr. José Horwitz Barak (Santiago, Recoleta)
#  6: 121109                 76                     Hospital Dr. Hernán Henríquez Aravena (Temuco)
#  7: 200050                 71                          Hospital Clínico Universidad de Los Andes
#  8: 124105                 65                                           Hospital de Puerto Montt
#  9: 200091                 63                                                    Mirandes S.P.A.
# 10: 109200                 60                              Hospital Clínico Universidad de Chile


data.table::as.data.table(degree_centrality2_filt, 
                          keep.rowname=T) %>% 
  dplyr::arrange(desc(degree_centrality2_filt)) %>% 
  head(10) %>% 
  tidylog::left_join(dplyr::mutate(X717508813_Establecimientos_ChileDEIS_MINSAL_29_04_2022[,c("codigo_vigente","nombre_oficial")], 
                                   codigo_vigente=as.character(codigo_vigente)), by=c("rn"="codigo_vigente"))

#         rn degree_centrality2_filt                                                     nombre_oficial
#     <char>                   <num>                                                             <char>
#  1: 200091                      47                                                    Mirandes S.P.A.
#  2: 112252                      32                                       Clínica Psiquiatrica Renacer
#  3: 109200                      32                              Hospital Clínico Universidad de Chile
#  4: 106104                      32                                Hospital Del Salvador de Valparaíso
#  5: 108105                      30                Hospital Psiquiátrico Dr. Philippe Pinel (Putaendo)
#  6: 112207                      30                                                 Clínica del Carmen
#  7: 109102                      28 Instituto Psiquiátrico Dr. José Horwitz Barak (Santiago, Recoleta)
#  8: 121109                      28                     Hospital Dr. Hernán Henríquez Aravena (Temuco)
#  9: 115100                      28                                      Hospital Regional de Rancagua
# 10: 112229                      27                                                Pensionado San José


g_aggr2 <- graph_from_data_frame(weighted_edges2, directed = TRUE)

#Grado de Centralidad (Degree Centrality)
#El grado de centralidad mide el número de conexiones directas que tiene un nodo. En términos simples, cuenta cuántos enlaces (aristas) entran o salen de un nodo. Esta métrica puede calcularse de dos formas:

#Modo entrante (mode = "in"): Cuenta solo los enlaces que llegan al nodo.
#Modo saliente (mode = "out"): Cuenta solo los enlaces que salen del nodo.
#Modo total (mode = "all"): Cuenta todos los enlaces, tanto entrantes como salientes.

num_aristas2 <- ecount(g2)
# Extraer las distancias para cada arista
distances2 <- numeric(num_aristas2)
to_vec2 <- as.numeric()

#tomamos donde aparezcan en la base original los IDs que aparecen en edges, tanto para ejes X como Y
for (i in 1:num_aristas2) {
  from2 <- which(data_long_est_24_std_imp_rec_imp_areaf_rec_filt3$lag_ESTAB_HOMO == edges2$from[i])[1]
  to2 <- which(data_long_est_24_std_imp_rec_imp_areaf_rec_filt3$ESTAB_HOMO == edges2$to[i])[1]
  to_vec2[i]<-edges2$to[i]
  distances2[i] <- dist_matrix2[from2, to2]
}

# Añadir distancias como atributos de las aristas
E(g2)$distance <- distances2
log_distances2 <- log(ifelse(E(g2)$distance==0,1,E(g2)$distance))
color_palette2 <- colorRampPalette(c("blue", "red"))(4)
edge_colors2 <- color_palette[cut(log_distances2, breaks=4, labels=FALSE)]


l2 <- layout_with_fr(g2_filt, niter=500000) # debiese agregarle otro 0 o dejarlo en 200000

plot(g2_filt,
     #  vertex.color=c("salmon", "lightblue"),#"orange", #fondo de los círculos #Node color
     #  vertex.shape="circle", #One of “none”, “circle”, “square”, “csquare”, “rectangle”“crectangle”, “vrectangle”, “pie”, “raster”, or “sphere”
     edge.color="gray50", # direcciones NO FUNCIONA
     vertex.label.color="black",
     vertex.frame.color=NA,#"#ffffff",
     #vertex.label.family	="Times",#Font family of the label
     vertex.size=log(degree_centrality2_filt),  #Size of the node (default is 15)
     #vertex.label=	"ju",# Character vector used to label the nodes
     #ETIQUETA
     vertex.label.font=1, #Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
     vertex.label.cex=.3, # Font size (multiplication factor, device-dependent)
     vertex.label.dist=0,#Distance between the label and the vertex
     vertex.label.color="black",
     edge.label.font=5,	 #Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
     vertex.label.degree= 0,#The position of the label in relation to the vertex, where 0 right, “pi” is left, “pi/2” is below, and “-pi/2” is above
     #vertex.label.dist=1,
     arrow.mode= 2, #Vector specifying whether edges should have arrows,possible values: 0 no arrow, 1 back, 2 forward, 3 both
     edge.arrow.size=.2,
     margin=-0.07,
     main="Redes de transiciones que tienen un establecimiento posterior\nen los 90 días siguientes o previo a esto\n(Tamaño del nodo: log-grado de centralidad)\n(se descartan loops)",
     layout=l2,
     asp =0 #aspect ratio defaults to 1, but often setting it to 0 is more useful so that we can exploit the maximum available plot area.
)

jpeg("_figs/redes2.jpg", height=21, width=24, res=500, units="in")
# Plot code goes here, for example:

plot(g2_filt,
     #  vertex.color=c("salmon", "lightblue"),#"orange", #fondo de los círculos #Node color
     #  vertex.shape="circle", #One of “none”, “circle”, “square”, “csquare”, “rectangle”“crectangle”, “vrectangle”, “pie”, “raster”, or “sphere”
     edge.color="gray50", # direcciones NO FUNCIONA
     vertex.label.color="black",
     vertex.frame.color=NA,#"#ffffff",
     #vertex.label.family	="Times",#Font family of the label
     vertex.size=log(degree_centrality2_filt),  #Size of the node (default is 15)
     #vertex.label=	"ju",# Character vector used to label the nodes
     #ETIQUETA
     vertex.label.font=1, #Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
     vertex.label.cex=.3, # Font size (multiplication factor, device-dependent)
     vertex.label.dist=0,#Distance between the label and the vertex
     vertex.label.color="black",
     edge.label.font=5,	 #Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
     vertex.label.degree= 0,#The position of the label in relation to the vertex, where 0 right, “pi” is left, “pi/2” is below, and “-pi/2” is above
     #vertex.label.dist=1,
     arrow.mode= 2, #Vector specifying whether edges should have arrows,possible values: 0 no arrow, 1 back, 2 forward, 3 both
     edge.arrow.size=.2,
     margin=-0.07,
     main="Redes de transiciones que tienen un establecimiento posterior\nen los 90 días siguientes o previo a esto\n(Tamaño del nodo: log-grado de centralidad)\n(se descartan loops)",
     layout=l2,
     asp =0 #aspect ratio defaults to 1, but often setting it to 0 is more useful so that we can exploit the maximum available plot area.
)
dev.off()

invisible("Saqué el promedio de distancias por cada ID recepetor, junto con una medida de centralidad. Luego saco la correlación")
db_centrality_distances2<-
  cbind.data.frame(to_vec2, distances2) %>% 
  dplyr::group_by(to_vec2) %>% 
  dplyr::summarise(mean_distances= mean(distances2, na.rm=T), mdn_distances= median(distances2, na.rm=T)) %>% 
  dplyr::mutate(to_vec2=as.character(to_vec2)) %>% 
  dplyr::left_join(cbind.data.frame(to= attr(degree_centrality2,"names"), centrality= degree_centrality2), by=c("to_vec2"="to"))

if(db_centrality_distances2 %>% 
  dplyr::filter(centrality==1 & mean_distances!=mdn_distances) %>% nrow()>0){
    stop("Error: hay casos con centralidad igual a 1 pero distinta distancia!?")}

#invisible("ACTUALIZACION: YA NO ES ASÏ. Como hay perdidos en los establecimientos privados, tuve que ponerlo así no más")

cor(db_centrality_distances2$centrality, db_centrality_distances2$mean_distances)
#[1] 0.04940837
cor(db_centrality_distances2$centrality, db_centrality_distances2$mdn_distances)
#[1] 0.03279029

invisible("Al parecer, mientras más distancia más centralidad??, incluso cuando hay loop")

#### 1.7.e.1. exportar para árboles --------------------------------------------------

rename_with(db_centrality_distances2,~ paste0("90d_less_", .))

arearef_dummy<-
  data_long_est_24_std_imp_rec_imp_areaf_rec_cod[, c("ESTAB_HOMO", "areaf_egr_glosa", "areaf_egr_glosa_rec", "code_niv_at_mod_area_compl","tiene_servicio_de_urgencia", "tipo_establecimiento", "nivel_de_atencion", 
                                               "tipo_de_prestador_sistema_de_salud", "nivel_de_complejidad", "modalidad_de_atencion")] %>%
  dplyr::mutate(estab_homo_str=as.character(ESTAB_HOMO)) %>%                                                 
  tidylog::left_join(rename_with(db_centrality_distances,~ paste0("not_rm_wo_loops_", .)),by=c("estab_homo_str"="not_rm_wo_loops_to_vec"), multiple="first") %>%  
  dplyr::mutate(not_rm_wo_loops_centrality= ifelse(is.na(not_rm_wo_loops_centrality),0,not_rm_wo_loops_centrality)) %>% 
  tidylog::left_join(rename_with(db_centrality_distances2,~ paste0("90d_less_", .)),by=c("estab_homo_str"="90d_less_to_vec2"), multiple="first") %>%  
  dplyr::mutate(`90d_less_centrality`= ifelse(is.na(`90d_less_centrality`),0,`90d_less_centrality`)) %>% 
model.matrix(~ areaf_egr_glosa - 1, .) %>% 
  janitor::clean_names()


invisible("Se une el area dummificada. Conste que esta es una base en base a los casos (eventos hospitalarios)")
invisible("Es clasificar el tipo de cuidado entregado (which) por el setting (where)")

cbind.data.frame(arearef_dummy,
                 data_long_est_24_std_imp_rec_imp_areaf_rec_cod[, c("ESTAB_HOMO", "areaf_egr_glosa", "areaf_egr_glosa_rec", "code_niv_at_mod_area_compl","tiene_servicio_de_urgencia", "tipo_establecimiento", "nivel_de_atencion", 
                                                                "tipo_de_prestador_sistema_de_salud", "nivel_de_complejidad", "modalidad_de_atencion")] %>%
                   dplyr::mutate(estab_homo_str=as.character(ESTAB_HOMO)) %>%                                                 
                   tidylog::left_join(rename_with(db_centrality_distances,~ paste0("not_rm_wo_loops_", .)),by=c("estab_homo_str"="not_rm_wo_loops_to_vec"), multiple="first") %>%  
                   dplyr::mutate(not_rm_wo_loops_centrality= ifelse(is.na(not_rm_wo_loops_centrality),0,not_rm_wo_loops_centrality)) %>% 
                   tidylog::left_join(rename_with(db_centrality_distances2,~ paste0("90d_less_", .)),by=c("estab_homo_str"="90d_less_to_vec2"), multiple="first") %>%  
                   dplyr::mutate(`90d_less_centrality`= ifelse(is.na(`90d_less_centrality`),0,`90d_less_centrality`))
) %>% 
  dplyr::select(-ESTAB_HOMO, -estab_homo_str) %>% 
  rio::export(file="H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/_output/unido_std_2024_2.xlsx")


invisible("Ejercicios con árboles de clasificación: H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/trees_que_no_funciono.R")

invisible("Ejercicios con árboles de clasificación con python: H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/un_inv_II/006_trees_para_where.py")

save.image(paste0("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/20240722.RData"))

## 1.8.  Cluster --------------------------------------------------

#load(paste0("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/20240722.RData"))

unido_std_2024_2<-
cbind.data.frame(arearef_dummy,
                 data_long_est_24_std_imp_rec_imp_areaf_rec_cod[, c("ESTAB_HOMO", "areaf_egr_glosa", "areaf_egr_glosa_rec", #"code_niv_at_mod_area_compl",
                                                                    "tiene_servicio_de_urgencia", "tipo_establecimiento", "nivel_de_atencion",
                                                                    "nivel_de_atencion_rec", "nivel_de_complejidad_rec", 
                                                                    "tipo_de_prestador_sistema_de_salud", "nivel_de_complejidad")] %>%
                   dplyr::mutate(estab_homo_str=as.character(ESTAB_HOMO)) %>%                                                 
                   tidylog::left_join(rename_with(db_centrality_distances,~ paste0("not_rm_wo_loops_", .)),by=c("estab_homo_str"="not_rm_wo_loops_to_vec"), multiple="first") %>%  
                   dplyr::mutate(not_rm_wo_loops_centrality= ifelse(is.na(not_rm_wo_loops_centrality),0,not_rm_wo_loops_centrality)) %>% 
                   tidylog::left_join(rename_with(db_centrality_distances2,~ paste0("90d_less_", .)),by=c("estab_homo_str"="90d_less_to_vec2"), multiple="first") %>%  
                   dplyr::mutate(`90d_less_centrality`= ifelse(is.na(`90d_less_centrality`),0,`90d_less_centrality`))
) %>% 
  dplyr::select(-ESTAB_HOMO, -areaf_egr_glosa, -estab_homo_str)#, -areaf_egr_glosa_rec)
#11,848
#
### 1.8.a.  Clasificar --------------------------------------------------

#Hay instituciones que no tienen medidas ya que no aparece en los filtros

# Contar los NAs por cada columna
na_count <- sapply(unido_std_2024_2, function(x) sum(is.na(x)))[sapply(unido_std_2024_2, function(x) sum(is.na(x)))>0]

#Imputamos las siguientes:
na_count
#
#paste(colnames(unido_std_2024_2),collapse= "+ ")
set.seed(2125)
unido_std_2024_2_imp<-
  missRanger::missRanger(
    formula =  not_rm_wo_loops_mean_distances + 
      not_rm_wo_loops_mdn_distances + 
        `90d_less_mean_distances` + 
          `90d_less_mdn_distances` ~ areaf_egr_glosa_area_cuidados_intensivos_adultos+ 
      areaf_egr_glosa_area_cuidados_intensivos_pediatricos+ areaf_egr_glosa_area_cuidados_intermedios_adultos+ 
      areaf_egr_glosa_area_cuidados_intermedios_pediatricos+ areaf_egr_glosa_area_de_hospitalizacion_de_cuidados_intensivos_en_psiquiatria_adulto+ 
      areaf_egr_glosa_area_de_hospitalizacion_de_cuidados_intensivos_en_psiquiatria_infanto_adolescente+ areaf_egr_glosa_area_medica_adulto_cuidados_basicos+ 
      areaf_egr_glosa_area_medica_adulto_cuidados_medios+ areaf_egr_glosa_area_medica_pediatrica_cuidados_basicos+ 
      areaf_egr_glosa_area_medica_pediatrica_cuidados_medios+ areaf_egr_glosa_area_medico_quirurgico_cuidados_basicos+ 
      areaf_egr_glosa_area_medico_quirurgico_cuidados_medios+ areaf_egr_glosa_area_medico_quirurgico_pediatrica_cuidados_basicos+ 
      areaf_egr_glosa_area_medico_quirurgico_pediatrica_cuidados_medios+ areaf_egr_glosa_area_neonatologia_cuidados_intermedios+ 
      areaf_egr_glosa_area_obstetricia+ areaf_egr_glosa_area_pensionado+ areaf_egr_glosa_area_psiquiatria_adulto_corta_estadia+ 
      areaf_egr_glosa_area_psiquiatria_adulto_larga_estadia+ areaf_egr_glosa_area_psiquiatria_adulto_mediana_estadia+ 
      areaf_egr_glosa_area_psiquiatria_forense_adulto_evaluacion_e_inicio_tto+ areaf_egr_glosa_area_psiquiatria_forense_adulto_tratamiento+ 
      areaf_egr_glosa_area_psiquiatria_forense_infanto_adolescente_tratamiento+ areaf_egr_glosa_area_psiquiatria_infanto_adolescente_corta_estadia+ 
      areaf_egr_glosa_area_psiquiatria_infanto_adolescente_mediana_estadia+ areaf_egr_glosa_area_sociosanitaria_adulto+ tiene_servicio_de_urgencia+ 
      tipo_establecimiento+ nivel_de_atencion+ tipo_de_prestador_sistema_de_salud+ nivel_de_complejidad+ 
      not_rm_wo_loops_centrality+ `90d_less_centrality`,#+ ttos+ promedio_dias_tto,
    data= unido_std_2024_2,
    num.trees = 900, 
    pmm.k = 100,
    returnOOB=T,
    maxiter= 500,
    verbose = 2, 
    seed = 2125)


#perdidos: not_rm_wo_loops_mean_distances not_rm_wo_loops_mdn_distances
unido_std_2024_2_imp$sc_90d_less_centrality <- scale(unido_std_2024_2_imp$`90d_less_centrality`)
# > attr(unido_std_2024_2_imp$sc_90d_less_centrality, "scaled:center")
# [1] 32.87297
# > attr(unido_std_2024_2_imp$sc_90d_less_centrality, "scaled:scale")
# [1] 29.64914
unido_std_2024_2_imp$sc_90d_less_mean_distances <- scale(unido_std_2024_2_imp$`90d_less_mean_distances`)
# > attr(unido_std_2024_2_imp$sc_90d_less_mean_distances, "scaled:center")
# [1] 120435.9
# > attr(unido_std_2024_2_imp$sc_90d_less_mean_distances, "scaled:scale")
# [1] 242103.7
unido_std_2024_2_imp$sc_90d_less_mdn_distances <- scale(unido_std_2024_2_imp$`90d_less_mdn_distances`)
# > attr(unido_std_2024_2_imp$sc_90d_less_mdn_distances, "scaled:scale")
# [1] 274263.2
# > attr(unido_std_2024_2_imp$sc_90d_less_mdn_distances, "scaled:center")
# [1] 99554.8
 
unido_std_2024_2_naomit <- na.omit(unido_std_2024_2)

# > nrow(unido_std_2024_2_imp)
# [1] 11848
# > nrow(unido_std_2024_2_naomit )
# [1] 10858
  
# 2. SSA --------------------------------------------------
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#load(paste0("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/20240722.RData"))

table(data_long_est_24_std_imp_rec_imp_areaf_rec_cod$alphabet)
# coc     cp cp_psu    mar     oh   otro    psu    psy 
# 229    231     39    170    426   2602     57   8099 
# 2024-07-22
# coc     cp cp_psu    mar     oh   otro    psu    psy 
# 229    231     39    170    425   2602     57   8095 

invisible("ojo: tengo que capturar lo que no veo también!!!!")
#De todas formas, se agrega que los periodos sin ingreso se alfabetizan como AUS (ausente). 
#Esta categoría se diferencia de (F0), para denominar a aquellos/as pacientes que fallecieron fuera de un episodio hospitalario.

# data_long_est_24_std_imp_rec_imp_areaf_rec_cod  %>%
#   dplyr::arrange(run, min_fecha_egreso_rec24_num , fecha_egreso_rec24_num) %>% 
#   dplyr::select(run, min_fecha_egreso_rec24_num , fecha_ingreso_rec, fecha_egreso_rec, alphabet) %>% View()

df_filled <- 
  data_long_est_24_std_imp_rec_imp_areaf_rec_cod  %>%
  dplyr::arrange(run, min_fecha_egreso_rec24_num, fecha_egreso_rec24_num) %>% 
  dplyr::group_by(run) %>% 
  dplyr::mutate(tto= dplyr::row_number()) %>% 
  dplyr::mutate(lag_fecha_egreso_rec_num= lag(unclass(as.Date(fecha_egreso_rec)), default= unclass(as.Date("2017-12-01")))) %>%
  dplyr::mutate(lag_fecha_ingreso_rec_num= lag(unclass(as.Date(fecha_ingreso_rec)), default= unclass(as.Date("2017-12-01")))) %>% 
  dplyr::ungroup()

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
# Make the databases to populate. We restricted to the years of interest (2018-2022), and used a lag with the previous month or quarter on the dates

expand_df_month_run<-
expand.grid(run = unique(df_filled$run), month = paste(rep(paste0("20",18:22),each=12),1:12, sep="-")) %>% 
  dplyr::arrange(run) %>% 
  dplyr::mutate(date_num= unclass(as.Date(paste0(month,"-01")))) %>% 
  dplyr::group_by(run) %>% 
  #dplyr::mutate(date_num_lag= lag(unclass(as.Date(paste0(month,"-01"))))) %>% #, default= unclass(as.Date("2023-01-01")))) %>% 
  dplyr::mutate(date_num_lead= lead(date_num, default=unclass(as.Date("2023-01-01")))) %>% 
  ungroup()
#    run                                                              month   date_num date_num_lead
#    <fct>                                                            <fct>      <dbl>         <dbl>
#  1 00004d52e7c3521618e946bd42f159155972209a11b63d5d26ccf4cbfebbc1a1 2018-1     17532         19358
#  2 00004d52e7c3521618e946bd42f159155972209a11b63d5d26ccf4cbfebbc1a1 2018-2     17563         17532
#  3 00004d52e7c3521618e946bd42f159155972209a11b63d5d26ccf4cbfebbc1a1 2018-3     17591         17563
#  4 00004d52e7c3521618e946bd42f159155972209a11b63d5d26ccf4cbfebbc1a1 2018-4     17622         17591
#  5 00004d52e7c3521618e946bd42f159155972209a11b63d5d26ccf4cbfebbc1a1 2018-5     17652         17622
#  6 00004d52e7c3521618e946bd42f159155972209a11b63d5d26ccf4cbfebbc1a1 2018-6     17683         17652
#  7 00004d52e7c3521618e946bd42f159155972209a11b63d5d26ccf4cbfebbc1a1 2018-7     17713         17683
#  8 00004d52e7c3521618e946bd42f159155972209a11b63d5d26ccf4cbfebbc1a1 2018-8     17744         17713
#  9 00004d52e7c3521618e946bd42f159155972209a11b63d5d26ccf4cbfebbc1a1 2018-9     17775         17744
# 10 00004d52e7c3521618e946bd42f159155972209a11b63d5d26ccf4cbfebbc1a1 2018-10    17805         17775  

expand_df_month_run_t_desde_primera_adm<-
  #debo sacarle un dígito al recuento porque la gente parte desde un tiempo 0 (primera admisión)
  expand.grid(run = unique(df_filled$run), 
              month = 0:(length(paste(rep(paste0("20",18:22),each=12),1:12, sep="-"))-1)) %>% 
  dplyr::arrange(run) %>% 
  ungroup()

expand_df_quarter_run<-
expand.grid(run = unique(df_filled$run), quarter = paste(rep(paste0("20",18:22),each=4),1:4, sep=".")) %>%
  dplyr::arrange(run) %>% 
  dplyr::mutate(date_num= unclass(lubridate::yq(quarter))) %>% 
  dplyr::group_by(run) %>% 
  dplyr::mutate(date_num_lead= lead(date_num, default=unclass(as.Date("2023-01-01")))) %>% 
  ungroup()
#    run                                                              quarter date_num date_num_lead
#    <fct>                                                            <fct>      <dbl>         <dbl>
#  1 00004d52e7c3521618e946bd42f159155972209a11b63d5d26ccf4cbfebbc1a1 2018.1     17532         17622
#  2 00004d52e7c3521618e946bd42f159155972209a11b63d5d26ccf4cbfebbc1a1 2018.2     17622         17713
#  3 00004d52e7c3521618e946bd42f159155972209a11b63d5d26ccf4cbfebbc1a1 2018.3     17713         17805
#  4 00004d52e7c3521618e946bd42f159155972209a11b63d5d26ccf4cbfebbc1a1 2018.4     17805         17897
#  5 00004d52e7c3521618e946bd42f159155972209a11b63d5d26ccf4cbfebbc1a1 2019.1     17897         17987
#  6 00004d52e7c3521618e946bd42f159155972209a11b63d5d26ccf4cbfebbc1a1 2019.2     17987         18078
#  7 00004d52e7c3521618e946bd42f159155972209a11b63d5d26ccf4cbfebbc1a1 2019.3     18078         18170
#  8 00004d52e7c3521618e946bd42f159155972209a11b63d5d26ccf4cbfebbc1a1 2019.4     18170         18262
#  9 00004d52e7c3521618e946bd42f159155972209a11b63d5d26ccf4cbfebbc1a1 2020.1     18262         18353
# 10 00004d52e7c3521618e946bd42f159155972209a11b63d5d26ccf4cbfebbc1a1 2020.2     18353         18444

expand_df_quarter_run_t_desde_primera_adm<-
  #debo sacarle un dígito al recuento porque la gente parte desde un tiempo 0 (primera admisión)
  expand.grid(run = unique(df_filled$run), 
              quarter = 0:(length(paste(rep(paste0("20",18:22),each=4),1:4, sep="."))-1)) %>% 
  dplyr::arrange(run) %>% 
  ungroup()

invisible("de cuánto sería el alfabeto con sólo los 6041 casos que tengo???")

nrow(expand_df_month_run)
#362460
nrow(expand_df_quarter_run)
#120800
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_



## 2.1. Poblar la base nueva para alfabeto ---------------------------------

invisible("UNIR LAS BASES Y EMPEZAR A POBLAR")
invisible("Base original")
dt_df_filled<- data.table::as.data.table(dplyr::mutate(df_filled, rn= dplyr::row_number())%>% 
      dplyr::select(run, fecha_ingreso_rec24_num, fecha_egreso_rec24_num, 
                   days_elapsed, alphabet, rn, paste0("diag",1:11,"_rec32"))) %>% 
      dplyr::select(run, fecha_ingreso_rec24_num, fecha_egreso_rec24_num, alphabet,paste0("diag",1:11,"_rec32"))

invisible("2024-09-02: Eliminamos los que tuvieron un primer registro otros antes que los psiquiátricos entre el 2018-2022")
dt_df_filled<-
dt_df_filled %>% 
  dplyr::arrange(run, fecha_ingreso_rec24_num, fecha_egreso_rec24_num) %>% 
  dplyr::group_by(run) %>% 
  dplyr::mutate(run_n= n(), run_rn= row_number()) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(dplyr::case_when(alphabet=="otro" & run_rn==1~F,T~T)) %>% 
  # 11,577 × 18
  dplyr::group_by(run) %>% 
  dplyr::mutate(run_n= n(), run_rn= row_number()) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(dplyr::case_when(alphabet=="otro" & run_rn==1~F,T~T)) %>% 
  #11,542 × 18
  dplyr::group_by(run) %>% 
  dplyr::mutate(run_n= n(), run_rn= row_number()) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(dplyr::case_when(alphabet=="otro" & run_rn==1~F,T~T)) %>% 
  #11,537 × 18
  dplyr::group_by(run) %>% 
  dplyr::mutate(run_n= n(), run_rn= row_number()) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(dplyr::case_when(alphabet=="otro" & run_rn==1~F,T~T)) %>% 
  #11,536 × 18  
  dplyr::group_by(run) %>% 
  dplyr::mutate(run_n= n(), run_rn= row_number()) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(dplyr::case_when(alphabet=="otro" & run_rn==1~F,T~T)) %>% 
  #11,535 x 18
  dplyr::group_by(run) %>% 
  dplyr::mutate(run_n= n(), run_rn= row_number()) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(dplyr::case_when(alphabet=="otro" & run_rn==1~F,T~T)) #%>%   
  #11,535 x 18  
invisible("Dejó de eliminar casos")

invisible("2024-09-02: Eliminamos los que tuvieron un primer registro otros antes que los psiquiátricos entre el 2018-2022")
df_filled <- 
  data_long_est_24_std_imp_rec_imp_areaf_rec_cod  %>%
  dplyr::arrange(run, min_fecha_egreso_rec24_num, fecha_egreso_rec24_num) %>% 
  dplyr::group_by(run) %>% 
  dplyr::mutate(tto= dplyr::row_number()) %>% 
  dplyr::mutate(lag_fecha_egreso_rec_num= lag(unclass(as.Date(fecha_egreso_rec)), default= unclass(as.Date("2017-12-01")))) %>%
  dplyr::mutate(lag_fecha_ingreso_rec_num= lag(unclass(as.Date(fecha_ingreso_rec)), default= unclass(as.Date("2017-12-01")))) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(run, fecha_ingreso_rec24_num, fecha_egreso_rec24_num) %>% 
  dplyr::group_by(run) %>% 
  dplyr::mutate(run_n= n(), run_rn= row_number()) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(dplyr::case_when(alphabet=="otro" & run_rn==1~F,T~T)) %>% 
  # 11,577 × 18
  dplyr::group_by(run) %>% 
  dplyr::mutate(run_n= n(), run_rn= row_number()) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(dplyr::case_when(alphabet=="otro" & run_rn==1~F,T~T)) %>% 
  #11,542 × 18
  dplyr::group_by(run) %>% 
  dplyr::mutate(run_n= n(), run_rn= row_number()) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(dplyr::case_when(alphabet=="otro" & run_rn==1~F,T~T)) %>% 
  #11,537 × 18
  dplyr::group_by(run) %>% 
  dplyr::mutate(run_n= n(), run_rn= row_number()) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(dplyr::case_when(alphabet=="otro" & run_rn==1~F,T~T)) %>% 
  #11,536 × 18  
  dplyr::group_by(run) %>% 
  dplyr::mutate(run_n= n(), run_rn= row_number()) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(dplyr::case_when(alphabet=="otro" & run_rn==1~F,T~T)) %>% 
  #11,535 x 18
  dplyr::group_by(run) %>% 
  dplyr::mutate(run_n= n(), run_rn= row_number()) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(dplyr::case_when(alphabet=="otro" & run_rn==1~F,T~T)) #%>%   


invisible("2024-09-02: Casos que tienen ausente y sin información despues")
casos_prob<- c("ef1193767a2df73d6fdb1dd59d35edce262a9031d9ca5b012a11d088081b746e", 
     "bd0c2f8dcc5209831487342eda88d0b0461c98150911e36b64b4055a846a3631")
#O829 PARTO POR CESAREA, SIN OTRA ESPECIFICACION
# L032 CELULITIS DE LA CARA

def_enc17_21 <- read_delim("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/_output/def_enc.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)

dt_df_filled %>% 
  dplyr::filter(run %in% unique(c(def_enc17_21$RUN))) %>% 
  nrow()
#[1] 170 (antes 180)

dt_df_filled<- 
  tidylog::left_join(dt_df_filled, janitor::clean_names(def_enc17_21[,c("RUN", "FECHA_DEF")]), by="run")
# left_join: added one column (fecha_def)
#            > rows only in dt_df_filled                11,365
#            > rows only in janitor::clean_names(de.. (523,817)
#            > matched rows                                170
#            > rows total                               11,535

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
##_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
cat("Meses")
dt_df_filled_month_t_desde_primera_adm<- 
  data.table::as.data.table(dplyr::mutate(df_filled, rn= dplyr::row_number()))%>% 
  tidylog::left_join(janitor::clean_names(def_enc17_21[,c("RUN", "FECHA_DEF")]), by="run") %>%
  dplyr::mutate(fecha_def= as.Date.character(fecha_def, format="%d-%m-%Y"))%>% #janitor::tabyl(fecha_def)
  dplyr::select(run, edad_anos, min_edad_anos, pueblo_originario_01, inclusivo_real_historico, prev_benef_rec_post,
                fecha_ingreso_rec24_num, fecha_egreso_rec24_num, factor_inclusivo_real_hist_mas_autperc, glosa_sexo, fecha_def,
                days_elapsed, alphabet, sum_autolesiones, 
                codigo_region, ESTAB_HOMO, rn, paste0("diag",1:11,"_rec32")) %>% 
  dplyr::select(run, edad_anos, min_edad_anos, pueblo_originario_01, inclusivo_real_historico, prev_benef_rec_post,
                factor_inclusivo_real_hist_mas_autperc, glosa_sexo, fecha_def,
                days_elapsed, fecha_ingreso_rec24_num, fecha_egreso_rec24_num, alphabet, 
                codigo_region, ESTAB_HOMO, sum_autolesiones, rn, paste0("diag",1:11,"_rec32")) %>% 
  dplyr::group_by(run) %>% 
  dplyr::mutate(min_fecha_ingreso_rec24_num= min(fecha_ingreso_rec24_num, na.rm=T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(month_ing2 = (fecha_ingreso_rec24_num - min_fecha_ingreso_rec24_num) / 30.436875) %>% 
  dplyr::mutate(month_egr2 = ifelse((fecha_egreso_rec24_num - min_fecha_ingreso_rec24_num) / 30.436875==month_ing2, 
                                      ((fecha_egreso_rec24_num - min_fecha_ingreso_rec24_num) / 30.436875)+0.0001,
                                      ((fecha_egreso_rec24_num - min_fecha_ingreso_rec24_num) / 30.436875))) %>% 
  # dplyr::mutate(month_ing = ceiling((fecha_ingreso_rec24_num - min_fecha_ingreso_rec24_num) / 30.436875)) %>% 
  # dplyr::mutate(month_egr = ceiling((fecha_egreso_rec24_num - min_fecha_ingreso_rec24_num) / 30.436875)) %>% 
  dplyr::mutate(month_ing = floor((fecha_ingreso_rec24_num - min_fecha_ingreso_rec24_num) / 30.436875)) %>% 
  dplyr::mutate(month_egr = floor((fecha_egreso_rec24_num - min_fecha_ingreso_rec24_num) / 30.436875)) %>% 
  dplyr::mutate(cens_time= (unclass(as.Date("2023-01-01"))-min_fecha_ingreso_rec24_num) / 30.436875) %>% 
  dplyr::mutate(death_time= (unclass(as.Date(fecha_def))-min_fecha_ingreso_rec24_num) / 30.436875) %>%
  dplyr::mutate(comb = paste0(run, "_", rn)) %>%
  dplyr::select(run, edad_anos, min_edad_anos, factor_inclusivo_real_hist_mas_autperc, glosa_sexo, pueblo_originario_01, inclusivo_real_historico, prev_benef_rec_post,
                comb, cens_time, death_time, month_ing, month_ing2, month_egr, month_egr2, alphabet, sum_autolesiones, codigo_region, ESTAB_HOMO, days_elapsed, paste0("diag",1:11,"_rec32"))
# left_join: added one column (fecha_def)
#            > rows only in x                           11,365
#            > rows only in janitor::clean_names(de.. (523,817)
#            > matched rows                                170
#            > rows total                               11,535


dt_df_filled_month_t_desde_primera_adm_expand<- 
  dt_df_filled_month_t_desde_primera_adm %>%
  dplyr::rowwise() %>%
  dplyr::mutate(month = list(seq(from = month_ing, to = month_egr))) %>%
  tidyr::unnest(cols = month) %>% 
  dplyr::select(run, factor_inclusivo_real_hist_mas_autperc, glosa_sexo, edad_anos, min_edad_anos, pueblo_originario_01, inclusivo_real_historico, prev_benef_rec_post,
                comb, cens_time, death_time, month, month_ing, month_ing2, month_egr, month_egr2, alphabet, sum_autolesiones, codigo_region, ESTAB_HOMO, days_elapsed, paste0("diag",1:11,"_rec32"))

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
##_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
cat("Trimestre")

dt_df_filled_quarter_t_desde_primera_adm<- 
  data.table::as.data.table(dplyr::mutate(df_filled, rn= dplyr::row_number()))%>% 
  tidylog::left_join(janitor::clean_names(def_enc17_21[,c("RUN", "FECHA_DEF")]), by="run") %>%
  dplyr::mutate(fecha_def= as.Date.character(fecha_def, format="%d-%m-%Y"))%>% #janitor::tabyl(fecha_def)
  dplyr::select(run, edad_anos, min_edad_anos, pueblo_originario_01, inclusivo_real_historico, prev_benef_rec_post,
                fecha_ingreso_rec24_num, fecha_egreso_rec24_num, factor_inclusivo_real_hist_mas_autperc, glosa_sexo, fecha_def,
                days_elapsed, alphabet, sum_autolesiones, 
                codigo_region, ESTAB_HOMO, rn, paste0("diag",1:11,"_rec32")) %>% 
  dplyr::select(run, factor_inclusivo_real_hist_mas_autperc, glosa_sexo, edad_anos, min_edad_anos, pueblo_originario_01, inclusivo_real_historico, prev_benef_rec_post,  fecha_def,
                days_elapsed, fecha_ingreso_rec24_num, fecha_egreso_rec24_num, alphabet, sum_autolesiones, 
                codigo_region, ESTAB_HOMO, rn, paste0("diag",1:11,"_rec32")) %>% 
  dplyr::group_by(run) %>% 
  dplyr::mutate(min_fecha_ingreso_rec24_num= min(fecha_ingreso_rec24_num, na.rm=T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(quarter_ing2 = (fecha_ingreso_rec24_num - min_fecha_ingreso_rec24_num) / 91.31063) %>% 
  dplyr::mutate(quarter_egr2 = ifelse((fecha_egreso_rec24_num - min_fecha_ingreso_rec24_num) / 91.31063==quarter_ing2, 
                                      ((fecha_egreso_rec24_num - min_fecha_ingreso_rec24_num) / 91.31063)+0.0001,
                                      ((fecha_egreso_rec24_num - min_fecha_ingreso_rec24_num) / 91.31063))) %>% 
  # dplyr::mutate(quarter_ing = ceiling((fecha_ingreso_rec24_num - min_fecha_ingreso_rec24_num) / 91.31063)) %>% 
  # dplyr::mutate(quarter_egr = ceiling((fecha_egreso_rec24_num - min_fecha_ingreso_rec24_num) / 91.31063)) %>% 
  dplyr::mutate(quarter_ing = floor((fecha_ingreso_rec24_num - min_fecha_ingreso_rec24_num) / 91.31063)) %>% 
  dplyr::mutate(quarter_egr = floor((fecha_egreso_rec24_num - min_fecha_ingreso_rec24_num) / 91.31063)) %>% 
  dplyr::mutate(cens_time= (unclass(as.Date("2023-01-01"))-min_fecha_ingreso_rec24_num)/ 91.31063) %>% 
  dplyr::mutate(death_time= (unclass(as.Date(fecha_def))-min_fecha_ingreso_rec24_num) / 91.31063) %>%
  dplyr::mutate(comb = paste0(run, "_", rn)) %>%
  dplyr::select(run, factor_inclusivo_real_hist_mas_autperc, glosa_sexo, edad_anos, min_edad_anos, pueblo_originario_01, inclusivo_real_historico, prev_benef_rec_post,
                comb, cens_time, death_time, quarter_ing, quarter_ing2, quarter_egr, quarter_egr2, alphabet, sum_autolesiones, 
                codigo_region, ESTAB_HOMO, days_elapsed, paste0("diag",1:11,"_rec32"))
# left_join: added one column (fecha_def)
#            > rows only in x                           11,365
#            > rows only in janitor::clean_names(de.. (523,817)
#            > matched rows                                170
#            > rows total                               11,535

dt_df_filled_quarter_t_desde_primera_adm_expand<- 
dt_df_filled_quarter_t_desde_primera_adm %>%
  dplyr::rowwise() %>%
  dplyr::mutate(quarter = list(seq(from = quarter_ing, to = quarter_egr))) %>%
  tidyr::unnest(cols = quarter) %>% 
  dplyr::select(run, factor_inclusivo_real_hist_mas_autperc, glosa_sexo, edad_anos, min_edad_anos, pueblo_originario_01, inclusivo_real_historico, prev_benef_rec_post,
                comb, cens_time, death_time, quarter, quarter_ing, quarter_ing2, quarter_egr, quarter_egr2, alphabet, sum_autolesiones, 
                codigo_region, ESTAB_HOMO, days_elapsed, paste0("diag",1:11,"_rec32"))

print("última fecha de ingreso (en trimestres desde la priemra admisión)")
max(dt_df_filled_quarter_t_desde_primera_adm_expand$quarter_egr2)
#[1] 17.80735

print("última fecha de egreso (en trimestres desde la priemra admisión)")
max(dt_df_filled_quarter_t_desde_primera_adm_expand$quarter_egr2)
#[1] 17.80735

### 2.1.1. Mensual ---------------------------------
invisible("Descartado en descartados_SSA_format.R")

#### 2.1.1.2 Mensual tiempo-primera adm ---------------------------------

dt_df_filled_month_t_desde_primera_adm_expand$prev_benef_rec_post <-  
  as.character(dt_df_filled_month_t_desde_primera_adm_expand$prev_benef_rec_post)

ing_calendar_month_t_desde_primera_adm <- janitor::clean_names(sqldf::sqldf("SELECT *
      FROM expand_df_month_run_t_desde_primera_adm AS x  
      LEFT JOIN dt_df_filled_month_t_desde_primera_adm_expand AS y 
      ON x.run = y.run 
      AND x.month = y.month
"))

ing_calendar_month_t_desde_primera_adm_dup<-
  ing_calendar_month_t_desde_primera_adm %>% 
  dplyr::select(-run_2) %>% #, fecha_ingreso_rec24_num, fecha_egreso_rec24_num) %>% 
  dplyr::mutate(alphabet= ifelse(is.na(alphabet),"AUS", alphabet)) %>% 
  dplyr::arrange(run, month) %>% 
  dplyr::group_by(run, month) %>% 
  dplyr::mutate(n_dup= n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(n_dup>1)

#15 x 17
ing_calendar_month_t_desde_primera_adm_dup %>% 
  dplyr::filter(n_dup==max(n_dup)) %>% 
  dplyr::select(run, month, factor_inclusivo_real_hist_mas_autperc, glosa_sexo, edad_anos,
                min_edad_anos, pueblo_originario_01, inclusivo_real_historico, prev_benef_rec_post,
                cens_time, month_ing2, month_egr2, alphabet, sum_autolesiones, days_elapsed, diag1_rec32, diag2_rec32) %>% 
  dplyr::mutate(across(c("cens_time", "month_ing2", "month_egr2"),~sprintf("%1.2f",.))) %>% 
  {
    knitr::kable(., "markdown") %>% print()
    rio::export(., file="H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/_output/month_dup_max.xlsx")  
  }
  

ing_calendar_month_t_desde_primera_adm_dup %>% 
  distinct(run, month, n_dup) %>% pull(n_dup) %>% 
  {
    hist(.)
    print(summary(.))
  }
# distinct: removed 1,577 rows (55%), 1,290 rows remaining
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.000   2.000   2.000   2.229   2.000  15.000 

# codigo_region, ESTAB_HOMO,
dt_ing_calendar_month_t_desde_primera_adm <- data.table::as.data.table(ing_calendar_month_t_desde_primera_adm)

dt_ing_calendar_month_t_desde_primera_adm[, `:=`(
  sum_tus2 = ifelse(!is.na(run_2) & run_2 != "", sum(.SD %in% c("coc", "mar", "oh"), na.rm = TRUE), NA_integer_),
  sum_coc2 = ifelse(!is.na(run_2) & run_2 != "", sum(.SD %in% c("coc"), na.rm = TRUE), NA_integer_),
  sum_mar2 = ifelse(!is.na(run_2) & run_2 != "", sum(.SD %in% c("mar"), na.rm = TRUE), NA_integer_),
  sum_oh2 = ifelse(!is.na(run_2) & run_2 != "", sum(.SD %in% c("oh"), na.rm = TRUE), NA_integer_),
  sum_tus_psy2 = ifelse(!is.na(run_2) & run_2 != "", sum(.SD %in% c("coc", "mar", "oh", "psy"), na.rm = TRUE), NA_integer_)
), by = 1:nrow(dt_ing_calendar_month_t_desde_primera_adm), .SDcols = patterns("rec32")]

dt_ing_calendar_month_t_desde_primera_adm_dedup<-
  dt_ing_calendar_month_t_desde_primera_adm %>% 
  dplyr::mutate(delta= round((((days_elapsed)+1)/30.436875)*100,1)) %>% 
  dplyr::mutate(codigo_region_rec=ifelse(codigo_region==13,"RM","noRM")) %>% 
  dplyr::mutate(estab_homo=as.character(estab_homo)) %>% 
  dplyr::group_by(run) %>%
  dplyr::mutate(
    min_edad_anos                          = mean(min_edad_anos, na.rm = TRUE),
    pueblo_originario_01                   = data.table::first(pueblo_originario_01, na.rm = TRUE),# el argumento no fue usado (na.rm = TRUE)
    inclusivo_real_historico               = max(inclusivo_real_historico, na.rm = TRUE),
    prev_benef_rec_post                    = max(prev_benef_rec_post, na.rm = TRUE),
    glosa_sexo                             = max(glosa_sexo, na.rm = TRUE),
    factor_inclusivo_real_hist_mas_autperc = max(factor_inclusivo_real_hist_mas_autperc, na.rm = TRUE),
    cens_time                              = mean(cens_time, na.rm=T),
    death_time                             = mean(death_time, na.rm=T),
    codigo_region_rec_base                 = data.table::first(codigo_region_rec, na.rm=T),
    estab_homo_base                        = data.table::first(estab_homo, na.rm=T)
  ) %>%
  ungroup() %>%   
  dplyr::group_by(run, month) %>%
  dplyr::summarise(
    sum_tus2      = sum(sum_tus2, na.rm = TRUE),
    sum_coc2      = sum(sum_coc2, na.rm = TRUE),
    sum_mar2      = sum(sum_mar2, na.rm = TRUE),
    sum_oh2       = sum(sum_oh2, na.rm = TRUE),
    sum_tus_psy2  = sum(sum_tus_psy2, na.rm = TRUE),
    alphabet_comb = paste(alphabet, collapse = " + "),
    delta_comb    = paste(delta, collapse = " + "),
    n_dup         = n(), 
    min_edad_anos             = mean(min_edad_anos, na.rm = TRUE),
    pueblo_originario_01      = data.table::first(pueblo_originario_01, na.rm = TRUE),
    inclusivo_real_historico  = max(inclusivo_real_historico, na.rm = TRUE),
    prev_benef_rec_post       = max(prev_benef_rec_post, na.rm = TRUE),
    glosa_sexo                             = max(glosa_sexo, na.rm = TRUE),
    factor_inclusivo_real_hist_mas_autperc = max(factor_inclusivo_real_hist_mas_autperc, na.rm = TRUE),
    cens_time                              = mean(cens_time, na.rm=T),
    death_time                             = mean(death_time, na.rm=T),
    codigo_region_rec_base                 = data.table::first(codigo_region_rec, na.rm=T),
    estab_homo_base                        = data.table::first(estab_homo, na.rm=T),
    sum_autolesiones                       = sum(sum_autolesiones, na.rm=T),
    sum_codigo_region_rec_rm               = sum(codigo_region_rec=="RM", na.rm=T),
    sum_codigo_region_rec_norm             = sum(codigo_region_rec=="noRM", na.rm=T)
  ) %>%
  ungroup() %>% 
  dplyr::mutate(alphabet2 = dplyr::case_when(
    sum_coc2 > 0 & sum_coc2 == sum_tus2 & sum_tus2 == sum_tus_psy2 ~ "coc",
    sum_mar2 > 0 & sum_mar2 == sum_tus2 & sum_tus2 == sum_tus_psy2 ~ "mar",
    sum_oh2 > 0 & sum_oh2 == sum_tus2 & sum_tus2 == sum_tus_psy2 ~ "oh",
    ((sum_coc2 > 0) + (sum_mar2 > 0) + (sum_oh2 > 0)) > 1 & sum_tus2 == sum_tus_psy2 ~ "psu",
    ((sum_coc2 > 0) + (sum_mar2 > 0) + (sum_oh2 > 0)) == 1 & sum_tus_psy2 > sum_tus2 ~ "cp",
    ((sum_coc2 > 0) + (sum_mar2 > 0) + (sum_oh2 > 0)) > 1 & sum_tus_psy2 > sum_tus2 ~ "cp_psu",
    sum_tus_psy2 > 0 & sum_tus2 == 0 ~ "psy",
    alphabet_comb=="NA"~"aus",
    TRUE ~ "otro"
  )) %>% 
  #2024-08-09: modificaron los estados del alfabeto
  dplyr::mutate(alphabet3 = dplyr::case_when(
    ((sum_coc2 > 0) + (sum_mar2 > 0) + (sum_oh2 > 0)) >= 1 & sum_tus_psy2 == sum_tus2 ~ "sus",
    ((sum_coc2 > 0) + (sum_mar2 > 0) + (sum_oh2 > 0)) >= 1 & sum_tus_psy2 > sum_tus2 ~ "cp",
    sum_tus_psy2 > 0 & sum_tus2 == 0 ~ "psi",
    alphabet_comb=="NA"~"aus",
    TRUE ~ "otro"
  )) %>% 
  #2024-08-09: para región
  dplyr::mutate(alphabet4 = dplyr::case_when(
    sum_codigo_region_rec_norm>0~ "noRM",
    alphabet_comb=="NA"~"aus",
    TRUE ~ "RM"
  )) %>% 
  #2024-08-10: corregir por ausencias del primer establecimiento en región y código
  dplyr::group_by(run) %>% 
  dplyr::mutate(codigo_region_rec_base= max(codigo_region_rec_base, na.rm=T),
                estab_homo_base= max(estab_homo_base, na.rm=T)) %>% 
  dplyr::ungroup() %>% 
  #codigo_region_rec_base
  dplyr::mutate(alphabet_comb= ifelse(alphabet_comb=="NA",NA, alphabet_comb),
                delta_comb= ifelse(delta_comb=="NA",NA, delta_comb))

if(nrow(dt_ing_calendar_month_t_desde_primera_adm_dedup)> length(unique(dt_ing_calendar_month_t_desde_primera_adm$run))*60){
  stop(paste0("Número de filas, debería ser: ",
              length(unique(dt_ing_calendar_month_t_desde_primera_adm$run))*60))  
}

ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide<-
  dt_ing_calendar_month_t_desde_primera_adm_dedup %>% 
  tidyr::pivot_wider(id_cols= c("run","pueblo_originario_01", "glosa_sexo", 
                                "factor_inclusivo_real_hist_mas_autperc", "inclusivo_real_historico",  "prev_benef_rec_post", 
                                "cens_time", "death_time","codigo_region_rec_base", "estab_homo_base"), #, 
                     names_from="month", values_from ="alphabet2") #%>% 
table(ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide$`0`, exclude=NULL)
# aus    coc     cp cp_psu    mar     oh    psu    psy 
#   2    173    167     30    143    330     50   5145 


invisible("No puedo meter codigo region rec base y estab homo base, porque generan filas duplicadas")

if(nrow(ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide)> length(unique(dt_ing_calendar_month_t_desde_primera_adm$run))){
  stop(paste0("Número de filas, debería ser: ",
              length(unique(dt_ing_calendar_month_t_desde_primera_adm$run))))  
}


ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide_cens <-
  ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide

ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide_cens$death_time<- 
  ifelse(is.nan(ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide_cens$death_time),60,ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide_cens$death_time)


#2024-08-09: modificaron los estados del alfabeto
ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide2<-
  dt_ing_calendar_month_t_desde_primera_adm_dedup %>% 
  tidyr::pivot_wider(id_cols= c("run","pueblo_originario_01", "glosa_sexo", 
                                "factor_inclusivo_real_hist_mas_autperc", "inclusivo_real_historico",  
                                "prev_benef_rec_post", "cens_time", "death_time", 
                                "codigo_region_rec_base", "estab_homo_base"), 
                     names_from="month", values_from ="alphabet3") #%>% 

table(ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide2$`0`, exclude=NULL)
# aus   cp  psi  sus 
#   2  197 5145  696 

#2024-08-09:  autolesiones
ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide3<-
  dt_ing_calendar_month_t_desde_primera_adm_dedup %>% 
  dplyr::mutate(autolesion= ifelse(sum_autolesiones>0,1,0)) %>% 
  tidyr::pivot_wider(id_cols= c("run","pueblo_originario_01", "glosa_sexo", 
                                "factor_inclusivo_real_hist_mas_autperc", "inclusivo_real_historico",  
                                "prev_benef_rec_post", "cens_time", "death_time", 
                                "codigo_region_rec_base", "estab_homo_base"), 
                     names_from="month", values_from ="autolesion") #%>% 
invisible("Son muy pocos")

#2024-08-09:  region hospitales
ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide4<-
  dt_ing_calendar_month_t_desde_primera_adm_dedup %>% 
  tidyr::pivot_wider(id_cols= c("run","pueblo_originario_01", "glosa_sexo", 
                                "factor_inclusivo_real_hist_mas_autperc", "inclusivo_real_historico", 
                                "prev_benef_rec_post", "cens_time", "death_time", 
                                "codigo_region_rec_base", "estab_homo_base"), 
                     names_from="month", values_from ="alphabet4") #%>% 

#2024-08-09: modificaron los estados del alfabeto
ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide2_cens <-
  ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide2

ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide4_cens <-
  ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide4

ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide2_cens$death_time<- 
  ifelse(is.nan(ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide2_cens$death_time),60,ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide2_cens$death_time)

ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide4_cens$death_time<- 
  ifelse(is.nan(ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide4_cens$death_time),60,ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide4_cens$death_time)


#tiene censura 59, si la cur_column es mayor o igual a la columna 59, "censura"
for(i in 59:1) {
  ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide_cens <- ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide_cens %>%
    mutate(!!as.character(i) := ifelse(ceiling(cens_time) <= i, "cens", !!sym(as.character(i))))
}
for(i in 59:1) {
  ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide_cens <- ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide_cens %>%
    mutate(!!as.character(i) := ifelse(ceiling(death_time) <= i, "cens", !!sym(as.character(i))))
}

#2024-08-09: modificaron los estados del alfabeto
for(i in 59:1) {
  ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide2_cens <- ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide2_cens %>%
    mutate(!!as.character(i) := ifelse(ceiling(cens_time) <= i, "cens", !!sym(as.character(i))))
}
for(i in 59:1) {
  ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide2_cens <- ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide2_cens %>%
    mutate(!!as.character(i) := ifelse(ceiling(death_time) <= i, "cens", !!sym(as.character(i))))
}

#2024-08-09: alphabet
for(i in 59:1) {
  ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide4_cens <- ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide4_cens %>%
    mutate(!!as.character(i) := ifelse(ceiling(cens_time) <= i, "cens", !!sym(as.character(i))))
}
for(i in 59:1) {
  ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide4_cens <- ing_dt_ing_calendar_month_t_desde_primera_adm_dedup_wide4_cens %>%
    mutate(!!as.character(i) := ifelse(ceiling(death_time) <= i, "cens", !!sym(as.character(i))))
}

### 2.1.2. Trimestral ---------------------------------

invisible("está en descartados_SSA_format.R")

#### 2.1.2.2 Trimestral tiempo-primera adm ---------------------------------

dt_df_filled_quarter_t_desde_primera_adm_expand$prev_benef_rec_post <-  
  as.character(dt_df_filled_quarter_t_desde_primera_adm_expand$prev_benef_rec_post)


ing_calendar_quarter_t_desde_primera_adm <- janitor::clean_names(sqldf::sqldf("
  SELECT *
  FROM expand_df_quarter_run_t_desde_primera_adm AS x  
  LEFT JOIN dt_df_filled_quarter_t_desde_primera_adm_expand AS y 
  ON x.run = y.run 
  AND x.quarter = y.quarter
"))
# AND x.quarter <= y.quarter_ing
# AND x.quarter + 1 > y.quarter_egr
#

nrow(ing_calendar_quarter_t_desde_primera_adm)-nrow(expand_df_quarter_run_t_desde_primera_adm)
invisible("Se añadieron 2212 filas")

nrow(ing_calendar_quarter_t_desde_primera_adm)
#123012

ing_calendar_quarter_t_desde_primera_adm_dup<-
  ing_calendar_quarter_t_desde_primera_adm %>% 
  dplyr::select(-run_2) %>% #, fecha_ingreso_rec24_num, fecha_egreso_rec24_num) %>% 
  dplyr::mutate(alphabet= ifelse(is.na(alphabet),"AUS", alphabet)) %>% 
  dplyr::arrange(run, quarter) %>% 
  dplyr::group_by(run, quarter) %>% 
  dplyr::mutate(n_dup= n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(n_dup>1)

ing_calendar_quarter_t_desde_primera_adm_dup %>% 
  dplyr::filter(n_dup==max(n_dup)) %>% 
  dplyr::select(run, quarter, factor_inclusivo_real_hist_mas_autperc, glosa_sexo, edad_anos,
                min_edad_anos, pueblo_originario_01, inclusivo_real_historico, prev_benef_rec_post,
                cens_time, quarter_ing2, quarter_egr2, alphabet, days_elapsed, diag1_rec32, diag2_rec32) %>% 
  dplyr::mutate(across(c("cens_time", "quarter_ing2", "quarter_egr2"),~sprintf("%1.2f",.))) %>% 
  {
    knitr::kable(., "markdown") %>% print()
    rio::export(., file="H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/_output/quarter_dup_max.xlsx")
  }
  
  

ing_calendar_quarter_t_desde_primera_adm_dup %>% 
  distinct(run, quarter, n_dup) %>% pull(n_dup) %>% 
  {
    hist(.)
    print(summary(.))
  }
# distinct: removed 2,212 rows (57%), 1,636 rows remaining
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   2.000   2.000   2.000   2.352   2.000  34.000 

dt_ing_calendar_quarter_t_desde_primera_adm <- data.table::as.data.table(ing_calendar_quarter_t_desde_primera_adm)

dt_ing_calendar_quarter_t_desde_primera_adm[, `:=`(
  sum_tus2 = ifelse(!is.na(run_2) & run_2 != "", sum(.SD %in% c("coc", "mar", "oh"), na.rm = TRUE), NA_integer_),
  sum_coc2 = ifelse(!is.na(run_2) & run_2 != "", sum(.SD %in% c("coc"), na.rm = TRUE), NA_integer_),
  sum_mar2 = ifelse(!is.na(run_2) & run_2 != "", sum(.SD %in% c("mar"), na.rm = TRUE), NA_integer_),
  sum_oh2 = ifelse(!is.na(run_2) & run_2 != "", sum(.SD %in% c("oh"), na.rm = TRUE), NA_integer_),
  sum_tus_psy2 = ifelse(!is.na(run_2) & run_2 != "", sum(.SD %in% c("coc", "mar", "oh", "psy"), na.rm = TRUE), NA_integer_)
), by = 1:nrow(dt_ing_calendar_quarter_t_desde_primera_adm), .SDcols = patterns("rec32")]

dt_ing_calendar_quarter_t_desde_primera_adm_dedup<-
  dt_ing_calendar_quarter_t_desde_primera_adm %>% 
  dplyr::mutate(delta= round((((days_elapsed)+1)/91.31063)*100,1)) %>% 
  dplyr::mutate(codigo_region_rec=ifelse(codigo_region==13,"RM","noRM")) %>% 
  dplyr::group_by(run) %>%
  dplyr::mutate(
    min_edad_anos                          = mean(min_edad_anos, na.rm = TRUE),
    pueblo_originario_01                   = data.table::first(pueblo_originario_01, na.rm = TRUE),
    inclusivo_real_historico               = max(inclusivo_real_historico, na.rm = TRUE),
    prev_benef_rec_post                    = max(prev_benef_rec_post, na.rm = TRUE),
    glosa_sexo                             = max(glosa_sexo, na.rm = TRUE),
    factor_inclusivo_real_hist_mas_autperc = max(factor_inclusivo_real_hist_mas_autperc, na.rm = TRUE),
    cens_time                              = mean(cens_time, na.rm=T),
    death_time                             = mean(death_time, na.rm=T),
    codigo_region_rec_base                 = data.table::first(codigo_region_rec, na.rm=T),
    estab_homo_base                        = data.table::first(estab_homo, na.rm=T)
  ) %>%
  ungroup() %>%   
  dplyr::group_by(run, quarter) %>%
  dplyr::summarise(
    sum_tus2      = sum(sum_tus2, na.rm = TRUE),
    sum_coc2      = sum(sum_coc2, na.rm = TRUE),
    sum_mar2      = sum(sum_mar2, na.rm = TRUE),
    sum_oh2       = sum(sum_oh2, na.rm = TRUE),
    sum_tus_psy2  = sum(sum_tus_psy2, na.rm = TRUE),
    alphabet_comb = paste(alphabet, collapse = " + "),
    delta_comb    = paste(delta, collapse = " + "),
    n_dup         = n(), 
    min_edad_anos             = mean(min_edad_anos, na.rm = TRUE),
    pueblo_originario_01      = data.table::first(pueblo_originario_01, na.rm = TRUE),
    inclusivo_real_historico  = max(inclusivo_real_historico, na.rm = TRUE),
    prev_benef_rec_post       = max(prev_benef_rec_post, na.rm = TRUE),
    glosa_sexo                             = max(glosa_sexo, na.rm = TRUE),
    factor_inclusivo_real_hist_mas_autperc = max(factor_inclusivo_real_hist_mas_autperc, na.rm = TRUE),
    cens_time                              = mean(cens_time, na.rm=T),
    death_time                             = mean(death_time, na.rm=T),
    codigo_region_rec_base                 = data.table::first(codigo_region_rec, na.rm=T),
    estab_homo_base                        = data.table::first(estab_homo, na.rm=T),    
    sum_autolesiones                       = sum(sum_autolesiones, na.rm=T),
    sum_codigo_region_rec_rm               = sum(codigo_region_rec=="RM", na.rm=T),
    sum_codigo_region_rec_norm             = sum(codigo_region_rec=="noRM", na.rm=T)
  ) %>%
  ungroup() %>%   
  dplyr::mutate(alphabet2 = dplyr::case_when(
    sum_coc2 > 0 & sum_coc2 == sum_tus2 & sum_tus2 == sum_tus_psy2 ~ "coc",
    sum_mar2 > 0 & sum_mar2 == sum_tus2 & sum_tus2 == sum_tus_psy2 ~ "mar",
    sum_oh2 > 0 & sum_oh2 == sum_tus2 & sum_tus2 == sum_tus_psy2 ~ "oh",
    ((sum_coc2 > 0) + (sum_mar2 > 0) + (sum_oh2 > 0)) > 1 & sum_tus2 == sum_tus_psy2 ~ "psu",
    ((sum_coc2 > 0) + (sum_mar2 > 0) + (sum_oh2 > 0)) == 1 & sum_tus_psy2 > sum_tus2 ~ "cp",
    ((sum_coc2 > 0) + (sum_mar2 > 0) + (sum_oh2 > 0)) > 1 & sum_tus_psy2 > sum_tus2 ~ "cp_psu",
    sum_tus_psy2 > 0 & sum_tus2 == 0 ~ "psy",
    alphabet_comb=="NA"~"aus",
    TRUE ~ "otro"
  )) %>% 
  #2024-08-09: modificaron los estados del alfabeto
  dplyr::mutate(alphabet3 = dplyr::case_when(
    ((sum_coc2 > 0) + (sum_mar2 > 0) + (sum_oh2 > 0)) >= 1 & sum_tus_psy2 == sum_tus2 ~ "sus",
    ((sum_coc2 > 0) + (sum_mar2 > 0) + (sum_oh2 > 0)) >= 1 & sum_tus_psy2 > sum_tus2 ~ "cp",
    sum_tus_psy2 > 0 & sum_tus2 == 0 ~ "psi",
    alphabet_comb=="NA"~"aus",
    TRUE ~ "otro"
  )) %>% 
  #2024-08-09: para región
  dplyr::mutate(alphabet4 = dplyr::case_when(
    sum_codigo_region_rec_norm>0~ "noRM",
    alphabet_comb=="NA"~"aus",
    TRUE ~ "RM"
  )) %>% 
  #2024-08-10: corregir por ausencias del primer establecimiento en región y código
  dplyr::group_by(run) %>% 
  dplyr::mutate(codigo_region_rec_base= max(codigo_region_rec_base, na.rm=T),
                estab_homo_base= max(estab_homo_base, na.rm=T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(alphabet_comb= ifelse(alphabet_comb=="NA",NA, alphabet_comb),
                delta_comb= ifelse(delta_comb=="NA",NA, delta_comb))

if(nrow(dt_ing_calendar_quarter_t_desde_primera_adm_dedup)> length(unique(dt_ing_calendar_quarter_t_desde_primera_adm$run))*20){
  stop(paste0("Número de filas, debería ser: ",
              length(unique(dt_ing_calendar_quarter_t_desde_primera_adm$run))*20))  
}



ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide<-
  dt_ing_calendar_quarter_t_desde_primera_adm_dedup %>% 
  tidyr::pivot_wider(id_cols= c("run","pueblo_originario_01", "glosa_sexo", 
                                "factor_inclusivo_real_hist_mas_autperc", "inclusivo_real_historico", 
                                "prev_benef_rec_post", "cens_time", "death_time", 
                                "codigo_region_rec_base", "estab_homo_base"), 
                     names_from="quarter", values_from ="alphabet2") 

if(nrow(ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide)> length(unique(dt_ing_calendar_quarter_t_desde_primera_adm$run))){
  stop(paste0("Número de filas, debería ser: ",
              length(unique(dt_ing_calendar_quarter_t_desde_primera_adm$run))))  
}


ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide_cens <-
  ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide

ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide_cens$death_time<- 
  ifelse(is.nan(ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide_cens$death_time),21,ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide_cens$death_time)


#2024-08-09: modificaron los estados del alfabeto
ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide2<-
  dt_ing_calendar_quarter_t_desde_primera_adm_dedup %>% 
  tidyr::pivot_wider(id_cols= c("run","pueblo_originario_01", "glosa_sexo", 
                                "factor_inclusivo_real_hist_mas_autperc", "inclusivo_real_historico", 
                                "prev_benef_rec_post", "cens_time", "death_time", 
                                "codigo_region_rec_base", "estab_homo_base"), 
                     names_from="quarter", values_from ="alphabet3") 

#2024-08-09:  autolesiones
ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide3<-
  dt_ing_calendar_quarter_t_desde_primera_adm_dedup %>% 
  dplyr::mutate(autolesion= ifelse(sum_autolesiones>0,1,0)) %>% 
  tidyr::pivot_wider(id_cols= c("run","pueblo_originario_01", "glosa_sexo", 
                                "factor_inclusivo_real_hist_mas_autperc", "inclusivo_real_historico",
                                "prev_benef_rec_post", "cens_time", "death_time", 
                                "codigo_region_rec_base", "estab_homo_base"), 
                     names_from="quarter", values_from ="autolesion") 

#2024-08-09:  region
ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide4<-
  dt_ing_calendar_quarter_t_desde_primera_adm_dedup %>% 
  tidyr::pivot_wider(id_cols= c("run","pueblo_originario_01", "glosa_sexo", 
                                "factor_inclusivo_real_hist_mas_autperc", "inclusivo_real_historico",
                                "prev_benef_rec_post", "cens_time", "death_time", 
                                "codigo_region_rec_base", "estab_homo_base"), 
                     names_from="quarter", values_from ="alphabet4") 

ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide2_cens <-
  ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide2

ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide2_cens$death_time<- 
  ifelse(is.nan(ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide2_cens$death_time),20,ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide2_cens$death_time)

ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide4_cens <-
  ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide4

ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide4_cens$death_time<- 
  ifelse(is.nan(ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide4_cens$death_time),20,ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide4_cens$death_time)


#tiene censura 59, si la cur_column es mayor o igual a la columna 59, "censura"
for(i in 19:1) {
  ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide_cens <- ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide_cens %>%
    mutate(!!as.character(i) := ifelse(ceiling(cens_time) <= i, "cens", !!sym(as.character(i))))
}
for(i in 19:1) {
  ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide_cens <- ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide_cens %>%
    mutate(!!as.character(i) := ifelse(ceiling(death_time) <= i, "cens", !!sym(as.character(i))))
}

#2024-08-09: modificaron los estados del alfabeto
for(i in 19:1) {
  ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide2_cens <- ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide2_cens %>%
    mutate(!!as.character(i) := ifelse(ceiling(cens_time) <= i, "cens", !!sym(as.character(i))))
}
for(i in 19:1) {
  ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide2_cens <- ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide2_cens %>%
    mutate(!!as.character(i) := ifelse(ceiling(death_time) <= i, "cens", !!sym(as.character(i))))
}

#2024-08-09: modificaron los estados del alfabeto4
for(i in 19:1) {
  ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide4_cens <- ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide4_cens %>%
    mutate(!!as.character(i) := ifelse(ceiling(cens_time) <= i, "cens", !!sym(as.character(i))))
}
for(i in 19:1) {
  ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide4_cens <- ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide4_cens %>%
    mutate(!!as.character(i) := ifelse(ceiling(death_time) <= i, "cens", !!sym(as.character(i))))
}

# 3. Exportar -------------------------------------------------------------

#save.image(paste0("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/20240728.RData"))
save.image(paste0("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/20240902.RData"))