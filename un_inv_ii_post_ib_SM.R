sink("./un_inv_ii_post_ib_SM.txt", type = "output")
sink("./un_inv_ii_post_ib_SM_err.txt", type = "message")


# 0.Fechas y paquetes --------------------------------------------------

# remover objetos y memoria utilizada
rm(list=ls());gc()
# paquetes en R
if(!require(dplyr)){install.packages("dplyr");require(dplyr)}
if(!require(lubridate)){install.packages("lubridate");require(lubridate)}
if(!require(ggplot2)){install.packages("ggplot2");require(ggplot2)}
if(!require(janitor)){install.packages("janitor");require(janitor)}

if(!require(arrow)){install.packages("arrow");require(arrow)}
if(!require(rio)){install.packages("rio");require(rio)}

if(!require(DiagrammeR)){install.packages("DiagrammeR");require(DiagrammeR)}
if(!require(rsvg)){install.packages("rsvg");require(rsvg)}
if(!require(DiagrammeRsvg)){install.packages("DiagrammeRsvg");require(DiagrammeRsvg)}
if(!require(webshot)){install.packages("webshot");require(webshot)}

if(!require(naniar)){install.packages("naniar");require(naniar)}

if(!require(epitools)){install.packages("epitools");require(epitools)}
if(!require(epiR)){install.packages("epiR");require(epiR)}
if(!require(psych)){install.packages("psych");require(psych)}

if(!require(pROC)){install.packages("pROC");require(pROC)}
if(!require(caret)){install.packages("caret");require(caret)}

if(!require(boot)){install.packages("boot");require(boot)}
if(!require(parallel)){install.packages("parallel");require(parallel)}

if(!require(tableone)){install.packages("tableone");require(tableone)}
if(!require(sqldf)){install.packages("sqldf");require(sqldf)}
if(!require(MatchIt)){install.packages("MatchIt");require(MatchIt)}
if(!require(cobalt)){install.packages("cobalt");require(cobalt)}
if(!require(SuperLearner)){install.packages("SuperLearner");require(SuperLearner)}
if(!require(caret)){install.packages("caret");require(caret)}
if(!require(glmnet)){install.packages("glmnet");require(glmnet)}
if(!require(randomForest)){install.packages("randomForest");require(randomForest)}
if(!require(ranger)){install.packages("ranger");require(ranger)}
if(!require(dbarts)){install.packages("dbarts");require(dbarts)}
if(!require(ggalluvial)){install.packages("ggalluvial");require(ggalluvial)}


#install.packages(c("caret", "glmnet", "randomForest", "ggplot2", "RhpcBLASctl"))


# 0b. Cargar bases --------------------------------------------------

#dtX2023_12_05_DatosEgresosHosp_encrip<-rio::import("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_ii/20231205_hosp_mod20240331.parquet.gzip")
dtX2023_12_05_DatosEgresosHosp_encrip<-rio::import("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_ii/20231205_hosp_mod20240404.parquet.gzip")

cnt_runs_original_bd<- paste0('Base de datos original\n(n= ', 
         formatC(nrow(dtX2023_12_05_DatosEgresosHosp_encrip), 
                 format='f', big.mark='.', digits=0), 
         ';\np= ',
         formatC(length(unique(dtX2023_12_05_DatosEgresosHosp_encrip$run)), 
                 format='f', big.mark='.', digits=0),')')

homolog_estab_misal<-
  readr::read_delim("https://docs.google.com/spreadsheets/d/1Ztg38BGjUzbY7acVfh4rXaXYaOi8EBuX/export?format=tsv&id=1Ztg38BGjUzbY7acVfh4rXaXYaOi8EBuX&gid=243483182")

homolog_estab_misal2<-
rio::import("https://repositoriodeis.minsal.cl/Egresos/egr2018/Establecimientos_2018.xlsx", skip=2)

# 1. definir lugar atención --------------------------------------------------


df_estab_tab<-
data.frame(estab = c("20101", "11322", "20108", "11324", "11299", 
"12520", "20072", "20008", "20086", "10122", "12670", "20028", 
"20070", "20056", "13320", "11415", "12222", "10120", "10222", 
"20055", "11505", "10726", "11020", "11851", "11298", "12105", 
"20019", "11821", "11421", "11524", "12427", "20088", "10526", 
"13317", "11317", "12438", "11321", "10325", "10123", "20009", 
"10420", "11605", "11016", "12414", "11420", "10711", "11850", 
"20071", "11135", "10820", "11222", "12413", "11250", "12411", 
"12412", "11027", "10620", "11227", "11251", "20006", "11316", 
"20023", "11720", "11014", "13316", "10424", "11122", "11620", 
"11011", "10621", "10121", "11252", "11127", "20005", "12425", 
"20048", "12426", "12421", "11722", "11225", "11626", "12320", 
"11315", "12020", "11226", "10520", "12810", "12112", "12620", 
"11511", "10322", "11013", "11520", "10220", "11015", "11313", 
"11121", "11319", "11223", "11123", "10321", "12811", "11119", 
"10320", "11295", "11611", "12510", "12220", "13315", "11253", 
"11422", "11522", "12610", "11318", "10110", "10722", "11129", 
"11012", "10721", "10210", "10720", "11820", "10410", "12111", 
"12310", "11920", "12910", "12120", "12410", "10810", "11010", 
"11910", "11120", "11310", "12110", "10310", "12210", "11710", 
"12010", "11110", "10610", "11510", "10910", "10510", "11220", 
"10710", "11221", "11810", "11210", "10920", "11610", "11224", 
"11410"), N = c(7L, 12L, 20L, 40L, 81L, 130L, 177L, 217L, 253L, 
316L, 343L, 356L, 414L, 424L, 514L, 672L, 754L, 834L, 861L, 903L, 
1026L, 1099L, 1108L, 1160L, 1179L, 1318L, 1382L, 1716L, 1761L, 
1877L, 1903L, 1908L, 1939L, 2042L, 2340L, 2429L, 2840L, 3237L, 
3338L, 3346L, 4233L, 4908L, 5047L, 5258L, 5756L, 6198L, 8711L, 
9834L, 12308L, 14392L, 14754L, 16860L, 16941L, 17283L, 18390L, 
18742L, 19534L, 19665L, 20479L, 23436L, 27280L, 30599L, 31122L, 
32395L, 32785L, 37368L, 41431L, 43325L, 45871L, 48662L, 49670L, 
54006L, 55998L, 57785L, 58999L, 61626L, 62635L, 62694L, 64885L, 
65813L, 70873L, 72200L, 72871L, 75273L, 78741L, 80268L, 82327L, 
83472L, 83766L, 85091L, 85798L, 86067L, 88111L, 92155L, 92219L, 
94552L, 99875L, 100482L, 103289L, 113754L, 115545L, 118843L, 
119607L, 124310L, 124616L, 132381L, 136659L, 137991L, 139533L, 
141248L, 149348L, 163003L, 173218L, 192180L, 194013L, 196945L, 
199321L, 212805L, 217793L, 220425L, 225061L, 232988L, 238888L, 
241729L, 242146L, 251703L, 260169L, 271394L, 284613L, 289526L, 
289897L, 338601L, 344860L, 362856L, 388455L, 388511L, 403107L, 
419655L, 428265L, 434467L, 480636L, 521708L, 524143L, 559438L, 
573410L, 613708L, 620132L, 629497L, 682962L, 736874L, 743064L, 
764587L, 945032L))

invisible("adaptar el código")
invisible("unir bases de datos originales ")
df_estab_tab %>% 
  dplyr::mutate(estab_homo_rec= gsub("^([0-9]{2})([0-9]{3})$", "\\1-\\2", estab)) %>% 
  dplyr::left_join(homolog_estab_misal[,1:2], by=c("estab_homo_rec"="Código Antiguo Establecimiento"))

df_estab_tab %>% 
  dplyr::mutate(estab_homo_rec= gsub("^([0-9]{2})([0-9]{3})$", "\\1-\\2", estab)) %>% 
  dplyr::inner_join(dplyr::mutate(homolog_estab_misal[,1:2], 
      cod_est=stringr::str_sub(`Código nuevo Establecimiento`,2,6)), by=c("estab"="cod_est"))
invisible("No funcionó, encontré 36; 115 no")

invisible("Qué pasa si le quito el primer dígito a Código Establecimiento?")
df_estab_tab %>% 
  dplyr::mutate(estab_homo_rec2= as.numeric(estab)) %>% 
dplyr::inner_join(dplyr::mutate(homolog_estab_misal2, cod_est=
                               stringr::str_sub(`Código Establecimiento`,2,6)), by=c("estab"="cod_est"))
invisible("Sólo uno 8")

invisible("Si uno quitando el último dígito?")
df_estab_tab %>% 
  dplyr::mutate(estab_homo_rec2= as.numeric(estab)) %>% 
  dplyr::inner_join(dplyr::mutate(homolog_estab_misal2, cod_est=
       stringr::str_sub(`Código Establecimiento`,1,5)), by=c("estab"="cod_est")) %>% 
  nrow()-
(df_estab_tab %>% 
  dplyr::mutate(estab_homo_rec2= as.numeric(estab)) %>% 
  dplyr::inner_join(dplyr::mutate(homolog_estab_misal2, cod_est=
                                    stringr::str_sub(`Código Establecimiento`,1,5)), by=c("estab"="cod_est")) %>% 
  distinct(estab) %>% nrow())
invisible("No une 1:1, deja muchos duplicados")

# 2. definir fechas --------------------------------------------------

#función para resumir fechas
sum_dates <- function(x){
  
  cbind.data.frame(
    min= as.Date(min(unclass(as.Date(x)), na.rm=T), origin = "1970-01-01"),
    p001= as.Date(quantile(unclass(as.Date(x)), .001, na.rm=T), origin = "1970-01-01"),
    p005= as.Date(quantile(unclass(as.Date(x)), .005, na.rm=T), origin = "1970-01-01"),
    p025= as.Date(quantile(unclass(as.Date(x)), .025, na.rm=T), origin = "1970-01-01"),
    p25= as.Date(quantile(unclass(as.Date(x)), .25, na.rm=T), origin = "1970-01-01"),
    p50= as.Date(quantile(unclass(as.Date(x)), .5, na.rm=T), origin = "1970-01-01"),
    p75= as.Date(quantile(unclass(as.Date(x)), .75, na.rm=T), origin = "1970-01-01"),
    p975= as.Date(quantile(unclass(as.Date(x)), .975, na.rm=T), origin = "1970-01-01"),
    p995= as.Date(quantile(unclass(as.Date(x)), .995, na.rm=T), origin = "1970-01-01"),
    p999= as.Date(quantile(unclass(as.Date(x)), .999, na.rm=T), origin = "1970-01-01"),
    max= as.Date(max(unclass(as.Date(x)), na.rm=T), origin = "1970-01-01")
  )
}

dtX2023_12_05_DatosEgresosHosp_encrip$fecha_ingreso_rec24 <-
  lubridate::parse_date_time(dtX2023_12_05_DatosEgresosHosp_encrip$fecha_ingreso, orders = c("dmy", "mdy", "Bdy"))
dtX2023_12_05_DatosEgresosHosp_encrip$fecha_egreso_rec24 <- 
  lubridate::parse_date_time(dtX2023_12_05_DatosEgresosHosp_encrip$fecha_egreso, orders = c("dmy", "mdy", "Bdy"))
warnings()

# 
# identical(dtX2023_12_05_DatosEgresosHosp_encrip$fecha_ingreso_rec242 , 
#           dtX2023_12_05_DatosEgresosHosp_encrip$fecha_ingreso_rec24)
#[1] TRUE

# identical(dtX2023_12_05_DatosEgresosHosp_encrip$fecha_egreso_rec242 , 
#           dtX2023_12_05_DatosEgresosHosp_encrip$fecha_egreso_rec24)
# #[1] FALSE
# 
# # Warning message:
# #   6 failed to parse.
dtX2023_12_05_DatosEgresosHosp_encrip$days_elapsed<-
lubridate::time_length(dtX2023_12_05_DatosEgresosHosp_encrip$fecha_egreso_rec24-
                         dtX2023_12_05_DatosEgresosHosp_encrip$fecha_ingreso_rec24, unit="days")
# #       Min.    1st Qu.     Median       Mean    3rd Qu.       Max.       NA's 
# # -20862.000      1.000      3.000      6.325     11.000  19457.000         91 
# 
# paste0("Percentil 1 y 99: ",
#        as.numeric(quantile(dtX2023_12_05_DatosEgresosHosp_encrip$days_elapsed, .01, na.rm=T)),", ",
#        as.numeric(quantile(dtX2023_12_05_DatosEgresosHosp_encrip$days_elapsed, .99, na.rm=T))
#        )
# #[1] "Percentil 1 y 99: -263, 276"
# 
# dtX2023_12_05_DatosEgresosHosp_encrip$days_elapsed2<-
#   lubridate::time_length(dtX2023_12_05_DatosEgresosHosp_encrip$fecha_egreso_rec242-
#                            dtX2023_12_05_DatosEgresosHosp_encrip$fecha_ingreso_rec242, unit="days")
# summary(dtX2023_12_05_DatosEgresosHosp_encrip$days_elapsed2)
# #       Min.    1st Qu.     Median       Mean    3rd Qu.       Max.       NA's
# # -20832.000      1.000      3.000      5.901      5.000  19457.000         91
# # 
# 
# paste0("Percentil 1 y 99: ",
#        as.numeric(quantile(dtX2023_12_05_DatosEgresosHosp_encrip$days_elapsed2, .01, na.rm=T)),", ",
#        as.numeric(quantile(dtX2023_12_05_DatosEgresosHosp_encrip$days_elapsed2, .99, na.rm=T))
# )
# #[1] "Percentil 1 y 99: 0, 49"

invisible("REsultó estar malo la fecha de egreso de Python, al 2024-03-31, pero el de ingreso sí")

invisible("Filtrar la base de datos para dejar fechas realistas de egresos")
invisible("Sacar eventos hospitalarios con tiempos de estadía negativos")
disc_negative_tr_days<-
dtX2023_12_05_DatosEgresosHosp_encrip %>% 
  dplyr::filter(days_elapsed<0) %>% 
  nrow()
disc_negative_tr_days
# 1835

invisible("Generar año de ingreso")
dtX2023_12_05_DatosEgresosHosp_encrip$ing_year<- lubridate::epiyear(dtX2023_12_05_DatosEgresosHosp_encrip$fecha_ingreso_rec24)


invisible("Hacemos el filtro")
dtX2023_12_05_DatosEgresosHosp_encrip_s1<-
  dtX2023_12_05_DatosEgresosHosp_encrip %>% 
  dplyr::filter(days_elapsed>=0) 

cnt_runs_s1<- paste0('Base de datos \n(n= ', 
                     formatC(nrow(dtX2023_12_05_DatosEgresosHosp_encrip_s1), 
                             format='f', big.mark='.', digits=0), 
                     ';\np= ',
                     formatC(length(unique(dtX2023_12_05_DatosEgresosHosp_encrip_s1$run)), 
                             format='f', big.mark='.', digits=0),')')

invisible("Sacar eventos hospitalarios con tiempos de estadía positivos superiores a 1826 días (5 años)")
disc_large_tr_days <-
  dtX2023_12_05_DatosEgresosHosp_encrip_s1 %>% 
  dplyr::filter(days_elapsed>=1827) %>% 
  nrow()
disc_large_tr_days
#[1] 1001


rm("dtX2023_12_05_DatosEgresosHosp_encrip");gc()

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

### 1.0.5 Obtener de ingresos anteriores a 2017 a gente que haya tenido ----
invisible("Obtener de ingresos anteriores a 2017 a gente que haya tenido diagnósticos COC, OH, PSY")

dtX2023_12_05_DatosEgresosHosp_encrip_s1samp<-dtX2023_12_05_DatosEgresosHosp_encrip_s1[1:5e3,]

disc_dates_2017_22_df <-
  dtX2023_12_05_DatosEgresosHosp_encrip_s1%>% 
  dplyr::filter(fecha_ingreso_rec24<"2018-01-01")
disc_dates_2017_22<-nrow(disc_dates_2017_22_df)


invisible("Excluir RUNs que hayan tenido PSY o TUS previa a 2017")
# > table(disc_dates_2017_22_df$alphabet, exclude=NULL)
# coc       cp      mar       oh     otro      psy 
# 6961     1983     1261    39141 13126047    70791 

# coc       cp      mar       oh     otro      psy 
# 377       58       73      294 12952629   292753 

#  coc   cp_psu      mar       oh     otro      psu      psy 
# 6683     4332     1051    37382 12985586      567   210583 

disc_dates_2017_22_df2<-
subset(disc_dates_2017_22_df, subset= alphabet!="otro", select="run") %>% distinct(run)

nrow(disc_dates_2017_22_df2)
#[1] 77948, luego de 2024-04-03: 188198 ; 2024-04-04: 167237

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_


dtX2023_12_05_DatosEgresosHosp_encrip_s2<-
  dtX2023_12_05_DatosEgresosHosp_encrip_s1 %>% 
  dplyr::filter(days_elapsed<1827) 
  
cnt_runs_s2<- paste0('Base de datos \n(n= ', 
                     formatC(nrow(dtX2023_12_05_DatosEgresosHosp_encrip_s2), 
                             format='f', big.mark='.', digits=0), 
                     ';\np= ',
                     formatC(length(unique(dtX2023_12_05_DatosEgresosHosp_encrip_s2$run)), 
                             format='f', big.mark='.', digits=0),')')
  
cnt_runs_s2
# [1] "Base de datos \n(n = 20.954.077;\npacientes: 9.645.652)"

hist(dtX2023_12_05_DatosEgresosHosp_encrip_s2$days_elapsed, breaks=600)


## 2.1. Sacar fechas de ingreso anteriores a 2017 -------------------------------------
invisible("Sacar eventos hospitalarios anteriores a 2017")

invisible("borramos el resto de los objetos")
rm(list = setdiff(ls(), c("cnt_runs_original_bd",
                          "disc_negative_tr_days",
                          "cnt_runs_s1",
                          "disc_large_tr_days",
                          "cnt_runs_s2",
                          "dtX2023_12_05_DatosEgresosHosp_encrip_s2", 
                          "homolog_estab_misal",
                          "disc_dates_2017_22_df2", #agregado 2024-04-01
                          "df_estab_tab")))

disc_dates_2017_22 <-
  dtX2023_12_05_DatosEgresosHosp_encrip_s2%>% 
  dplyr::filter(fecha_ingreso_rec24<"2018-01-01") %>% 
  nrow()
disc_dates_2017_22
#13245183
#[1] fecha mínima "2005-02-23 UTC"

dtX2023_12_05_DatosEgresosHosp_encrip_s3<-
  dtX2023_12_05_DatosEgresosHosp_encrip_s2 %>% 
  dplyr::filter(fecha_ingreso_rec24>="2018-01-01") 
#[1] 7708894

cnt_runs_s3<- paste0('Base de datos \n(n= ', 
                     formatC(nrow(dtX2023_12_05_DatosEgresosHosp_encrip_s3), 
                             format='f', big.mark='.', digits=0), 
                     ';\np= ',
                     formatC(length(unique(dtX2023_12_05_DatosEgresosHosp_encrip_s3$run)), 
                             format='f', big.mark='.', digits=0),')')

invisible("borramos el resto de los objetos (2da parte)")
rm(list = setdiff(ls(), c("cnt_runs_original_bd",
                          "disc_negative_tr_days",
                          "cnt_runs_s1",
                          "disc_large_tr_days",
                          "cnt_runs_s2",
                          "disc_dates_2017_22",
                          "cnt_runs_s3",
                          "dtX2023_12_05_DatosEgresosHosp_encrip_s3", 
                          "homolog_estab_misal", 
                          "disc_dates_2017_22_df2", #agregado 2024-04-01
                          "df_estab_tab")))
#eliminar RAM
gc()

invisible("Recodificar fechas a numérico")
dtX2023_12_05_DatosEgresosHosp_encrip_s3$fecha_ingreso_rec24_num<-
  as.numeric(unclass(as.Date.character(as.character(substr(dtX2023_12_05_DatosEgresosHosp_encrip_s3$fecha_ingreso_rec24, 1, 10)))))
dtX2023_12_05_DatosEgresosHosp_encrip_s3$fecha_egreso_rec24_num<-
  as.numeric(unclass(as.Date.character(as.character(substr(dtX2023_12_05_DatosEgresosHosp_encrip_s3$fecha_egreso_rec24, 1, 10)))))
invisible("Recodificar fechas a Date (no tengo horas y segundos)")
dtX2023_12_05_DatosEgresosHosp_encrip_s3$fecha_ingreso_rec24<-
  as.Date.character(as.character(substr(dtX2023_12_05_DatosEgresosHosp_encrip_s3$fecha_ingreso_rec24, 1, 10)))
dtX2023_12_05_DatosEgresosHosp_encrip_s3$fecha_egreso_rec24<-
  as.Date.character(as.character(substr(dtX2023_12_05_DatosEgresosHosp_encrip_s3$fecha_egreso_rec24, 1, 10)))

invisible("Dividir en secciones")
dtX2023_12_05_DatosEgresosHosp_encrip_s3$fecha_ingreso_rec24_cut<-
  cut(dtX2023_12_05_DatosEgresosHosp_encrip_s3$fecha_ingreso_rec24_num, 
      breaks= 30, dig.lab = 4, ordered_result= T)
dtX2023_12_05_DatosEgresosHosp_encrip_s3$fecha_egreso_rec24_cut<-
  cut(dtX2023_12_05_DatosEgresosHosp_encrip_s3$fecha_egreso_rec24_num, 
      breaks= 30, dig.lab = 4, ordered_result= T)

labs <- levels(dtX2023_12_05_DatosEgresosHosp_encrip_s3$fecha_ingreso_rec24_cut)
labs_date<- paste0(as.Date(as.numeric( sub("\\((.+),.*", "\\1", labs)), origin = "1970-01-01"),";\n",
                   as.Date(as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs)), origin = "1970-01-01"))
labs2 <- levels(dtX2023_12_05_DatosEgresosHosp_encrip_s3$fecha_egreso_rec24_cut)
labs2_date<- paste0(as.Date(as.numeric( sub("\\((.+),.*", "\\1", labs2)), origin = "1970-01-01"),";\n",
                    as.Date(as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs2)), origin = "1970-01-01"))

dtX2023_12_05_DatosEgresosHosp_encrip_s3 %>% 
  dplyr::mutate(fecha_egreso_rec24_cut= factor(fecha_egreso_rec24_cut, labels=labs2_date)) %>% 
  dplyr::mutate(fecha_ingreso_rec24_cut= factor(fecha_ingreso_rec24_cut, labels=labs2_date)) %>% 
ggplot( aes(x = fecha_egreso_rec24_cut)) +
  geom_bar(fill = "steelblue", color = "white") +
  xlab("Categoría de fechas de egreso") +
  ylab("Frecuencia") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


dtX2023_12_05_DatosEgresosHosp_encrip_s3 %>% 
  dplyr::mutate(fecha_egreso_rec24_cut= factor(fecha_egreso_rec24_cut, labels=labs2_date)) %>% 
  dplyr::mutate(fecha_ingreso_rec24_cut= factor(fecha_ingreso_rec24_cut, labels=labs2_date)) %>% 
  ggplot( aes(x = fecha_ingreso_rec24_cut)) +
  geom_bar(fill = "steelblue", color = "white") +
  xlab("Categoría de fechas de ingreso") +
  ylab("Frecuencia") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# 3. otros datos ----------------------------------------------------------

## 3.1 corregir CIE-10 ----------------------------------------------------------

for(i in 1:11){
  dtX2023_12_05_DatosEgresosHosp_encrip_s3[[paste0("diag",i)]]<-
    ifelse(dtX2023_12_05_DatosEgresosHosp_encrip_s3[[paste0("diag",i)]]=="None",
           NA,
           dtX2023_12_05_DatosEgresosHosp_encrip_s3[[paste0("diag",i)]])
}

## 3.2 filtrar registros con RUNs erróneos ----------------------------------------------------------

invisible("Se eliminan RUNs erróneos: 1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464")

disc_run_erroneos<-
dtX2023_12_05_DatosEgresosHosp_encrip_s3 %>% 
  dplyr::filter(run =="1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464") %>% 
  nrow()
disc_run_erroneos
#[1] 55730
  
dtX2023_12_05_DatosEgresosHosp_encrip_s4<-
  dtX2023_12_05_DatosEgresosHosp_encrip_s3 %>% 
  dplyr::mutate(fecha_egreso_rec24_cut= factor(fecha_egreso_rec24_cut, labels=labs2_date)) %>% 
  dplyr::mutate(fecha_ingreso_rec24_cut= factor(fecha_ingreso_rec24_cut, labels=labs2_date)) %>% 
  dplyr::filter(!run =="1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464")

## 3.3 explorar categorías criterios pertenencia PPOO ----------------------------------------------------------

invisible("Recodificar la variable para tener un marcador binario")
table(dtX2023_12_05_DatosEgresosHosp_encrip_s4$glosa_pueblo_originario, 
  dtX2023_12_05_DatosEgresosHosp_encrip_s4$pueblo_originario, exclude=NULL) %>% 
  data.frame() %>% 
  dplyr::filter(Freq>0)
#                       Var1 Var2    Freq
# 1                  MAPUCHE    1  120878
# 2                   AYMARA    2   10128
# 3     RAPA NUI (PASCUENSE)    3    3896
# 4  LICAN ANTAI (ATACAMEÑO)    4    1182
# 5                  QUECHUA    5     855
# 6                    COLLA    6    1323
# 7                 DIAGUITA    7    2087
# 8                 KAWÉSQAR    8    1709
# 9           YAGÁN (YÁMANA)    9    1181
# 10      OTRO (ESPECIFICAR)   10    2513
# 11                 NINGUNO   96 7507412

dtX2023_12_05_DatosEgresosHosp_encrip_s4$pueblo_originario_rec<- 
  ifelse(dtX2023_12_05_DatosEgresosHosp_encrip_s4$pueblo_originario!=96,1,0)

invisible("Recodifico RSH para descartar perdidos y dejarlos como 0")

table(dtX2023_12_05_DatosEgresosHosp_encrip_s4$rsh, exclude=NULL)
#      0       1    <NA> 
# 128108  517470 7007586

dtX2023_12_05_DatosEgresosHosp_encrip_s4$rsh_rec<- 
  ifelse(is.na(dtX2023_12_05_DatosEgresosHosp_encrip_s4$rsh),0,dtX2023_12_05_DatosEgresosHosp_encrip_s4$rsh)

table(dtX2023_12_05_DatosEgresosHosp_encrip_s4 $cya_conadi,
      dtX2023_12_05_DatosEgresosHosp_encrip_s4 $ci_conadi, exclude=NULL)
#            0       1    <NA>
# 0     271833  273464       0
# 1      53586   46695       0
# <NA>       0       0 7007586
#lo mismo estos: mucho perdido

# Autodeclaración en el Registro Social de Hogares (variable RSH), 
# el Registro de Calidades Indígenas de Conadi (variable CI_Conadi) 
# y el Registro de Comunidades y Asociaciones Indígenas (variable CyA_Conadi). 

invisible("generamos un criterio inclusivo")
dtX2023_12_05_DatosEgresosHosp_encrip_s4$inclusivo_rec<-
  ifelse(rowSums(dtX2023_12_05_DatosEgresosHosp_encrip_s4[,c("cya_conadi","ci_conadi")], na.rm=T)>0,1,0)
#"rsh","pueblo_originario_rec"

table(dtX2023_12_05_DatosEgresosHosp_encrip_s4$inclusivo_rec)
#       0       1 
# 7279419  373745 

dtX2023_12_05_DatosEgresosHosp_encrip_s4$inclusivo_rec<-
  ifelse(rowSums(dtX2023_12_05_DatosEgresosHosp_encrip_s4[,c("cya_conadi","ci_conadi")], na.rm=T)>0,1,0)


table(dtX2023_12_05_DatosEgresosHosp_encrip_s4$glosa_pueblo_originario, 
      dtX2023_12_05_DatosEgresosHosp_encrip_s4$inclusivo_rec, exclude=NULL) %>% 
  data.frame() %>% 
  dplyr::filter(Freq>0)
#                       Var1 Var2    Freq
# 1                   AYMARA    0    6101
# 2                    COLLA    0     918
# 3                 DIAGUITA    0    1522
# 4                 KAWÉSQAR    0    1632
# 5  LICAN ANTAI (ATACAMEÑO)    0     694
# 6                  MAPUCHE    0   62332
# 7                  NINGUNO    0 7200516
# 8       OTRO (ESPECIFICAR)    0    2160
# 9                  QUECHUA    0     625
# 10    RAPA NUI (PASCUENSE)    0    1789
# 11          YAGÁN (YÁMANA)    0    1130
# 12                  AYMARA    1    4027
# 13                   COLLA    1     405
# 14                DIAGUITA    1     565
# 15                KAWÉSQAR    1      77
# 16 LICAN ANTAI (ATACAMEÑO)    1     488
# 17                 MAPUCHE    1   58546
# 18                 NINGUNO    1  306896
# 19      OTRO (ESPECIFICAR)    1     353
# 20                 QUECHUA    1     230
# 21    RAPA NUI (PASCUENSE)    1    2107
# 22          YAGÁN (YÁMANA)    1      51

## 3.5 filtrar país de origen == Chile ----------------------------------------------------------

invisible("Limpiar memoria")
gc()

dtX2023_12_05_DatosEgresosHosp_encrip_s5<-
dtX2023_12_05_DatosEgresosHosp_encrip_s4  %>% 
  dplyr::mutate(nac_chile= ifelse(grepl("CHILE", glosa_pais_origen),1,0)) %>% 
  dplyr::group_by(run) %>% 
  dplyr::mutate(perc_nac_cl= sum(nac_chile)/n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(perc_nac_cl==1) 

gc()
invisible("Limpiar memoria")
result <- sqldf("SELECT COUNT(*) as nrow_count
                 FROM dtX2023_12_05_DatosEgresosHosp_encrip_s4
                 WHERE row_num_index NOT IN (
                   SELECT DISTINCT row_num_index
                   FROM dtX2023_12_05_DatosEgresosHosp_encrip_s5
                 )")
result
#   nrow_count
# 1     296820

result2 <- sqldf("SELECT COUNT(DISTINCT run) as unique_count
                 FROM (
                   SELECT run,
                          SUM(CASE WHEN INSTR(glosa_pais_origen, 'CHILE') > 0 THEN 1 ELSE 0 END) as nac_chile,
                          COUNT(*) as total
                   FROM dtX2023_12_05_DatosEgresosHosp_encrip_s4
                   GROUP BY run
                 ) 
                 WHERE (nac_chile * 1.0 / total) < 1")

# Print the count of rows
print(result2)
#   unique_count
# 1       194339

disc_nac_nocl<- paste0('(n= ', formatC(as.numeric(result), format='f', big.mark='.', digits=0), 
                     ' p= ',formatC(as.numeric(result2), format='f', big.mark='.', digits=0),')')



cnt_runs_s5<- paste0('Base de datos \n(n= ', 
                     formatC(nrow(dtX2023_12_05_DatosEgresosHosp_encrip_s5), 
                             format='f', big.mark='.', digits=0), 
                     ';\np= ',
                     formatC(length(unique(dtX2023_12_05_DatosEgresosHosp_encrip_s5$run)), 
                             format='f', big.mark='.', digits=0),')')

cnt_runs_s5
#[1] "Base de datos \n(n= 7.356.344;\np= 4.614.409)"

## 3.6 filtrar POR EDAD EN años 2018 & PPOO----------------------------------------------------------

gc()

invisible("Generar año de ingreso")
dtX2023_12_05_DatosEgresosHosp_encrip_s5$ing_year<- lubridate::epiyear(dtX2023_12_05_DatosEgresosHosp_encrip_s5$fecha_ingreso_rec24)


table(dtX2023_12_05_DatosEgresosHosp_encrip_s5$pueblo_originario_rec, 
      dtX2023_12_05_DatosEgresosHosp_encrip_s5$inclusivo_rec, 
      dtX2023_12_05_DatosEgresosHosp_encrip_s5$rsh_rec, exclude=NULL) %>% 
  data.frame() %>% 
  dplyr::filter(Freq>0) %>% dput()

ppoo_conadi_rsh<-
cbind.data.frame(Var1 = structure(c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 
2L), .Label = c("0", "1"), class = "factor"), Var2 = structure(c(1L, 
1L, 2L, 2L, 1L, 1L, 2L, 2L), .Label = c("0", "1"), class = "factor"), 
    Var3 = structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L), .Label = c("0", 
    "1"), class = "factor"), Freq = c(6675975L, 42066L, 108935L, 
    18664L, 231312L, 34717L, 196782L, 47893L))
#   pueblo_originario_rec inclusivo_rec rsh_rec    Freq
# 1    0                    0             0       6675975
# 2    1                    0             0       42066
# 3    0                    1             0       108935
# 4    1                    1             0       18664
# 5    0                    0             1       231312
# 6    1                    0             1       34717
# 7    0                    1             1       196782
# 8    1                    1             1       47893

invisible("Lista de usuarios en criterio inclusivo")
hash_ppoo_conadi<-
subset(dtX2023_12_05_DatosEgresosHosp_encrip_s5, inclusivo_rec==1, run)

hash_no_ppoo<-
  subset(dtX2023_12_05_DatosEgresosHosp_encrip_s5, inclusivo_rec== 0 & pueblo_originario_rec==0 & rsh_rec==0, run)

hash_no_ppo_flex<-
  subset(dtX2023_12_05_DatosEgresosHosp_encrip_s5, inclusivo_rec==0, run)


invisible("Number of users")
length(hash_ppoo_conadi$run)
#[1] 372274

invisible("Number of treatment episodes")
dtX2023_12_05_DatosEgresosHosp_encrip_s5 %>% 
  dplyr::filter(run %in% hash_ppoo_conadi$run) %>% 
  nrow()
#[1] 372274

invisible("Number of users")
length(hash_no_ppoo$run)
#[1] 6675975

invisible("Number of treatment episodes")
dtX2023_12_05_DatosEgresosHosp_encrip_s5 %>% 
  dplyr::filter(run %in% hash_no_ppoo$run) %>% 
  nrow()
#[1] 6694352


### 3.6a filtrar ttos. base ----------------------------------------------------------

invisible("ver cond egreso")
#  dplyr::arrange(run, fecha_ingreso_rec24) %>% 
# # Print the table of conditional discharge frequencies
# print(
#   pd.concat([
#     pd.crosstab(X20231205_DatosEgresosHosp_encrip.cond_egr, columns="count"),
#     pd.crosstab(X20231205_DatosEgresosHosp_encrip.cond_egr, columns="count", normalize='all')
#   ], axis=1)
# )
# # col_0        count     count
# # cond_egr                    
# # 1         20475745  0.977036
# # 2           481259  0.022964

table(dtX2023_12_05_DatosEgresosHosp_encrip_s5$cond_egr, exclude=NULL)
#       1       2 
# 7157753  198591 


invisible("al 2018, 15-29 años, pero ahora sin gente con complicaciones previas")
dtX2023_12_05_DatosEgresosHosp_encrip_s5a<-
dtX2023_12_05_DatosEgresosHosp_encrip_s5 %>% 
dplyr::group_by(run) %>% 
  dplyr::slice(1) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(
    dplyr::case_when(ing_year==2018 & grepl("Años",glosa_tipo_edad) & edad_anos>=15 & edad_anos<=29 & alphabet!="otro"~TRUE,TRUE~FALSE)
  ) %>%
  #2024-04-02: eliminar casos con complicaciones previas al 2018
  dplyr::filter(!run %in% disc_dates_2017_22_df2$run) %>% 
  dplyr::mutate(subsample= dplyr::case_when(inclusivo_rec==1~1,inclusivo_rec== 0 & pueblo_originario_rec==0 & rsh_rec==0~0, T~NA_real_)) %>% 
  dplyr::filter(cond_egr==1)

length(unique(dtX2023_12_05_DatosEgresosHosp_encrip_s5a$run))
#[1] 6070, sin muertos 6061
require(readr)
filtered_df_csv <- readr::read_csv("filtered_df.csv.gz", col_types = cols(...1 = col_skip()))

invisible("Incorporar una variable que indique si la base de datos desde 2010 indica pertenecer a PPOO por MINSAL o RSH al menos una vez")
dtX2023_12_05_DatosEgresosHosp_encrip_s5a$ppoo_minsal_y_rsh_2010 <- 
  ifelse(dtX2023_12_05_DatosEgresosHosp_encrip_s5a$run %in%  filtered_df_csv$run,1,0)
# table(dtX2023_12_05_DatosEgresosHosp_encrip_s5a$ppoo_minsal_y_rsh_2010)
#    0    1 
# 5014 1056  

dtX2023_12_05_DatosEgresosHosp_encrip_s5a$inclusivo_real_historico <- 
  ifelse(dtX2023_12_05_DatosEgresosHosp_encrip_s5a$ppoo_minsal_y_rsh_2010>0 | 
           dtX2023_12_05_DatosEgresosHosp_encrip_s5a$inclusivo_rec>0,1,0)

table(dtX2023_12_05_DatosEgresosHosp_encrip_s5a$inclusivo_real_historico )
#    0    1 
# 4881 1180 

invisible("15 a 24 años al 2018")
dtX2023_12_05_DatosEgresosHosp_encrip_s5b<-
  dtX2023_12_05_DatosEgresosHosp_encrip_s5 %>% 
  dplyr::group_by(run) %>% 
  dplyr::slice(1) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(
    dplyr::case_when(ing_year==2018 & grepl("Años",glosa_tipo_edad) & edad_anos>=15 & edad_anos<=24 & alphabet!="otro"~TRUE,TRUE~FALSE)
  ) %>%
  #2024-04-02: eliminar casos con complicaciones previas al 2018
  dplyr::filter(!run %in% disc_dates_2017_22_df2$run) %>% 
  dplyr::mutate(subsample= dplyr::case_when(inclusivo_rec==1~1,inclusivo_rec== 0 & pueblo_originario_rec==0 & rsh_rec==0~0, T~NA_real_)) %>% 
  dplyr::filter(cond_egr==1)

dtX2023_12_05_DatosEgresosHosp_encrip_s5b$ppoo_minsal_y_rsh_2010 <- 
  ifelse(dtX2023_12_05_DatosEgresosHosp_encrip_s5b$run %in%  filtered_df_csv$run,1,0)
dtX2023_12_05_DatosEgresosHosp_encrip_s5b$inclusivo_real_historico <- 
  ifelse(dtX2023_12_05_DatosEgresosHosp_encrip_s5b$ppoo_minsal_y_rsh_2010>0 | 
           dtX2023_12_05_DatosEgresosHosp_encrip_s5b$inclusivo_rec>0,1,0)

table(dtX2023_12_05_DatosEgresosHosp_encrip_s5b$inclusivo_real_historico )
#    0    1 
# 3697  891 

# dtX2023_12_05_DatosEgresosHosp_encrip_s5 %>% 
#   +     dplyr::filter(run %in% dtX2023_12_05_DatosEgresosHosp_encrip_s5b$run) %>% nrow()
# [1] 9932

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

invisible("Cuántos son los pertenecientes a PPOO (solo CONADI) para cada base?")
table(dtX2023_12_05_DatosEgresosHosp_encrip_s5a$subsample, exclude=NULL)
#    0    1 <NA> 
# 5201  505  355 

invisible("Cuántos son los pertenecientes a PPOO (criterio inclusivo e histórico) para cada base?")
table(dtX2023_12_05_DatosEgresosHosp_encrip_s5a$inclusivo_real_historico)
#    0    1 
# 4881 1180 

invisible("Porcentajes de pertenecientes a PPOO (solo CONADI) [antes de filtro]")
table(dtX2023_12_05_DatosEgresosHosp_encrip_s4$inclusivo_rec)
#      0       1 
# 7279419  373745 
373745/(7279419+373745)
#[1] 0.04883536

invisible("Porcentajes de pertenecientes a PPOO (solo CONADI) [después de filtro]")
table(dtX2023_12_05_DatosEgresosHosp_encrip_s5a$inclusivo_rec)
#    0    1 
# 5556  505
505/(5556+505)
#[1] 0.08331958

invisible("Porcentajes de pertenecientes a PPOO (inclusivo e histórico) [después de filtro]")
table(dtX2023_12_05_DatosEgresosHosp_encrip_s5a$inclusivo_real_historico)
#    0    1 
# 4881 1180
1180/(4881+1180)
#[1] 0.1946873

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
invisible("Estandarizar algunas variables")
dtX2023_12_05_DatosEgresosHosp_encrip_s5a$estab_homo_fac<- 
  factor(dtX2023_12_05_DatosEgresosHosp_encrip_s5a$estab_homo)

dtX2023_12_05_DatosEgresosHosp_encrip_s5a$benef_rec<- 
  ifelse(dtX2023_12_05_DatosEgresosHosp_encrip_s5a$glosa_prevision!="FONASA",
         "NA", dtX2023_12_05_DatosEgresosHosp_encrip_s5a$benef)


### 3.6.0 alternativa ----------------------------------------------------------

#### 3.6.0 primera ----------------------------------------------------------

invisible("Hacer las variables que encuentran la presnencia de comorbilidad o TUS o ambas")


#'Trastornos mentales orgánicos, incluidos los sintomáticos(F00-09)', 
#'trastorno esquizotípico y trastornos de ideas delirantes(F20-29),
#'Trastornos del humor (afectivos)(F30-39)', 
#'Trastornos neuróticos, secundarios a situaciones estresantes y somatomorfos(F40-49)', 
#''Trastornos del comportamiento asociados a disfunciones fisiológicas y a factores somáticos(F50-59)'
#''Trastornos de la personalidad y del comportamiento del adulto(F60-69)', 
#'Retraso Mental(F70-79)', 
#''Trastornos del Desarrollo Psicológico(F80-89)' , '
#'#'Trs. del comportamiento y de las emociones de comienzo habitual en la infancia y adolescencia(F90-98)'.

psych_comorbidity_pattern <- "F0[0-9]|F2[0-9]|F3[0-9]|F4[0-9]|F5[0-9]|F6[0-9]|F7[0-9]|F8[0-9]|F9[0-8]"

invisible("Make the variables to detect them")
dtX2023_12_05_DatosEgresosHosp_encrip_s5a2<-
  dtX2023_12_05_DatosEgresosHosp_encrip_s5a %>%
  # dplyr::mutate(across(
  #   .cols = diag1:diag11,
  #   .fns = ~ dplyr::case_when(grepl("F14|R782|T405", .x)~"coc",
  #                             grepl("F12|T407", .x)~"mar",
  #                             grepl("F10|T510",.x)~"oh",
  #                             grepl(psych_comorbidity_pattern, .x)~"psy",
  #                             T~""),
  #   .names = "{.col}_rec3")) %>%
  rowwise() %>%
  dplyr::mutate(
    sum_occurrences = sum(c_across(ends_with("rec3")) %in% c("coc", "mar", "oh"), na.rm = TRUE),
    recoded = as.integer(sum_occurrences > 0)
  ) %>%
  dplyr::mutate(
    sum_occurrences2 = sum(c_across(ends_with("rec3")) %in% c("coc", "mar", "oh","psy"), na.rm = TRUE),
    recoded2 = as.integer(sum_occurrences2 > 0)
  ) %>%
  ungroup()

invisible("Sólo por drogas, 169 pacientes con ingresos por drogas a la base y pertenecientes a PPOO [inclusivo real] que serán seguidos por 4 años")
table(dtX2023_12_05_DatosEgresosHosp_encrip_s5a2$recoded, dtX2023_12_05_DatosEgresosHosp_encrip_s5a2$inclusivo_real_historico)
#      0    1
  # 0 4178 1004
  # 1  703  176

paste0("Gente perteneciente a PPOO de la submuestra que ingresa por COC, OH o MAR: ",
scales::percent(176/nrow(dtX2023_12_05_DatosEgresosHosp_encrip_s5a2),.1)
)
#"2.9%"


#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
invisible("Sacar los que están en TUS y PSY a la base. Tienen a futuro alguna complicación??")
invisible("c('coc', 'mar', 'oh','psy')")

dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl<-
  dtX2023_12_05_DatosEgresosHosp_encrip_s5 %>% 
  dplyr::filter(run %in% dtX2023_12_05_DatosEgresosHosp_encrip_s5a2$run) %>% 
  dplyr::arrange(run, fecha_ingreso_rec24) %>% 
  dplyr::group_by(run) %>% 
  dplyr::mutate(rn= row_number()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(across(
    .cols = diag1:diag11,
    .fns = ~ dplyr::case_when(grepl("F14|R782|T405", .x)~"coc",
                              grepl("F12|T407", .x)~"mar",
                              grepl("F10|T510",.x)~"oh",
                              grepl(psych_comorbidity_pattern, .x)~"psy",
                              T~""),
    .names = "{.col}_rec32")) %>%
#2024-06-14: autoinflinjidas
  dplyr::mutate(across(
    .cols = diag1:diag11,
    .fns = ~ dplyr::case_when(grepl("X6[0-9]|X7[0-9]|X8[0-4]", .x)~"lesion",
                              T~""),
    .names = "{.col}_rec42")) %>%
  rowwise() %>%
  dplyr::mutate(
    sum_tus2    = sum(c_across(ends_with("rec32")) %in% c("coc", "mar", "oh"), na.rm = TRUE),
    sum_coc2           = sum(c_across(ends_with("rec32")) %in% c("coc"), na.rm = TRUE),
    sum_mar2           = sum(c_across(ends_with("rec32")) %in% c("mar"), na.rm = TRUE),
    sum_oh2            = sum(c_across(ends_with("rec32")) %in% c("oh"), na.rm = TRUE),
    sum_tus_psy2       = sum(c_across(ends_with("rec32")) %in% c("coc", "mar", "oh","psy"), na.rm = TRUE),
    sum_conadi2        = sum(c_across(ends_with("_conadi")), na.rm = TRUE),
    recoded_rec        = as.integer(sum_tus > 0),
    recoded2_rec       = as.integer(sum_tus_psy > 0),
    ppo_conadi2        = as.integer(sum_conadi2 > 0),
    sum_autolesiones       = sum(c_across(ends_with("rec42")) %in% c("lesion"), na.rm = TRUE)
  ) %>%
  dplyr::mutate(alphabet  = dplyr::case_when(sum_coc2>0 & sum_coc2==sum_tus2 & sum_tus2==sum_tus_psy2~ "coc",
                                             sum_mar2>0 & sum_mar2==sum_tus2 & sum_tus2==sum_tus_psy2~ "mar",
                                             sum_oh2>0 & sum_oh2==sum_tus2 & sum_tus2==sum_tus_psy2~ "oh",
                                             ((sum_coc2 > 0) + (sum_mar2 > 0) + (sum_oh2 > 0)) > 1 & sum_tus2==sum_tus_psy2~ "psu",
                                             ((sum_coc2 > 0) + (sum_mar2 > 0) + (sum_oh2 > 0)) == 1 & sum_tus_psy2>sum_tus2~ "cp",
                                             ((sum_coc2 > 0) + (sum_mar2 > 0) + (sum_oh2 > 0)) > 1 & sum_tus_psy2>sum_tus2~ "cp_psu",
                                             sum_tus_psy2>0 & sum_tus2==0~ "psy",
                                             T~"otro")) %>% 
  dplyr::ungroup() %>% 
  #2024-07-04: sacar eventos del 2021
  dplyr::filter(ing_year<2022)
  
  
invisible("13,025 trayectorias de personas que cursaron")

dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl$ppoo_minsal_y_rsh_2010 <- 
  ifelse(dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl$run %in%  filtered_df_csv$run,1,0)

dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl$inclusivo_real_historico <- 
  ifelse(dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl$ppoo_minsal_y_rsh_2010>0 | 
           dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl$ppo_conadi2>0,1,0)


#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
invisible("Explorar")
dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl %>% 
  dplyr::select(run, rn, sum_coc, sum_coc2, sum_mar, sum_mar2, sum_oh, sum_oh2, sum_tus, sum_tus2, sum_tus_psy, alphabet) %>% 
  sample_n(size = 30)

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

invisible("gente con psiquiátrico o TUS al ingreso, igual hay gente que evoluciona a SUD")
table(dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl$recoded,
      dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl$ppo_conadi)
#       0     1
# 0 10858   981
# 1  1057   129

invisible("Sólo por drogas, 823 pacientes que serán seguidos por 4 años")
table(dtX2023_12_05_DatosEgresosHosp_encrip_s5a2$subsample,dtX2023_12_05_DatosEgresosHosp_encrip_s5a2$recoded)
  #      0    1
  # 0 4458  743
  # 1  425   80

paste0("Gente perteneciente a pueblos originarios de la submuestra que ingresa por COC, OH o MAR: ",
       scales::percent(80/(743+80),.1))
#[1] "Gente perteneciente a pueblos originarios de la submuestra que ingresa por COC, OH o MAR: 9.7%"


invisible("Más no-PPOO con eventos posteriores")

invisible("gente con psiquiátrico o TUS al ingreso, ")
table(dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl$recoded2,
      dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl$ppo_conadi)
#      0    1
  # 0 2954  287
  # 1 8961  823
invisible("Increible la cantidad de eventos psiquiatricos y TUS que tienen los PPOO COnadi (x2.9)")

table(dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl$recoded2,
      dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl$inclusivo_real_historico)
#      0    1
# 0 2515  726
# 1 7804 1980

invisible("Increible la cantidad de eventos psiquiatricos y TUS que tienen los PPOO COnadi (x2.7)")


##### 3.0.1 unión con mortalidad ----------------------------------------------------------

library(readr)
def_enc17_21 <- read_delim("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/_output/def_enc.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)

dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl %>% 
  dplyr::filter(run %in% unique(c(def_enc17_21$RUN))) %>% 
  nrow()
# 180

cnt_runs_mort<-
  paste0("Usuarios emparejados\n(fallecidos registrados)\n(p= ",
  formatC(
    dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl %>% 
      dplyr::filter(run %in% unique(c(def_enc17_21$RUN))) %>% 
      distinct(run) %>% nrow(), format='f', big.mark='.', digits=0),")")

paste0("Porcentaje de personas que mueren en los registros de mortalidad: ",
scales::percent(
dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl %>% 
  dplyr::filter(run %in% unique(c(def_enc17_21$RUN))) %>% 
  nrow()/length(unique(dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl$run))
)
)
#[1] "Porcentaje de personas que mueren en los registros de mortalidad: 3%"

invisible("Ver si los que mueren internamente también lo hacen en la base de hospitalizaciones")
filter(dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl,cond_egr==2) %>% distinct(run) %>% nrow()
#19

dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl %>% 
  dplyr::filter(run %in% unique(c(def_enc17_21$RUN))) %>% 
  dplyr::filter(cond_egr==2) %>% nrow()
#[1] 12
invisible("12 personas tienen cond. de egreso con alta ")
invisible("intrahospitalaria de aquellos RUNs con defunciones, de 19")

invisible("Hay 7 muertos que no aparecen en las bases hasta el 2021, posiblemente del 2022")

# 4. diseñar base ----------------------------------------------------------

invisible("Seleccionamos la primera obs. de los runs válidos")
invisible("elegir franajas de edad, la que sea menos populosa")

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
invisible("definir bases de datos")
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

cnt_runs_s5_adultl<- paste0('Base de datos final \n(n= ', 
                     formatC(nrow(dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl), 
                             format='f', big.mark='.', digits=0), 
                     ';\np= ',
                     formatC(length(unique(dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl$run)), 
                             format='f', big.mark='.', digits=0),')')

cnt_runs_s5_adultl

jpeg("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_ii/_figs/edades_hist_post_ib_SM.jpg", width=8, height = 6, units="in", res=600)
hist(dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl %>% 
       dplyr::group_by(run) %>% 
       dplyr::mutate(n=n()) %>% 
       dplyr::ungroup() %>% 
       dplyr::pull(n),
     breaks = "Sturges", # Or try "Sturges", "Scott", "FD" for automatic calculation
     col = "skyblue",
     border = "white",
    # ylim = c(0, 35e3),
     #xlim = c(0, 60),
     main = "Histograma de muestra, por edades (15-29)",
     xlab = "Número de hospitalizaciones",
     ylab = "Frecuencia",
     freq = TRUE) # Change to FALSE to plot densities instead of frequencies

dev.off()


summary(dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl %>% 
          dplyr::group_by(run) %>% 
          dplyr::mutate(n=n()) %>% 
          dplyr::ungroup() %>% 
          dplyr::pull(n))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   2.000   3.489   4.000  36.000

invisible("percentil 95, 7  hosp")
quantile(dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl %>% 
           dplyr::group_by(run) %>% 
           dplyr::mutate(n=n()) %>% 
           dplyr::ungroup() %>% 
           dplyr::pull(n),.95)
# 95% 
# 10

#https://www.minsal.cl/wp-content/uploads/2015/10/MATRIZ-DE-CUIDADOS-A-LO-LARGO-DEL-CURSO-DE-VIDA.pdf
# Por otro lado, se reconoce que los jóvenes (20 y 24 años) que aún estudian y están bajo la dependencia de sus padres, no sólo son considerados adoles
# centes según definición de adolescencia basada en que no alcanzan la autonomía social total o parcial, sino que sus demandas y necesidades de salud 
# no son diferentes a las del grupo de 10 a 19 años. Por esta razón, conforme a criterios OMS-OPS, el Programa Nacional de Adolescentes y Jóvenes, con
# sidera la población de 20-24 años como población joven y realiza recomendaciones de manera que toda la gente joven (10 a 24 años) pueda tener un 
# desarrollo en un contexto favorable de apoyo y protección


nrow(dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl ) #FINAL
#[1] 11957

invisible("En ninguno de los dos hay opc 3 en sexo; ~700 casos rel. con SUD; ")

invisible("No es realista clasificar por sexo")

invisible("Nivel de Cuidado del cual egresó el paciente (Niveles de Cuidado)")


# 5. unir a defunciones --------------------------------------------------
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

library(readr)
def_enc17_21 <- read_delim("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/_output/def_enc.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
nac_enc17_22 <- read_delim("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/_output/nac_enc.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)

#Keen, C., Kinner, S. A., Young, J. T., Jang, K., Gan, W., Samji, H., Zhao, B., Krausz, M., & Slaunwhite, A. (2022). Prevalence of co-occurring mental illness and substance use disorder and association with overdose: A linked data cohort study among residents of British Columbia, Canada. Addiction, 117(1), 129-140. https://doi.org/10.1111/add.15580
#Vital statistics	ICD-10 code of T40.0, T40.1, T40.3, T40.4, T40.6

def_enc17_21_T<-
  def_enc17_21 %>% dplyr::mutate(across(
    .cols = DIAG1:DIAG2,
    .fns = ~ dplyr::case_when(grepl("T400|T401|T403|T404|T406", .x)~1,T~0),
    .names = "{.col}_od"))

invisible("Make the variables to detect them")
def_enc17_21<-
  def_enc17_21 %>%
  rowwise() %>%
  dplyr::mutate(
    ods = sum(c_across(ends_with("_od")), na.rm = TRUE)) %>% 
  ungroup()
invisible("2024-04-03: sólo 9 casos con sobredosis")

invisible("Si agrego la base del proyecto nDP, no calzan los RUNs con las especificaciones del fallecimiento, no son compatibles")

invisible("Guardar imagen")
#save.image("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/20240320.RData")
save.image("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/20240405.RData")
