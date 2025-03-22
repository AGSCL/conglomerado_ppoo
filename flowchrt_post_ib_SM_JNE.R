
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


# rm(list = setdiff(ls(), c("cnt_runs_original_bd",
#                           "disc_negative_tr_days",
#                           "cnt_runs_s1",
#                           "disc_large_tr_days",
#                           "cnt_runs_s2",
#                           "dtX2023_12_05_DatosEgresosHosp_encrip_s3", 
#                           "disc_dates_2017",
#                           "cnt_runs_s3",
#                           "homolog_estab_misal", 
#                           "df_estab_tab")))

invisible("Cargar imagenes: necesito las dos")

load("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/20240405.RData")
load("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/20240903.RData")

# 99. diagrama de flujo ---------------------------------------------------


#Siguiendo un criterio inclusivo, se seleccionará a personas que registren una autodeclaración en 
#el Registro Social de Hogares (variable “RSH”), el Registro de Calidades Indígenas de Conadi 
#(variable “CI_Conadi”), en el Registro de Comunidades y Asociaciones Indígenas (variable 
#“CyA_Conadi”) o reporten pertenecer a un pueblo originario en algún episodio hospitalario 
#(variable “PUEBLO ORIGINARIO”). De este subgrupo, se descartan RUNs erróneos 
#(“1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464”), ciertos valores 
#inconsistentes en las fechas de atención de ingresos hospitalarios (i.e., tiempos de 
#atención negativos o superiores a 5 años), personas que no posean nacionalidad Chilena en 
#alguno de los registros, y que haya registrado un episodio hospitalario en el año 2018 y 
#presente edades distintas al rango de 20 a 24 años de edad. Los pacientes seleccionados 
#y sus subsecuentes registros de episodios hospitalarios fueron vinculados con datos 
#de mortalidad del 2017 al 2021.

tab1_lab<-    cnt_runs_original_bd

#cnt_runs_s1

tab2a1_lab<-  paste0("Episodios con tiempos de hospitalización negativos (n= ", formatC(disc_negative_tr_days, format='f', big.mark='.', digits=0),")")

tab2a2_lab<-  paste0("Episodios con hospitalizaciones aberrantemente largas (>5 años) (n= ", formatC(disc_large_tr_days, format='f', big.mark='.', digits=0),")")
tab2a3_lab<-  paste0("Episodios con fechas de ingreso entre 2005 y 2017 (n= ", formatC(disc_dates_2017_22, format='f', big.mark='.', digits=0),")")
tab2a4_lab<-  paste0("Usuarios con diagnósticos rel. a salud mental o TUS entre el 2005 y 2017 (p= ", formatC(nrow(disc_dates_2017_22_df2), format='f', big.mark='.', digits=0),")")
tab2a5_lab<-  paste0("RUNs erróneos (1bad6b8cf97131fceab8543e81f7757195fbb1d36b376ee994ad1cf17699c464) (n= ", formatC(disc_run_erroneos, format='f', big.mark='.', digits=0),")")
tab2a6_lab<-  paste0("Usuarios con registros sin nacionalidad Chilena ", disc_nac_nocl,"")
tab2a_lab<-   paste0('&#8226;',
                     tab2a1_lab,'\\\\\\l&#8226;',
                     tab2a2_lab,'\\\\\\l&#8226;',
                     tab2a3_lab,'\\\\\\l&#8226;',
                     tab2a4_lab,'\\\\\\l&#8226;',
                     tab2a5_lab,'\\\\\\l&#8226;',
                     tab2a6_lab,'\\\\\\l')
tab3_lab<-    cnt_runs_s5

tab3to4a_lab<- paste0()

tab4ato5_lab<- paste0('  - Usuarios que ingresaron en 2018 con 15 a 29 años con diagnósticos rel. a salud mental o TUS sin registros de hosp.\\\\\\l  previas por dichas causas\\\\\\l',
                      '  - Usuarios pertenecientes a PPOO por CONADI (p= ', 
                      formatC(nrow(subset(dtX2023_12_05_DatosEgresosHosp_encrip_s5a,subsample==1)), format='f', big.mark='.', digits=0),')\\\\\\l ',
                      ' - Usuarios que se han identificado al menos una vez desde el 2005 como pertenecientes a PPOO o reconocidos por\\\\\\l CONADI (p= ', 
                      formatC(nrow(subset(dtX2023_12_05_DatosEgresosHosp_encrip_s5a,inclusivo_real_historico==1)), format='f', big.mark='.', digits=0),
                      ')',
                      '\\\\\\l')

tab4a1_lab<-  paste0('Registros del año 2022 (n=', formatC(nrow(dplyr::filter(dtX2023_12_05_DatosEgresosHosp_encrip_s5, run %in% dtX2023_12_05_DatosEgresosHosp_encrip_s5a2$run) %>% dplyr::filter(ing_year>=2022)), format='f', big.mark='.', digits=0),';p= ',
                     formatC(nrow(dplyr::filter(dtX2023_12_05_DatosEgresosHosp_encrip_s5, run %in% dtX2023_12_05_DatosEgresosHosp_encrip_s5a2$run) %>% dplyr::filter(ing_year>=2022) %>% distinct(run)), format='f', big.mark='.', digits=0),')')
tab4a2_lab<-  paste0('Usuarios que al 2018 tenían episodios de salud mental antes de cumplir 15 años (n=', formatC(dplyr::filter(dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl, run %in% (dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl %>% dplyr::filter(edad_anos<15) %>% pull(run))) %>% nrow(), format='f', big.mark='.', digits=0),';p= ',
                     formatC(dplyr::filter(dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl, run %in% (dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl %>% dplyr::filter(edad_anos<15) %>% pull(run))) %>% distinct(run) %>%  nrow(), format='f', big.mark='.', digits=0),")")

dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl_2 <- dplyr::filter(dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl, !run %in% (dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl %>% dplyr::filter(edad_anos<15) %>% pull(run)))
cnt_runs_s5_adultl_2<- paste0('Base de datos pre-final \n(n= ', 
                                formatC(nrow(dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl_2), 
                                        format='f', big.mark='.', digits=0), 
                                ';\np= ',
                                formatC(length(unique(dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl_2$run)), 
                                        format='f', big.mark='.', digits=0),')')
invisible("Duplicated rows")
vector_duplicates<- c("run", "estab_homo",  "areaf_egr", "fecha_ingreso", "fecha_egreso", "glosa_prevision", "benef", "glosa_sexo", "edad_anos", "diag1", "diag2", "diag3", "diag4", "diag5")

dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl_3 <-
dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl_2%>%
  dplyr::group_by(across(all_of(vector_duplicates))) %>% 
  dplyr::mutate(n_duplicated=dplyr::row_number()) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(n_duplicated<2)


tab4a3_lab<-  paste0('Registros duplicados en RUN, cód. establecimiento, área de atención, fecha de ingreso y egreso, previsión,\\\\\\ltramo, sexo, edad y 5 primeros diagnósticos (n=', 
                     formatC(subset(dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl_2,duplicated(dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl_2[,vector_duplicates])) %>% nrow(), format='f', big.mark='.', digits=0),
                     ';p= ',
                     formatC(subset(dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl_2,duplicated(dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl_2[,vector_duplicates])) %>% distinct(run) %>%  nrow(), format='f', big.mark='.', digits=0),")")

tab4a_lab<-   paste0('&#8226;',
                     tab4a1_lab,'\\\\\\l&#8226;',
                     tab4a2_lab,'\\\\\\l&#8226;',
                     tab4a3_lab,'\\\\\\l')


cnt_runs_s5_adultl_3<- paste0('Base de datos final \n(n= ', 
                              formatC(nrow(dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl_3), 
                                      format='f', big.mark='.', digits=0), 
                              ';\np= ',
                              formatC(length(unique(dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl_3$run)), 
                                      format='f', big.mark='.', digits=0),')')

tab5_lab<-    cnt_runs_s5_adultl_3 #actualizado al 2024-05-10: filtering cases with less than 14 years

invisible("Por qué saqué estos casos")
discarded_run_post_jne<-
dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl_3 %>% 
  dplyr::filter(!run %in% unique(ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide2$run)) %>% pull(run) %>% unique()

length(unique(data_long$run))#6041
length(unique(data_long_establecimiento_2024_std$run))#6041
length(unique(data_long_est_24_std_imp_rec_imp$run))#6041
length(unique(data_long_est_24_std_imp_rec_imp_areaf$run))#6041
data_long_est_24_std_imp_rec_imp_areaf %>%
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
  )) %>% dplyr::filter(areaf_egr_glosa_rec=="descartar") %>%
  {
    print(paste0("RUNs= ", nrow(distinct(., run))))
    print(paste0("n= ",nrow(.)))
  }
# [1] "RUNs= 5"
# [1] "n= 5"
invisible("Descartados hasta llegar a un_inv_ii3, aunque no sé por qué")
descartados_desconocidos<-
data_long_est_24_std_imp_rec_imp_areaf_rec_cod  %>%
  dplyr::arrange(run, min_fecha_egreso_rec24_num, fecha_egreso_rec24_num) %>% 
  dplyr::group_by(run) %>% 
  dplyr::mutate(tto= dplyr::row_number()) %>% 
  dplyr::mutate(lag_fecha_egreso_rec_num= lag(unclass(as.Date(fecha_egreso_rec)), default= unclass(as.Date("2017-12-01")))) %>%
  dplyr::mutate(lag_fecha_ingreso_rec_num= lag(unclass(as.Date(fecha_ingreso_rec)), default= unclass(as.Date("2017-12-01")))) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(!run %in% unique(ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide2$run)) 

glimpse(descartados_desconocidos)
#AMBAS APARECEN NO FALLECIDAS COND_EGR==1
#bd0c2f8dcc5209831487342eda88d0b0461c98150911e36b64b4055a846a3631
#ef1193767a2df73d6fdb1dd59d35edce262a9031d9ca5b012a11d088081b746e
#
invisible("Esto es lo que tengo anotado en el archivo un_inv_ii2")
invisible("2024-09-02: Casos que tienen ausente y sin información despues")
casos_prob<- c("ef1193767a2df73d6fdb1dd59d35edce262a9031d9ca5b012a11d088081b746e", 
               "bd0c2f8dcc5209831487342eda88d0b0461c98150911e36b64b4055a846a3631")
#O829 PARTO POR CESAREA, SIN OTRA ESPECIFICACION
# L032 CELULITIS DE LA CARA
#O829: Este código corresponde a "Parto espontáneo, sin otra especificación".
#L032: Este código se refiere a "Celulitis del tronco".

tab5a1_lab<-  paste0('Registros en áreas de atención de Neonatología (se sospecha error de clasificación) (n=5; p=5;\\\\\\l1 paciente eliminado [sin otros registros])')
tab5a2_lab<-  paste0('Registros descartados producto del reordenamiento de causas (n=2; p=2)')

data_long_est_24_std_imp_rec_imp_areaf_rec_cod  %>%
  dplyr::arrange(run, min_fecha_egreso_rec24_num, fecha_egreso_rec24_num) %>% 
  dplyr::group_by(run) %>% 
  dplyr::mutate(tto= dplyr::row_number()) %>% 
  dplyr::mutate(lag_fecha_egreso_rec_num= lag(unclass(as.Date(fecha_egreso_rec)), default= unclass(as.Date("2017-12-01")))) %>%
  dplyr::mutate(lag_fecha_ingreso_rec_num= lag(unclass(as.Date(fecha_ingreso_rec)), default= unclass(as.Date("2017-12-01")))) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(!run %in% unique(df_filled$run))

tab5a_lab<-   paste0('&#8226;',
                     tab5a1_lab,'\\\\\\l&#8226;',
                     tab5a2_lab,'\\\\\\l')

length(unique(data_long_est_24_std_imp_rec_imp_areaf_rec$run))#6040
length(unique(data_long_est_24_std_imp_rec_imp_areaf_rec_cod$run))#6040
length(unique(df_filled$run)) #6038
invisible("Pero, con el código para haecr df_filled...")
data_long_est_24_std_imp_rec_imp_areaf_rec_cod  %>%
  dplyr::arrange(run, min_fecha_egreso_rec24_num, fecha_egreso_rec24_num) %>% 
  dplyr::group_by(run) %>% 
  dplyr::mutate(tto= dplyr::row_number()) %>% 
  dplyr::mutate(lag_fecha_egreso_rec_num= lag(unclass(as.Date(fecha_egreso_rec)), default= unclass(as.Date("2017-12-01")))) %>%
  dplyr::mutate(lag_fecha_ingreso_rec_num= lag(unclass(as.Date(fecha_ingreso_rec)), default= unclass(as.Date("2017-12-01")))) %>% 
  dplyr::ungroup() %>% 
  distinct(run) %>% nrow() #6040

invisible("Corregimos la etiqueta de la base que era final antes de esta")
cnt_runs_s5_adultl_3<- paste0('Base de datos final \n(n= ', 
                              formatC(nrow(dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl_3), 
                                      format='f', big.mark='.', digits=0), 
                              ';\np= ',
                              formatC(length(unique(dtX2023_12_05_DatosEgresosHosp_encrip_s5_adultl_3$run)), 
                                      format='f', big.mark='.', digits=0),')')

tab5_lab<-    cnt_runs_s5_adultl_3 #actualizado al 2024-05-10: filtering cases with less than 14 years


tab5_jne_lab<- paste0("Base de datos final\n(p= ", formatC(length(unique(ing_dt_ing_calendar_quarter_t_desde_primera_adm_dedup_wide2$run)), 
                                                           format='f', big.mark='.', digits=0),")")

tab6_lab <- paste0('Mortalidad
(2017-2021)
(p= ', formatC(length(unique(def_enc17_21$RUN)), format='f', big.mark='.', digits=0),')')

#https://stackoverflow.com/questions/46750364/diagrammer-and-graphviz
#https://mikeyharper.uk/flowcharts-in-r-using-diagrammer/
#http://blog.nguyenvq.com/blog/2012/05/29/better-decision-tree-graphics-for-rpart-via-party-and-partykit/
#http://blog.nguyenvq.com/blog/2014/01/17/skeleton-to-create-fast-automatic-tree-diagrams-using-r-and-graphviz/
#https://cran.r-project.org/web/packages/DiagrammeR/vignettes/graphviz-mermaid.html
#https://stackoverflow.com/questions/39133058/how-to-use-graphviz-graphs-in-diagrammer-for-r
#https://subscription.packtpub.com/book/big_data_and_business_intelligence/9781789802566/1/ch01lvl1sec21/creating-diagrams-via-the-diagrammer-package
#https://justlegal.be/2019/05/using-flowcharts-to-display-legal-procedures/
#

library(DiagrammeR) #⋉style='filled', 

plot_merge_flowchart<-
  grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Times, shape = rectangle, fontsize = 9]        
      tab1 [label = '@@1']
      blank [label = '', width = 0.0001, height = 0.0001]
      tab2 [label = '@@2',fontsize = 7]
      tab3 [label = '@@3']
      tab4 [label = '@@9', fontsize= 7]

      tab5 [label = '@@5']
      blank15 [label = '', width = 0.0001, height = 0.0001]
      tab6 [label = '@@7']
      blank15_jne [label = '', width = 0.0001, height = 0.0001]
      tab5a [label = '@@10', fontsize= 7]
      tab5_jne [label = '@@11']      
      blank2 [label= '⋉' fontsize = 11, width = 0.0001, height = 0.0001, fillcolor='transparent', color='transparent']
      tab7 [label = '@@8']

      # edge definitions with the node IDs
      rankdir='TB'; rank= same; tab1 -> blank [arrowhead = none, label='  - Estructuración de la base de datos de hospitalizaciones', fontsize = 7];
      rankdir='TB'; rank= same; tab1; tab3;   
      blank -> tab2;
                  subgraph {
              rank = same; tab2; blank;
                  }
      rankdir='TB'; rank= same; blank -> tab3 [label= '  - Recodificar fechas, variables de pertenencia a PPOO (NAs por 0), creación de año de ingreso a tto., \\\\\\l  clasificación códigos CIE-10\\\\\\l', fontsize = 7];   
      
      #paso 2
      rankdir='TB'; rank= same; tab3 -> blank15 [arrowhead = none];
      rankdir='TB'; rank= same; blank15 ->  tab5 [label= '@@6', fontsize = 7];
      blank15 -> tab4;
                  subgraph {
              rank = same; tab4; blank15;
                  }   
      #paso 3: post-JNE
      rankdir='TB'; rank= same; tab5 -> blank15_jne [arrowhead = none];
      rankdir='TB'; rank= same; blank15_jne -> tab5_jne [arrowhead = none];
      blank15_jne -> tab5a;      
                        subgraph {
              rank = same; tab5a; blank15_jne
                        }
      #paso 3 (cierre)
      rankdir='TB'; rank= same; tab5_jne -> blank2 [arrowhead = none];
      rankdir='TB'; rank= same; blank2 -> tab6 [arrowhead = none];
                        subgraph {
              rank = same; tab6; tab5_jne; blank2
                        }
      blank2 -> tab7;
      }
      [1]:  tab1_lab
      [2]:  tab2a_lab
      [3]:  tab3_lab
      [4]:  tab4a_lab
      [5]:  tab5_lab
      [6]:  tab4ato5_lab
      [7]:  tab6_lab
      [8]:  cnt_runs_mort
      [9]:  tab4a_lab
      [10]: tab5a_lab
      [11]: tab5_jne_lab            
      ", 
width = 800,
height = 900)

plot_merge_flowchart

# WidthCM<-8
# HeightCM<-6
# DPI<-600
unlink("_diagrama_flujo_post_ib_SM_JNE_files", recursive = TRUE)
htmlwidgets::saveWidget(plot_merge_flowchart, "H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/_figs/_diagrama_flujo_post_ib_SM_JNE.html")
webshot::webshot("H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/_figs/_diagrama_flujo_post_ib_SM_JNE.html", 
                 "H:/Mi unidad/PERSONAL ANDRES/UCH_salud_publica/asignaturas/un_inv_II/_figs/_diagrama_flujo_post_ib_SM_JNE.png",
                 vwidth = 300*1.2, vheight = 300,  zoom=10, expand=100)  # Prueba con diferentes coordenadas top, left, width, and height.

#https://stackoverflow.com/questions/1554635/graphviz-how-to-have-a-subgraph-be-left-to-right-when-main-graph-is-top-to-bot
#https://stackoverflow.com/questions/65509087/diagrammer-flowchart-align-vertical-nodes
#https://stackoverflow.com/questions/39451158/how-to-specify-vertical-alignment-of-nodes-in-r-package-diagrammer
#https://stackoverflow.com/questions/64323943/graphviz-and-dot-files-horizontal-and-vertical-node-alignment-intervening-node
#https://stackoverflow.com/questions/5424555/changing-edge-direction-in-dot
#https://graphviz.org/docs/attrs/rankdir/