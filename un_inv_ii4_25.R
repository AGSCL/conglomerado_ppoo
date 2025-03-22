# remover objetos y memoria utilizada
rm(list=ls());gc()

if(!require(job)){install.packages("job");require(job)}
if(!require(kableExtra)){install.packages("kableExtra");require(kableExtra)}
if(!require(tidyverse)){install.packages("tidyverse");require(tidyverse)}
if(!require(cluster)){install.packages("cluster"); require(cluster)}
if(!require(WeightedCluster)){install.packages("WeightedCluster"); require(WeightedCluster)}
if(!require(devtools)){install.packages("devtools"); require(devtools)}
if(!require(TraMineR)){install.packages("TraMineR"); require(TraMineR)}
if(!require(TraMineRextras)){install.packages("TraMineRextras"); require(TraMineRextras)}
if(!require(NbClust)){install.packages("NbClust"); require(NbClust)}
if(!require(haven)){install.packages("haven"); require(haven)}
if(!require(ggseqplot)){install.packages("ggseqplot"); require(ggseqplot)}
if(!require(gridExtra)){install.packages("gridExtra"); require(gridExtra)}
if(!require(Tmisc)){install.packages("Tmisc"); require(Tmisc)}
if(!require(factoextra)){install.packages("factoextra"); require(factoextra)}
if(!require(pbapply)){install.packages("pbapply"); require(pbapply)}
if(!require(progressr)){install.packages("progressr"); require(progressr)}


interactive()
library(pbapply)
pboptions(type = "txt") # o "txt" o "timer" según el soporte de tu terminal

library(progressr)
handlers("txtprogressbar")  # Barra textual compatible con notebooks

#elegir repositorio
if(Sys.info()["sysname"]=="Windows"){
  options(repos = c(CRAN = "https://cran.dcc.uchile.cl/"))
}

States_Wide.seq_month_t_prim_adm <- readRDS("_perm/States_Wide.seq_month_t_prim_adm.rds")
States_Wide.seq_quarter_t_prim_adm <- readRDS("_perm/States_Wide.seq_quarter_t_prim_adm.rds")
om_dist_month <- readRDS("_perm/om_dist_month.rds")
lcs_dist_month <- readRDS("_perm/lcs_dist_month.rds")
om_dist_quarter <- readRDS("_perm/om_dist_quarter.rds")
lcs_dist_quarter <- readRDS("_perm/lcs_dist_quarter.rds")
om_dist_month_c <- readRDS("_perm/om_dist_month_c.rds")
om_dist_quarter_c <- readRDS("_perm/om_dist_quarter_c.rds")
lcs_dist_month_c <- readRDS("_perm/lcs_dist_month_c.rds")
lcs_dist_quarter_c <- readRDS("_perm/lcs_dist_quarter_c.rds")
pamRange_month_om <- readRDS("_perm/pamRange_month_om.rds")
pamRange_month_om2 <- readRDS("_perm/pamRange_month_om2.rds")
pamRange_quarter_om <- readRDS("_perm/pamRange_quarter_om.rds")
pamRange_quarter_om2 <- readRDS("_perm/pamRange_quarter_om2.rds")
pamRange_month_lcs <- readRDS("_perm/pamRange_month_lcs.rds")
pamRange_month_lcs2 <- readRDS("_perm/pamRange_month_lcs2.rds")
pamRange_quarter_lcs <- readRDS("_perm/pamRange_quarter_lcs.rds")
pamRange_quarter_lcs2 <- readRDS("_perm/pamRange_quarter_lcs2.rds")
costmatrix_month <- readRDS("_perm/costmatrix_month.rds")
costmatrix_quarter <- readRDS("_perm/costmatrix_quarter.rds")
dist_month_om <- readRDS("_perm/dist_month_om.rds")
dist_month_lcs <- readRDS("_perm/dist_month_lcs.rds")
dist_quarter_om <- readRDS("_perm/dist_quarter_om.rds")
dist_quarter_lcs <- readRDS("_perm/dist_quarter_lcs.rds")

r_num= 1000

# Jerárquico ---------------------------------------------------------------------

## OM ---------------------------------------------------------------------

### Trimestral ---------------------------------------------------------------------

# OM Quarter Combined
om_quarter_null_comb <- WeightedCluster::seqnullcqi(
  States_Wide.seq_quarter_t_prim_adm,
  om_dist_quarter_c,
  R=r_num,
  model=c("combined"),
  seqdist.args=list(method="OM", sm = costmatrix_quarter),
  hclust.method="ward.D",
  parallel = TRUE
)
save(om_quarter_null_comb, file="_perm/null_ssa_hc_quarter_om_comb_20250319.Rda")

gc()

# OM Quarter Sequencing
om_quarter_null_seq <- WeightedCluster::seqnullcqi(
  States_Wide.seq_quarter_t_prim_adm,
  om_dist_quarter_c,
  R=r_num,
  model=c("sequencing"),
  seqdist.args=list(method="OM", sm = costmatrix_quarter),
  hclust.method="ward.D",
  parallel = TRUE
)
save(om_quarter_null_seq, file="_perm/null_ssa_hc_quarter_om_seq_20250319.Rda")

gc()

### Mensual ---------------------------------------------------------------------

# OM Month Combined
om_month_null_comb <- WeightedCluster::seqnullcqi(
  States_Wide.seq_month_t_prim_adm,
  om_dist_month_c,
  R=r_num,
  model=c("combined"),
  seqdist.args=list(method="OM", sm = costmatrix_month),
  hclust.method="ward.D",
  parallel = TRUE
)
save(om_month_null_comb, file="_perm/null_ssa_hc_month_om_comb_20250319.Rda")

gc()

# OM Month Sequencing
om_month_null_seq <- WeightedCluster::seqnullcqi(
  States_Wide.seq_month_t_prim_adm,
  om_dist_month_c,
  R=r_num,
  model=c("sequencing"),
  seqdist.args=list(method="OM", sm = costmatrix_month),
  hclust.method="ward.D",
  parallel = TRUE
)
save(om_month_null_seq, file="_perm/null_ssa_hc_month_om_seq_20250319.Rda")

gc()

## LCS ---------------------------------------------------------------------

### Trimestral ---------------------------------------------------------------------

# LCS Quarter Combined
lcs_quarter_null_comb <- WeightedCluster::seqnullcqi(
  States_Wide.seq_quarter_t_prim_adm,
  lcs_dist_quarter_c,
  R=r_num,
  model=c("combined"),
  seqdist.args=list(method="LCS", sm = costmatrix_quarter),
  hclust.method="ward.D",
  parallel = TRUE
)
save(lcs_quarter_null_comb, file="_perm/null_ssa_hc_quarter_lcs_comb_20250319.Rda")

gc()

# LCS Quarter Sequencing
lcs_quarter_null_seq <- WeightedCluster::seqnullcqi(
  States_Wide.seq_quarter_t_prim_adm,
  lcs_dist_quarter_c,
  R=r_num,
  model=c("sequencing"),
  seqdist.args=list(method="LCS", sm = costmatrix_quarter),
  hclust.method="ward.D",
  parallel = TRUE
)
save(lcs_quarter_null_seq, file="_perm/null_ssa_hc_quarter_lcs_seq_20250319.Rda")


gc()
### Mensual ---------------------------------------------------------------------

# LCS Month Combined
lcs_month_null_comb <- WeightedCluster::seqnullcqi(
  States_Wide.seq_month_t_prim_adm,
  lcs_dist_month_c,
  R=r_num,
  model=c("combined"),
  seqdist.args=list(method="LCS", sm = costmatrix_month),
  hclust.method="ward.D",
  parallel = TRUE
)
save(lcs_month_null_comb, file="_perm/null_ssa_hc_month_lcs_comb_20250319.Rda")

gc()

# LCS Month Sequencing
lcs_month_null_seq <- WeightedCluster::seqnullcqi(
  States_Wide.seq_month_t_prim_adm,
  lcs_dist_month_c,
  R=r_num,
  model=c("sequencing"),
  seqdist.args=list(method="LCS", sm = costmatrix_month),
  hclust.method="ward.D",
  parallel = TRUE
)
save(lcs_month_null_seq, file="_perm/null_ssa_hc_month_lcs_seq_20250319.Rda")

gc()

#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:
#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:

# PAM ---------------------------------------------------------------------

## OM ---------------------------------------------------------------------

### Trimestral ---------------------------------------------------------------------

# PAM OM Trimestre Combined
pam_om_quarter_null_comb <- WeightedCluster::seqnullcqi(
  States_Wide.seq_quarter_t_prim_adm,
  pamRange_quarter_om,
  R=r_num,
  model=c("combined"),
  seqdist.args=list(method="OM", sm = costmatrix_quarter),
  kmedoid = TRUE,
  parallel = TRUE
)
save(pam_om_quarter_null_comb, file="_perm/null_ssa_pam_quarter_om_comb_20250319.Rda")

gc()

# PAM OM Trimestre Sequencing
pam_om_quarter_null_seq <- WeightedCluster::seqnullcqi(
  States_Wide.seq_quarter_t_prim_adm,
  pamRange_quarter_om,
  R=r_num,
  model=c("sequencing"),
  seqdist.args=list(method="OM", sm = costmatrix_quarter),
  kmedoid = TRUE,
  parallel = TRUE
)
save(pam_om_quarter_null_seq, file="_perm/null_ssa_pam_quarter_om_seq_20250319.Rda")

gc()

# PAM OM Trimestre Combined
pam_om_quarter_null_comb2 <- WeightedCluster::seqnullcqi(
  States_Wide.seq_quarter_t_prim_adm,
  pamRange_quarter_om2,
  R=r_num,
  model=c("combined"),
  seqdist.args=list(method="OM", sm = costmatrix_quarter),
  kmedoid = TRUE,
  parallel = TRUE
)
save(pam_om_quarter_null_comb2, file="_perm/null_ssa_pam_quarter_om_comb2_20250319.Rda")

gc()

# PAM OM Trimestre Sequencing
pam_om_quarter_null_seq2 <- WeightedCluster::seqnullcqi(
  States_Wide.seq_quarter_t_prim_adm,
  pamRange_quarter_om2,
  R=r_num,
  model=c("sequencing"),
  seqdist.args=list(method="OM", sm = costmatrix_quarter),
  kmedoid = TRUE,
  parallel = TRUE
)
save(pam_om_quarter_null_seq2, file="_perm/null_ssa_pam_quarter_om_seq2_20250319.Rda")

gc()

### Mensual ---------------------------------------------------------------------

# PAM OM Mes Combined
pam_om_month_null_comb <- WeightedCluster::seqnullcqi(
  States_Wide.seq_month_t_prim_adm,
  pamRange_month_om,
  R=r_num,
  model=c("combined"),
  seqdist.args=list(method="OM", sm = costmatrix_month),
  kmedoid = TRUE,
  parallel = TRUE
)
save(pam_om_month_null_comb, file="_perm/null_ssa_pam_month_om_comb_20250319.Rda")

gc()

# PAM OM Mes Sequencing
pam_om_month_null_seq <- WeightedCluster::seqnullcqi(
  States_Wide.seq_month_t_prim_adm,
  pamRange_month_om,
  R=r_num,
  model=c("sequencing"),
  seqdist.args=list(method="OM", sm = costmatrix_month),
  kmedoid = TRUE,
  parallel = TRUE
)
save(pam_om_month_null_seq, file="_perm/null_ssa_pam_month_om_seq_20250319.Rda")

gc()
#HACIENDOLOS EN UN JOB 2025-03-21

## LCS ---------------------------------------------------------------------

### Trimestral ---------------------------------------------------------------------

# PAM LCS Trimestre Combined
pam_lcs_quarter_null_comb <- WeightedCluster::seqnullcqi(
  States_Wide.seq_quarter_t_prim_adm,
  pamRange_quarter_lcs,
  R=r_num,
  model=c("combined"),
  seqdist.args=list(method="LCS", sm = costmatrix_quarter),
  kmedoid = TRUE,
  parallel = TRUE
)
save(pam_lcs_quarter_null_comb, file="_perm/null_ssa_pam_quarter_lcs_comb_20250319.Rda")

gc()

# PAM LCS Trimestre Sequencing
pam_lcs_quarter_null_seq <- WeightedCluster::seqnullcqi(
  States_Wide.seq_quarter_t_prim_adm,
  pamRange_quarter_lcs,
  R=r_num,
  model=c("sequencing"),
  seqdist.args=list(method="LCS", sm = costmatrix_quarter),
  kmedoid = TRUE,
  parallel = TRUE
)
save(pam_lcs_quarter_null_seq, file="_perm/null_ssa_pam_quarter_lcs_seq_20250319.Rda")

gc()

#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:

# PAM OM Month Combined
pam_om_month_null_comb2 <- WeightedCluster::seqnullcqi(
  States_Wide.seq_month_t_prim_adm, #2024-09-08 no lo había hecho con la base censurada
  pamRange_month_om2,
  R= r_num,
  model= c("combined"),
  seqdist.args= list(method="OM", sm = costmatrix_month),
  kmedoid = TRUE,
  parallel = TRUE
)
save(pam_om_month_null_comb2, file="_perm/null_ssa_pam_om_month_null_comb2_20250319.Rda")

gc()

# PAM OM Month Cens Sequencing
pam_om_month_null_seq2 <- WeightedCluster::seqnullcqi(
  States_Wide.seq_month_t_prim_adm, #2024-09-08 no lo había hecho con la base censurada
  pamRange_month_om2,
  R= r_num,
  model= c("sequencing"),
  seqdist.args= list(method="OM", sm = costmatrix_month),
  kmedoid = TRUE,
  parallel = TRUE
)
save(pam_om_month_null_seq2, file="_perm/null_ssa_pam_om_month_null_seq2_20250319.Rda")

gc()


### Mensual ---------------------------------------------------------------------

# PAM LCS Mes Combined
pam_lcs_month_null_comb <- WeightedCluster::seqnullcqi(
  States_Wide.seq_month_t_prim_adm,
  pamRange_month_lcs,
  R=r_num,
  model=c("combined"),
  seqdist.args=list(method="LCS", sm = costmatrix_month),
  kmedoid = TRUE,
  parallel = TRUE
)
save(pam_lcs_month_null_comb, file="_perm/null_ssa_pam_month_lcs_comb_20250319.Rda")

gc()

# PAM LCS Mes Sequencing
pam_lcs_month_null_seq <- WeightedCluster::seqnullcqi(
  States_Wide.seq_month_t_prim_adm,
  pamRange_month_lcs,
  R=r_num,
  model=c("sequencing"),
  seqdist.args=list(method="LCS", sm = costmatrix_month),
  kmedoid = TRUE,
  parallel = TRUE
)
save(pam_lcs_month_null_seq, file="_perm/null_ssa_pam_month_lcs_seq_20250319.Rda")

gc()

#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:

# PAM LCS Mes Combined
pam_lcs_month_null_comb2 <- WeightedCluster::seqnullcqi(
  States_Wide.seq_month_t_prim_adm,
  pamRange_month_lcs2,
  R=r_num,
  model=c("combined"),
  seqdist.args=list(method="LCS", sm = costmatrix_month),
  kmedoid = TRUE,
  parallel = TRUE
)
save(pam_lcs_month_null_comb2, file="_perm/null_ssa_pam_month_lcs_comb2_20250319.Rda")

gc()

# PAM LCS Mes Sequencing
pam_lcs_month_null_seq2 <- WeightedCluster::seqnullcqi(
  States_Wide.seq_month_t_prim_adm,
  pamRange_month_lcs2,
  R=r_num,
  model=c("sequencing"),
  seqdist.args=list(method="LCS", sm = costmatrix_month),
  kmedoid = TRUE,
  parallel = TRUE
)
save(pam_lcs_month_null_seq2, file="_perm/null_ssa_pam_month_lcs_seq2_20250319.Rda")

gc()
