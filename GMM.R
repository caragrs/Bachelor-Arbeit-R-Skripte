library(readxl)
library(dplyr)
library(plm)
library(writexl)
df_raw <- read_excel("/Users/caragross/Desktop/Bachelor_Arbeit/Cleaner_DF.xlsx")

df <- df_raw %>%
  rename(Country = `Country`,
         Year   = `Year`,
         LGDP   = `LGDP`,
         BBS     = `BBS: Broadband Subscription`,
         IU      = `IU: Internet use`,
         MPS     = `MPS: Mobile Phone Subscriptions`,
         GFCF   = `GFCF: Gross Fixed Capital Formation`,
         TO     = `TO: Trade Openness  (expbs+Impbs)`,
         Labor  = `Labor (Hlabor+Flabor)`,
         LCPI   = `LCPI: Consumers Price Index`,
         LPOP   = `LPOP: Poplulation`,
         consum = `consum: Government Consuption`,
         RD     = `RD`) %>%
  mutate(Country = as.factor(Country),
         year    = as.numeric(Year)) %>%
  arrange(Country, Year)

pdata <- pdata.frame(df, index = c("Country", "Year"))

gmm_mod <- pgmm(LGDP ~ lag(LGDP, 1) + BBS + IU + MPS + GFCF + TO + Labor + LCPI + LPOP + consum + RD |
                  lag(LGDP, 2:4) + lag(BBS, 2:4) + lag(IU, 2:4) + lag(MPS, 2:4),
                data = pdata,
                effect = "individual",
                model = "twosteps",
                transformation = "d")

summary(gmm_mod, robust = TRUE)

gmm_sys <- pgmm(LGDP ~ lag(LGDP, 1) +  BBS + IU + MPS + GFCF + TO + Labor + LCPI + LPOP + consum + RD |
                  lag(LGDP, 2:4) + lag(BBS, 2:4) + lag(IU, 2:4) + lag(MPS, 2:4),
                data = pdata,
                effect = "individual",
                model = "twosteps",
                transformation = "ld")

summary(gmm_sys, robust = TRUE)

#Reduzierung der Lags
gmm_sys_red <- pgmm(LGDP ~ lag(LGDP, 1) +  BBS + IU + MPS + GFCF + TO + Labor + LCPI + LPOP + consum + RD |
                      lag(LGDP, 1:2) + lag(BBS, 2:4) + lag(IU, 2:4) + lag(MPS, 2:4),
                    data = pdata,
                    effect = "individual",
                    model = "twosteps",
                    transformation = "ld")

summary(gmm_sys_red, robust = TRUE)

# Extraktion
get_coef_df <- function(gmm_obj, robust = TRUE) {
  s <- summary(gmm_obj, robust = robust)
  df <- as.data.frame(s$coefficients)
  df$term <- rownames(df)
  rownames(df) <- NULL
  # schöne Spaltennamen
  names(df) <- sub("Pr\\(>\\|z\\|\\)", "p_value", names(df))
  df <- df[, c("term", setdiff(names(df), "term"))]
  df
}

get_vars_df <- function(gmm_obj) {
  f <- formula(gmm_obj)
  data.frame(
    component = c("dependent", "regressors"),
    content   = c(as.character(f[[2]]), deparse(f[[3]])),
    stringsAsFactors = FALSE
  )
}

get_tests_df <- function(gmm_obj, robust = TRUE) {
  s <- summary(gmm_obj, robust = robust)
  
  # Sargan/Hansen: je nach plm-Version heißen die Slots manchmal unterschiedlich
  sargan_stat <- tryCatch(as.numeric(s$sargan$statistic), error = function(e) NA_real_)
  sargan_df   <- tryCatch(as.numeric(s$sargan$parameter), error = function(e) NA_real_)
  sargan_p    <- tryCatch(as.numeric(s$sargan$p.value), error = function(e) NA_real_)
  
  # Arellano-Bond AR(1)/AR(2): ebenfalls Slot-abhängig
  ar1_stat <- tryCatch(as.numeric(s$autocor[1, "statistic"]), error = function(e) NA_real_)
  ar1_p    <- tryCatch(as.numeric(s$autocor[1, "p.value"]),   error = function(e) NA_real_)
  ar2_stat <- tryCatch(as.numeric(s$autocor[2, "statistic"]), error = function(e) NA_real_)
  ar2_p    <- tryCatch(as.numeric(s$autocor[2, "p.value"]),   error = function(e) NA_real_)
  
  # Wald test (gesamt)
  wald_stat <- tryCatch(as.numeric(s$wald$statistic), error = function(e) NA_real_)
  wald_df   <- tryCatch(as.numeric(s$wald$parameter), error = function(e) NA_real_)
  wald_p    <- tryCatch(as.numeric(s$wald$p.value),   error = function(e) NA_real_)
  
  data.frame(
    test = c("Sargan/Hansen overid", "AR(1) autocorrelation", "AR(2) autocorrelation", "Wald (joint significance)"),
    statistic = c(sargan_stat, ar1_stat, ar2_stat, wald_stat),
    df        = c(sargan_df,   NA,       NA,       wald_df),
    p_value   = c(sargan_p,    ar1_p,     ar2_p,    wald_p),
    stringsAsFactors = FALSE
  )
}

get_meta_df <- function(gmm_obj) {
  data.frame(
    item  = c("class", "nobs_used", "call"),
    value = c(
      paste(class(gmm_obj), collapse = ", "),
      tryCatch(as.character(nobs(gmm_obj)), error = function(e) NA_character_),
      paste(deparse(gmm_obj$call), collapse = " ")
    ),
    stringsAsFactors = FALSE
  )
}

get_raw_summary_df <- function(gmm_obj, robust = TRUE) {
  txt <- capture.output(summary(gmm_obj, robust = robust))
  data.frame(line = txt, stringsAsFactors = FALSE)
}

#Modelle: summary + Tabellen bauen
models <- list(
  gmm_mod     = gmm_mod,
  gmm_sys     = gmm_sys,
  gmm_sys_red = gmm_sys_red
)

sheets <- list()

for (nm in names(models)) {
  obj <- models[[nm]]
  
  sheets[[paste0(nm, "_coefs")]] <- get_coef_df(obj, robust = TRUE)
  sheets[[paste0(nm, "_vars")]]  <- get_vars_df(obj)      # zeigt dir die komplette RHS inkl. lag(...)
  sheets[[paste0(nm, "_meta")]]  <- get_meta_df(obj)
  sheets[[paste0(nm, "_tests")]] <- get_tests_df(obj, robust = TRUE)
  
  # optional: kompletter summary als Text im Anhang
  sheets[[paste0(nm, "_raw")]]   <- get_raw_summary_df(obj, robust = TRUE)
}

#Export nach Excel
write_xlsx(
  sheets,
  path = "/Users/caragross/Desktop/Bachelor_Arbeit/GMM_Anhang.xlsx"
)

