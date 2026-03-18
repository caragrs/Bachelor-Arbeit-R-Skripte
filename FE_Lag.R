options(error = NULL)
options(warn  = 0)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(fixest)
  library(writexl)
})

df_raw <- readxl::read_excel("/Users/caragross/Desktop/Bachelor_Arbeit/Cleaner_DF.xlsx")
names(df_raw) <- trimws(names(df_raw))

df <- df_raw %>%
  rename(
    Country = Country,
    year    = Year,
    Income  = Income,
    LGDP    = LGDP,
    DDI     = DDI,
    GFCF    = `GFCF: Gross Fixed Capital Formation`,
    TO      = `TO: Trade Openness  (expbs+Impbs)`,
    Labor   = `Labor (Hlabor+Flabor)`,
    LCPI    = `LCPI: Consumers Price Index`,
    LPOP    = `LPOP: Poplulation`,
    consum  = `consum: Government Consuption`,
    RD      = RD,
    BBS     = `BBS: Broadband Subscription`,
    IU      = `IU: Internet use`,
    MPS     = `MPS: Mobile Phone Subscriptions`
  ) %>%
  mutate(
    Country = as.factor(Country),
    Income  = as.factor(Income),
    year    = as.integer(year)
  ) %>%
  arrange(Country, year)

#Lags (innerhalb jedes Landes, nicht über Ländergrenzen)


df <- df %>%
  group_by(Country) %>%
  mutate(
    DDI_lag1 = dplyr::lag(DDI, 1),
    BBS_lag1 = dplyr::lag(BBS, 1),
    IU_lag1  = dplyr::lag(IU,  1),
    MPS_lag1 = dplyr::lag(MPS, 1)
  ) %>%
  ungroup()

controls <- "GFCF + TO + Labor + LCPI + LPOP + consum + RD"

#LAG-MODELLE: GESAMTSTICHPROBE

cat("\nLag-Modelle: Gesamtstichprobe\n")

#DDI: zeitgleich + Lag
m_lag_ddi <- feols(
  as.formula(paste("LGDP ~ DDI + DDI_lag1 +", controls, "| Country + year")),
  data    = df,
  cluster = ~Country
)

#BBS: zeitgleich + Lag
m_lag_bbs <- feols(
  as.formula(paste("LGDP ~ BBS + BBS_lag1 +", controls, "| Country + year")),
  data    = df,
  cluster = ~Country
)

#IU: zeitgleich + Lag
m_lag_iu <- feols(
  as.formula(paste("LGDP ~ IU + IU_lag1 +", controls, "| Country + year")),
  data    = df,
  cluster = ~Country
)

#MPS: zeitgleich + Lag
m_lag_mps <- feols(
  as.formula(paste("LGDP ~ MPS + MPS_lag1 +", controls, "| Country + year")),
  data    = df,
  cluster = ~Country
)

#Konsolen-Übersicht
etable(
  m_lag_ddi, m_lag_bbs, m_lag_iu, m_lag_mps,
  headers  = c("DDI", "BBS", "IU", "MPS"),
  title    = "Lag-Modelle (Two-Way FE): Gesamtstichprobe",
  dict     = c(
    DDI     = "DDI (t)",     DDI_lag1 = "DDI (t–1)",
    BBS     = "BBS (t)",     BBS_lag1 = "BBS (t–1)",
    IU      = "IU (t)",      IU_lag1  = "IU (t–1)",
    MPS     = "MPS (t)",     MPS_lag1 = "MPS (t–1)",
    GFCF    = "Investitionen (GFCF)",
    TO      = "Handelsöffnung (TO)",
    Labor   = "Arbeitskraft (Labor)",
    LCPI    = "Inflation (LCPI)",
    LPOP    = "Bevölkerung (LPOP)",
    consum  = "Staatskonsum",
    RD      = "F&E (RD)"
  ),
  se.below = TRUE
)

#LAG-MODELLE: NACH INCOME-GRUPPEN (nur DDI als Hauptindikator)

cat("\nLag-Modelle: Nach Income-Gruppe (DDI)\n")

income_levels <- levels(df$Income)

models_lag_income <- lapply(income_levels, function(g) {
  dsub <- df %>% filter(Income == g)
  n_c  <- dplyr::n_distinct(dsub$Country)
  n_y  <- dplyr::n_distinct(dsub$year)
  if (n_c < 3 || n_y < 3) {
    message(sprintf("  Übersprungen: '%s' (%d Länder, %d Jahre)", g, n_c, n_y))
    return(NULL)
  }
  feols(
    as.formula(paste("LGDP ~ DDI + DDI_lag1 +", controls, "| Country + year")),
    data    = dsub,
    cluster = ~Country
  )
})
names(models_lag_income) <- income_levels
models_lag_income <- Filter(Negate(is.null), models_lag_income)

etable(
  models_lag_income,
  title    = "Lag-Modelle (Two-Way FE): Nach Income-Gruppe (DDI)",
  dict     = c(
    DDI     = "DDI (t)",  DDI_lag1 = "DDI (t–1)",
    GFCF    = "Investitionen (GFCF)", TO     = "Handelsöffnung (TO)",
    Labor   = "Arbeitskraft (Labor)", LCPI   = "Inflation (LCPI)",
    LPOP    = "Bevölkerung (LPOP)",   consum = "Staatskonsum",
    RD      = "F&E (RD)"
  ),
  se.below = TRUE
)

#ERGEBNISSE AUFBEREITEN (für Excel-Export)

model_to_df <- function(model, model_name = "") {
  ct  <- coeftable(model)
  dfm <- as.data.frame(ct)
  dfm$Variable  <- rownames(ct)
  dfm$Model     <- model_name
  dfm$N         <- nobs(model)
  dfm$R2_within <- round(r2(model, "wr2"), 4)
  pvals <- dfm[["Pr(>|t|)"]]
  dfm$Stars <- dplyr::case_when(
    pvals <= 0.01 ~ "***",
    pvals <= 0.05 ~ "**",
    pvals <= 0.10 ~ "*",
    TRUE          ~ ""
  )
  dfm <- dfm %>%
    select(Model, Variable, Estimate, `Std. Error`,
           `t value`, `Pr(>|t|)`, Stars, N, R2_within)
  rownames(dfm) <- NULL
  dfm
}

#Tabelle 9a: Gesamtstichprobe – alle vier Indikatoren
tbl9_full <- bind_rows(
  model_to_df(m_lag_ddi, "DDI"),
  model_to_df(m_lag_bbs, "BBS"),
  model_to_df(m_lag_iu,  "IU"),
  model_to_df(m_lag_mps, "MPS")
)

#Tabelle 9b: Nach Income-Gruppe (DDI)
tbl9_income <- bind_rows(
  lapply(names(models_lag_income), function(g)
    model_to_df(models_lag_income[[g]], g))
)

#Excel
writexl::write_xlsx(
  list(
    "Lag_Gesamt"        = tbl9_full,
    "Lag_IncomeGruppen" = tbl9_income
  ),
  path = paste0(out_dir, "Lag_Ergebnisse.xlsx")
)