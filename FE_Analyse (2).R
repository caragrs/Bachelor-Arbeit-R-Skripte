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

controls <- c("GFCF", "TO", "Labor", "LCPI", "LPOP", "consum", "RD")

var_labels <- c(
  DDI    = "Digitalisierungsindex (DDI)",
  BBS    = "Breitband (BBS)",
  IU     = "Internetnutzung (IU)",
  MPS    = "Mobilfunk (MPS)",
  GFCF   = "Investitionen (GFCF)",
  TO     = "Handelsöffnung (TO)",
  Labor  = "Arbeitskraft (Labor)",
  LCPI   = "Inflation (LCPI)",
  LPOP   = "Bevölkerung (LPOP)",
  consum = "Staatskonsum (consum)",
  RD     = "F&E (RD)"
)

#DDI-Modell + Einzelmodelle

m_ddi <- feols(
  LGDP ~ DDI + GFCF + TO + Labor + LCPI + LPOP + consum + RD | Country + year,
  data    = df,
  cluster = ~Country
)

# Einzelmodelle der Komponenten
m_bbs <- feols(
  LGDP ~ BBS + GFCF + TO + Labor + LCPI + LPOP + consum + RD | Country + year,
  data    = df,
  cluster = ~Country
)

m_iu <- feols(
  LGDP ~ IU + GFCF + TO + Labor + LCPI + LPOP + consum + RD | Country + year,
  data    = df,
  cluster = ~Country
)

m_mps <- feols(
  LGDP ~ MPS + GFCF + TO + Labor + LCPI + LPOP + consum + RD | Country + year,
  data    = df,
  cluster = ~Country
)

# Ausgabe Konsole
cat("\nTABELLE 1: Gesamtstichprobe\n")
etable(
  m_ddi, m_bbs, m_iu, m_mps,
  headers  = c("DDI (Index)", "BBS", "IU", "MPS"),
  title    = "Two-Way FE: Gesamtstichprobe",
  dict     = var_labels,
  se.below = TRUE
)

#DDI-Modell nach Income (Hauptspezifikation)

income_levels <- levels(df$Income)

models_ddi_income <- lapply(income_levels, function(g) {
  dsub <- df %>% filter(Income == g)
  
  n_countries <- dplyr::n_distinct(dsub$Country)
  n_years     <- dplyr::n_distinct(dsub$year)
  
  if (n_countries < 3 || n_years < 3) {
    message(sprintf("  Warnung: '%s' hat nur %d Länder und %d Jahre – Modell übersprungen.", 
                    g, n_countries, n_years))
    return(NULL)
  }
  
  feols(
    LGDP ~ DDI + GFCF + TO + Labor + LCPI + LPOP + consum + RD | Country + year,
    data    = dsub,
    cluster = ~Country
  )
})

names(models_ddi_income) <- income_levels

# NULL-Einträge entfernen (falls eine Gruppe übersprungen wurde)
models_ddi_income <- Filter(Negate(is.null), models_ddi_income)

cat("\nTABELLE 2: Nach Income-Gruppe (DDI-Modell)\n")
etable(
  models_ddi_income,
  title    = "Two-Way FE nach Income-Gruppe: Digitalisierungsindex (DDI)",
  dict     = var_labels,
  se.below = TRUE
)

#STANDARDISIERTE KOEFFIZIENTEN (globale Standardisierung)
#Welche Variable hat den grössten relativen Einfluss?
#Standardisierung global (nicht per Income-Gruppe),damit Beta-Koeffizienten über Gruppen vergleichbar bleiben.

z_global <- function(x) as.numeric(scale(x))   #Mittelwert=0,SD=1,global

all_vars_std <- c("LGDP", "DDI", controls)

df_std <- df %>%
  mutate(across(all_of(all_vars_std), z_global, .names = "{.col}_z"))

#Standardisiertes Gesamtmodell
m_std_all <- feols(
  LGDP_z ~ DDI_z + GFCF_z + TO_z + Labor_z + LCPI_z + LPOP_z + consum_z + RD_z |
    Country + year,
  data    = df_std,
  cluster = ~Country
)

cat("\nStandardisierte Koeffizienten (Gesamtstichprobe)\n")
coef_all_std <- coef(m_std_all)
print(sort(abs(coef_all_std), decreasing = TRUE))

# Standardisierte Modelle je Income-Gruppe
std_labels <- setNames(
  paste0(gsub(" ", "_", all_vars_std), "_z"),
  all_vars_std
)

models_std_income <- lapply(income_levels, function(g) {
  dsub_std <- df_std %>% filter(Income == g)
  n_countries <- dplyr::n_distinct(dsub_std$Country)
  n_years     <- dplyr::n_distinct(dsub_std$year)
  if (n_countries < 3 || n_years < 3) return(NULL)
  
  feols(
    LGDP_z ~ DDI_z + GFCF_z + TO_z + Labor_z + LCPI_z + LPOP_z + consum_z + RD_z |
      Country + year,
    data    = dsub_std,
    cluster = ~Country
  )
})
names(models_std_income) <- income_levels
models_std_income <- Filter(Negate(is.null), models_std_income)

cat("\nStandardisierte Koeffizienten nach Income-Gruppe\n")
for (g in names(models_std_income)) {
  cat(sprintf("\n--- %s ---\n", g))
  coef_g <- coef(models_std_income[[g]])
  print(sort(abs(coef_g), decreasing = TRUE))
}

#Export
model_to_df <- function(model, model_name = "") {
  ct   <- coeftable(model)
  df_m <- as.data.frame(ct)
  df_m$Variable  <- rownames(ct)
  df_m$Model     <- model_name
  df_m$N         <- nobs(model)
  df_m$R2_within <- round(r2(model, type = "wr2"), 4)
  
  #Signifikanzsterne
  pvals <- df_m[["Pr(>|t|)"]]
  df_m$Stars <- dplyr::case_when(
    pvals <= 0.01 ~ "***",
    pvals <= 0.05 ~ "**",
    pvals <= 0.10 ~ "*",
    TRUE          ~ ""
  )
  
  df_m <- df_m %>%
    select(Model, Variable, Estimate, `Std. Error`, `t value`,
           `Pr(>|t|)`, Stars, N, R2_within)
  rownames(df_m) <- NULL
  df_m
}

#Tabelle 1: Gesamtstichprobe (DDI + Einzelmodelle)
tbl1 <- bind_rows(
  model_to_df(m_ddi, "DDI (Index)"),
  model_to_df(m_bbs, "BBS"),
  model_to_df(m_iu,  "IU"),
  model_to_df(m_mps, "MPS")
)

#Tabelle 2: Income-Gruppen (DDI-Modell)
tbl2 <- bind_rows(
  lapply(names(models_ddi_income), function(g)
    model_to_df(models_ddi_income[[g]], g))
)

#Tabelle 3: Standardisierte Koeffizienten – Gesamtstichprobe
tbl3 <- data.frame(
  Variable = names(coef_all_std),
  Beta     = round(as.numeric(coef_all_std), 4),
  Abs_Beta = round(abs(as.numeric(coef_all_std)), 4)
) %>% arrange(desc(Abs_Beta))

#Tabelle 4: Standardisierte Koeffizienten je Income-Gruppe
tbl4 <- bind_rows(
  lapply(names(models_std_income), function(g) {
    coef_g <- coef(models_std_income[[g]])
    data.frame(
      Income_Group = g,
      Variable     = names(coef_g),
      Beta         = round(as.numeric(coef_g), 4),
      Abs_Beta     = round(abs(as.numeric(coef_g)), 4)
    )
  })
) %>% arrange(Income_Group, desc(Abs_Beta))

# Excel-Export
writexl::write_xlsx(
  list(
    "FE_Gesamt"          = tbl1,
    "FE_IncomeGruppen"   = tbl2,
    "Beta_Gesamt"        = tbl3,
    "Beta_IncomeGruppen" = tbl4
  ),
  path = paste0(out_dir, "FE_Ergebnisse.xlsx")
)