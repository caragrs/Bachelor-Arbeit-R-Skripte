suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(plm)
  library(writexl)
})

options(error = NULL)
options(warn  = 0)

df_raw <- read_excel("/Users/caragross/Desktop/Bachelor_Arbeit/Cleaner_DF.xlsx")

df <- df_raw %>%
  rename(
    Country = Country,
    year    = Year,
    Income  = Income,
    LGDP    = LGDP,
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
    Country = as.character(Country),
    Income  = as.character(Income),
    year    = as.integer(year)
  ) %>%
  arrange(Income, Country, year)

vars  <- c("LGDP", "BBS", "IU", "MPS", "GFCF", "TO",
           "Labor", "LCPI", "LPOP", "consum", "RD")
alpha <- 0.05

MIN_OBS <- 8

run_cips <- function(df_sub, v, level, income) {

#Hilfsfunktion
  make_row <- function(stat = NA_real_, pval = NA_real_,
                       decision = NA_character_, error = NA_character_,
                       n_countries = NA_integer_) {
    data.frame(Income = income, Variable = v, Level = level,
               N_Countries = n_countries,
               Statistic = stat, P_Value = pval,
               Decision = decision, Error = error,
               stringsAsFactors = FALSE)
  }

#Differenzieren bei L1 (auf data.frame Ebene, nicht pseries)
  df_work <- df_sub %>% arrange(Country, year)

  if (level == "L1") {
    df_work <- df_work %>%
      group_by(Country) %>%
      mutate(!!v := c(NA_real_, diff(.data[[v]]))) %>%
      ungroup()
  }

#Länder mit zu wenigen gültigen Werten entfernen
  ok_countries <- df_work %>%
    group_by(Country) %>%
    summarise(n_ok = sum(!is.na(.data[[v]])), .groups = "drop") %>%
    filter(n_ok >= MIN_OBS) %>%
    pull(Country)

  n_ok <- length(ok_countries)

  if (n_ok < 2) {
    return(make_row(
      decision = "Too few countries with sufficient observations",
      error    = sprintf("Need >= 2 countries with >= %d non-NA obs; only %d qualify",
                         MIN_OBS, n_ok)
    ))
  }

  df_work <- df_work %>% filter(Country %in% ok_countries)

  #pdata.frame erstellen
  pdata_work <- try(
    pdata.frame(df_work, index = c("Country", "year"), drop.index = FALSE),
    silent = TRUE
  )
  if (inherits(pdata_work, "try-error")) {
    return(make_row(decision = "pdata.frame creation failed",
                    error = as.character(pdata_work)))
  }

  pseries_v <- pdata_work[[v]]

#Alle NA?
  if (all(is.na(suppressWarnings(as.numeric(pseries_v))))) {
    return(make_row(decision = "All values NA after filtering",
                    error    = "Series entirely NA"))
  }

#cipstest mit try() fängt auch interne Fehler sicher ab.
  test <- try(
    suppressWarnings(
      cipstest(x = pseries_v, lags = 1L, type = "trend",
               model = "cmg", truncated = FALSE)
    ),
    silent = TRUE
  )

  if (inherits(test, "try-error")) {
    err <- trimws(gsub("Error.*?:\n?", "", as.character(test)))
    return(make_row(decision = "Test failed", error = err))
  }

#Ergebnis
  stat <- suppressWarnings(as.numeric(test$statistic))
  pval <- suppressWarnings(as.numeric(test$p.value))

#p-Wert aus kritischen Werten ableiten wenn p.value NULL/NA
  if (is.na(pval) && !is.na(stat)) {
    cv <- try(as.numeric(test$critical.value), silent = TRUE)
    if (!inherits(cv, "try-error") && length(cv) >= 3) {
      #cv[1]=1%, cv[2]=5%, cv[3]=10% (linksseitig: kleiner = signifikanter)
      pval <- dplyr::case_when(
        stat <= cv[1] ~ 0.01,
        stat <= cv[2] ~ 0.05,
        stat <= cv[3] ~ 0.10,
        TRUE          ~ 0.15
      )
    }
  }

  decision <- dplyr::case_when(
    is.na(pval)  ~ "P-value unavailable",
    pval <= 0.01 ~ "Reject H0 (stationary) ***",
    pval <= 0.05 ~ "Reject H0 (stationary) **",
    pval <= 0.10 ~ "Reject H0 (stationary) *",
    TRUE         ~ "Fail to reject H0 (unit root)"
  )

  make_row(stat = stat, pval = pval, decision = decision, n_countries = n_ok)
}

#Hauptschleife
results <- list()

for (inc in unique(df$Income)) {

  df_i <- df %>% filter(Income == inc)

  if (dplyr::n_distinct(df_i$Country) < 2) {
    message("Überspringe '", inc, "': weniger als 2 Länder")
    next
  }

  for (v in vars) {
    if (!v %in% names(df_i)) next

    vals_numeric <- suppressWarnings(as.numeric(df_i[[v]]))
    if (sum(!is.na(vals_numeric)) < 10) next

    results[[paste(inc, v, "L0", sep = "_")]] <- run_cips(df_i, v, "L0", inc)
    results[[paste(inc, v, "L1", sep = "_")]] <- run_cips(df_i, v, "L1", inc)

    message(sprintf("  OK  %-25s | %-10s | L0 & L1", inc, v))
  }
}

#Ergebnis zusammenführen
if (length(results) == 0) {
  results_df <- data.frame(
    Income = NA, Variable = NA, Level = NA, N_Countries = NA,
    Statistic = NA, P_Value = NA,
    Decision = "No results, check Income groups / variables",
    Error = NA, stringsAsFactors = FALSE
  )
} else {
  results_df <- dplyr::bind_rows(results) %>%
    dplyr::arrange(Income, Variable, Level)
}

#Export
out_path <- "/Users/caragross/Desktop/Bachelor_Arbeit/CIPS_results_by_income_L0_L1_lag1.xlsx"

writexl::write_xlsx(
  list(
    "CIPS_All"    = results_df,
    "CIPS_L0"     = results_df %>% dplyr::filter(Level == "L0"),
    "CIPS_L1"     = results_df %>% dplyr::filter(Level == "L1"),
    "CIPS_Errors" = results_df %>% dplyr::filter(!is.na(Error))
  ),
  out_path
)

message(sprintf("Gesamt: %d  |  Erfolgreich: %d  |  Fehler: %d",
                nrow(results_df),
                sum(is.na(results_df$Error)),
                sum(!is.na(results_df$Error))))
