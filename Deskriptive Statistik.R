library(dplyr)
library(tidyr)
library(writexl)

df_raw <- readxl::read_excel("/Users/caragross/Desktop/Bachelor_Arbeit/Cleaner_DF.xlsx")
names(df_raw) <- trimws(names(df_raw))

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
  ) 

vars <- c("LGDP", "BBS", "MPS", "IU", "GFCF", "TO", "Labor", "LCPI", "LPOP", "consum", "RD")

missing_vars <- setdiff(vars, names(df))
if (length(missing_vars) > 0) {
  stop(paste("Diese Variablen fehlen in df:", paste(missing_vars, collapse = ", ")))
}

#Helper-Funktion für Summary-Stats
desc_stats <- function(data, vars) {
  data %>%
    summarise(
      N_obs = n(),
      N_countries = n_distinct(Country),
      year_min = min(year, na.rm = TRUE),
      year_max = max(year, na.rm = TRUE),
      across(all_of(vars),
             list(
               mean = ~ mean(.x, na.rm = TRUE),
               sd   = ~ sd(.x, na.rm = TRUE),
               min  = ~ min(.x, na.rm = TRUE),
               max  = ~ max(.x, na.rm = TRUE),
               n    = ~ sum(!is.na(.x))
             ),
             .names = "{.col}__{.fn}"
      )
    ) %>%
    pivot_longer(
      cols = matches("__"),
      names_to = c("Variable", "Stat"),
      names_sep = "__",
      values_to = "Value"
    ) %>%
    pivot_wider(names_from = Stat, values_from = Value) %>%
    select(Variable, n, mean, sd, min, max)
}

#Alle
desc_all <- desc_stats(df, vars)

#Nach Einkommensgruppen
desc_by_income <- df %>%
  group_by(Income) %>%
  group_modify(~ desc_stats(.x, vars)) %>%
  ungroup()

#Panel-Übersicht pro Income (N Länder, Jahre)
panel_overview <- df %>%
  group_by(Income) %>%
  summarise(
    N_obs = n(),
    N_countries = n_distinct(Country),
    year_min = min(year, na.rm = TRUE),
    year_max = max(year, na.rm = TRUE),
    .groups = "drop"
  )

#Export als Excel
write_xlsx(
  list(
    "Desc_All" = desc_all,
    "Desc_By_Income" = desc_by_income,
    "Panel_Overview" = panel_overview
  ),
  path = "/Users/caragross/Desktop/Bachelor_Arbeit/Descriptive_Statistics.xlsx"
)
