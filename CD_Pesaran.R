library(readxl)
library(dplyr)
library(plm)
library(writexl)

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
  arrange(Country, year)

vars <- c("LGDP","BBS","IU","MPS","GFCF","TO","Labor","LCPI","LPOP","consum","RD")
df <- df %>% mutate(across(all_of(vars), ~ suppressWarnings(as.numeric(.x))))

fe_formula <- LGDP ~ DIGI + GFCF + TO + Labor + LCPI + LPOP + consum + RD

digital_vars <- c("BBS","IU","MPS")

#Helper
cd_row <- function(ht, digi, sample) {
  data.frame(
    Sample    = sample,
    Digital   = digi,
    Test      = ht$method,
    Statistic = as.numeric(ht$statistic),
    P_Value   = as.numeric(ht$p.value),
    stringsAsFactors = FALSE
  )
}

#CD-Test: Full Sample (Two-Way FE)
results_full <- list()

for (d in digital_vars) {
  
  fml <- update(fe_formula, . ~ . - DIGI + get(d))
  
  ht <- try(
    pcdtest(
      x      = fml,
      data   = df,
      index  = c("Country","year"),
      model  = "within",
      effect = "twoways",
      test   = "cd"
    ),
    silent = TRUE
  )
  
  results_full[[d]] <-
    if (inherits(ht, "try-error")) {
      data.frame(
        Sample="Full sample",
        Digital=d,
        Test="Pesaran CD (failed)",
        Statistic=NA_real_,
        P_Value=NA_real_,
        stringsAsFactors = FALSE
      )
    } else cd_row(ht, d, "Full sample")
}

cd_full_df <- bind_rows(results_full)

#CD-Test nach Income-Gruppen
results_income <- list()

for (inc in unique(df$Income)) {
  
  df_i <- df %>% filter(Income == inc)
  if (n_distinct(df_i$Country) < 2) next
  
  for (d in digital_vars) {
    
    fml <- update(fe_formula, . ~ . - DIGI + get(d))
    
    ht <- try(
      pcdtest(
        x      = fml,
        data   = df_i,
        index  = c("Country","year"),
        model  = "within",
        effect = "twoways",
        test   = "cd"
      ),
      silent = TRUE
    )
    
    results_income[[paste(inc,d,sep="_")]] <-
      if (inherits(ht, "try-error")) {
        data.frame(
          Sample=paste0("Income: ",inc),
          Digital=d,
          Test="Pesaran CD (failed)",
          Statistic=NA_real_,
          P_Value=NA_real_,
          stringsAsFactors = FALSE
        )
      } else cd_row(ht, d, paste0("Income: ",inc))
  }
}

cd_income_df <- bind_rows(results_income)

#Export
write_xlsx(
  list(
    "CD_FullSample_FE" = cd_full_df,
    "CD_ByIncome_FE"  = cd_income_df
  ),
  "/Users/caragross/Desktop/Bachelor_Arbeit/Pesaran_CD_FE.xlsx"
)