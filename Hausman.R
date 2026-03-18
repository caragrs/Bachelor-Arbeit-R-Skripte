library(readxl)
library(dplyr)
library(tidyr)
library(lme4)
library(writexl)

df_raw <- read_excel("/Users/caragross/Desktop/Bachelor_Arbeit/Cleaner_DF.xlsx")
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
  ) %>%
  mutate(
    Country = as.factor(Country),
    Income  = as.factor(Income),
    year    = as.integer(year),
    year_factor = as.factor(year) 
  ) %>%
  arrange(Country, year)

#vollständige Fälle
df_h <- df %>%
  select(LGDP, BBS, IU, MPS, GFCF, TO, Labor, LCPI, LPOP, consum, RD, Country, year_factor) %>%
  drop_na()

# Hausmann Code von Github (https://rdrr.io/github/lrocconi/mlmhelpr/src/R/hausman.R)

hausman <- function(re_model) {
  # Extract data from the model
  data <- re_model@frame
  
  # Extract random effect names
  grps <- as.data.frame(lme4::VarCorr(re_model))[1]
  groups <- subset(grps, grps != "Residual")
  groups$grp <- paste0("as.factor(", groups$grp, ")")
  groups <- utils::capture.output(cat(groups[, 1], sep = " + "))
  
  # Extract fixed effect names using terms function
  fixed_terms <- attr(terms(re_model), "term.labels")
  
  # Concatenate fixed effect names
  fixed <- utils::capture.output(cat(fixed_terms, sep = " + "))
  
  # Set intercept
  intercept <- if ("(Intercept)" %in% fixed_terms) {
    1
  } else {
    0
  }
  
  # Rebuild formula for fixed effects model
  dv <- as.character(re_model@call[["formula"]][[2]])
  fe_formula <- paste0(dv, " ~ ", intercept, " + ", fixed, " + ", groups)
  
  # Estimate fixed effects model
  fe_model <- stats::lm(as.formula(fe_formula), data = data)
  
  # Begin Hausman test
  fe_coef <- stats::coef(fe_model)
  re_coef <- lme4::fixef(re_model)
  fe_vcov <- stats::vcov(fe_model)
  re_vcov <- stats::vcov(re_model)
  fe_names <- names(fe_coef)
  re_names <- names(re_coef)
  common_coef_names <- re_names[re_names %in% fe_names]
  coefs <- common_coef_names[!(common_coef_names %in% "(Intercept)")] # drop intercept if included
  
  betas <- fe_coef[coefs] - re_coef[coefs]
  vcovs <- fe_vcov[coefs, coefs] - re_vcov[coefs, coefs]
  
  z <- as.numeric(abs(t(betas) %*% solve(vcovs) %*% betas))
  df <- length(betas)
  p <- stats::pchisq(z, df, lower.tail = FALSE)
  
  # Prep results
  stat <- z
  names(stat) <- "chi-square"
  parameter <- df
  names(parameter) <- "df"
  alpha = .05
  
  results <- list(statistic  = stat,
                  p.value      = p,
                  parameter    = parameter,
                  method       = "Hausman Test",
                  data.name    = "hsb"
  )
  class(results) <- "htest" # Object of class "htest"
  
  # Check for random slopes
  varcorr_df <- as.data.frame(lme4::VarCorr(re_model))
  if (sum(!is.na(varcorr_df$var2)) > 0) {
    warning("Random slopes detected! Interpret with caution.\nSee ?mlmhelpr::de() for more information.")
  }
  
  # Caution: might have gotten this backwards!
  message_text <- if (p < .05) {
    "\n\nResults are significantly different. \nThe multilevel model may not be suitable."
  } else {
    "\n\nResults are not significantly different. \nThe multilevel model may be more suitable."
  }
  
  message(message_text)
  return(results)
}

re_model <- lmer(
  LGDP ~ BBS + IU + MPS + GFCF + TO + Labor + LCPI + LPOP + consum + RD + year_factor + (1 | Country),
  data = df_h,
  REML = FALSE
)

#Hausman-Test ausführen
ht <- hausman(re_model)
print(ht)

#Export

ht_df <- data.frame(
  statistic = unname(ht$statistic),
  parameter = unname(ht$parameter),
  p_value   = unname(ht$p.value),
  method    = ht$method,
  stringsAsFactors = FALSE
)

write_xlsx(
  list(
    "Hausman_Test" = ht_df
  ),
  "/Users/caragross/Desktop/Bachelor_Arbeit/Hausman.xlsx"
)
