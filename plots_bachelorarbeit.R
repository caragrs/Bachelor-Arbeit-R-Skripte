install.packages("extrafont")
library(dplyr)
library(ggplot2)
library(extrafont)
font_import(prompt = FALSE)  
loadfonts(device = "win")

pfad <- "/Users/caragross/Desktop/Bachelor_Arbeit/"

# PLOT 1: Dot-and-Whisker – DDI-Koeffizienten (unstandardisiert)

df_coef <- data.frame(
  gruppe = factor(
    c("High Income", "Low Income", "Lower Middle Income", "Upper Middle Income"),
    levels = c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income")
  ),
  b    = c(-0.026, -0.028,  0.175,  0.125),
  se   = c( 0.035,  0.031,  0.070,  0.050),
  sig  = c("n.s.", "n.s.", "p < .05", "p < .05")
)

df_coef <- df_coef %>%
  mutate(
    ci_low  = b - 1.96 * se,
    ci_high = b + 1.96 * se
  )

plot1 <- ggplot(df_coef, aes(x = b, y = gruppe, color = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.6) +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high),
                 height = 0.2, linewidth = 0.8) +
  geom_point(size = 4) +
  scale_color_manual(
    values = c("p < .05" = "#2166ac", "n.s." = "#b2b2b2"),
    name = "Signifikanz"
  ) +
  labs(
    title    = "Abbildung 1",
    subtitle = "DDI-Koeffizient nach Einkommensgruppe (95%-KI)",
    x        = "Unstandardisierter Koeffizient (b)",
    y        = NULL,
    caption  = "Anmerkung. Two-Way Fixed Effects; geclusterte Standardfehler auf Laenderebene.\nReferenzlinie bei b = 0."
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman", size = 12),
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(size = 11, color = "grey30"),
    plot.caption       = element_text(size = 9, color = "grey40", hjust = 0),
    legend.position    = "right",
    axis.text.y        = element_text(size = 11),
    panel.grid.major.x = element_line(color = "grey92")
  )

ggsave(paste0(pfad, "plot1_koeffizientenplot.png"), plot = plot1,
       width = 7, height = 4, dpi = 300, bg = "white")

# PLOT 2: Balkendiagramm – Standardisierte Beta-Koeffizienten

df_beta <- data.frame(
  gruppe = factor(
    c("High Income", "Low Income", "Lower Middle Income", "Upper Middle Income"),
    levels = c("High Income", "Low Income", "Lower Middle Income", "Upper Middle Income")
  ),
  beta = c(-0.018, -0.019, 0.119, 0.085),
  sig  = c("n.s.", "n.s.", "p < .05", "p < .05")
)

plot2 <- ggplot(df_beta, aes(x = gruppe, y = beta, fill = sig)) +
  geom_col(width = 0.6, color = "white") +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.5) +
  geom_text(
    aes(
      label = ifelse(sig == "p < .05", "**", ""),
      vjust = -0.3
    ),
    size = 5, color = "grey20"
  ) +
  coord_cartesian(ylim = c(-0.06, 0.17)) +
  scale_fill_manual(
    values = c("p < .05" = "#2166ac", "n.s." = "#b2b2b2"),
    name = "Signifikanz"
  ) +
  scale_x_discrete(labels = c(
    "High Income"         = "High\nIncome",
    "Low Income"          = "Low\nIncome",
    "Lower Middle Income" = "Lower Middle\nIncome",
    "Upper Middle Income" = "Upper Middle\nIncome"
  )) +
  labs(
    title    = "Abbildung 2",
    subtitle = "Standardisierte DDI-Koeffizienten nach Einkommensgruppe",
    x        = NULL,
    y        = "Standardisierter Koeffizient (Beta)",
    caption  = "Anmerkung. Beta-Koeffizienten auf Basis globaler Standardabweichungen.\n** p < .05; n.s. = nicht signifikant."
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "Times New Roman", size = 12),
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(size = 11, color = "grey30"),
    plot.caption       = element_text(size = 9, color = "grey40", hjust = 0),
    legend.position    = "right",
    axis.text.x        = element_text(size = 10),
    panel.grid.major.y = element_line(color = "grey92")
  )

ggsave(paste0(pfad, "plot2_beta_koeffizienten.png"), plot = plot2,
       width = 7, height = 4.5, dpi = 300, bg = "white")
cat("\nBeide Plots wurden gespeichert unter:", pfad, "\n")
