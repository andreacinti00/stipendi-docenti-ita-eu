# ==============================================================================
# 05_confronto_netto_europa.R
# Confronto europeo salari NETTI (OCSE Education at a Glance + Taxing Wages)
# ==============================================================================

library(tidyverse)
library(scales)

# 1. Caricamento e pulizia OCSE Taxing Wages -----------------------------------
tw_raw <- read_csv("01_data_raw/oecd_taxing_wages_raw.csv", show_col_types = FALSE)

nomi_paesi_tw <- c(
  "ITA"  = "Italia",
  "FRA"  = "Francia",
  "DEU"  = "Germania",
  "BEL"  = "Belgio",
  "ESP"  = "Spagna",
  "POL"  = "Polonia",
  "GRC"  = "Grecia",
  "OECD_REP" = "Media OCSE"
)

tw_clean <- tw_raw %>%
  transmute(
    ref_area    = REF_AREA,
    paese_it    = recode(REF_AREA, !!!nomi_paesi_tw),
    anno        = as.integer(TIME_PERIOD),
    livello_aw  = parse_number(as.character(INCOME_PRINCIPAL)), # 67, 100, 167
    aliquota_pct = as.numeric(OBS_VALUE)
  ) %>%
  filter(!is.na(aliquota_pct), !is.na(livello_aw))

write_csv(tw_clean, "02_data_clean/oecd_taxing_wages_pulito.csv")

# Ultimo anno disponibile nel dataset Taxing Wages
ultimo_anno_tw <- max(tw_clean$anno, na.rm = TRUE)
cat("\n=== ALIQUOTE EFFETTIVE NETTE (SINGLE, NO FIGLI) - ANNO", ultimo_anno_tw, "===\n")

tw_latest <- tw_clean %>%
  filter(anno == ultimo_anno_tw) %>%
  select(paese_it, livello_aw, aliquota_pct) %>%
  pivot_wider(names_from = livello_aw, values_from = aliquota_pct, names_prefix = "AW_") %>%
  arrange(desc(AW_100))

print(tw_latest, width = Inf)

tabella_ppp <- read_csv("02_data_clean/tabella_ppp.csv", show_col_types = FALSE)

# ==============================================================================
# 2. Calcolo Indice Salari NETTI in Europa (EXP15, Italia = 100)
# ==============================================================================

df_lordo_netto <- tabella_ppp %>%
  select(paese_it, lordo_idx = EXP15) %>%
  inner_join(tw_latest %>% select(paese_it, aliq_100 = AW_100), by = "paese_it") %>%
  mutate(
    # Applichiamo (1 - aliquota) al lordo indicizzato
    netto_raw = lordo_idx * (1 - aliq_100 / 100),
    # Ri-indicizziamo ponendo Italia Netto = 100
    netto_idx = netto_raw / netto_raw[paese_it == "Italia"] * 100,
    delta_idx = round(netto_idx - lordo_idx, 1)
  ) %>%
  arrange(desc(netto_idx))

cat("\n=== CONFRONTO LORDO vs NETTO IN EUROPA (EXP15, ITALIA = 100) ===\n")
df_lordo_netto %>%
  select(paese_it, aliq_100, lordo_idx, netto_idx, delta_idx) %>%
  mutate(across(c(aliq_100, lordo_idx, netto_idx), ~ round(.x, 1))) %>%
  print(width = Inf)

write_csv(df_lordo_netto, "02_data_clean/europa_lordo_vs_netto_2025.csv")

# ==============================================================================
# 3. Grafico 7: Dumbbell chart Lordo vs Netto in Europa (2025)
# ==============================================================================

df_plot_ln <- df_lordo_netto %>%
  mutate(
    paese_it = fct_reorder(paese_it, netto_idx),
    col_netto = case_when(
      paese_it == "Italia"     ~ "#d73027",
      paese_it == "Media OCSE" ~ "grey20",
      TRUE                     ~ "#1f78b4"
    )
  )

p7_europa_netto <- ggplot(df_plot_ln, aes(y = paese_it)) +
  geom_vline(xintercept = 100, linetype = "dashed", color = "grey55") +
  # Segmento che collega Lordo e Netto
  geom_segment(
    aes(x = lordo_idx, xend = netto_idx, yend = paese_it),
    color = "grey70", linewidth = 1.6
  ) +
  # Punto Lordo
  geom_point(aes(x = lordo_idx, shape = "Lordo (PPP)"), color = "grey50", size = 4) +
  # Punto Netto
  geom_point(aes(x = netto_idx, color = I(col_netto), shape = "Netto stimato (PPP)"), size = 4.5) +
  # Etichetta valore Netto
  # Etichetta valore Netto
  geom_label(
    aes(
      x     = ifelse(netto_idx >= lordo_idx, netto_idx + 3.5, netto_idx - 3.5),
      hjust = ifelse(netto_idx >= lordo_idx, 0, 1),
      label = round(netto_idx, 1),
      color = I(col_netto)
    ),
    fill          = "white",
    label.size    = 0,
    label.padding = unit(0.12, "lines"),
    size          = 3.5,
    fontface      = "bold"
  ) +
  scale_shape_manual(
    name   = NULL,
    values = c("Lordo (PPP)" = 16, "Netto stimato (PPP)" = 16)
  ) +
  scale_x_continuous(breaks = seq(60, 220, by = 20), limits = c(50, 230)) +
  labs(
    title    = "Salari dei docenti in Europa: dal Lordo al Netto in PPP (2025)",
    subtitle = "Scuola secondaria II grado, 15 anni di esperienza | Indice Italia = 100 (aliquota OCSE single 100% AW)",
    x        = "Indice salario reale in PPP (Italia = 100)",
    y        = NULL,
    caption  = "Fonte: OCSE Education at a Glance + Taxing Wages 2025.\nNota: per la Germania l'aliquota si riferisce ai dipendenti ordinari; i docenti di ruolo (Beamte) godono di un netto superiore."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position    = "top"
  )

print(p7_europa_netto)
ggsave("04_output/07_europa_lordo_vs_netto_2025.png", p7_europa_netto,
       width = 10.5, height = 6, dpi = 300, bg = "white")

cat("\nGrafico 07 salvato correttamente in 04_output/!\n")
# ==============================================================================
# 4. Grafico 8: Evoluzione pressione fiscale in Europa (2000-2025, 100% AW)
# ==============================================================================

df_tax_trend <- tw_clean %>%
  filter(livello_aw == 100, anno >= 2000, anno <= 2025) %>%
  mutate(
    paese_it = factor(
      paese_it,
      levels = c("Belgio", "Germania", "Italia", "Francia", "Grecia", "Media OCSE", "Polonia", "Spagna")
    )
  )

# Tabella sintetica variazione 2000 -> 2025 (punti percentuali)
cat("\n=== VARIAZIONE PRESSIONE FISCALE 2000-2025 (SINGLE 100% AW) ===\n")
df_tax_trend %>%
  filter(anno %in% c(2000, 2025)) %>%
  select(paese_it, anno, aliquota_pct) %>%
  pivot_wider(names_from = anno, values_from = aliquota_pct, names_prefix = "y_") %>%
  mutate(delta_pp = round(y_2025 - y_2000, 1)) %>%
  arrange(desc(y_2025)) %>%
  print(width = Inf)

palette_eu <- c(
  "Italia"     = "#d73027",
  "Media OCSE" = "#222222",
  "Belgio"     = "#4575b4",
  "Germania"   = "#1b9e77",
  "Francia"    = "#7570b3",
  "Grecia"     = "#e6ab02",
  "Polonia"    = "#a6761d",
  "Spagna"     = "#66a61e"
)

p8_pressione_eu <- ggplot(
  df_tax_trend,
  aes(x = anno, y = aliquota_pct, color = paese_it)
) +
  geom_line(
    aes(
      linewidth = ifelse(paese_it == "Italia", 1.4, 0.9),
      linetype  = ifelse(paese_it == "Media OCSE", "dashed", "solid")
    )
  ) +
  scale_linewidth_identity() +
  scale_linetype_identity() +
  scale_color_manual(values = palette_eu, name = "Paese") +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
  scale_y_continuous(
    labels = label_percent(scale = 1, accuracy = 1),
    breaks = seq(15, 45, by = 5)
  ) +
  labs(
    title    = "Evoluzione della pressione fiscale effettiva in Europa (2000–2025)",
    subtitle = "Net personal average tax rate (% del lordo) | Lavoratore single senza figli al 100% del salario medio",
    x        = NULL,
    y        = "Aliquota effettiva netta (%)",
    caption  = "Fonte: elaborazione su dati OCSE Taxing Wages (2000–2025)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position  = "right"
  )

print(p8_pressione_eu)
ggsave("04_output/08_europa_pressione_fiscale_2000_2025.png", p8_pressione_eu,
       width = 10.5, height = 6, dpi = 300, bg = "white")

cat("\nGrafico 08 salvato correttamente in 04_output/!\n")
# ==============================================================================
# 5. Grafico 9: La "trappola della progressività" in Europa (2025)
#    Confronto aliquote effettive al 67%, 100% e 167% del salario medio (AW)
# ==============================================================================

df_prog <- tw_latest %>%
  mutate(
    spread_pp = round(AW_167 - AW_67, 1),
    paese_it  = fct_reorder(paese_it, spread_pp),
    is_ita    = paese_it == "Italia"
  )

cat("\n=== PROGRESSIVITA' FISCALE IN EUROPA (67% -> 167% AW, ANNO", ultimo_anno_tw, ") ===\n")
df_prog %>%
  arrange(desc(spread_pp)) %>%
  select(paese_it, AW_67, AW_100, AW_167, spread_pp) %>%
  print(width = Inf)

p9_progressivita <- ggplot(df_prog, aes(y = paese_it)) +
  # Segmento di escursione fiscale (67% -> 167% AW)
  geom_segment(
    aes(
      x     = AW_67,
      xend  = AW_167,
      yend  = paese_it,
      color = ifelse(is_ita, "#d73027", "grey70")
    ),
    linewidth = 2.2,
    show.legend = FALSE
  ) +
  scale_color_identity() +
  # Punti per i tre livelli di reddito
  geom_point(aes(x = AW_67,  fill = "67% AW (ingresso)"),   shape = 21, size = 4,   color = "white", stroke = 0.6) +
  geom_point(aes(x = AW_100, fill = "100% AW (medio)"),     shape = 21, size = 4,   color = "white", stroke = 0.6) +
  geom_point(aes(x = AW_167, fill = "167% AW (medio-alto)"), shape = 21, size = 4.5, color = "white", stroke = 0.6) +
  # Etichette estremi (67% a sinistra, 167% a destra)
  geom_text(
    aes(x = AW_67 - 1.2, label = paste0(round(AW_67, 1), "%")),
    hjust = 1, size = 3.2, color = "grey30"
  ) +
  geom_text(
    aes(x = AW_167 + 1.2, label = paste0(round(AW_167, 1), "%")),
    hjust = 0, size = 3.2, color = "grey30"
  ) +
  # Colonna destra con il salto complessivo (+p.p.)
  geom_text(
    aes(
      x        = 54.5,
      label    = paste0("+", format(spread_pp, nsmall = 1), " p.p."),
      fontface = ifelse(is_ita, "bold", "plain"),
      color    = ifelse(is_ita, "#d73027", "grey25")
    ),
    hjust = 0, size = 3.6
  ) +
  annotate(
    "text", x = 54.5, y = 8.45,
    label = "Salto\n67%→167%", hjust = 0, size = 3, fontface = "bold", color = "grey35"
  ) +
  scale_fill_manual(
    name   = "Livello di reddito",
    values = c(
      "67% AW (ingresso)"    = "#74add1",
      "100% AW (medio)"      = "#4575b4",
      "167% AW (medio-alto)" = "#08306b"
    ),
    breaks = c("67% AW (ingresso)", "100% AW (medio)", "167% AW (medio-alto)")
  ) +
  scale_x_continuous(
    labels = label_percent(scale = 1, accuracy = 1),
    breaks = seq(15, 50, by = 5),
    limits = c(14, 60.5)
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title    = "La trappola della progressività fiscale in Europa (2025)",
    subtitle = "Aliquota effettiva netta per lavoratore single senza figli al 67%, 100% e 167% del salario medio (AW)",
    x        = "Aliquota effettiva netta (% del lordo)",
    y        = NULL,
    caption  = "Fonte: elaborazione su dati OCSE Taxing Wages 2025. Paesi ordinati per ampiezza del salto fiscale (167% − 67% AW)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position    = "top",
    plot.margin        = margin(10, 15, 10, 10)
  )

print(p9_progressivita)
ggsave("04_output/09_europa_progressivita_fiscale_2025.png", p9_progressivita,
       width = 11, height = 6, dpi = 300, bg = "white")

cat("\nGrafico 09 salvato correttamente in 04_output/!\n")