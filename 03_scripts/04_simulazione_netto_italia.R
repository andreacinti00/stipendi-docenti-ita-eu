# ==============================================================================
# 04_simulazione_netto_italia.R
# Microsimulazione fiscale storica (2000-2025) - Versione ad alta accuratezza
# Profilo standard: docente single senza carichi familiari, regime TFR
# ==============================================================================

library(tidyverse)
library(scales)

it_df <- read_csv("02_data_clean/ccnl_italia_pulito_2000_2027.csv", show_col_types = FALSE) %>%
  filter(anno <= 2025, !is.na(lordo_reale_2025))

# 1. IRPEF lorda storica (scaglioni e aliquote 2000-2025) ----------------------
calcola_irpef_lorda <- function(anno, imp_irpef) {
  if (imp_irpef <= 0) return(0)
  
  if (anno == 2000) {
    s <- c(0, 10329.14, 15493.71, 30987.41, 69721.68, Inf)
    a <- c(0.185, 0.255, 0.335, 0.395, 0.455)
  } else if (anno %in% 2001:2002) {
    s <- c(0, 10329.14, 15493.71, 30987.41, 69721.68, Inf)
    a <- c(0.18, 0.24, 0.32, 0.39, 0.45)
  } else if (anno %in% 2003:2004) {
    s <- c(0, 15000, 29000, 32600, 70000, Inf)
    a <- c(0.23, 0.29, 0.31, 0.39, 0.45)
  } else if (anno %in% 2005:2006) {
    s <- c(0, 26000, 33500, 100000, Inf)
    a <- c(0.23, 0.33, 0.39, 0.43)
  } else if (anno %in% 2007:2021) {
    s <- c(0, 15000, 28000, 55000, 75000, Inf)
    a <- c(0.23, 0.27, 0.38, 0.41, 0.43)
  } else if (anno %in% 2022:2023) {
    s <- c(0, 15000, 28000, 50000, Inf)
    a <- c(0.23, 0.25, 0.35, 0.43)
  } else { # 2024-2025
    s <- c(0, 28000, 50000, Inf)
    a <- c(0.23, 0.35, 0.43)
  }
  
  irpef <- 0
  for (i in seq_along(a)) {
    if (imp_irpef > s[i]) {
      irpef <- irpef + (min(imp_irpef, s[i + 1]) - s[i]) * a[i]
    }
  }
  return(irpef)
}

# 2. Detrazioni lavoro dipendente + Bonus storici (2000-2025) ------------------
calcola_detrazioni_e_bonus <- function(anno, imp) {
  detr  <- 0
  bonus <- 0
  
  if (anno %in% 2000:2002) {
    # Detrazione lavoro dipendente per scaglioni (pre-riforma Tremonti)
    detr <- case_when(
      imp <= 15494 ~ 826,
      imp <= 20658 ~ 620,
      imp <= 25823 ~ 516,
      imp <= 30987 ~ 413,
      imp <= 36152 ~ 310,
      TRUE         ~ 207
    )
  } else if (anno %in% 2003:2006) {
    # La No-Tax Area 2003-2006 è applicata come deduzione prima dell'IRPEF
    detr <- 0
  } else if (anno %in% 2007:2013) {
    detr <- case_when(
      imp <= 8000  ~ 1840,
      imp <= 15000 ~ 1338 + 502 * (15000 - imp) / 7000,
      imp <= 55000 ~ 1338 * (55000 - imp) / 40000,
      TRUE         ~ 0
    )
  } else if (anno %in% 2014:2021) {
    detr <- case_when(
      imp <= 8000  ~ 1880,
      imp <= 28000 ~ 978 + 902 * (28000 - imp) / 20000,
      imp <= 55000 ~ 978 * (55000 - imp) / 27000,
      TRUE         ~ 0
    )
    # Bonus Renzi 2014 (8 mesi: mag-dic, soglia 24k-26k)
    if (anno == 2014) {
      if (imp <= 24000) bonus <- 640
      else if (imp <= 26000) bonus <- 640 * (26000 - imp) / 2000
      # Bonus Renzi 2015-2017 (12 mesi, soglia 24k-26k)
    } else if (anno %in% 2015:2017) {
      if (imp <= 24000) bonus <- 960
      else if (imp <= 26000) bonus <- 960 * (26000 - imp) / 2000
      # Bonus Renzi 2018-2019 (12 mesi, soglia alzata a 24.6k-26.6k)
    } else if (anno %in% 2018:2019) {
      if (imp <= 24600) bonus <- 960
      else if (imp <= 26600) bonus <- 960 * (26600 - imp) / 2000
      # Anno di transizione 2020 (gen-giu 80€, lug-dic 100€ + ulteriore detrazione)
    } else if (anno == 2020) {
      b_h1 <- if (imp <= 24600) 480 else if (imp <= 26600) 480 * (26600 - imp) / 2000 else 0
      b_h2 <- if (imp <= 28000) 600 else 0
      bonus <- b_h1 + b_h2
      if (imp > 28000 && imp <= 35000) detr <- detr + 0.5 * (960 + 240 * (35000 - imp) / 7000)
      else if (imp > 35000 && imp <= 40000) detr <- detr + 0.5 * (960 * (40000 - imp) / 5000)
      # Trattamento integrativo 2021 (anno pieno)
    } else if (anno == 2021) {
      if (imp <= 28000) bonus <- 1200
      else if (imp <= 35000) detr <- detr + 960 + 240 * (35000 - imp) / 7000
      else if (imp <= 40000) detr <- detr + 960 * (40000 - imp) / 5000
    }
  } else if (anno %in% 2022:2023) {
    detr <- case_when(
      imp <= 15000 ~ 1880,
      imp <= 28000 ~ 1910 + 1190 * (28000 - imp) / 13000,
      imp <= 50000 ~ 1910 * (50000 - imp) / 22000,
      TRUE         ~ 0
    )
    if (imp > 25000 && imp <= 35000) detr <- detr + 65
  } else { # 2024-2025
    detr <- case_when(
      imp <= 15000 ~ 1955,
      imp <= 28000 ~ 1910 + 1190 * (28000 - imp) / 13000,
      imp <= 50000 ~ 1910 * (50000 - imp) / 22000,
      TRUE         ~ 0
    )
    if (imp > 25000 && imp <= 35000) detr <- detr + 65
    # Ulteriore detrazione Legge di Bilancio 2025 (sostituisce esonero IVS)
    if (anno == 2025) {
      if (imp > 20000 && imp <= 32000) detr <- detr + 1000
      else if (imp > 32000 && imp <= 40000) detr <- detr + 1000 * (40000 - imp) / 8000
    }
  }
  
  return(list(detr = detr, bonus = bonus))
}

# 3. Aliquota media storica addizionali regionali + comunali (stima MEF) -------
aliquota_addizionali <- function(anno) {
  case_when(
    anno %in% 2000:2001 ~ 0.011,
    anno %in% 2002:2006 ~ 0.013,
    anno %in% 2007:2011 ~ 0.015,
    anno %in% 2012:2014 ~ 0.017,
    TRUE                ~ 0.019
  )
}

# 4. Funzione completa Lordo -> Netto ------------------------------------------
simula_netto <- function(anno, lordo) {
  # Contributi dipendente pubblico (regime TFR standardizzato)
  aliq_contr <- ifelse(anno <= 2006, 0.0910, 0.0915)
  
  # Esonero contributivo IVS 2022-2024 (su base annua effettiva 12/13 mensilità)
  if (anno == 2022 && lordo <= 35000) {
    aliq_contr <- aliq_contr - 0.014 # 0.8% I sem + 2% II sem (incl. 13a)
  } else if (anno == 2023) {
    if (lordo <= 25000)      aliq_contr <- aliq_contr - 0.0485 # 3% (7m) + 7% (6m) / 13
    else if (lordo <= 35000) aliq_contr <- aliq_contr - 0.0385 # 2% (7m) + 6% (6m) / 13
  } else if (anno == 2024) {
    if (lordo <= 25000)      aliq_contr <- aliq_contr - (0.07 * 12 / 13) # no 13a
    else if (lordo <= 35000) aliq_contr <- aliq_contr - (0.06 * 12 / 13) # no 13a
  }
  
  contributi <- lordo * aliq_contr
  imponibile <- lordo - contributi
  
  # No-Tax Area 2003-2006: vera deduzione dall'imponibile prima degli scaglioni
  deduz_notax <- 0
  if (anno %in% 2003:2006) {
    deduz_notax <- pmin(7500, pmax(0, 7500 * (26000 + 7500 - imponibile) / 26000))
  }
  imp_irpef <- max(0, imponibile - deduz_notax)
  
  irpef_lorda <- calcola_irpef_lorda(anno, imp_irpef)
  db          <- calcola_detrazioni_e_bonus(anno, imponibile)
  irpef_netta <- max(0, irpef_lorda - db$detr)
  addizionali <- imp_irpef * aliquota_addizionali(anno)
  
  netto_annuo <- imponibile - irpef_netta - addizionali + db$bonus
  tibble(
    contributi       = contributi,
    irpef_netta      = irpef_netta,
    addizionali      = addizionali,
    netto_annuo      = netto_annuo,
    netto_mensile13  = netto_annuo / 13,
    aliquota_eff_pct = (1 - netto_annuo / lordo) * 100
  )
}

# 5. Applicazione a tutto il dataset 2000-2025 ---------------------------------
it_netto <- it_df %>%
  mutate(res = map2(anno, lordo_totale, simula_netto)) %>%
  unnest(res) %>%
  mutate(
    fattore_reale    = lordo_reale_2025 / lordo_totale,
    netto_reale_2025 = netto_annuo * fattore_reale
  )

# Controllo di validazione su anni chiave (Ingresso, 15-20, 35+)
cat("\n=== VALIDAZIONE NETTO REALE (PREZZI 2025) E ALIQUOTA EFFETTIVA ===\n")
it_netto %>%
  filter(
    fascia_std %in% c("Ingresso (0-2 / 0-8)", "15-20", "35+"),
    anno %in% c(2000, 2007, 2015, 2021, 2025)
  ) %>%
  select(fascia_std, anno, lordo_totale, netto_annuo, netto_mensile13,
         aliquota_eff_pct, netto_reale_2025) %>%
  mutate(
    across(c(lordo_totale, netto_annuo, netto_mensile13, netto_reale_2025), ~ round(.x, 0)),
    aliquota_eff_pct = round(aliquota_eff_pct, 1)
  ) %>%
  arrange(fascia_std, anno) %>%
  print(n = 20, width = Inf)
# ==============================================================================
# 6. Salvataggio dataset e Tabella Fiscal Drag (Picco 2007 vs 2025)
# ==============================================================================

fasce_ordine <- c("35+", "28-34", "21-27", "15-20", "9-14", "Ingresso (0-2 / 0-8)")

it_netto_plot <- it_netto %>%
  filter(fascia_std %in% fasce_ordine) %>%
  mutate(fascia_std = factor(fascia_std, levels = fasce_ordine))

write_csv(it_netto, "02_data_clean/italia_netto_2000_2025.csv")

# Quantificazione Fiscal Drag / Effetto Fiscale (Picco 2007 -> 2025)
tab_fiscal_drag <- it_netto_plot %>%
  filter(anno %in% c(2007, 2025)) %>%
  select(fascia_std, anno, lordo_reale_2025, netto_reale_2025, aliquota_eff_pct) %>%
  pivot_wider(
    names_from  = anno,
    values_from = c(lordo_reale_2025, netto_reale_2025, aliquota_eff_pct)
  ) %>%
  mutate(
    var_lordo_pct   = round((lordo_reale_2025_2025 / lordo_reale_2025_2007 - 1) * 100, 1),
    var_netto_pct   = round((netto_reale_2025_2025 / netto_reale_2025_2007 - 1) * 100, 1),
    delta_aliq_pp   = round(aliquota_eff_pct_2025 - aliquota_eff_pct_2007, 1),
    # Netto 2025 controfattuale se l'aliquota effettiva fosse rimasta quella del 2007
    netto_cf_2007   = lordo_reale_2025_2025 * (1 - aliquota_eff_pct_2007 / 100),
    effetto_fisco_eur = round(netto_reale_2025_2025 - netto_cf_2007, 0)
  ) %>%
  select(fascia_std, var_lordo_pct, var_netto_pct, delta_aliq_pp, effetto_fisco_eur) %>%
  arrange(fascia_std)

cat("\n=== IMPATTO FISCALE E FISCAL DRAG: PICCO 2007 vs 2025 ===\n")
cat("(effetto_fisco_eur < 0 = perdita da fiscal drag; > 0 = beneficio da riforme/detrazioni)\n")
print(tab_fiscal_drag, width = Inf)

# ==============================================================================
# 7. Grafico 5: Salari NETTI reali per fascia di anzianità (2000-2025)
# ==============================================================================

p5_netto <- ggplot(it_netto_plot, aes(x = anno, y = netto_reale_2025, color = fascia_std)) +
  annotate("rect", xmin = 2011, xmax = 2015, ymin = -Inf, ymax = Inf,
           fill = "grey85", alpha = 0.5) +
  annotate("text", x = 2013, y = 35800, label = "Blocco\n2011-2015",
           size = 3.2, fontface = "italic", color = "grey35") +
  geom_vline(xintercept = 2007, linetype = "dashed", color = "grey45") +
  annotate("text", x = 2007.3, y = 21200, label = "Picco reale\n(2007)",
           hjust = 0, size = 3.2, color = "grey35") +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  scale_color_viridis_d(option = "viridis", direction = 1, name = "Fascia di anzianità") +
  scale_y_continuous(
    labels = label_number(big.mark = ".", suffix = " €"),
    breaks = seq(20000, 38000, by = 4000)
  ) +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
  labs(
    title    = "Salari netti reali dei docenti italiani per fascia di anzianità (2000–2025)",
    subtitle = "Netto annuo (13 mensilità + RPD, profilo single TFR) a prezzi costanti 2025 (HICP Eurostat)",
    x        = NULL,
    y        = "Netto annuo reale (EUR 2025)",
    caption  = "Fonte: microsimulazione fiscale su CCNL ARAN, normativa IRPEF/INPS storica e Eurostat HICP"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p5_netto)
ggsave("04_output/05_italia_netto_reale_2000_2025.png", p5_netto,
       width = 10, height = 6, dpi = 300, bg = "white")

# ==============================================================================
# 8. Grafico 6: Pressione fiscale e contributiva effettiva (2000-2025)
# ==============================================================================

p6_fisco <- ggplot(it_netto_plot, aes(x = anno, y = aliquota_eff_pct, color = fascia_std)) +
  geom_vline(xintercept = 2014, linetype = "dotted", color = "grey45") +
  annotate("text", x = 2014.2, y = 20.5, label = "Bonus 80€\n(2014)",
           hjust = 0, size = 3, color = "grey35") +
  geom_vline(xintercept = 2022, linetype = "dotted", color = "grey45") +
  annotate("text", x = 2021.8, y = 20.5, label = "Riforme IRPEF\ne Cuneo (2022-25)",
           hjust = 1, size = 3, color = "grey35") +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  scale_color_viridis_d(option = "viridis", direction = 1, name = "Fascia di anzianità") +
  scale_y_continuous(
    labels = label_number(suffix = "%", accuracy = 1),
    breaks = seq(20, 36, by = 2),
    limits = c(19.5, 36)
  ) +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
  labs(
    title    = "Pressione fiscale e contributiva effettiva sui docenti italiani (2000–2025)",
    subtitle = "Quota di lordo trattenuta (contributi + IRPEF netta + addizionali - bonus) | Profilo single TFR",
    x        = NULL,
    y        = "Aliquota effettiva (% sul lordo)",
    caption  = "Fonte: microsimulazione fiscale storica (2000–2025) su CCNL ARAN"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p6_fisco)
ggsave("04_output/06_italia_pressione_fiscale_2000_2025.png", p6_fisco,
       width = 10, height = 6, dpi = 300, bg = "white")

cat("\nGrafici 05 e 06 salvati correttamente in 04_output/!\n")
# ==============================================================================
# 7b. Grafico 5b: Salari NETTI MENSILI reali (su 13 mensilità, 2000-2025)
# ==============================================================================

it_netto_plot <- it_netto_plot %>%
  mutate(netto_mensile_reale_2025 = netto_reale_2025 / 13)

p5b_netto_mensile <- ggplot(it_netto_plot, aes(x = anno, y = netto_mensile_reale_2025, color = fascia_std)) +
  annotate("rect", xmin = 2011, xmax = 2015, ymin = -Inf, ymax = Inf,
           fill = "grey85", alpha = 0.5) +
  annotate("text", x = 2013, y = 2750, label = "Blocco\n2011-2015",
           size = 3.2, fontface = "italic", color = "grey35") +
  geom_vline(xintercept = 2007, linetype = "dashed", color = "grey45") +
  annotate("text", x = 2007.3, y = 1630, label = "Picco reale\n(2007)",
           hjust = 0, size = 3.2, color = "grey35") +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  scale_color_viridis_d(option = "viridis", direction = 1, name = "Fascia di anzianità") +
  scale_y_continuous(
    labels = label_number(big.mark = ".", suffix = " €"),
    breaks = seq(1600, 2800, by = 200)
  ) +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
  labs(
    title    = "Salari netti mensili reali dei docenti italiani per fascia (2000–2025)",
    subtitle = "Netto mensile medio (su 13 mensilità, profilo single TFR) a prezzi costanti 2025 (HICP Eurostat)",
    x        = NULL,
    y        = "Netto mensile reale (EUR 2025)",
    caption  = "Fonte: microsimulazione fiscale su CCNL ARAN, normativa IRPEF/INPS storica e Eurostat HICP"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p5b_netto_mensile)
ggsave("04_output/05b_italia_netto_mensile_reale_2000_2025.png", p5b_netto_mensile,
       width = 10, height = 6, dpi = 300, bg = "white")
# ==============================================================================
# Grafico 10: Scomposizione perdita reale 2000-2025 (Contratto vs Fisco)
# ==============================================================================

livelli_fasce <- c("Ingresso (0-2 / 0-8)", "9-14", "15-20", "21-27", "28-34", "35+")

df_scomp <- it_netto %>%
  filter(anno %in% c(2000, 2025), fascia_std %in% livelli_fasce) %>%
  select(fascia_std, anno, lordo_reale_2025, netto_reale_2025) %>%
  pivot_wider(
    names_from  = anno,
    values_from = c(lordo_reale_2025, netto_reale_2025)
  ) %>%
  mutate(
    fascia_std            = factor(fascia_std, levels = livelli_fasce),
    effetto_contratto_pct = (lordo_reale_2025_2025 - lordo_reale_2025_2000) / lordo_reale_2025_2000 * 100,
    var_netto_tot_pct     = (netto_reale_2025_2025 - netto_reale_2025_2000) / netto_reale_2025_2000 * 100,
    effetto_fisco_pct     = var_netto_tot_pct - effetto_contratto_pct
  ) %>%
  arrange(fascia_std)

cat("\n=== SCOMPOSIZIONE PERDITA REALE 2000-2025 (%) ===\n")
df_scomp %>%
  select(fascia_std, effetto_contratto_pct, effetto_fisco_pct, var_netto_tot_pct) %>%
  mutate(across(-fascia_std, ~ round(.x, 2))) %>%
  print(width = Inf)

df_scomp_long <- df_scomp %>%
  select(fascia_std, effetto_contratto_pct, effetto_fisco_pct) %>%
  pivot_longer(
    cols      = c(effetto_contratto_pct, effetto_fisco_pct),
    names_to  = "componente",
    values_to = "valore_pct"
  ) %>%
  mutate(
    componente = recode(
      componente,
      "effetto_contratto_pct" = "Effetto Contratto / Inflazione (Lordo reale)",
      "effetto_fisco_pct"     = "Effetto Fisco (Riforme IRPEF / Cuneo vs Fiscal Drag)"
    )
  )

p10_scomposizione <- ggplot() +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.6) +
  geom_col(
    data     = df_scomp_long,
    aes(x = fascia_std, y = valore_pct, fill = componente),
    position = "stack",
    width    = 0.62
  ) +
  # Punto e valore della variazione NETTA totale
  geom_point(
    data  = df_scomp,
    aes(x = fascia_std, y = var_netto_tot_pct, shape = "Variazione Netto Reale Totale"),
    size  = 3.5,
    color = "#111111"
  ) +
  geom_text(
    data = df_scomp,
    aes(
      x     = fascia_std,
      y     = pmin(var_netto_tot_pct, effetto_contratto_pct) - 0.9,
      label = paste0(ifelse(var_netto_tot_pct > 0, "+", ""), round(var_netto_tot_pct, 1), "%")
    ),
    size     = 3.5,
    fontface = "bold",
    color    = "#111111"
  ) +
  scale_fill_manual(
    name   = NULL,
    values = c(
      "Effetto Contratto / Inflazione (Lordo reale)"         = "#4575b4",
      "Effetto Fisco (Riforme IRPEF / Cuneo vs Fiscal Drag)" = "#d73027"
    )
  ) +
  scale_shape_manual(
    name   = NULL,
    values = c("Variazione Netto Reale Totale" = 18)
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1, accuracy = 1),
    breaks = seq(-10, 8, by = 2),
    expand = expansion(mult = c(0.12, 0.12))
  ) +
  labs(
    title    = "Italia (2000–2025): chi ha eroso gli stipendi dei docenti?",
    subtitle = "Scomposizione della variazione del salario netto reale: Effetto Contratto/Inflazione vs Effetto Fisco",
    x        = "Fascia di anzianità (scuola secondaria II grado)",
    y        = "Variazione reale cumulata 2000–2025 (%)",
    caption  = "Fonte: elaborazione su CCNL ARAN, Eurostat HICP e microsimulazione fiscale italiana (lavoratore single senza figli)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position    = "top",
    legend.box         = "vertical"
  )

print(p10_scomposizione)
ggsave("04_output/10_italia_scomposizione_contratto_vs_fisco.png", p10_scomposizione,
       width = 10.5, height = 6.2, dpi = 300, bg = "white")

cat("\nGrafico 10 salvato correttamente in 04_output/!\n")