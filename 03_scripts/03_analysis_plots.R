# ==============================================================================
# 03_analysis_plots.R
# Fase 1: Analisi longitudinale salari docenti Italia (2000-2025, prezzi 2025)
# ==============================================================================

library(tidyverse)
library(scales)

# 1. Caricamento dataset puliti ------------------------------------------------
it_df    <- read_csv("02_data_clean/ccnl_italia_pulito_2000_2027.csv", show_col_types = FALSE)
oecd_tr  <- read_csv("02_data_clean/oecd_trends_pulito.csv", show_col_types = FALSE)
oecd_ppp <- read_csv("02_data_clean/oecd_ppp_2025_pulito.csv", show_col_types = FALSE)

# Filtriamo gli anni con HICP completo (2000-2025)
it_reale <- it_df %>%
  filter(anno <= 2025, !is.na(lordo_reale_2025))

# 2. Analisi A: Fascia fissa (2000 vs Picco reale vs 2025) ---------------------
picchi_fasce <- it_reale %>%
  group_by(fascia_std) %>%
  slice_max(lordo_reale_2025, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(fascia_std, anno_picco = anno, picco_reale = lordo_reale_2025)

confronto_fasce <- it_reale %>%
  filter(anno %in% c(2000, 2025)) %>%
  select(anno, fascia_std, lordo_reale_2025) %>%
  pivot_wider(names_from = anno, values_from = lordo_reale_2025) %>%
  left_join(picchi_fasce, by = "fascia_std") %>%
  mutate(
    diff_2000_2025_eur = `2025` - `2000`,
    diff_2000_2025_pct = (`2025` / `2000` - 1) * 100,
    diff_picco_2025_pct = (`2025` / picco_reale - 1) * 100
  ) %>%
  mutate(
    across(c(`2000`, picco_reale, `2025`, diff_2000_2025_eur), ~ round(.x, 2)),
    across(c(diff_2000_2025_pct, diff_picco_2025_pct), ~ round(.x, 1))
  )

cat("\n=== ANALISI A: SALARIO REALE (PREZZI 2025) PER FASCIA FISSA ===\n")
print(confronto_fasce, width = Inf)

# 3. Analisi B: Coorte assunta nel 2000 (0 anni -> 25 anni nel 2025) -----------
mappa_fascia_coorte <- function(anz) {
  case_when(
    anz <= 2  ~ "0-2",
    anz <= 8  ~ "3-8",
    anz <= 14 ~ "9-14",
    anz <= 20 ~ "15-20",
    anz <= 27 ~ "21-27",
    anz <= 34 ~ "28-34",
    TRUE      ~ "35+"
  )
}

coorte_2000 <- tibble(anno = 2000:2025) %>%
  mutate(
    anzianita     = anno - 2000,
    fascia_attesa = mappa_fascia_coorte(anzianita)
  ) %>%
  left_join(it_reale, by = "anno") %>%
  mutate(
    fascia_effettiva = case_when(
      anno >= 2011 & fascia_attesa %in% c("0-2", "3-8") ~ "0-8",
      TRUE ~ fascia_attesa
    )
  ) %>%
  filter(fascia == fascia_effettiva) %>%
  arrange(anno) %>%
  mutate(
    var_reale_annua_pct = (lordo_reale_2025 / lag(lordo_reale_2025) - 1) * 100,
    # <-- RIGA AGGIUNTA: forza l'ordine corretto nella legenda del grafico
    fascia = factor(fascia, levels = c("0-2", "3-8", "9-14", "15-20", "21-27"))
  )
cat("\n=== ANALISI B: COORTE ASSUNTA NEL 2000 (TAPPE CHIAVE) ===\n")
coorte_2000 %>%
  filter(anno %in% c(2000, 2003, 2009, 2010, 2015, 2020, 2021, 2025)) %>%
  select(anno, anzianita, fascia, lordo_totale, lordo_reale_2025) %>%
  mutate(across(c(lordo_totale, lordo_reale_2025), ~ round(.x, 2))) %>%
  print()

var_coorte_pct <- (last(coorte_2000$lordo_reale_2025) / first(coorte_2000$lordo_reale_2025) - 1) * 100
cat(sprintf("\nVariazione reale complessiva coorte 2000 (2000 -> 2025): %+.1f%%\n", var_coorte_pct))
# 4. Grafico 1: Salario reale per fascia fissa (2000-2025, prezzi 2025) -------
fasce_plot <- c("Ingresso (0-2 / 0-8)", "9-14", "15-20", "21-27", "28-34", "35+")

p1_fasce <- it_reale %>%
  filter(fascia_std %in% fasce_plot) %>%
  mutate(fascia_std = factor(fascia_std, levels = rev(fasce_plot))) %>%
  ggplot(aes(x = anno, y = lordo_reale_2025, color = fascia_std)) +
  # Evidenziazione blocco contrattuale 2011-2015
  annotate("rect", xmin = 2011, xmax = 2015, ymin = -Inf, ymax = Inf,
           fill = "grey85", alpha = 0.45) +
  annotate("text", x = 2013, y = 54000, label = "Blocco\n2011-2015",
           size = 3, color = "grey35", fontface = "italic") +
  # Linea verticale sul picco reale 2007
  geom_vline(xintercept = 2007, linetype = "dashed", color = "grey45", linewidth = 0.6) +
  annotate("text", x = 2007.3, y = 27500, label = "Picco reale\n(2007)",
           hjust = 0, size = 3, color = "grey35") +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.6) +
  scale_y_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ",", suffix = " €"),
    limits = c(26000, 56000)
  ) +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
  scale_color_viridis_d(option = "D", end = 0.9) +
  labs(
    title = "Salari reali dei docenti italiani per fascia di anzianità (2000-2025)",
    subtitle = "Lordo annuo (13 mensilità + RPD) a prezzi costanti 2025 (deflatore HICP Eurostat)",
    x = NULL,
    y = "Lordo annuo reale (EUR 2025)",
    color = "Fascia di anzianità",
    caption = "Fonte: elaborazioni su CCNL ARAN (docenti laureati sec. II grado) e Eurostat HICP"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p1_fasce)
ggsave("04_output/01_italia_fasce_reali_2000_2025.png", p1_fasce,
       width = 10, height = 6, dpi = 300, bg = "white")

# 5. Grafico 2: Traiettoria reale della coorte assunta nel 2000 ---------------
scatti_coorte <- coorte_2000 %>%
  filter(anno %in% c(2000, 2009, 2015, 2021, 2025))
p2_coorte <- coorte_2000 %>%
  ggplot(aes(x = anno, y = lordo_reale_2025)) +
  annotate("rect", xmin = 2011, xmax = 2015, ymin = -Inf, ymax = Inf,
           fill = "grey85", alpha = 0.45) +
  geom_line(color = "#1f78b4", linewidth = 1.2) +
  geom_point(aes(color = fascia), size = 2.6) +
  geom_text(
    data  = scatti_coorte, # (o il nome del df che usi per le etichette)
    aes(
      label = paste0(number(lordo_reale_2025, accuracy = 1, big.mark = "."), " €"),
      hjust = ifelse(anno == 2000, 0.7, 0.5)
    ),
    vjust = -1.2,
    size  = 3.2,
    fontface = "bold",
    color = "grey20"
  ) +
  scale_y_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ",", suffix = " €"),
    limits = c(29000, 46500)
  ) +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
  labs(
    title = "Carriera reale di un docente assunto nel 2000 (2000-2025)",
    subtitle = "Avanzamento negli scatti di anzianità vs erosione inflattiva (prezzi costanti 2025)",
    x = NULL,
    y = "Lordo annuo reale (EUR 2025)",
    color = "Fascia attiva",
    caption = "Fonte: elaborazioni su CCNL ARAN e Eurostat HICP | Variazione reale complessiva 2000-2025: +27,9%"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p2_coorte)
ggsave("04_output/02_italia_coorte_2000.png", p2_coorte,
       width = 10, height = 6, dpi = 300, bg = "white")
# ==============================================================================
# 6. Confronto Europeo OCSE: Livelli 2025 in Indice (Italia 15 anni = 100)
# ==============================================================================

# Etichette italiane leggibili per i paesi OCSE
# Accorpamento Belgio (media semplice Fiandre + Vallonia -> BEL)
oecd_ppp <- oecd_ppp %>%
  mutate(ref_area = if_else(ref_area %in% c("BEFL", "BEFR"), "BEL", ref_area)) %>%
  group_by(anno, ref_area, exp_level) %>%
  summarise(valore_usd_ppp_2025 = mean(valore_usd_ppp_2025, na.rm = TRUE), .groups = "drop")

oecd_tr <- oecd_tr %>%
  mutate(ref_area = if_else(ref_area %in% c("BEFL", "BEFR"), "BEL", ref_area)) %>%
  group_by(anno, ref_area, unit_measure, exp_level) %>%
  summarise(valore = mean(valore, na.rm = TRUE), .groups = "drop")
nomi_paesi <- c(
  "DEU"      = "Germania",
  "BEL"      = "Belgio",
  "ESP"      = "Spagna",
  "OECD"     = "Media OCSE",
  "OECD_REP" = "Media OCSE",
  "FRA"      = "Francia",
  "ITA"      = "Italia",
  "POL"      = "Polonia",
  "GRC"      = "Grecia"
)

# Base di riferimento: Italia a 15 anni di esperienza (in USD PPP 2025)
base_ita_15 <- oecd_ppp %>%
  filter(ref_area == "ITA", exp_level == "EXP15") %>%
  pull(valore_usd_ppp_2025)

ppp_plot_df <- oecd_ppp %>%
  mutate(
    paese_it = recode(ref_area, !!!nomi_paesi),
    indice_ita100 = (valore_usd_ppp_2025 / base_ita_15) * 100,
    exp_label = factor(
      exp_level,
      levels = c("EXP0", "EXP10", "EXP15", "EXPMAX"),
      labels = c("Ingresso (0 anni)", "10 anni", "15 anni", "Fine carriera")
    )
  )

# Tabella riassuntiva in indice (Italia 15 anni = 100) ordinata per EXP15
tabella_ppp <- ppp_plot_df %>%
  select(paese_it, exp_level, indice_ita100) %>%
  pivot_wider(names_from = exp_level, values_from = indice_ita100) %>%
  mutate(
    crescita_carriera_pct = round((EXPMAX / EXP0 - 1) * 100, 1),
    across(c(EXP0, EXP10, EXP15, EXPMAX), ~ round(.x, 1))
  ) %>%
  arrange(desc(EXP15))

cat("\n=== CONFRONTO OCSE 2025: INDICE POTERE D'ACQUISTO (Italia 15 anni = 100) ===\n")
print(tabella_ppp, width = Inf)

# Ordine paesi sul grafico (dal più alto al più basso a 15 anni)
ordine_paesi <- rev(tabella_ppp$paese_it)

ppp_segmenti <- tabella_ppp %>%
  mutate(paese_it = factor(paese_it, levels = ordine_paesi))

ppp_punti <- ppp_plot_df %>%
  filter(exp_level %in% c("EXP0", "EXP15", "EXPMAX")) %>%
  mutate(paese_it = factor(paese_it, levels = ordine_paesi))

# Grafico 3: Dumbbell plot in indice (Italia 15 anni = 100)
p3_ppp <- ggplot() +
  geom_vline(xintercept = 100, linetype = "dashed", color = "grey45", linewidth = 0.7) +
  geom_segment(
    data = ppp_segmenti,
    aes(x = EXP0, xend = EXPMAX, y = paese_it, yend = paese_it),
    color = "grey75", linewidth = 2.2
  ) +
  geom_point(
    data = ppp_punti,
    aes(x = indice_ita100, y = paese_it, color = exp_label),
    size = 4
  ) +
  geom_text(
    data = filter(ppp_punti, exp_level == "EXP15"),
    aes(x = indice_ita100, y = paese_it,
        label = number(indice_ita100, accuracy = 0.1, decimal.mark = ",")),
    vjust = -1.1, size = 3.2, fontface = "bold", color = "grey20"
  ) +
  scale_x_continuous(
    breaks = seq(50, 250, by = 50),
    limits = c(40, 260)
  ) +
  scale_color_manual(
    values = c("Ingresso (0 anni)" = "#66c2a5",
               "15 anni"           = "#2b8cbe",
               "Fine carriera"     = "#084081"),
    name = "Fase di carriera"
  ) +
  labs(
    title    = "Salari statutari lordi dei docenti superiori in Europa (2025)",
    subtitle = "Indice a parità di potere d'acquisto (Italia 15 anni = 100) | Etichette a 15 anni",
    x        = "Indice potere d'acquisto (Italia 15 anni = 100)",
    y        = NULL,
    caption  = "Fonte: elaborazione su dati OCSE Education at a Glance (USD PPP 2025, ISCED 34)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.y      = element_text(face = ifelse(ordine_paesi == "Italia", "bold", "plain"))
  )

print(p3_ppp)
ggsave("04_output/03_ocse_ppp_2025_confronto.png", p3_ppp,
       width = 10, height = 6, dpi = 300, bg = "white")
# ==============================================================================
# 7. Grafico 4: Evoluzione reale salari docenti in Europa (2015-2025, 2015 = 100)
# ==============================================================================

df_trend_eu <- oecd_tr %>%
  filter(unit_measure == "IX", exp_level == "EXP15", anno >= 2015, anno <= 2025) %>%
  mutate(
    paese_it = recode(ref_area, !!!nomi_paesi),
    gruppo = case_when(
      paese_it == "Italia"     ~ "Italia",
      paese_it == "Media OCSE" ~ "Media OCSE",
      paese_it == "Polonia"    ~ "Polonia",
      TRUE                     ~ "Altri paesi EU"
    )
  )

# Etichette finali sull'ultimo anno disponibile (2025)
etichette_2025 <- df_trend_eu %>%
  group_by(paese_it) %>%
  slice_max(anno, n = 1) %>%
  ungroup() %>%
  mutate(
    y_label = case_when(
      paese_it == "Spagna"   ~ valore + 1.5,
      paese_it == "Francia"  ~ valore + 0.3,
      paese_it == "Belgio"   ~ valore - 0.3,
      paese_it == "Germania" ~ valore - 0.7,
      TRUE                   ~ valore
    )
  )

p4_trend <- ggplot(df_trend_eu, aes(x = anno, y = valore, group = paese_it)) +
  geom_hline(yintercept = 100, linetype = "dotted", color = "grey45") +
  # Altri paesi EU sullo sfondo
  geom_line(
    data = filter(df_trend_eu, gruppo == "Altri paesi EU"),
    color = "grey75", linewidth = 0.8
  ) +
  # Media OCSE
  geom_line(
    data = filter(df_trend_eu, paese_it == "Media OCSE"),
    color = "grey20", linetype = "dashed", linewidth = 1.1
  ) +
  # Polonia
  geom_line(
    data = filter(df_trend_eu, paese_it == "Polonia"),
    color = "#1b9e77", linewidth = 1.1
  ) +
  # Italia in primo piano
  geom_line(
    data = filter(df_trend_eu, paese_it == "Italia"),
    color = "#d73027", linewidth = 1.4
  ) +
  geom_point(
    data = filter(df_trend_eu, paese_it == "Italia"),
    color = "#d73027", size = 2
  ) +
  geom_text(
    data = etichette_2025,
    aes(
      y = y_label,
      label = paste0(paese_it, " (", round(valore, 1), ")"),
      color = gruppo
    ),
    hjust = -0.08, size = 3.1, fontface = "bold", show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Italia"         = "#d73027",
      "Media OCSE"     = "grey20",
      "Polonia"        = "#1b9e77",
      "Altri paesi EU" = "grey50"
    )
  ) +
  scale_x_continuous(breaks = seq(2015, 2025, by = 2), limits = c(2015, 2027.8)) +
  scale_y_continuous(breaks = seq(85, 135, by = 5)) +
  labs(
    title    = "Andamento reale dei salari statutari lordi dei docenti in Europa (2015–2025)",
    subtitle = "Docenti scuola secondaria II grado con 15 anni di esperienza (Indice 2015 = 100)",
    x        = NULL,
    y        = "Indice reale (2015 = 100)",
    caption  = "Fonte: OCSE Education at a Glance (ISCED 34, prezzi costanti; Belgio: media Fiandre e Vallonia)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p4_trend)
ggsave("04_output/04_ocse_trend_2015_2025.png", p4_trend,
       width = 10.5, height = 6, dpi = 300, bg = "white")

cat("\nGrafico 04 salvato correttamente in 04_output/! Fase Lordo completata.\n")
write_csv(tabella_ppp, "02_data_clean/tabella_ppp.csv")