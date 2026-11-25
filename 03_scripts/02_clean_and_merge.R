# ==============================================================================
# 02_clean_and_merge.R
# Pulizia dati CCNL Italia, calcolo lordo totale e deflazione con HICP Eurostat
# ==============================================================================

library(tidyverse)

# 1. Caricamento dati ----------------------------------------------------------
ccnl_it <- read_csv("02_data_clean/ccnl_italia_2000_2027.csv", show_col_types = FALSE)
hicp    <- readRDS("01_data_raw/eurostat_hicp_2000_oggi.rds")

# Controllo rapido dei nomi colonne di hicp (utile per il join)
glimpse(hicp)

# 2. Calcolo Lordo Annuo Totale Italia (Tabellare * 13/12 + RPD * 12) ----------
ccnl_it <- ccnl_it %>%
  mutate(
    tabellare_13m = tabellare_12m * (13 / 12),
    lordo_totale  = tabellare_13m + rpd_annua,
    # Armonizziamo la fascia iniziale per i confronti diretti 2000-2027
    fascia_std = case_when(
      fascia == "0-2" ~ "Ingresso (0-2 / 0-8)",
      fascia == "0-8" ~ "Ingresso (0-2 / 0-8)",
      TRUE            ~ fascia
    )
  )

# Controllo visivo dei primi e ultimi anni
ccnl_it %>%
  filter(anno %in% c(2000, 2010, 2015, 2020, 2025, 2026)) %>%
  select(anno, fascia, lordo_totale) %>%
  pivot_wider(names_from = anno, values_from = lordo_totale) %>%
  print()
# 3. Estrazione HICP Italia e deflazione a prezzi costanti 2025 --------------
hicp_it <- hicp %>%
  filter(geo == "IT") %>%
  select(anno, hicp_index)

# Verifica che non manchi alcun anno tra 2000 e 2025
anni_mancanti_hicp <- setdiff(2000:2025, hicp_it$anno)
if (length(anni_mancanti_hicp) > 0) {
  warning("Anni mancanti in HICP Italia: ", paste(anni_mancanti_hicp, collapse = ", "))
} else {
  message("OK: copertura HICP Italia completa dal 2000 al 2025.")
}

# Deflatore base 2025: se HICP ha base diversa da 100 nel 2025, normalizziamo
# Salario reale (prezzi 2025) = Lordo nominale * (HICP_2025 / HICP_anno)
base_2025 <- hicp_it %>%
  filter(anno == 2025) %>%
  pull(hicp_index)

ccnl_it_def <- ccnl_it %>%
  left_join(hicp_it, by = "anno") %>%
  mutate(
    # Per anni oltre l'ultimo anno HICP disponibile (es. 2026-2027), teniamo NA su reale
    # finché non aggiungiamo eventuale inflazione stimata/consuntiva
    fattore_2025 = if_else(!is.na(hicp_index), base_2025 / hicp_index, NA_real_),
    lordo_reale_2025 = lordo_totale * fattore_2025,
    var_reale_vs_2000_pct = (lordo_reale_2025 / first(lordo_reale_2025, order_by = anno) - 1) * 100
  )

# Controllo rapido: quanti NA su lordo_reale_2025? (Devono essere solo 2026-2027)
ccnl_it_def %>%
  filter(is.na(lordo_reale_2025)) %>%
  distinct(anno, hicp_index, lordo_totale) %>%
  print()

# Controllo evoluzione reale (prezzi 2025) per fascia 28-34 anni
ccnl_it_def %>%
  filter(fascia == "28-34", anno %in% c(2000, 2005, 2010, 2015, 2020, 2025)) %>%
  select(anno, lordo_totale, hicp_index, lordo_reale_2025) %>%
  mutate(across(c(lordo_totale, lordo_reale_2025), ~ round(.x, 2))) %>%
  print()

# Salvataggio dataset pulito Italia
write_csv(ccnl_it_def, "02_data_clean/ccnl_italia_pulito_2000_2027.csv")
# 4. Controllo e pulizia dati OCSE (trends e PPP 2025) ------------------------
paesi_target <- c("IT", "DE", "BE", "ES", "FR", "PL", "EL")

# Ispezione rapida delle colonne disponibili nei due oggetti OCSE
cat("\n--- Colonne oecd_trends ---\n")
print(names(oecd_trends))

cat("\n--- Colonne oecd_ppp ---\n")
print(names(oecd_ppp))

# Controllo rapido valori univoci di eventuali colonne di classificazione
# (adattiamo i nomi colonna appena vediamo l'output esatto)
# 5. Pulizia e salvataggio dataset OCSE --------------------------------------
oecd_trends_clean <- oecd_trends %>%
  transmute(
    anno         = as.integer(TIME_PERIOD),
    ref_area     = REF_AREA,
    paese        = `Reference area`,
    unit_measure = UNIT_MEASURE,
    exp_level    = PERS_EXP_LEV,
    valore       = as.numeric(OBS_VALUE)
  ) %>%
  filter(!is.na(anno), !is.na(valore)) %>%
  arrange(ref_area, unit_measure, exp_level, anno)

oecd_ppp_clean <- oecd_ppp %>%
  mutate(
    anno                = as.integer(anno),
    valore_usd_ppp_2025 = as.numeric(valore_usd_ppp_2025)
  ) %>%
  filter(!is.na(valore_usd_ppp_2025))

write_csv(oecd_trends_clean, "02_data_clean/oecd_trends_pulito.csv")
write_csv(oecd_ppp_clean,    "02_data_clean/oecd_ppp_2025_pulito.csv")

# Riepilogo finale di integrità sui file salvati in 02_data_clean/
cat("\n=== RIEPILOGO FINALE PULIZIA DATI ===\n")
cat("ccnl_italia_pulito_2000_2027.csv -> righe:", nrow(ccnl_it_def),
    "| NA lordo_totale:", sum(is.na(ccnl_it_def$lordo_totale)), "\n")
cat("oecd_trends_pulito.csv           -> righe:", nrow(oecd_trends_clean),
    "| anni:", min(oecd_trends_clean$anno), "-", max(oecd_trends_clean$anno), "\n")
cat("oecd_ppp_2025_pulito.csv         -> righe:", nrow(oecd_ppp_clean),
    "| anni:", min(oecd_ppp_clean$anno), "-", max(oecd_ppp_clean$anno), "\n")