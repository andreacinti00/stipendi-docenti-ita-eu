# ==============================================================================
# Script: 01_download_api.R
# Scopo:  Scaricare l'indice HICP annuale (inflazione) da Eurostat (2000-oggi)
#         per Italia e paesi europei di confronto.
# ==============================================================================

library(tidyverse)
library(eurostat)

# 1. Definisci i paesi di interesse (Nota: EL = Grecia in Eurostat)
paesi_target <- c("IT", "DE", "BE", "ES", "FR", "PL", "EL")

# 2. Scarica il dataset completo HICP annuale da Eurostat
# time_format = "num" trasforma automaticamente la colonna anno in numero (es. 2000)
hicp_raw <- get_eurostat(
  id = "prc_hicp_aind",
  time_format = "num"
)

# 3. Filtra solo le righe che ci servono
hicp_filtered <- hicp_raw |>
  filter(
    coicop == "CP00",           # Indice generale (tutti i beni e servizi)
    unit == "INX_A_AVG",        # Indice medio annuo (base 2015 = 100)
    geo %in% paesi_target,      # Solo i nostri 7 paesi
    TIME_PERIOD >= 2000         # Dal 2000 fino all'ultimo anno disponibile
  ) |>
  select(
    geo,
    anno = TIME_PERIOD,
    hicp_index = values
  ) |>
  arrange(geo, anno)

# 4. Salva i dati grezzi filtrati in 01_data_raw/
# Salviamo sia in CSV (leggibile ovunque) sia in RDS (formato nativo R velocissimo)
write_csv(hicp_filtered, "01_data_raw/eurostat_hicp_2000_oggi.csv")
saveRDS(hicp_filtered, "01_data_raw/eurostat_hicp_2000_oggi.rds")

message("Download completato e file salvati in 01_data_raw/!")