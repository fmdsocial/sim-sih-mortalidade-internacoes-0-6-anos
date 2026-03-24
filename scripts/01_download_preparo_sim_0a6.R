############################################################
# PROJETO: SIM + SIH Brasil (2015-2024)
# MÓDULO: SIM-DO
# FOCO: Crianças de 0 a 6 anos
# ETAPA 1: Download, estruturação e limpeza inicial
# DESTINO: Repositório público no GitHub
############################################################

# 1) Pacotes -------------------------------------------------------------------
pacotes <- c("microdatasus", "dplyr", "stringr", "readr")

for (p in pacotes) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

library(microdatasus)
library(dplyr)
library(stringr)
library(readr)

# 2) Diretórios do projeto -----------------------------------------------------
# Estrutura esperada do repositório:
# sim-sih-mortalidade-internacoes-0a6-brasil/
# ├── data/
# │   ├── raw/
# │   └── processed/
# ├── outputs/
# │   ├── figuras/
# │   └── tabelas/
# └── scripts/

dir_data_raw <- file.path("data", "raw")
dir_data_processed <- file.path("data", "processed")
dir_outputs_figuras <- file.path("outputs", "figuras")
dir_outputs_tabelas <- file.path("outputs", "tabelas")

for (dir in c(dir_data_raw, dir_data_processed, dir_outputs_figuras, dir_outputs_tabelas)) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}

ufs_brasil <- c(
  "AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA",
  "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN",
  "RS", "RO", "RR", "SC", "SP", "SE", "TO"
)

# 3) Funções auxiliares --------------------------------------------------------
calcular_idade_anos <- function(idade_str) {
  idade_str <- as.character(idade_str)
  unidade <- substr(idade_str, 1, 1)
  valor <- suppressWarnings(as.numeric(substr(idade_str, 2, 3)))

  case_when(
    is.na(unidade) | is.na(valor) ~ NA_real_,
    unidade %in% c("0", "1", "2", "3") ~ 0,  # menores de 1 ano
    unidade == "4" ~ valor,                     # anos
    unidade == "5" ~ 100 + valor,              # 100 anos ou mais
    TRUE ~ NA_real_
  )
}

rot_sexo <- function(x) {
  case_when(
    x == "1" ~ "Masculino",
    x == "2" ~ "Feminino",
    TRUE ~ "Ignorado/Outro"
  )
}

rot_raca <- function(x) {
  case_when(
    x == "1" ~ "Branca",
    x == "2" ~ "Preta",
    x == "3" ~ "Parda",
    x == "4" ~ "Amarela",
    x == "5" ~ "Indígena",
    TRUE ~ "Sem informação"
  )
}

# 4) Download e limpeza em lote ------------------------------------------------
cat("Iniciando processamento em lote do SIM-DO Brasil (2015-2024)...\n")
cat("Foco do projeto: crianças de 0 a 6 anos.\n")
cat("Atenção: serão baixadas todas as variáveis disponíveis do SIM-DO.\n")

lista_anos_processados <- list()

for (ano in 2015:2024) {
  cat(sprintf("\n>>> Baixando dados de %d...\n", ano))

  temp_bruto <- fetch_datasus(
    year_start = ano,
    year_end = ano,
    uf = ufs_brasil,
    information_system = "SIM-DO"
  )

  cat(sprintf("Filtrando e processando %d...\n", ano))

  temp_limpo <- temp_bruto %>%
    mutate(
      IDADE_ANOS = calcular_idade_anos(IDADE),
      ANO_OBITO = as.numeric(substr(DTOBITO, nchar(DTOBITO) - 3, nchar(DTOBITO))),
      SEXO_DESC = rot_sexo(SEXO),
      RACA_DESC = rot_raca(RACACOR),
      CAPITULO_CID = substr(CAUSABAS, 1, 1)
    ) %>%
    filter(!is.na(IDADE_ANOS) & IDADE_ANOS < 18)

  lista_anos_processados[[as.character(ano)]] <- temp_limpo

  rm(temp_bruto, temp_limpo)
  gc()
}

cat("\nJuntando todos os anos em um único dataset...\n")
sim_menores_18 <- bind_rows(lista_anos_processados)
rm(lista_anos_processados)
gc()

# 5) Subpopulação principal do projeto ----------------------------------------
cat("Criando subpopulação principal: crianças de 0 a 6 anos...\n")
sim_0a6 <- sim_menores_18 %>%
  filter(IDADE_ANOS <= 6)

# 6) Salvando arquivos ---------------------------------------------------------
cat("Salvando bases processadas no disco...\n")

# Base ampliada (<18) - opcional para referência
saveRDS(
  sim_menores_18,
  file.path(dir_data_processed, "sim_brasil_menores_18_todas_vars_2015_2024.rds")
)
write_csv(
  sim_menores_18,
  file.path(dir_data_processed, "sim_brasil_menores_18_todas_vars_2015_2024.csv")
)

# Base principal do projeto (0 a 6 anos)
saveRDS(
  sim_0a6,
  file.path(dir_data_processed, "sim_brasil_0a6_todas_vars_2015_2024.rds")
)
write_csv(
  sim_0a6,
  file.path(dir_data_processed, "sim_brasil_0a6_todas_vars_2015_2024.csv")
)

cat("\nETAPA 1 CONCLUÍDA!\n")
cat("Arquivos principais gerados em: data/processed\n")
cat("Base principal do projeto: sim_brasil_0a6_todas_vars_2015_2024.rds\n")
