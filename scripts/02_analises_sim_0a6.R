############################################################
# PROJETO: SIM + SIH Brasil (2015-2024)
# MÓDULO: SIM-DO
# FOCO: Crianças de 0 a 6 anos
# ETAPA 2: Análises executivas e outputs gráficos
# DESTINO: Repositório público no GitHub
############################################################

# 1) Pacotes -------------------------------------------------------------------
pacotes <- c("dplyr", "ggplot2", "readr", "writexl", "tidyr", "geobr", "sf",
             "RColorBrewer", "stringr", "scales", "forcats", "utils")
for (p in pacotes) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)

library(dplyr)
library(ggplot2)
library(readr)
library(writexl)
library(tidyr)
library(geobr)
library(sf)
library(RColorBrewer)
library(stringr)
library(scales)
library(forcats)

# 2) Parâmetros e Diretórios ---------------------------------------------------
dir_dados    <- "./Dados" 
dir_analises <- "./Analises"

if (!dir.exists(dir_analises)) dir.create(dir_analises, recursive = TRUE)

# Tema Executivo ---------------------------------------------------------------
tema_executivo <- function(...) {
  theme_classic(base_size = 14) +
    theme(
      plot.title    = element_text(face = "bold", size = 16, color = "#1a252f"),
      plot.subtitle = element_text(size = 12, color = "#7f8c8d", margin = margin(b = 15)),
      plot.caption  = element_text(size = 9, color = "#95a5a6", hjust = 0),
      axis.title    = element_text(face = "bold", color = "#2c3e50"),
      axis.text     = element_text(color = "#34495e"),
      axis.line     = element_line(color = "#bdc3c7", linewidth = 0.5),
      panel.grid.major.y = element_line(color = "#ecf0f1", linetype = "dashed"),
      legend.position = "bottom",
      legend.title    = element_blank(),
      plot.background  = element_rect(fill = "white", color = "white"),
      panel.background = element_rect(fill = "white", color = "white"),
      ...
    )
}

cores_faixa <- c(
  "Neonatal Precoce (0-6 dias)"      = "#c0392b",
  "Neonatal Tardia (7-27 dias)"      = "#d35400",
  "Pós-Neonatal (28 dias a <1 ano)"  = "#f39c12",
  "1 a 6 Anos"                       = "#2980b9"
)

# 3) Carregamento e Preparação dos Dados de Óbitos -----------------------------
cat("══════════════════════════════════════════════════════════\n")
cat("  ETAPA 1: PREPARANDO BASE DE ÓBITOS\n")
cat("══════════════════════════════════════════════════════════\n\n")

arquivo_obitos <- file.path(dir_dados, "sim_brasil_0_a_6_anos_todas_vars_2015_2024.rds")
if(!file.exists(arquivo_obitos)) stop("Arquivo de óbitos não encontrado!")

base_0_a_6 <- readRDS(arquivo_obitos)

mapa_regioes <- c(
  "1" = "Norte", "2" = "Nordeste", "3" = "Sudeste",
  "4" = "Sul",   "5" = "Centro-Oeste"
)

mapa_estados <- c(
  "11" = "Rondônia",     "12" = "Acre",           "13" = "Amazonas",
  "14" = "Roraima",      "15" = "Pará",           "16" = "Amapá",
  "17" = "Tocantins",    "21" = "Maranhão",       "22" = "Piauí",
  "23" = "Ceará",        "24" = "Rio Grande do Norte", "25" = "Paraíba",
  "26" = "Pernambuco",   "27" = "Alagoas",        "28" = "Sergipe",
  "29" = "Bahia",        "31" = "Minas Gerais",   "32" = "Espírito Santo",
  "33" = "Rio de Janeiro","35" = "São Paulo",     "41" = "Paraná",
  "42" = "Santa Catarina","43" = "Rio Grande do Sul",
  "50" = "Mato Grosso do Sul", "51" = "Mato Grosso",
  "52" = "Goiás",        "53" = "Distrito Federal"
)

# Dicionário reverso para cruzar nome do estado com o código
mapa_estados_inverso <- setNames(names(mapa_estados), mapa_estados)

base_analise <- base_0_a_6 %>%
  mutate(
    UNIDADE_IDADE = substr(as.character(IDADE), 1, 1),
    VALOR_IDADE   = suppressWarnings(as.numeric(substr(as.character(IDADE), 2, 3))),
    
    GRUPO_ETARIO = case_when(
      UNIDADE_IDADE %in% c("0", "1") |
        (UNIDADE_IDADE == "2" & VALOR_IDADE <= 6)  ~ "Neonatal Precoce (0-6 dias)",
      UNIDADE_IDADE == "2" & VALOR_IDADE >= 7 &
        VALOR_IDADE <= 27                          ~ "Neonatal Tardia (7-27 dias)",
      (UNIDADE_IDADE == "2" & VALOR_IDADE > 27) |
        UNIDADE_IDADE == "3"                       ~ "Pós-Neonatal (28 dias a <1 ano)",
      UNIDADE_IDADE == "4" & VALOR_IDADE >= 1 &
        VALOR_IDADE <= 6                           ~ "1 a 6 Anos",
      TRUE ~ "Sem informação precisa"
    ),
    
    COD_ESTADO   = substr(CODMUNRES, 1, 2),
    COD_MUN_RES  = substr(CODMUNRES, 1, 6),
    COD_MUN_OCOR = substr(CODMUNOCOR, 1, 6),
    MACRORREGIAO = mapa_regioes[substr(COD_ESTADO, 1, 1)],
    NOME_ESTADO  = mapa_estados[COD_ESTADO],
    
    CAPITULO_DESC = case_when(
      CAPITULO_CID %in% c("A", "B")        ~ "Infecciosas e Parasitárias",
      CAPITULO_CID == "J"                  ~ "Aparelho Respiratório",
      CAPITULO_CID == "P"                  ~ "Afecções Perinatais",
      CAPITULO_CID == "Q"                  ~ "Malformações Congênitas",
      CAPITULO_CID %in% c("V","W","X","Y") ~ "Causas Externas",
      TRUE                                 ~ "Outras Causas"
    ),
    
    CAUSA_PRIORITARIA = case_when(
      CAPITULO_CID %in% c("A", "B", "J")  ~ "Ação Prioritária (Prevenção/Tratamento)",
      CAPITULO_CID == "P"                 ~ "Atenção à Gestação e Parto",
      CAPITULO_CID == "Q"                 ~ "Malformações (Alta Complexidade)",
      TRUE                                ~ "Outras Causas / Difícil Prevenção"
    )
  )

# ==============================================================================
# 4) DENOMINADOR: Lendo CSV Estadual do SINASC Formatado
# ==============================================================================
cat("══════════════════════════════════════════════════════════\n")
cat("  ETAPA 2: LENDO NASCIDOS VIVOS (SINASC POR UF)\n")
cat("══════════════════════════════════════════════════════════\n\n")

arquivo_sinasc <- file.path(dir_dados, "sinasc_cnv_nvuf113232189_120_73_168.csv")

if (!file.exists(arquivo_sinasc)) {
  stop(paste("Erro: Arquivo do SINASC não encontrado em", arquivo_sinasc))
}

# Lendo o CSV com locale ajustado para pegar acentuação padrão de Excel no Brasil
sinasc_raw <- read_csv2(arquivo_sinasc, locale = locale(encoding = "ISO-8859-1"), show_col_types = FALSE)

sinasc_uf <- sinasc_raw %>%
  rename(UF_NOME = 1) %>% # Pega a primeira coluna (região/uf) independente do nome exato
  mutate(
    NOME_ESTADO = str_trim(UF_NOME),
    cod_estado  = mapa_estados_inverso[NOME_ESTADO]
  ) %>%
  # Filtra para manter apenas as linhas que bateram com o nome de algum Estado
  filter(!is.na(cod_estado)) %>%
  # Transforma os anos em colunas para formato longo
  pivot_longer(
    cols = matches("^[0-9]{4}$"), # Pega todas as colunas que são "2015", "2016", etc
    names_to = "ano",
    values_to = "nascidos_vivos"
  ) %>%
  mutate(
    ano = as.numeric(ano),
    nascidos_vivos = as.numeric(nascidos_vivos)
  ) %>%
  select(ano, cod_estado, NOME_ESTADO, nascidos_vivos) %>%
  filter(!is.na(nascidos_vivos))

# Agregações do Denominador
sinasc_br <- sinasc_uf %>%
  group_by(ano) %>%
  summarise(nascidos_br = sum(nascidos_vivos), .groups = "drop")

sinasc_macro <- sinasc_uf %>%
  mutate(MACRORREGIAO = mapa_regioes[substr(cod_estado, 1, 1)]) %>%
  group_by(ano, MACRORREGIAO) %>%
  summarise(nascidos_macro = sum(nascidos_vivos), .groups = "drop")

cat("  ✓ Arquivo Estadual do SINASC lido e formatado com sucesso!\n")
cat("    Total de nascidos vivos (2024):", 
    format(sinasc_br$nascidos_br[sinasc_br$ano == max(sinasc_br$ano)], big.mark = "."), "\n\n")


# ==============================================================================
# 5) CALIBRAGEM DA TAXA
# ==============================================================================
cat("══════════════════════════════════════════════════════════\n")
cat("  ETAPA 3: CALIBRAGEM DA TAXA DE MORTALIDADE\n")
cat("══════════════════════════════════════════════════════════\n\n")

MULT <- 1000
LABEL_TAXA <- "por 1.000 nascidos vivos"

# Calculando a TMI (menores de 1 ano) para referência do console
obitos_infantis <- base_analise %>%
  filter(GRUPO_ETARIO %in% c("Neonatal Precoce (0-6 dias)", 
                             "Neonatal Tardia (7-27 dias)", 
                             "Pós-Neonatal (28 dias a <1 ano)"))

tmi_media <- (nrow(obitos_infantis) / length(unique(base_analise$ANO_OBITO))) / 
  (sum(sinasc_br$nascidos_br) / length(unique(sinasc_br$ano))) * MULT

cat("  Taxa de Mortalidade Infantil (< 1 ano) média no período:\n")
cat("  →", round(tmi_media, 2), LABEL_TAXA, "\n\n")


# ==============================================================================
# 6) FIGURAS
# ==============================================================================
cat("══════════════════════════════════════════════════════════\n")
cat("  ETAPA 4: GERANDO FIGURAS\n")
cat("══════════════════════════════════════════════════════════\n\n")

# ── Fig01: Evolução por Faixa Etária (ABSOLUTO) ────────
cat("  Fig01: Evolução por Faixa Etária (absoluto)...\n")

evolucao_etaria <- base_analise %>%
  filter(GRUPO_ETARIO != "Sem informação precisa") %>%
  count(ANO_OBITO, GRUPO_ETARIO)

g1 <- ggplot(evolucao_etaria, aes(x = ANO_OBITO, y = n, color = GRUPO_ETARIO, group = GRUPO_ETARIO)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3, shape = 21, fill = "white", stroke = 1.2) +
  scale_color_manual(values = cores_faixa) +
  scale_x_continuous(breaks = 2015:2024) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Evolução da Mortalidade por Faixa Etária (0 a 6 Anos)",
    subtitle = "Brasil, 2015–2024 | Óbitos absolutos",
    x = "Ano", y = "Número de Óbitos",
    caption = "Fonte: SIM/DATASUS"
  ) +
  tema_executivo()

ggsave(file.path(dir_analises, "Fig01_Evolucao_Faixa_Etaria_Absoluto.png"), g1, width = 10, height = 6, dpi = 300)

# ── Fig02: Evolução das Causas — TAXA ponderada pelo SINASC ────────────────
cat("  Fig02: Evolução das Causas (taxa)...\n")

causas_taxa <- base_analise %>%
  count(ANO_OBITO, CAPITULO_DESC, name = "obitos") %>%
  left_join(sinasc_br, by = c("ANO_OBITO" = "ano")) %>%
  mutate(taxa = obitos / nascidos_br * MULT)

g2 <- ggplot(causas_taxa, aes(x = ANO_OBITO, y = taxa, fill = CAPITULO_DESC)) +
  geom_area(alpha = 0.85, color = "white", linewidth = 0.2) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(breaks = 2015:2024) +
  labs(
    title    = "Evolução das Causas de Mortalidade (0 a 6 Anos)",
    subtitle = paste0("Taxa ", LABEL_TAXA, " | 2015–2024"),
    x = "Ano", y = paste0("Taxa (", LABEL_TAXA, ")"),
    caption = "Fonte: SIM e SINASC/DATASUS"
  ) +
  tema_executivo() +
  theme(legend.position = "right")

ggsave(file.path(dir_analises, "Fig02_Evolucao_Causas_Taxa.png"), g2, width = 10, height = 6, dpi = 300)

# ── Fig03: Causas Prioritárias POR FAIXA ETÁRIA ──────────────────
cat("  Fig03: Causas Prioritárias por Faixa Etária...\n")

causas_prio_faixa <- base_analise %>%
  filter(GRUPO_ETARIO != "Sem informação precisa") %>%
  count(GRUPO_ETARIO, CAUSA_PRIORITARIA) %>%
  group_by(GRUPO_ETARIO) %>%
  mutate(Perc = n / sum(n)) %>%
  ungroup()

g3 <- ggplot(causas_prio_faixa, aes(x = reorder(CAUSA_PRIORITARIA, Perc), y = Perc)) +
  geom_col(fill = "#2c3e50", width = 0.65) +
  geom_text(aes(label = percent(Perc, accuracy = 0.1)), hjust = -0.1, color = "#2c3e50", fontface = "bold", size = 3.2) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.25))) +
  facet_wrap(~GRUPO_ETARIO, ncol = 2, scales = "free_x") +
  labs(
    title    = "Causas Prioritárias de Mortalidade por Faixa Etária",
    subtitle = "Proporção de óbitos por grupo de ação | Brasil, 2015–2024",
    x = NULL, y = "Proporção do Total de Óbitos",
    caption = "Fonte: SIM/DATASUS"
  ) +
  tema_executivo() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        strip.text = element_text(face = "bold", size = 11, color = "#2c3e50"),
        strip.background = element_rect(fill = "#ecf0f1", color = NA))

ggsave(file.path(dir_analises, "Fig03_Causas_Prioritarias_por_Faixa.png"), g3, width = 12, height = 8, dpi = 300)

# ── Fig04: Heterogeneidade por Macrorregião — TAXA ponderada ─────────────────
cat("  Fig04: Heterogeneidade Regional (taxa)...\n")

hetero_taxa <- base_analise %>%
  filter(!is.na(MACRORREGIAO), GRUPO_ETARIO != "Sem informação precisa") %>%
  count(MACRORREGIAO, GRUPO_ETARIO, ANO_OBITO, name = "obitos") %>%
  left_join(sinasc_macro, by = c("ANO_OBITO" = "ano", "MACRORREGIAO")) %>%
  group_by(MACRORREGIAO, GRUPO_ETARIO) %>%
  summarise(
    obitos_total = sum(obitos),
    nv_total     = sum(nascidos_macro, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(taxa_media = obitos_total / nv_total * MULT)

g4 <- ggplot(hetero_taxa, aes(x = reorder(MACRORREGIAO, -taxa_media), y = taxa_media, fill = GRUPO_ETARIO)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = cores_faixa) +
  labs(
    title    = "Heterogeneidade Regional — Taxa de Mortalidade por Faixa Etária",
    subtitle = paste0("Taxa média ", LABEL_TAXA, " | Acumulado 2015–2024"),
    x = "Macrorregião", y = paste0("Taxa (", LABEL_TAXA, ")"),
    caption = "Fonte: SIM e SINASC/DATASUS"
  ) +
  tema_executivo()

ggsave(file.path(dir_analises, "Fig04_Heterogeneidade_Macro_Taxa.png"), g4, width = 10, height = 6, dpi = 300)

# ==============================================================================
# 7) MAPAS DE TAXA (Estado e Macrorregião)
# ==============================================================================

# ── Fig05: Mapa por Estado ───────────────────────────────────────────────────
cat("  Fig05: Mapa de Taxa por Estado...\n")

obitos_uf <- base_analise %>%
  count(cod_estado = COD_ESTADO, ANO_OBITO, name = "obitos") %>%
  left_join(sinasc_uf, by = c("ANO_OBITO" = "ano", "cod_estado")) %>%
  group_by(cod_estado) %>%
  summarise(
    obitos_total = sum(obitos),
    nv_total     = sum(nascidos_vivos, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    code_state = as.numeric(cod_estado),
    taxa_estado = obitos_total / nv_total * MULT
  )

malha_br <- read_state(year = 2020, showProgress = FALSE) %>%
  mutate(code_state = as.numeric(code_state))

mapa_uf <- left_join(malha_br, obitos_uf, by = "code_state")

g5_uf <- ggplot(mapa_uf) +
  geom_sf(aes(fill = taxa_estado), color = "black", linewidth = 0.2) +
  scale_fill_gradientn(
    colors = brewer.pal(9, "YlOrRd")[2:9],
    name = "Taxa por\n1.000 N.V.",
    labels = function(x) round(x, 1)
  ) +
  labs(
    title    = "Taxa de Mortalidade por Estado (0 a 6 Anos)",
    subtitle = paste0("Óbitos ", LABEL_TAXA, " | Acumulado 2015–2024"),
    caption  = "Fonte: SIM e SINASC/DATASUS"
  ) +
  theme_void() +
  theme(
    plot.title    = element_text(face = "bold", size = 16, color = "#2c3e50", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "#7f8c8d", hjust = 0.5, margin = margin(b = 15)),
    plot.caption  = element_text(size = 9, color = "#95a5a6", hjust = 0),
    legend.position = "right",
    plot.background  = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white")
  )

ggsave(file.path(dir_analises, "Fig05_Mapa_Taxa_Estado.png"), g5_uf, width = 8, height = 8, dpi = 300)

# ── Fig06: Mapa por Macrorregião ──────────────────────────────────────────
cat("  Fig06: Mapa de Taxa por Macrorregião...\n")

obitos_macro <- base_analise %>%
  filter(!is.na(MACRORREGIAO)) %>%
  count(MACRORREGIAO, ANO_OBITO, name = "obitos") %>%
  left_join(sinasc_macro, by = c("ANO_OBITO" = "ano", "MACRORREGIAO")) %>%
  group_by(MACRORREGIAO) %>%
  summarise(
    obitos_total = sum(obitos),
    nv_total     = sum(nascidos_macro, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(taxa_macro = obitos_total / nv_total * MULT)

macro_lookup <- tibble(
  code_state   = as.numeric(names(mapa_estados)),
  MACRORREGIAO = mapa_regioes[substr(names(mapa_estados), 1, 1)]
)

mapa_macro <- malha_br %>%
  left_join(macro_lookup, by = "code_state") %>%
  left_join(obitos_macro %>% select(MACRORREGIAO, taxa_macro), by = "MACRORREGIAO") %>%
  group_by(MACRORREGIAO) %>%
  summarise(taxa_macro = first(taxa_macro), .groups = "drop")

g6_macro <- ggplot(mapa_macro) +
  geom_sf(aes(fill = taxa_macro), color = "black", linewidth = 0.3) +
  scale_fill_gradientn(
    colors = brewer.pal(9, "YlOrRd")[2:9],
    name = "Taxa por\n1.000 N.V.",
    labels = function(x) round(x, 1)
  ) +
  labs(
    title    = "Taxa de Mortalidade por Macrorregião (0 a 6 Anos)",
    subtitle = paste0("Óbitos ", LABEL_TAXA, " | Acumulado 2015–2024"),
    caption  = "Fonte: SIM e SINASC/DATASUS"
  ) +
  theme_void() +
  theme(
    plot.title    = element_text(face = "bold", size = 16, color = "#2c3e50", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "#7f8c8d", hjust = 0.5, margin = margin(b = 15)),
    plot.caption  = element_text(size = 9, color = "#95a5a6", hjust = 0),
    legend.position = "right",
    plot.background  = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white")
  )

ggsave(file.path(dir_analises, "Fig06_Mapa_Taxa_Macrorregiao.png"), g6_macro, width = 8, height = 8, dpi = 300)

# ==============================================================================
# 8) FLUXO RESIDÊNCIA → MUNICÍPIO DE ÓBITO
# ==============================================================================
cat("\n══════════════════════════════════════════════════════════\n")
cat("  ETAPA 5: ANÁLISE DE FLUXO MUNICIPAL E ESTADUAL\n")
cat("══════════════════════════════════════════════════════════\n\n")

fluxo <- base_analise %>%
  filter(!is.na(COD_MUN_RES), !is.na(COD_MUN_OCOR),
         nchar(COD_MUN_RES) >= 6, nchar(COD_MUN_OCOR) >= 6) %>%
  mutate(
    OBITO_FORA    = COD_MUN_RES != COD_MUN_OCOR,
    UF_RES        = substr(COD_MUN_RES, 1, 2),
    UF_OCOR       = substr(COD_MUN_OCOR, 1, 2),
    OBITO_FORA_UF = UF_RES != UF_OCOR
  )

# ── Fig07: Proporção fora da residência por UF ───────────────────────────────
cat("  Fig07: Proporção de óbitos fora do município...\n")

fluxo_uf <- fluxo %>%
  group_by(UF_RES) %>%
  summarise(
    total        = n(),
    fora_mun     = sum(OBITO_FORA),
    fora_uf      = sum(OBITO_FORA_UF),
    perc_fora_mun = fora_mun / total,
    perc_fora_uf  = fora_uf / total,
    .groups = "drop"
  ) %>%
  mutate(NOME_ESTADO = mapa_estados[UF_RES]) %>%
  filter(!is.na(NOME_ESTADO))

g7 <- ggplot(fluxo_uf, aes(x = reorder(NOME_ESTADO, perc_fora_mun), y = perc_fora_mun)) +
  geom_col(fill = "#8e44ad", width = 0.6) +
  geom_text(aes(label = percent(perc_fora_mun, accuracy = 0.1)), hjust = -0.1, size = 3, color = "#2c3e50") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Proporção de Óbitos Fora do Município de Residência",
    subtitle = "Crianças de 0 a 6 anos | Por UF de residência | 2015–2024",
    x = NULL, y = "% de óbitos fora do município",
    caption = "Fonte: SIM/DATASUS"
  ) +
  tema_executivo() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())

ggsave(file.path(dir_analises, "Fig07_Fluxo_Obitos_Fora_Municipio.png"), g7, width = 11, height = 8, dpi = 300)

# ── Fig08: TOP 30 Municípios Polo ─────────────
cat("  Fig08: Top 30 Polos de Saúde Infantil...\n")

polos <- fluxo %>%
  filter(OBITO_FORA) %>%
  count(COD_MUN_OCOR, name = "obitos_recebidos") %>%
  arrange(desc(obitos_recebidos)) %>%
  slice_head(n = 30)

mun_nomes <- tryCatch({
  geobr::lookup_muni(code_muni = "all") %>%
    mutate(cod_mun_6 = substr(as.character(code_muni), 1, 6)) %>%
    select(cod_mun_6, name_muni, abbrev_state) %>%
    distinct()
}, error = function(e) { NULL })

if (!is.null(mun_nomes)) {
  polos <- polos %>%
    left_join(mun_nomes, by = c("COD_MUN_OCOR" = "cod_mun_6")) %>%
    mutate(label = ifelse(!is.na(name_muni), paste0(name_muni, " (", abbrev_state, ")"), COD_MUN_OCOR))
} else {
  polos$label <- polos$COD_MUN_OCOR
}

g8 <- ggplot(polos, aes(x = reorder(label, obitos_recebidos), y = obitos_recebidos)) +
  geom_col(fill = "#e74c3c", width = 0.6) +
  geom_text(aes(label = comma(obitos_recebidos)), hjust = -0.1, size = 3, color = "#2c3e50", fontface = "bold") +
  coord_flip() +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Top 30 Municípios Polo de Saúde Infantil",
    subtitle = "Óbitos de crianças NÃO residentes ocorridos no município | 2015–2024",
    x = NULL, y = "Óbitos recebidos",
    caption  = "Fonte: SIM/DATASUS"
  ) +
  tema_executivo() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())

ggsave(file.path(dir_analises, "Fig08_Polos_Saude_Infantil_Top30.png"), g8, width = 11, height = 9, dpi = 300)

# ── Fig09: Heatmap de Fluxo Inter-UF ─────────────────────────────────────────
cat("  Fig09: Heatmap de Fluxo Inter-UF...\n")

fluxo_interuf <- fluxo %>%
  filter(OBITO_FORA_UF) %>%
  count(UF_RES, UF_OCOR, name = "obitos") %>%
  mutate(NM_RES = mapa_estados[UF_RES], NM_OCOR = mapa_estados[UF_OCOR]) %>%
  filter(!is.na(NM_RES), !is.na(NM_OCOR)) %>%
  arrange(desc(obitos)) %>%
  slice_head(n = 50) 

g9 <- ggplot(fluxo_interuf, aes(x = NM_OCOR, y = NM_RES, fill = obitos)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradientn(colors = brewer.pal(9, "Reds")[2:9], name = "Óbitos", labels = comma) +
  geom_text(aes(label = comma(obitos)), size = 2.5, color = "white", fontface = "bold") +
  labs(
    title    = "Fluxo de Mortalidade Infantil entre UFs",
    subtitle = "Top 50 pares UF residência → UF de óbito | 2015–2024",
    x = "UF de Ocorrência do Óbito", y = "UF de Residência",
    caption = "Fonte: SIM/DATASUS"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 14, color = "#2c3e50"),
    plot.subtitle = element_text(size = 10, color = "#7f8c8d"),
    axis.text.x   = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y   = element_text(size = 8),
    plot.background  = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white")
  )

ggsave(file.path(dir_analises, "Fig09_Heatmap_Fluxo_InterUF.png"), g9, width = 12, height = 10, dpi = 300)


# ==============================================================================
# 9) TABELAS EXECUTIVAS (EXCEL)
# ==============================================================================
cat("\n══════════════════════════════════════════════════════════\n")
cat("  ETAPA 6: TABELAS EXECUTIVAS (EXCEL)\n")
cat("══════════════════════════════════════════════════════════\n\n")

tab_evolucao      <- evolucao_etaria %>% pivot_wider(names_from = GRUPO_ETARIO, values_from = n, values_fill = 0) %>% arrange(ANO_OBITO)
tab_causas        <- base_analise %>% count(ANO_OBITO, CAPITULO_DESC) %>% pivot_wider(names_from = CAPITULO_DESC, values_from = n, values_fill = 0) %>% arrange(ANO_OBITO)
tab_prio_faixa    <- causas_prio_faixa %>% select(GRUPO_ETARIO, CAUSA_PRIORITARIA, n, Perc) %>% arrange(GRUPO_ETARIO, desc(Perc))
tab_fluxo         <- fluxo_uf %>% select(NOME_ESTADO, total, fora_mun, perc_fora_mun, fora_uf, perc_fora_uf) %>% arrange(desc(perc_fora_mun))
tab_polos         <- polos %>% select(any_of(c("label", "COD_MUN_OCOR", "obitos_recebidos"))) %>% arrange(desc(obitos_recebidos))
tab_fluxo_interuf <- fluxo_interuf %>% select(NM_RES, NM_OCOR, obitos) %>% arrange(desc(obitos))

tab_taxa_uf <- obitos_uf %>%
  mutate(NOME_ESTADO = mapa_estados[cod_estado]) %>%
  select(NOME_ESTADO, obitos_total, nascidos_vivos = nv_total, taxa_estado) %>%
  arrange(desc(taxa_estado))

tab_taxa_macro <- obitos_macro %>%
  select(MACRORREGIAO, obitos_total, nascidos_vivos = nv_total, taxa_macro) %>%
  arrange(desc(taxa_macro))

abas_excel <- list(
  "1_Evolucao_Idade"       = tab_evolucao,
  "2_Evolucao_Causas"      = tab_causas,
  "3_Prio_por_Faixa"       = tab_prio_faixa,
  "4_Fluxo_por_UF"         = tab_fluxo,
  "5_Polos_Saude_Infantil" = tab_polos,
  "6_Fluxo_InterUF"        = tab_fluxo_interuf,
  "7_Taxa_por_UF"          = tab_taxa_uf,
  "8_Taxa_por_Macro"       = tab_taxa_macro
)

write_xlsx(abas_excel, path = file.path(dir_analises, "Tabelas_Executivas_Mortalidade_v2.xlsx"))

cat("══════════════════════════════════════════════════════════\n")
cat("  PROCESSAMENTO CONCLUÍDO COM SUCESSO!\n")
cat("══════════════════════════════════════════════════════════\n")
