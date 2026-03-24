############################################################
# PROJETO: SIM + SIH Brasil (2015-2024)
# MÓDULO: SIM-DO
# FOCO: Crianças de 0 a 6 anos
# ETAPA 2: Análises executivas e outputs gráficos
# DESTINO: Repositório público no GitHub
############################################################

# 1) Pacotes -------------------------------------------------------------------
pacotes <- c(
  "dplyr", "ggplot2", "readr", "writexl", "tidyr",
  "geobr", "sf", "RColorBrewer", "stringr", "scales"
)

for (p in pacotes) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

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

# 2) Diretórios do projeto -----------------------------------------------------
dir_data_processed <- file.path("data", "processed")
dir_outputs_figuras <- file.path("outputs", "figuras")
dir_outputs_tabelas <- file.path("outputs", "tabelas")

for (dir in c(dir_outputs_figuras, dir_outputs_tabelas)) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}

arquivo_base <- file.path(dir_data_processed, "sim_brasil_0a6_todas_vars_2015_2024.rds")

if (!file.exists(arquivo_base)) {
  stop(
    paste0(
      "A base principal do projeto não foi encontrada em: ", arquivo_base, "\n",
      "Execute primeiro o script 01_download_preparo_sim_0a6.R."
    )
  )
}

# 3) Tema gráfico --------------------------------------------------------------
tema_executivo <- function(...) {
  theme_classic(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16, color = "#1a252f"),
      plot.subtitle = element_text(size = 12, color = "#7f8c8d", margin = margin(b = 15)),
      axis.title = element_text(face = "bold", color = "#2c3e50"),
      axis.text = element_text(color = "#34495e"),
      axis.line = element_line(color = "#bdc3c7", linewidth = 0.5),
      panel.grid.major.y = element_line(color = "#ecf0f1", linetype = "dashed"),
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.background = element_rect(fill = "white", color = "white"),
      panel.background = element_rect(fill = "white", color = "white"),
      ...
    )
}

# 4) Carregamento e preparação dos dados --------------------------------------
cat("Carregando base principal do projeto (0 a 6 anos)...\n")
base_0a6 <- readRDS(arquivo_base)

cat("Categorizando faixas etárias, regiões e causas prioritárias...\n")

mapa_regioes <- c(
  "1" = "Norte", "2" = "Nordeste", "3" = "Sudeste",
  "4" = "Sul", "5" = "Centro-Oeste"
)

mapa_estados <- c(
  "11" = "Rondônia", "12" = "Acre", "13" = "Amazonas", "14" = "Roraima",
  "15" = "Pará", "16" = "Amapá", "17" = "Tocantins", "21" = "Maranhão",
  "22" = "Piauí", "23" = "Ceará", "24" = "Rio Grande do Norte", "25" = "Paraíba",
  "26" = "Pernambuco", "27" = "Alagoas", "28" = "Sergipe", "29" = "Bahia",
  "31" = "Minas Gerais", "32" = "Espírito Santo", "33" = "Rio de Janeiro",
  "35" = "São Paulo", "41" = "Paraná", "42" = "Santa Catarina", "43" = "Rio Grande do Sul",
  "50" = "Mato Grosso do Sul", "51" = "Mato Grosso", "52" = "Goiás", "53" = "Distrito Federal"
)

base_analise <- base_0a6 %>%
  mutate(
    UNIDADE_IDADE = substr(as.character(IDADE), 1, 1),
    VALOR_IDADE = suppressWarnings(as.numeric(substr(as.character(IDADE), 2, 3))),

    GRUPO_ETARIO = case_when(
      UNIDADE_IDADE %in% c("0", "1") | (UNIDADE_IDADE == "2" & VALOR_IDADE <= 6) ~ "Neonatal Precoce (0-6 dias)",
      UNIDADE_IDADE == "2" & VALOR_IDADE >= 7 & VALOR_IDADE <= 27 ~ "Neonatal Tardia (7-27 dias)",
      (UNIDADE_IDADE == "2" & VALOR_IDADE > 27) | UNIDADE_IDADE == "3" ~ "Pós-Neonatal (28 dias a <1 ano)",
      UNIDADE_IDADE == "4" & VALOR_IDADE >= 1 & VALOR_IDADE <= 6 ~ "1 a 6 anos",
      TRUE ~ "Sem informação precisa"
    ),

    COD_ESTADO = substr(CODMUNRES, 1, 2),
    MACRORREGIAO = mapa_regioes[substr(COD_ESTADO, 1, 1)],
    NOME_ESTADO = mapa_estados[COD_ESTADO],

    CAPITULO_DESC = case_when(
      CAPITULO_CID %in% c("A", "B") ~ "Infecciosas e Parasitárias",
      CAPITULO_CID == "J" ~ "Aparelho Respiratório",
      CAPITULO_CID == "P" ~ "Afecções Perinatais",
      CAPITULO_CID == "Q" ~ "Malformações Congênitas",
      CAPITULO_CID %in% c("V", "W", "X", "Y") ~ "Causas Externas",
      TRUE ~ "Outras Causas"
    ),

    CAUSA_PRIORITARIA = case_when(
      CAPITULO_CID %in% c("A", "B", "J") ~ "Ação Prioritária (Prevenção/Tratamento)",
      CAPITULO_CID == "P" ~ "Atenção à Gestação e Parto",
      CAPITULO_CID == "Q" ~ "Malformações (Necessidade de Alta Complexidade)",
      TRUE ~ "Outras Causas / Difícil Prevenção"
    )
  )

# 5) Figura 1: Evolução por padrão etário -------------------------------------
cat("Gerando Figura 1: Evolução por faixa etária...\n")

evolucao_etaria <- base_analise %>%
  filter(GRUPO_ETARIO != "Sem informação precisa") %>%
  count(ANO_OBITO, GRUPO_ETARIO)

g1 <- ggplot(evolucao_etaria, aes(x = ANO_OBITO, y = n, color = GRUPO_ETARIO, group = GRUPO_ETARIO)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3, shape = 21, fill = "white", stroke = 1.2) +
  scale_color_manual(values = c(
    "Neonatal Precoce (0-6 dias)" = "#c0392b",
    "Neonatal Tardia (7-27 dias)" = "#d35400",
    "Pós-Neonatal (28 dias a <1 ano)" = "#f39c12",
    "1 a 6 anos" = "#2980b9"
  )) +
  scale_x_continuous(breaks = 2015:2024) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Evolução da mortalidade por padrão etário",
    subtitle = "Brasil, 2015-2024 | Crianças de 0 a 6 anos",
    x = "Ano de ocorrência",
    y = "Número de óbitos"
  ) +
  tema_executivo()

ggsave(
  file.path(dir_outputs_figuras, "Fig01_Evolucao_Faixa_Etaria.png"),
  g1, width = 10, height = 6, dpi = 300
)

# 6) Figura 2: Evolução das causas --------------------------------------------
cat("Gerando Figura 2: Evolução das causas...\n")

causas_evolucao <- base_analise %>%
  count(ANO_OBITO, CAPITULO_DESC)

g2 <- ggplot(causas_evolucao, aes(x = ANO_OBITO, y = n, fill = CAPITULO_DESC)) +
  geom_area(alpha = 0.85, color = "white", linewidth = 0.2) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(breaks = 2015:2024) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Evolução das principais causas de mortalidade",
    subtitle = "Brasil, 2015-2024 | Crianças de 0 a 6 anos",
    x = "Ano",
    y = "Total de óbitos"
  ) +
  tema_executivo() +
  theme(legend.position = "right")

ggsave(
  file.path(dir_outputs_figuras, "Fig02_Evolucao_Causas.png"),
  g2, width = 10, height = 6, dpi = 300
)

# 7) Figura 3: Causas prioritárias --------------------------------------------
cat("Gerando Figura 3: Grupos de causas prioritárias...\n")

causas_prio <- base_analise %>%
  count(CAUSA_PRIORITARIA) %>%
  mutate(Perc = n / sum(n))

g3 <- ggplot(causas_prio, aes(x = reorder(CAUSA_PRIORITARIA, Perc), y = Perc)) +
  geom_col(fill = "#2c3e50", width = 0.6) +
  geom_text(
    aes(label = scales::percent(Perc, accuracy = 0.1)),
    hjust = -0.2, color = "#2c3e50", fontface = "bold"
  ) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = "Grupos de causas prioritárias para combate",
    subtitle = "Brasil, 2015-2024 | Crianças de 0 a 6 anos",
    x = NULL,
    y = "Proporção do total de óbitos"
  ) +
  tema_executivo() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())

ggsave(
  file.path(dir_outputs_figuras, "Fig03_Causas_Prioritarias.png"),
  g3, width = 10, height = 5, dpi = 300
)

# 8) Figura 4: Heterogeneidade regional ---------------------------------------
cat("Gerando Figura 4: Heterogeneidade regional...\n")

hetero_regiao <- base_analise %>%
  filter(!is.na(MACRORREGIAO), GRUPO_ETARIO != "Sem informação precisa") %>%
  count(MACRORREGIAO, GRUPO_ETARIO)

g4 <- ggplot(hetero_regiao, aes(x = reorder(MACRORREGIAO, -n), y = n, fill = GRUPO_ETARIO)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c(
    "Neonatal Precoce (0-6 dias)" = "#c0392b",
    "Neonatal Tardia (7-27 dias)" = "#d35400",
    "Pós-Neonatal (28 dias a <1 ano)" = "#f39c12",
    "1 a 6 anos" = "#2980b9"
  )) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Heterogeneidade regional da mortalidade",
    subtitle = "Brasil, 2015-2024 | Crianças de 0 a 6 anos",
    x = "Macrorregião",
    y = "Total de óbitos"
  ) +
  tema_executivo()

ggsave(
  file.path(dir_outputs_figuras, "Fig04_Heterogeneidade_Macrorregiao.png"),
  g4, width = 10, height = 6, dpi = 300
)

# 9) Figura 5: Mapa por estado -------------------------------------------------
cat("Gerando Figura 5: Mapa por estado...\n")

obitos_estado <- base_analise %>%
  count(code_state = as.numeric(COD_ESTADO), name = "total_obitos")

malha_br <- read_state(year = 2020, showProgress = FALSE) %>%
  mutate(code_state = as.numeric(code_state))

mapa_dados <- left_join(malha_br, obitos_estado, by = "code_state")

g_mapa <- ggplot(mapa_dados) +
  geom_sf(aes(fill = total_obitos), color = "black", linewidth = 0.2) +
  scale_fill_gradientn(
    colors = brewer.pal(9, "Blues")[3:9],
    name = "Óbitos\n(2015-2024)",
    labels = scales::comma
  ) +
  labs(
    title = "Distribuição espacial da mortalidade",
    subtitle = "Brasil, 2015-2024 | Crianças de 0 a 6 anos",
    caption = "Fonte: SIM/DataSUS"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "#7f8c8d", hjust = 0.5, margin = margin(b = 15)),
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white")
  )

ggsave(
  file.path(dir_outputs_figuras, "Fig05_Mapa_Mortalidade_Estados.png"),
  g_mapa, width = 8, height = 8, dpi = 300
)

# 10) Tabelas executivas -------------------------------------------------------
cat("Gerando tabelas executivas em Excel...\n")

tab_evolucao_etaria <- base_analise %>%
  filter(GRUPO_ETARIO != "Sem informação precisa") %>%
  count(ANO_OBITO, GRUPO_ETARIO) %>%
  pivot_wider(names_from = GRUPO_ETARIO, values_from = n, values_fill = 0) %>%
  arrange(ANO_OBITO)

tab_causas_ano <- base_analise %>%
  count(ANO_OBITO, CAPITULO_DESC) %>%
  pivot_wider(names_from = CAPITULO_DESC, values_from = n, values_fill = 0) %>%
  arrange(ANO_OBITO)

tab_prio_regiao <- base_analise %>%
  filter(!is.na(MACRORREGIAO)) %>%
  count(MACRORREGIAO, CAUSA_PRIORITARIA) %>%
  pivot_wider(names_from = CAUSA_PRIORITARIA, values_from = n, values_fill = 0) %>%
  arrange(MACRORREGIAO)

tab_estado_idade <- base_analise %>%
  filter(!is.na(NOME_ESTADO), GRUPO_ETARIO != "Sem informação precisa") %>%
  count(NOME_ESTADO, GRUPO_ETARIO) %>%
  pivot_wider(names_from = GRUPO_ETARIO, values_from = n, values_fill = 0) %>%
  arrange(NOME_ESTADO)

tab_regiao_idade <- base_analise %>%
  filter(!is.na(MACRORREGIAO), GRUPO_ETARIO != "Sem informação precisa") %>%
  count(MACRORREGIAO, GRUPO_ETARIO) %>%
  pivot_wider(names_from = GRUPO_ETARIO, values_from = n, values_fill = 0) %>%
  arrange(MACRORREGIAO)

tab_raca_ano <- base_analise %>%
  count(ANO_OBITO, RACA_DESC) %>%
  pivot_wider(names_from = RACA_DESC, values_from = n, values_fill = 0) %>%
  arrange(ANO_OBITO)

write_xlsx(
  list(
    "1_Evolucao_Idade" = tab_evolucao_etaria,
    "2_Evolucao_Causas" = tab_causas_ano,
    "3_Prioridades_Regiao" = tab_prio_regiao,
    "4_Estado_Idade" = tab_estado_idade,
    "5_Macrorregiao_Idade" = tab_regiao_idade,
    "6_Demografia_Raca" = tab_raca_ano
  ),
  path = file.path(dir_outputs_tabelas, "Tabelas_Executivas_Mortalidade_0a6.xlsx")
)

cat("\nETAPA 2 CONCLUÍDA!\n")
cat("Figuras geradas em: outputs/figuras\n")
cat("Tabela Excel gerada em: outputs/tabelas/Tabelas_Executivas_Mortalidade_0a6.xlsx\n")
