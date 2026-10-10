# =============================================================================
# Gráficos do relatório: mortalidade de menores de 5 anos por pneumonia,
# diarreia e demais causas (Brasil, regiões, e comparações internacionais)
# Fonte dos dados: Global Burden of Disease (GBD / IHME), arquivos CSV anexos
#
# Como usar:
#   1. Coloque os arquivos CSV em uma pasta (ajuste `dir_dados` abaixo).
#   2. Execute o script inteiro. Os gráficos são salvos em `dir_saida`
#      (PNG 300 dpi + PDF vetorial) e também ficam nos objetos g1, g2, g3...
# =============================================================================

# ---- 0. Pacotes -------------------------------------------------------------
pacotes <- c("readr", "dplyr", "tidyr", "stringr", "forcats",
             "ggplot2", "scales", "ggrepel")
faltam <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(faltam) > 0) install.packages(faltam)
invisible(lapply(pacotes, library, character.only = TRUE))

# ---- 1. Configurações -------------------------------------------------------
dir_dados <- "references/literature review and goals" 
dir_saida <- "outputs/reports" 
dir.create(dir_saida, showWarnings = FALSE, recursive = TRUE)

# Metas de referência (ver texto do relatório)
meta_diarreia   <- 100     # 1 por mil ~ 100 por 100 mil (aproximação)
meta_pneumonia  <-  60     # limiar do GBD 2023: < 60 por 100 mil

# Paleta e tema ---------------------------------------------------------------
cor_brasil <- "#009C3B"    # destaque para o Brasil
cor_neutra <- "#8C96A0"

cores_paises <- c(
  "Brasil"     = cor_brasil,
  "Argentina"  = "#56B4E9",
  "Chile"      = "#D55E00",
  "Costa Rica" = "#CC79A7",
  "Japão"      = "#E69F00",
  "Finlândia"  = "#0072B2",
  "Noruega"    = "#7B3294"
)

# Regiões do Brasil (nomes traduzidos a partir do CSV original em inglês)
regioes_rename <- c(
  "Brazil North"        = "Brasil - Norte",
  "Brazil Northeast"    = "Brasil - Nordeste",
  "Brazil Central-West" = "Brasil - Centro-Oeste",
  "Brazil Southeast"    = "Brasil - Sudeste",
  "Brazil South"        = "Brasil - Sul"
)
cores_regioes <- c(
  "Brasil - Norte"        = "#1B9E77",
  "Brasil - Nordeste"     = "#D95F02",
  "Brasil - Centro-Oeste" = "#7570B3",
  "Brasil - Sudeste"      = "#E7298A",
  "Brasil - Sul"          = "#66A61E"
)

# Países de grande população incluídos na Figura 8c
cores_grandes <- c(
  "China"          = "#B2182B",
  "Índia"          = "#F4A582",
  "Estados Unidos" = "#542788"
)

cores_full <- c(cores_paises, cores_regioes, cores_grandes)

# Paleta única de categorias de doença, reaproveitada em TODOS os gráficos de
# barra por doença (Fig. 6, Fig. 7 e Fig. 9), para que cada categoria apareça
# sempre com a mesma cor.
cores_causas <- c(
  "Afecções maternas e neonatais"              = "#E7298A",
  "Outras doenças não transmissíveis"          = "#7570B3",
  "Infecções respiratórias e tuberculose"      = "#1B9E77",
  "Ferimentos não intencionais"                = "#E6AB02",
  "Neoplasias"                                 = "#D95F02",
  "Transtornos neurológicos"                   = "#66A61E",
  "Enterite infecciosa"                        = "#A6761D",
  "Outras doenças infecciosas"                 = "#999999",
  "Acidentes de transporte"                    = "#E31A1C",
  "Doenças tropicais negligenciadas e malária" = "#B15928",
  "Doenças cardiovasculares"                   = "#6A3D9A",
  "Demais causas"                              = "grey70"
)
# Restante (não destacado) da categoria "Outras doenças não transmissíveis"
# na Fig. 9, em um tom mais claro da mesma cor da categoria-mãe.
cor_ndt_resto <- "#BFBDE0"
# Cores fixas para as duas causas destacadas a pedido do relatório
cor_congenitas <- "#CC3311"
cor_endocrino  <- "#0077BB"

tema_rel <- theme_minimal(base_size = 11) +
  theme(
    plot.title        = element_text(face = "bold", size = 13),
    plot.subtitle     = element_text(colour = "grey30"),
    plot.caption      = element_text(colour = "grey40", size = 8, hjust = 0),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    panel.grid.minor  = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position   = "bottom",
    strip.text        = element_text(face = "bold"),
    axis.title        = element_text(colour = "grey25")
  )
theme_set(tema_rel)

# Formatação numérica em português (vírgula decimal)
num_br <- label_number(big.mark = ".", decimal.mark = ",", accuracy = 0.1)
int_br <- label_number(big.mark = ".", decimal.mark = ",", accuracy = 1)
pct_br <- label_percent(decimal.mark = ",", accuracy = 0.1)

# ---- 2. Leitura e padronização dos dados ------------------------------------
ler <- function(arquivo) {
  read_csv(file.path(dir_dados, arquivo), show_col_types = FALSE,
           locale = locale(encoding = "UTF-8")) |>
    rename(local = Localidade, ano = Ano, idade = Idade, causa = 6,
           medida = Medir, valor = Valor, li = `Limite inferior`,
           ls = `Limite superior`) |>
    filter(!is.na(ano)) |>                     # remove linhas vazias no fim
    mutate(ano = as.integer(ano))
}
renomear_regioes <- function(df) mutate(df, local = recode(local, !!!regioes_rename))

# Séries temporais 1990-2023 (mortes por 100 mil, menores de 5 anos)
# Agora incluem as regiões do Brasil, China, Índia e Estados Unidos.
diarreia  <- ler("diarreia_2.csv")                       |> renomear_regioes()
resp      <- ler("infeccao_respiratoria_tuberculose_2.csv") |> renomear_regioes()
neoplasia <- ler("neoplasias_2.csv")                     |> renomear_regioes()
neonatal  <- ler("afeccoes_maternas_2.csv")              |> renomear_regioes()

# Corte transversal 2023 (não alterado nesta rodada)
paises_all  <- ler("countries_under_five_mortality.csv")   # todas as causas
causas_pais <- ler("under_five_causes_countries.csv")      # % por causa

# Causas detalhadas por faixa etária, 2023 (nível mais granular: ~300 causas)
detalhe <- bind_rows(ler("brasil_2.csv"), ler("chile_2.csv"), ler("japao_2.csv"))

# Ordem dos países nas legendas / painéis
ordem_paises <- names(cores_paises)
ordena_pais <- function(df) mutate(df, local = factor(local, levels = ordem_paises))

# Função para salvar PNG + PDF
salvar <- function(grafico, nome, largura = 8, altura = 5) {
  ggsave(file.path(dir_saida, paste0(nome, ".png")), grafico,
         width = largura, height = altura, dpi = 300, bg = "white")
  ggsave(file.path(dir_saida, paste0(nome, ".pdf")), grafico,
         width = largura, height = altura, device = cairo_pdf)
  invisible(grafico)
}

# =============================================================================
# FIGURA 1 - Diarreia no Brasil vs. meta do GAPPD
# =============================================================================
d_br <- diarreia |> filter(local == "Brasil")

g1 <- ggplot(d_br, aes(ano, valor)) +
  geom_ribbon(aes(ymin = li, ymax = ls), fill = cor_brasil, alpha = .18) +
  geom_line(colour = cor_brasil, linewidth = 1.1) +
  geom_hline(yintercept = meta_diarreia, linetype = "dashed", colour = "firebrick") +
  annotate("text", x = 2023, y = meta_diarreia + 8, hjust = 1, size = 3.3,
           colour = "firebrick",
           label = "Meta GAPPD 2025: < 1 por mil (≈ 100 por 100 mil)") +
  geom_point(data = filter(d_br, ano %in% c(1990, 2023)), colour = cor_brasil, size = 2.5) +
  geom_text(data = filter(d_br, ano %in% c(1990, 2023)),
            aes(label = num_br(valor), hjust = if_else(ano == 1990, -0.2, 1)),
            vjust = -1, size = 3.5, fontface = "bold") +
  scale_x_continuous(breaks = seq(1990, 2020, 5), expand = expansion(mult = c(.02, .04))) +
  scale_y_continuous(labels = int_br, limits = c(0, NA), expand = expansion(mult = c(0, .12))) +
  labs(title = "Mortalidade por doenças diarreicas em menores de 5 anos, Brasil",
       subtitle = "Óbitos por 100 mil crianças, 1990-2023 (faixa: intervalo de incerteza de 95%)",
       x = NULL, y = "Óbitos por 100 mil",
       caption = "A linha da meta converte 1 por mil nascidos vivos em 100 por 100 mil de forma aproximada.")
salvar(g1, "fig1_diarreia_brasil")

# =============================================================================
# FIGURA 2 - Infecções respiratórias (proxy de pneumonia): Brasil e regiões
# =============================================================================
r_br_reg <- resp |>
  filter(local %in% c("Brasil", names(cores_regioes))) |>
  mutate(local = factor(local, levels = c("Brasil", names(cores_regioes))))

g2 <- ggplot(r_br_reg, aes(ano, valor, colour = local, group = local)) +
  geom_hline(yintercept = meta_pneumonia, linetype = "dashed", colour = "firebrick") +
  annotate("text", x = 1990, y = meta_pneumonia + 10, hjust = 0, size = 3.3,
           colour = "firebrick", label = "Limiar GBD 2023: < 60 por 100 mil") +
  geom_line(aes(linewidth = as.character(local == "Brasil"))) +
  scale_colour_manual(values = cores_full, name = NULL) +
  scale_linewidth_manual(values = c("TRUE" = 1.5, "FALSE" = .8), guide = "none") +
  scale_x_continuous(breaks = seq(1990, 2020, 5), expand = expansion(mult = c(.02, .04))) +
  scale_y_continuous(labels = int_br, limits = c(0, NA), expand = expansion(mult = c(0, .12))) +
  guides(colour = guide_legend(nrow = 1, override.aes = list(linewidth = 1.2))) +
  labs(title = "Mortalidade por infecções respiratórias e tuberculose em menores de 5 anos",
       subtitle = "Brasil e grandes regiões, óbitos por 100 mil crianças, 1990-2023",
       x = NULL, y = "Óbitos por 100 mil",
       caption = "A categoria do GBD agrega infecções respiratórias e tuberculose; é usada aqui como aproximação da pneumonia.")
salvar(g2, "fig2_pneumonia_brasil_regioes", largura = 9, altura = 5.5)

# =============================================================================
# FIGURA 3 - Trajetórias comparadas (diarreia e respiratórias), escala log
# =============================================================================
comp <- bind_rows(
  diarreia |> mutate(painel = "Doenças diarreicas"),
  resp     |> mutate(painel = "Infecções respiratórias e tuberculose")
) |> ordena_pais()

metas <- tibble(painel = c("Doenças diarreicas", "Infecções respiratórias e tuberculose"),
                meta   = c(meta_diarreia, meta_pneumonia))

rotulos_fim <- comp |> filter(ano == 2023)

g3 <- ggplot(comp, aes(ano, valor, colour = local, group = local)) +
  geom_hline(data = metas, aes(yintercept = meta), linetype = "dashed", colour = "grey40") +
  geom_line(aes(linewidth = as.character(local == "Brasil"))) +
  geom_text_repel(data = rotulos_fim, aes(label = local), size = 3, direction = "y",
                  hjust = 0, nudge_x = 1.2, segment.size = .2, show.legend = FALSE,
                  xlim = c(2023.5, NA)) +
  facet_wrap(~ painel, ncol = 2) +
  scale_colour_manual(values = cores_paises, guide = "none") +
  scale_linewidth_manual(values = c("TRUE" = 1.4, "FALSE" = .7), guide = "none") +
  scale_x_continuous(breaks = seq(1990, 2020, 10), expand = expansion(mult = c(.02, .02))) +
  scale_y_log10(labels = num_br) +
  coord_cartesian(clip = "off", xlim = c(1990, 2033)) +
  labs(title = "Mortalidade por diarreia e por infecções respiratórias em menores de 5 anos",
       subtitle = "Brasil e países de comparação, 1990-2023 (escala logarítmica; tracejado: referência de meta)",
       x = NULL, y = "Óbitos por 100 mil (escala log)")
salvar(g3, "fig3_comparacao_series", largura = 10, altura = 4.8)

# =============================================================================
# FIGURA 4 - Valores de 2023 por país (barras com intervalo de incerteza)
# =============================================================================
b23 <- comp |> filter(ano == 2023) |>
  group_by(painel) |>
  mutate(local = fct_reorder(as.character(local), valor)) |>
  ungroup()

g4 <- ggplot(b23, aes(valor, local, fill = local == "Brasil")) +
  geom_col(width = .7) +
  geom_errorbar(aes(xmin = li, xmax = ls), width = .25, colour = "grey25", orientation = "y") +
  geom_text(aes(x = ls, label = num_br(valor)), hjust = -.2, size = 3.2) +
  geom_vline(data = metas, aes(xintercept = meta), linetype = "dashed", colour = "firebrick") +
  facet_wrap(~ painel, scales = "free_x") +
  scale_fill_manual(values = c("TRUE" = cor_brasil, "FALSE" = cor_neutra), guide = "none") +
  scale_x_continuous(labels = num_br, expand = expansion(mult = c(0, .2))) +
  labs(title = "Mortalidade por diarreia e infecções respiratórias, 2023",
       subtitle = "Óbitos por 100 mil menores de 5 anos (linha tracejada: meta/limiar de referência)",
       x = "Óbitos por 100 mil", y = NULL)
salvar(g4, "fig4_comparacao_2023", largura = 10, altura = 4.5)

# =============================================================================
# FIGURA 5 - Mortalidade de menores de 5 anos por todas as causas, 2023
# =============================================================================
selecionados <- c(
  # menores taxas do mundo
  "Cingapura", "Japão", "Finlândia", "Noruega", "Suécia", "Coreia do Sul", "Portugal",
  # América Latina
  "Uruguai", "Chile", "Costa Rica", "Cuba", "Argentina", "Colômbia",
  # foco
  "Brasil"
)
america_latina <- c("Uruguai", "Chile", "Costa Rica", "Cuba", "Argentina", "Colômbia")

t5 <- paises_all |>
  filter(local %in% selecionados) |>
  mutate(grupo = case_when(local == "Brasil"           ~ "Brasil",
                           local %in% america_latina   ~ "América Latina",
                           TRUE                        ~ "Menores taxas do mundo"),
         local = fct_reorder(local, valor))

g5 <- ggplot(t5, aes(valor, local, fill = grupo)) +
  geom_col(width = .7) +
  geom_errorbar(aes(xmin = li, xmax = ls), width = .25, colour = "grey25", orientation = "y") +
  geom_text(aes(x = ls, label = int_br(valor)), hjust = -.2, size = 3.2) +
  scale_fill_manual(values = c("Brasil" = cor_brasil, "América Latina" = "#E69F00",
                               "Menores taxas do mundo" = "#0072B2"), name = NULL) +
  scale_x_continuous(labels = int_br, expand = expansion(mult = c(0, .12))) +
  labs(title = "Mortalidade de menores de 5 anos por todas as causas, 2023",
       subtitle = "Óbitos por 100 mil crianças menores de 5 anos (barras de erro: intervalo de incerteza de 95%)",
       x = "Óbitos por 100 mil", y = NULL)
salvar(g5, "fig5_mortalidade_total_2023", largura = 8.5, altura = 5.5)

# Razões citadas no texto (impressas no console)
v <- function(p) paises_all$valor[paises_all$local == p]
message("Brasil / Japão     = ", round(v("Brasil") / v("Japão"), 1))
message("Brasil / Finlândia = ", round(v("Brasil") / v("Finlândia"), 1))
message("Brasil / Suécia    = ", round(v("Brasil") / v("Suécia"), 1))

# =============================================================================
# FIGURA 6 - Principais causas de morte de menores de 5 anos no Brasil, 2023
# =============================================================================
cb <- causas_pais |>
  filter(local == "Brasil", !is.na(valor), valor > 0) |>
  slice_max(valor, n = 10) |>
  mutate(cor_key = causa,
         causa = fct_reorder(str_wrap(causa, 32), valor))

g6 <- ggplot(cb, aes(valor, causa, fill = cor_key)) +
  geom_col(width = .7) +
  geom_errorbar(aes(xmin = li, xmax = ls), width = .25, colour = "grey25", orientation = "y") +
  geom_text(aes(x = ls, label = pct_br(valor)), hjust = -.2, size = 3.2) +
  scale_fill_manual(values = cores_causas, guide = "none") +
  scale_x_continuous(labels = label_percent(decimal.mark = ",", accuracy = 1),
                     expand = expansion(mult = c(0, .12))) +
  labs(title = "Principais causas de morte de menores de 5 anos no Brasil, 2023",
       subtitle = "Participação no total de óbitos (%); dez maiores causas",
       x = "% do total de óbitos", y = NULL,
       caption = "'Enterite infecciosa' corresponde ao grupo das doenças diarreicas.")
salvar(g6, "fig6_causas_brasil_2023", largura = 8.5, altura = 5)

# =============================================================================
# FIGURA 7 - Composição das causas de morte: Brasil e países de comparação
# =============================================================================
top_causas <- causas_pais |>
  filter(!is.na(valor)) |>
  group_by(causa) |> summarise(m = mean(valor), .groups = "drop") |>
  slice_max(m, n = 6) |> pull(causa)

comp_causas <- causas_pais |>
  filter(!is.na(valor)) |>
  mutate(causa_g = if_else(causa %in% top_causas, causa, "Demais causas")) |>
  group_by(local, causa_g) |> summarise(valor = sum(valor), .groups = "drop") |>
  mutate(causa_g = factor(causa_g, levels = c(top_causas, "Demais causas")),
         local   = factor(local, levels = rev(ordem_paises)))

g7 <- ggplot(comp_causas, aes(valor, local, fill = causa_g)) +
  geom_col(width = .75, colour = "white", linewidth = .3) +
  geom_text(aes(label = if_else(valor >= .05, label_percent(accuracy = 1)(valor), "")),
            position = position_stack(vjust = .5), size = 3, colour = "white") +
  scale_fill_manual(values = cores_causas, name = NULL,
                    labels = function(x) str_wrap(x, 28)) +
  scale_x_continuous(labels = label_percent(), expand = c(0, 0)) +
  guides(fill = guide_legend(nrow = 3)) +
  labs(title = "Composição das causas de morte de menores de 5 anos, 2023",
       subtitle = "Participação de cada causa no total de óbitos",
       x = NULL, y = NULL)
salvar(g7, "fig7_composicao_causas_paises", largura = 9.5, altura = 5.5)

# =============================================================================
# FIGURA 8 - Séries das principais causas: Brasil, regiões e comparações
#   8a - apenas as regiões do Brasil
#   8b - Argentina e Noruega + regiões do Brasil + total Brasil
#   8c - China, Índia, Estados Unidos, Brasil e os demais países
# =============================================================================
series_base <- bind_rows(
  neonatal  |> mutate(painel = "Afecções maternas e neonatais"),
  resp      |> mutate(painel = "Infecções respiratórias e tuberculose"),
  diarreia  |> mutate(painel = "Doenças diarreicas"),
  neoplasia |> mutate(painel = "Neoplasias")
) |>
  mutate(painel = factor(painel, levels = c("Afecções maternas e neonatais",
                                            "Infecções respiratórias e tuberculose",
                                            "Doenças diarreicas", "Neoplasias")))

plot_series_paineis <- function(df, paleta, destaque = character(0), legend_rows = 1) {
  ggplot(df, aes(ano, valor, colour = local, group = local)) +
    geom_line(aes(linewidth = as.character(local %in% destaque))) +
    facet_wrap(~ painel, ncol = 2, scales = "free_y") +
    scale_colour_manual(values = paleta, name = NULL) +
    scale_linewidth_manual(values = c("TRUE" = 1.4, "FALSE" = .7), guide = "none") +
    scale_x_continuous(breaks = seq(1990, 2020, 10)) +
    scale_y_continuous(labels = int_br, limits = c(0, NA)) +
    guides(colour = guide_legend(nrow = legend_rows, override.aes = list(linewidth = 1.2))) +
    labs(x = NULL, y = "Óbitos por 100 mil")
}

## 8a --------------------------------------------------------------------
df8a <- series_base |>
  filter(local %in% names(cores_regioes)) |>
  mutate(local = factor(local, levels = names(cores_regioes)))

g8a <- plot_series_paineis(df8a, cores_regioes, legend_rows = 1) +
  labs(title = "Mortalidade pelas principais causas, grandes regiões do Brasil",
       subtitle = "Óbitos por 100 mil crianças menores de 5 anos, 1990-2023")
salvar(g8a, "fig8a_regioes_brasil", largura = 10, altura = 7)

## 8b --------------------------------------------------------------------
paleta8b <- c(cores_paises[c("Brasil", "Argentina", "Noruega")], cores_regioes)
df8b <- series_base |>
  filter(local %in% names(paleta8b)) |>
  mutate(local = factor(local, levels = names(paleta8b)))

g8b <- plot_series_paineis(df8b, paleta8b, destaque = "Brasil", legend_rows = 2) +
  labs(title = "Mortalidade pelas principais causas: Argentina, Noruega e regiões do Brasil",
       subtitle = "Óbitos por 100 mil crianças menores de 5 anos, 1990-2023; Brasil (total) em destaque")
salvar(g8b, "fig8b_argentina_noruega_regioes", largura = 10, altura = 7.5)

## 8c --------------------------------------------------------------------
paises_grandes_todos <- c("Brasil", "China", "Índia", "Estados Unidos",
                          "Argentina", "Chile", "Costa Rica", "Japão", "Finlândia", "Noruega")
paleta8c <- cores_full[paises_grandes_todos]
df8c <- series_base |>
  filter(local %in% paises_grandes_todos) |>
  mutate(local = factor(local, levels = paises_grandes_todos))

g8c <- plot_series_paineis(df8c, paleta8c, destaque = "Brasil", legend_rows = 2) +
  labs(title = "Mortalidade pelas principais causas: China, Índia, Estados Unidos e demais países",
       subtitle = "Óbitos por 100 mil crianças menores de 5 anos, 1990-2023; Brasil em destaque")
salvar(g8c, "fig8c_china_india_eua_outros", largura = 10, altura = 7.5)

# =============================================================================
# FIGURA 9 - Malformações congênitas e transtornos endócrinos/metabólicos
#   por faixa etária, no mesmo formato da Figura 8 (linhas por país,
#   painéis por grupo de causa), em % do total de óbitos de cada faixa
# =============================================================================
faixas_banda <- list(
  "menos_1_mes"    = c("0-6 dias", "7-27 dias"),
  "1_mes_a_1_ano"  = c("1-5 months", "6-11 months"),
  "1_ano_ou_mais"  = c("12-23 meses", "2-4 anos")
)
idade_para_banda <- setNames(
  rep(names(faixas_banda), lengths(faixas_banda)),
  unlist(faixas_banda)
)
ordem_bandas <- c("menos_1_mes", "1_mes_a_1_ano", "1_ano_ou_mais")
rotulos_banda <- c(
  "menos_1_mes"   = "< 1 mês",
  "1_mes_a_1_ano" = "1 mês a 1 ano",
  "1_ano_ou_mais" = "\u2265 1 ano"
)

causas_congenitas <- c(
  "Defeitos do tubo neural", "Anomalias congênitas do coração",
  "Fissura labial e fissura palatina", "Síndrome de Down", "Síndrome de Turner",
  "Síndrome de Klinefelter", "Outras anomalias cromossômicas",
  "Anomalias congênitas musculoesqueléticas e dos membros",
  "Anomalias urogenitais congênitas", "Anomalias digestivas congênitas",
  "Outras anomalias congênitas"
)
causas_endocrinas <- c(
  "Transtornos da tireóide", "Diabetes mellitus tipo I", "Diabetes mellitus tipo II",
  "Deficiência de iodo", "Deficiência de vitamina A", "Deficiência de G6PD",
  "Traço G6PD", "Doenças da célula falciforme", "Traço da célula falciforme",
  "Talassemias", "Traço talassêmico", "Outras hemoglobinopatias e anemias hemolíticas",
  "Anemia ferropriva", "Outros distúrbios endócrinos, metabólicos, sanguíneos e imunológicos"
)

detalhe5 <- detalhe |> filter(idade %in% names(idade_para_banda), !is.na(valor))

# ---- 9.1 Participação (%) de malformações congênitas e de transtornos
#      endócrinos/metabólicos, por sub-faixa etária, a partir das causas
#      detalhadas (nível mais granular do GBD) ---------------------------
destaque_subfaixa <- detalhe5 |>
  mutate(grupo = case_when(causa %in% causas_congenitas ~ "Malformações congênitas",
                           causa %in% causas_endocrinas ~ "Transtornos endócrinos/metabólicos",
                           TRUE ~ NA_character_)) |>
  filter(!is.na(grupo)) |>
  group_by(local, idade, grupo) |>
  summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")

destaque_banda <- destaque_subfaixa |>
  mutate(banda = idade_para_banda[idade]) |>
  group_by(local, banda, grupo) |>
  summarise(valor = mean(valor), .groups = "drop")

# ---- 9.2 Composição por categorias "clássicas" do relatório (as mesmas da
#      Fig. 6/Fig. 7), agora por faixa etária, a partir dos arquivos originais
#      por idade (brasil.csv, chile.csv, japao.csv) ----------------------
coarse_subfaixa <- bind_rows(ler("brasil.csv"), ler("chile.csv"), ler("japao.csv")) |>
  filter(idade %in% names(idade_para_banda), !is.na(valor))

coarse_banda <- coarse_subfaixa |>
  mutate(banda = idade_para_banda[idade]) |>
  group_by(local, banda, causa) |>
  summarise(valor = mean(valor), .groups = "drop")

top6_coarse <- coarse_banda |>
  group_by(causa) |> summarise(m = mean(valor), .groups = "drop") |>
  slice_max(m, n = 6, with_ties = FALSE) |> pull(causa)

composicao <- coarse_banda |>
  mutate(causa_g = if_else(causa %in% top6_coarse, causa, "Demais causas")) |>
  group_by(local, banda, causa_g) |>
  summarise(valor = sum(valor), .groups = "drop")

# ---- 9.3 Dentro da categoria "Outras doenças não transmissíveis" (X%),
#      destacar malformações congênitas (Y1%) e transtornos endócrinos/
#      metabólicos (Y2%); o restante vira "Outras doenças não
#      transmissíveis (resto)" = X - Y1 - Y2 ------------------------------
ndt <- composicao |> filter(causa_g == "Outras doenças não transmissíveis") |>
  rename(total_ndt = valor) |> select(local, banda, total_ndt)

destaque_largo <- destaque_banda |>
  pivot_wider(names_from = grupo, values_from = valor, values_fill = 0)

partes_ndt <- ndt |>
  left_join(destaque_largo, by = c("local", "banda")) |>
  mutate(across(c(`Malformações congênitas`, `Transtornos endócrinos/metabólicos`),
                ~ replace_na(., 0)),
         resto = pmax(total_ndt - `Malformações congênitas` -
                       `Transtornos endócrinos/metabólicos`, 0)) |>
  select(local, banda, `Malformações congênitas`, `Transtornos endócrinos/metabólicos`, resto) |>
  pivot_longer(-c(local, banda), names_to = "causa_g", values_to = "valor") |>
  mutate(causa_g = recode(causa_g, "resto" = "Outras doenças não transmissíveis"))

composicao_final <- composicao |>
  filter(causa_g != "Outras doenças não transmissíveis") |>
  bind_rows(partes_ndt)

ordem_causas9 <- c("Demais causas", "Transtornos neurológicos", "Ferimentos não intencionais",
                   "Neoplasias", "Infecções respiratórias e tuberculose",
                   "Afecções maternas e neonatais",
                   "Outras doenças não transmissíveis",
                   "Transtornos endócrinos/metabólicos", "Malformações congênitas")

composicao_final <- composicao_final |>
  mutate(causa_g = factor(causa_g, levels = ordem_causas9),
         banda   = factor(rotulos_banda[banda], levels = rotulos_banda[ordem_bandas]),
         local   = factor(local, levels = c("Brasil", "Chile", "Japão")))

paleta9 <- c(cores_causas,
            "Outras doenças não transmissíveis" = cor_ndt_resto,
            "Malformações congênitas"                   = cor_congenitas,
            "Transtornos endócrinos/metabólicos"         = cor_endocrino)

g9 <- ggplot(composicao_final, aes(banda, valor, fill = causa_g)) +
  geom_col(width = .7, colour = "white", linewidth = .3) +
  geom_text(aes(label = if_else(valor >= .05, pct_br(valor), "")),
            position = position_stack(vjust = .5), size = 2.7, colour = "white") +
  facet_wrap(~ local, ncol = 3) +
  scale_fill_manual(values = paleta9, breaks = ordem_causas9, name = NULL,
                    labels = function(x) str_wrap(x, 26)) +
  scale_y_continuous(labels = label_percent(decimal.mark = ",", accuracy = 1),
                     expand = c(0, 0)) +
  guides(fill = guide_legend(ncol = 3, reverse = TRUE)) +
  labs(title = "Causas de óbito por faixa etária",
       subtitle = "Participação no total de óbitos de menores de 5 anos, por país, 2023",
       x = NULL, y = "% do total de óbitos da faixa etária")
salvar(g9, "fig9_composicao_malformacoes_endocrino", largura = 10, altura = 6)



cor_global <- "grey35"
cores_fig10 <- c(cores_paises["Brasil"], cores_grandes["China"], cores_grandes["Estados Unidos"],
                 cores_paises["Chile"], cores_paises["Japão"], "Global" = cor_global)
 
total_paises <- ler("all_time_countries.csv") |>
  mutate(local = factor(local, levels = names(cores_fig10)))
 
rotulos_fim10 <- total_paises |> filter(ano == max(ano))
 
g10 <- ggplot(total_paises, aes(ano, valor, colour = local, group = local)) +
  geom_line(aes(linewidth = as.character(local == "Brasil"),
                linetype  = as.character(local == "Global"))) +
  geom_text_repel(data = rotulos_fim10, aes(label = local), size = 3.2, direction = "y",
                  hjust = 0, nudge_x = 1, segment.size = .2, show.legend = FALSE,
                  xlim = c(2023.8, NA)) +
  scale_colour_manual(values = cores_fig10, guide = "none") +
  scale_linewidth_manual(values = c("TRUE" = 1.5, "FALSE" = 1), guide = "none") +
  scale_linetype_manual(values = c("TRUE" = "22", "FALSE" = "solid"), guide = "none") +
  scale_x_continuous(breaks = seq(1990, 2020, 5), expand = expansion(mult = c(.02, .02))) +
  scale_y_continuous(labels = int_br, limits = c(0, NA), expand = expansion(mult = c(0, .06))) +
  coord_cartesian(clip = "off", xlim = c(1990, 2031)) +
  labs(title = "Mortalidade total de menores de 5 anos",
       subtitle = "Óbitos por 100 mil crianças, 1990-2023",
       x = NULL, y = "Óbitos por 100 mil")
salvar(g10, "fig10_mortalidade_total_paises", largura = 9, altura = 5.5)
 

message("\nGráficos salvos em: ", normalizePath(dir_saida))
