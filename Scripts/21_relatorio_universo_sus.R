# ============================================================================
# 21_relatorio_universo_sus.R — documento Word com as tabelas do universo SUS
# Lê Dash/universo_sus/tabelas_universo_sus.rds (saída da PARTE 6 do script 20)
# e o texto de achados em Dash/universo_sus/achados.txt (uma linha por achado).
# ============================================================================
raiz <- c(Sys.getenv("INSPER_RAIZ"), "G:/My Drive/INSPER/Trabalho", "G:/Meu Drive/INSPER/Trabalho")
raiz <- raiz[nzchar(raiz) & dir.exists(raiz)][1]
dir_out <- Sys.getenv("INSPER_SAIDA", file.path(raiz, "Dash/universo_sus"))
suppressPackageStartupMessages({ library(officer); library(flextable); library(data.table) })
T <- readRDS(file.path(dir_out, "tabelas_universo_sus.rds"))
achados <- if (file.exists(file.path(dir_out, "achados.txt"))) readLines(file.path(dir_out, "achados.txt"), encoding = "UTF-8") else character(0)

br <- function(x, d = 0) formatC(x, format = "f", digits = d, big.mark = ".", decimal.mark = ",")
ft <- function(x, header = NULL, widths = NULL) {
  x <- as.data.frame(x)
  for (j in seq_along(x)) if (is.numeric(x[[j]])) x[[j]] <- if (all(x[[j]] == round(x[[j]]), na.rm = TRUE)) br(x[[j]]) else br(x[[j]], 1)
  f <- flextable(x)
  if (!is.null(header)) f <- set_header_labels(f, values = setNames(as.list(header), names(x)))
  f <- theme_vanilla(f); f <- fontsize(f, size = 8.5, part = "all"); f <- font(f, fontname = "Calibri", part = "all")
  f <- bg(f, bg = "#1f4e79", part = "header"); f <- color(f, color = "white", part = "header"); f <- bold(f, part = "header")
  f <- align(f, j = 2:ncol(x), align = "center", part = "all"); f <- padding(f, padding = 2, part = "all")
  f <- autofit(f); f <- set_table_properties(f, layout = "autofit", width = 1)
  f
}
nota <- function(doc, txt) body_add_par(doc, txt, style = "Normal") |> body_add_par("", style = "Normal")
titulo <- function(doc, txt) body_add_par(doc, txt, style = "heading 1")
cap <- function(doc, txt) body_add_par(doc, txt, style = "Normal")

perfil <- function(t) {
  t <- copy(t); t[, `Pareados n (%)` := sprintf("%s (%s)", br(Pareados), br(pct_par, 1))]
  t[, `Sem AIH n (%)` := sprintf("%s (%s)", br(`Sem AIH`), br(pct_sem, 1))]
  t[, `% sem AIH na categoria` := br(pct_sem_cat, 1)]
  t[, .(Variavel = variavel, Categoria = cat, `Pareados n (%)`, `Sem AIH n (%)`, `% sem AIH na categoria`)]
}

doc <- read_docx()
doc <- body_add_fpar(doc, fpar(ftext("Óbitos do SIM sem AIH correspondente — universo SUS", fp_text(bold = TRUE, font.size = 16, font.family = "Calibri", color = "#1f4e79"))))
doc <- body_add_par(doc, sprintf("Crianças de 0 a 6 anos · Brasil, 2015–2024 · óbito em hospital que atende SUS · linkage SIM–SIH–SINASC (%s)",
                                 format(Sys.Date(), "%d/%m/%Y")), style = "Normal")
doc <- titulo(doc, "Principais achados")
for (a in achados) doc <- body_add_par(doc, a, style = "Normal")
doc <- body_add_par(doc, "", style = "Normal")

doc <- titulo(doc, "Universo")
doc <- body_add_flextable(doc, ft(T$t_univ2, c("Etapa", "Óbitos (n)", "%")))
doc <- nota(doc, "Universo = óbito em hospital (LOCOCOR 1) cujo CNES tem ao menos uma AIH de 0–6 anos no SIH 2015–2024. Saíram UPA/PS/UBS (LOCOCOR 2 e unidades de urgência pelo tipo no CNES, mesmo com AIH) e hospitais sem nenhuma AIH 0–6 no período (saúde suplementar/não conveniados). Unidade = óbito do SIM.")
doc <- body_add_flextable(doc, ft(T$t_univ, c("Componente do SIM", "Óbitos (n)", "%")))
doc <- body_add_par(doc, "", style = "Normal")

doc <- titulo(doc, "De onde vêm os óbitos sem AIH")
doc <- body_add_flextable(doc, ft(T$t3a, c("Componente", "Óbitos (n)", "%")))
doc <- body_add_par(doc, "", style = "Normal")
doc <- body_add_flextable(doc, ft(T$t3b, c("Explicação (hierárquica, SIH + SINASC)", "Óbitos (n)", "%")))
doc <- nota(doc, "Classificação hierárquica e mutuamente exclusiva: cada óbito entra na primeira linha em que se encaixa. Retroação = todas as AIHs da mesma criança (data de nascimento + sexo + município de residência) até a data do óbito. Local de nascimento pelo SINASC (CNES do nascimento = CNES do óbito).")

doc <- titulo(doc, "Perfil geral — pareados × sem AIH")
doc <- body_add_flextable(doc, ft(perfil(T$t4)))
m <- T$med; doc <- nota(doc, sprintf("Idade ao óbito em dias, mediana (IIQ): pareados %s (%s–%s); sem AIH %s (%s–%s).",
  br(m[grupo == "Pareados"]$mediana_idade_dias), br(m[grupo == "Pareados"]$p25), br(m[grupo == "Pareados"]$p75),
  br(m[grupo == "Sem AIH"]$mediana_idade_dias), br(m[grupo == "Sem AIH"]$p25), br(m[grupo == "Sem AIH"]$p75)))

doc <- titulo(doc, "Mãe e nascimento (menores de 1 ano, variáveis da DO)")
doc <- body_add_flextable(doc, ft(perfil(T$t5)))
doc <- body_add_par(doc, "", style = "Normal")

doc <- titulo(doc, "Regiões e UFs")
doc <- body_add_flextable(doc, ft(T$t6, c("Região", "Óbitos no universo", "Sem AIH (n)", "% sem AIH")))
f1 <- file.path(dir_out, "Fig01_pct_sem_AIH_UF_universo_SUS.png"); f2 <- file.path(dir_out, "Fig02_mapa_pct_sem_AIH_UF_universo_SUS.png")
if (file.exists(f2)) { doc <- body_add_img(doc, f2, width = 6, height = 5.6); doc <- cap(doc, "Figura 1. % dos óbitos do universo SUS sem AIH pareada, por UF de residência.") }
if (file.exists(f1)) { doc <- body_add_img(doc, f1, width = 5.6, height = 6); doc <- cap(doc, "Figura 2. Mesma medida, em ordem; linha tracejada = Brasil.") }

doc <- titulo(doc, "Causas")
doc <- body_add_flextable(doc, ft(perfil(T$t7)[, -1]))
doc <- body_add_par(doc, "", style = "Normal")
doc <- body_add_flextable(doc, ft(T$t7top[, .(causabas, N, pct_dos_sem, pct_sem_cod)], c("CID-10", "Sem AIH (n)", "% dos sem AIH", "% sem AIH no código")))
doc <- body_add_par(doc, "", style = "Normal")

doc <- titulo(doc, "Retroação no SIH — internações da criança até o óbito")
doc <- body_add_flextable(doc, ft(T$t8, c("Grupo", "Óbitos", "% com alguma AIH", "% AIH do nascimento", "% internação prévia",
                                        "% reinternação ≤30 d", "Episódios (mediana)", "% AIH aberta no hospital do óbito", "% chave repetida no SIM")))
doc <- body_add_par(doc, "", style = "Normal")
doc <- body_add_flextable(doc, ft(T$t8desf, c("Motivo de saída da AIH aberta no hospital do óbito (sem AIH de óbito)", "Óbitos (n)", "%")))
doc <- body_add_par(doc, "", style = "Normal")

if (!is.null(T$t9)) {
  doc <- titulo(doc, "Linkage SIM × SINASC")
  doc <- body_add_flextable(doc, ft(T$t9, c("Recorte", "Óbitos", "Pareados ao SINASC", "%")))
  doc <- body_add_par(doc, "", style = "Normal")
  doc <- body_add_flextable(doc, ft(T$t9f, c("Faixa etária", "Óbitos", "Pareados", "%")))
  doc <- body_add_par(doc, "", style = "Normal")
  doc <- body_add_flextable(doc, ft(T$t9c, c("Classe", "Passo", "Pares", "%")))
  doc <- body_add_par(doc, "", style = "Normal")
  doc <- body_add_flextable(doc, ft(T$t9v, c("Óbitos dia 0 pareados", "% nasceu no hospital do óbito", "% peso concorda (±20 g)", "% idade da mãe igual")))
  doc <- nota(doc, "Blocagem: data de nascimento + sexo + UF de residência (passo A) e, para quem não pareou, data + sexo + CNES do óbito = CNES do nascimento (passo B). Escore com município, estabelecimento, peso, semanas de gestação, idade e escolaridade da mãe, tipo de parto e de gravidez, raça/cor; aceite ≥ 7 (alta ≥ 10 com margem ≥ 2). Passo C: sem variável da mãe na DO, só chave única nos dois lados.")
}
if (!is.null(T$t10)) {
  doc <- titulo(doc, "Nascidos vivos com internação no SIH")
  doc <- body_add_flextable(doc, ft(T$t10, c("Ano de nascimento", "Nascidos vivos", "Com AIH vinculada", "% com AIH", "% chave única")))
  doc <- nota(doc, "Vínculo SINASC × SIH por data de nascimento + sexo + município de residência; aceito quando a chave é única no SINASC ou, havendo empate, quando só um nascido vivo nasceu no CNES da AIH iniciada nos 2 primeiros dias de vida. O SIH cobre 2015–2024: nascidos antes de 2015 só têm internações a partir de 2015.")
}
doc <- titulo(doc, "Bases no GitHub")
for (b in c("obitos_sim_0_6_classificados_linkados.csv.gz — todos os óbitos 0–6 do SIM com o universo, pareamento SIH, retroação e SINASC",
            "obitos_universo_sus_linkados.csv.gz — só o universo SUS",
            "obitos_sinasc_linkados.csv.gz — óbitos pareados ao SINASC, com as variáveis do nascimento",
            "nascidos_vivos_internacoes/nv_internacoes_AAAA.csv.gz — nascidos vivos com ao menos uma AIH vinculada (um por linha)",
            "resumo_nascidos_vivos_aih.csv — todos os nascidos vivos por ano e UF, com e sem AIH"))
  doc <- body_add_par(doc, b, style = "Normal")
print(doc, target = file.path(dir_out, "Perfil_obitos_SIM_sem_AIH_universo_SUS.docx"))
cat("ok\n")
