# ============================================================================
# OBSERVATÓRIO DE SAÚDE INFANTIL (0–6 ANOS) — BRASIL | INSPER
# SIM (mortalidade) + SIH-SUS (internações) · denominadores SINASC + população
# Foco clínico: Hospital Pequeno Príncipe.
#
# VERSÃO 4 — revisão da reunião HPP de 27/07
#   [1] Classificação CID-10 por SISTEMA DO ORGANISMO (motor determinístico)
#   [2] Agrupamento de patologias semelhantes (cardiopatias, sepse, etc.)
#   [3] Revisão de nomenclatura e reclassificação de CIDs (inclui W84)
#   [4] Marcação de códigos GENÉRICOS / mal definidos / administrativos
#   [5] Trajetória hospitalar T0 → T1 → T2 e linkage SIH↔SIM
#   [6] Matriz de TRANSIÇÃO DE CID (entrada SIH → óbito SIM), com os quatro
#       casos do quadro: genérico→específico, específico→genérico,
#       genérico→genérico (causa nunca esclarecida) e troca de sistema
#   [7] Δ entrada/óbito por região (gráfico do quadro branco)
#   [8] Denominadores corretos: NV até 1 ano; crianças da faixa de 1 a 6
#   [9] Filtros temporais auditados em TODOS os painéis (com selo explícito
#       nos painéis cuja tabela de origem não tem dimensão anual)
#  [10] Todos os gráficos empilhados em proporção (100%), sem absolutos
#
# Arquivos esperados no diretório do app:
#   app.R
#   Tabelas_Executivas_Mortalidade_v2.xlsx
#   Tabelas_Executivas_Internacoes_0_6_Anos.xlsx
#   uf_sf_simplified.rds
# Opcionais (desbloqueiam painéis; ver aba Metodologia):
#   populacao_uf_faixa.csv      — saída de 01_baixar_populacao.R (IBGE)
#   sih_sim_linkado.rds         — saída de 00_link_sih_sim_v3.R (fluxo 5 etapas)
#   linkage_qualidade.rds       — métricas de qualidade do linkage (v3)
#   sih_nao_obito_agregado.rds  — Etapa 5 agregada (v3)
# ============================================================================

# Pré-checagem de dependências: falha com mensagem clara em vez de erro
# opaco no meio do carregamento (protege também o deploy no shinyapps.io).
.pkgs_req <- c("shiny", "bslib", "bsicons", "readxl", "dplyr", "tidyr",
               "plotly", "DT", "sf", "ggplot2", "scales", "htmltools")
.falta <- .pkgs_req[!vapply(.pkgs_req, requireNamespace, logical(1), quietly = TRUE)]
if (length(.falta))
  stop("Pacotes ausentes: ", paste(.falta, collapse = ", "), "\n",
       "Instale com:  install.packages(c(\"",
       paste(.falta, collapse = "\", \""), "\"))", call. = FALSE)
rm(.pkgs_req, .falta)

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(bsicons)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(plotly)
  library(DT)
  library(sf)
  library(ggplot2)
  library(scales)
  library(htmltools)
})

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ----------------------------------------------------------------------------
# 0. PALETA & TEMA
# ----------------------------------------------------------------------------
pal <- list(primary = "#004B87", secondary = "#00A3A1", accent = "#F4A261",
            alert = "#D9534F", dark = "#1A1A2E", gray_1 = "#495057",
            gray_2 = "#6c757d", gray_3 = "#E9ECEF", white = "#FFFFFF",
            purple = "#8E44AD", green = "#1E7B4F",
            map_mort_lo = "#FFF3D6", map_mort_hi = "#7F0000",
            map_int_lo  = "#EAF3FB", map_int_hi  = "#08306B")

# ----------------------------------------------------------------------------
# 1. DADOS TABULARES (carregamento tolerante a falhas)
# ----------------------------------------------------------------------------
load_health_data <- function(path) {
  if (!file.exists(path)) {
    warning(sprintf("Arquivo não encontrado: %s", path))
    return(NULL)
  }
  sheets <- excel_sheets(path)
  get_sheet <- function(i) if (length(sheets) >= i)
    tryCatch(read_excel(path, sheet = sheets[i]), error = function(e) NULL) else NULL
  out <- list(
    idade      = get_sheet(1),
    causas     = get_sheet(2),
    prio       = get_sheet(3),
    fluxo_uf   = get_sheet(4),
    polos      = get_sheet(5),
    inter_uf   = get_sheet(6),
    taxa_uf    = get_sheet(7),
    taxa_macro = get_sheet(8)
  )
  rn <- function(df) {
    if (is.null(df)) return(df)
    if ("ANO_OBITO"  %in% names(df)) df <- dplyr::rename(df, ANO = ANO_OBITO)
    if ("ANO_EVENTO" %in% names(df)) df <- dplyr::rename(df, ANO = ANO_EVENTO)
    df
  }
  out$idade  <- rn(out$idade)
  out$causas <- rn(out$causas)
  out
}

data_mort <- load_health_data("Tabelas_Executivas_Mortalidade_v2.xlsx")
data_int  <- load_health_data("Tabelas_Executivas_Internacoes_0_6_Anos.xlsx")

empty_pack <- list(idade = NULL, causas = NULL, prio = NULL, fluxo_uf = NULL,
                   polos = NULL, inter_uf = NULL, taxa_uf = NULL, taxa_macro = NULL)
if (is.null(data_mort)) data_mort <- empty_pack
if (is.null(data_int))  data_int  <- empty_pack

col_polos_mort <- "obitos_recebidos";       col_polos_int <- "internacoes_recebidas"
col_iuf_mort   <- "obitos";                 col_iuf_int   <- "internacoes"
col_tot_mort   <- "obitos_total";           col_tot_int   <- "internacoes_total"

if (!is.null(data_mort$idade) && "ANO" %in% names(data_mort$idade)) {
  ano_min <- suppressWarnings(min(data_mort$idade$ANO, na.rm = TRUE))
  ano_max <- suppressWarnings(max(data_mort$idade$ANO, na.rm = TRUE))
} else { ano_min <- 2015; ano_max <- 2024 }
if (!is.finite(ano_min)) ano_min <- 2015
if (!is.finite(ano_max)) ano_max <- 2024

# Período completo coberto pelas tabelas agregadas sem dimensão anual.
PERIODO_FIXO <- paste0(ano_min, "–", ano_max)

# ----------------------------------------------------------------------------
# 1b. DEFINIÇÃO DAS FAIXAS ETÁRIAS (motor dos "3 dashboards em 1")
# ----------------------------------------------------------------------------
faixas <- list(
  neo = list(
    id = "neo", n = 1,
    titulo = "Neonatal (0–27 dias)",
    curto  = "Neonatal",
    regex  = "precoce|tardia",
    denom  = "nv",
    lead   = paste0("Mortes nos primeiros 27 dias de vida. Dominada por prematuridade, ",
                    "baixo peso, asfixia no parto e infecções perinatais. É a faixa mais ",
                    "ligada à qualidade do pré-natal e da assistência ao parto — e a de ",
                    "menor participação do Hospital Pequeno Príncipe.")
  ),
  pos = list(
    id = "pos", n = 2,
    titulo = "Pós-neonatal (28 dias a <1 ano)",
    curto  = "28 d – 1 ano",
    regex  = "p[oó]s-?neonatal|28 ?dias",
    denom  = "nv",
    lead   = paste0("Do 28º dia até completar 1 ano. Peso crescente de infecções ",
                    "respiratórias e diarreicas, malformações e um núcleo de causas de ",
                    "difícil prevenção (morte súbita, doenças raras). Público majoritário ",
                    "do Hospital Pequeno Príncipe.")
  ),
  inf = list(
    id = "inf", n = 3,
    titulo = "1 a 6 anos",
    curto  = "1 a 6 anos",
    regex  = "1 ?a ?6",
    denom  = "pop",
    lead   = paste0("Perfil COMPLETAMENTE diferente das faixas anteriores: causas externas ",
                    "(acidentes) e doenças de alta complexidade — câncer infantil, doenças ",
                    "raras e cardiopatias — ganham protagonismo. Núcleo estratégico do ",
                    "Hospital Pequeno Príncipe.")
  )
)

faixa_cols <- function(idade_df, regex) {
  if (is.null(idade_df)) return(character(0))
  cols <- setdiff(names(idade_df), "ANO")
  cols[grepl(regex, cols, ignore.case = TRUE, perl = TRUE)]
}

faixa_serie <- function(idade_df, regex, y0, y1) {
  cols <- faixa_cols(idade_df, regex)
  if (is.null(idade_df) || length(cols) == 0 || !"ANO" %in% names(idade_df))
    return(data.frame(ANO = integer(0), Valor = numeric(0)))
  idade_df %>%
    filter(ANO >= y0, ANO <= y1) %>%
    transmute(ANO, Valor = rowSums(across(all_of(cols)), na.rm = TRUE)) %>%
    arrange(ANO)
}
causas_dificeis <- list(
  pos = list(
    list(nome = "Morte súbita do lactente (SMSL)",
         oque = "Bebê aparentemente saudável que morre durante o sono, sem causa aparente. Assusta porque não há sintoma prévio.",
         fazer = "Colocar o bebê para dormir SEMPRE de barriga para cima, em colchão firme, sem travesseiros, cobertores soltos ou bichos de pelúcia no berço. Amamentar, manter o ambiente sem fumaça e não superaquecer o quarto."),
    list(nome = "Sufocação e engasgo acidental",
         oque = "O bebê engasga com leite, regurgitação ou pequenos objetos, ou fica preso em roupa de cama/almofada.",
         fazer = "Berço livre de objetos moles, posição correta na amamentação e supervisão. Cuidadores treinados na manobra de desengasgo (para lactentes) fazem diferença nos primeiros minutos."),
    list(nome = "Infecções graves de evolução rápida (sepse, meningite)",
         oque = "Uma infecção que em poucas horas se espalha e leva à falência do organismo, mesmo em bebê antes saudável.",
         fazer = "Reconhecer sinais de alerta e buscar emergência imediatamente: febre alta ou temperatura baixa, moleza extrema, recusa alimentar, respiração rápida, manchas na pele, moleira tensa. Vacinação em dia reduz muito o risco."),
    list(nome = "Doenças raras e erros inatos do metabolismo",
         oque = "Condições genéticas em que o corpo não processa certas substâncias; podem se manifestar de forma súbita e grave no primeiro ano.",
         fazer = "Ampliar e garantir o Teste do Pezinho (quanto mais ampliado, mais doenças detectadas cedo). Encaminhamento rápido a centros de referência como o Hospital Pequeno Príncipe permite tratamento antes da crise."),
    list(nome = "Cardiopatias congênitas descompensadas",
         oque = "Problemas no coração presentes desde o nascimento que se agravam nos primeiros meses.",
         fazer = "Teste do Coraçãozinho na maternidade, acompanhamento cardiológico e cirurgia oportuna em centros de alta complexidade.")
  ),
  inf = list(
    list(nome = "Câncer infantil (leucemias e tumores)",
         oque = "Principal doença 'difícil de prevenir' nesta idade. Não se previne, mas se cura em altas taxas quando detectado cedo.",
         fazer = "Atenção a sinais persistentes: palidez e hematomas fáceis, febre sem causa, dor óssea, aumento de gânglios, mancha branca no olho em fotos, dor de cabeça matinal com vômito. Encaminhamento rápido à oncologia pediátrica (referência do Hospital Pequeno Príncipe) é decisivo."),
    list(nome = "Doenças raras e genéticas",
         oque = "Centenas de condições individualmente raras que, somadas, pesam nesta faixa e exigem diagnóstico especializado.",
         fazer = "Reduzir o tempo até o diagnóstico com centros de referência e triagem genética. Cuidado multidisciplinar contínuo evita descompensações fatais."),
    list(nome = "Complicações de doenças crônicas graves",
         oque = "Crianças com condições crônicas (neurológicas, respiratórias, metabólicas) que sofrem agravamentos agudos.",
         fazer = "Plano de cuidado individualizado, vacinação reforçada, acesso rápido à emergência e linhas de cuidado que evitem internações e óbitos evitáveis."),
    list(nome = "Causas externas de baixa previsibilidade",
         oque = "Eventos súbitos — afogamento, aspiração de corpo estranho, reações graves — que fogem à supervisão comum.",
         fazer = "Ambientes seguros (cercar piscinas, guardar medicamentos e produtos), supervisão da água, e primeiros socorros pediátricos entre cuidadores."),
    list(nome = "Infecções graves e sepse",
         oque = "Infecções que evoluem rapidamente para quadros críticos mesmo com atendimento.",
         fazer = "Vacinação completa, reconhecimento precoce de sinais de gravidade e acesso ágil a UTI pediátrica.")
  )
)

card_causa <- function(item) {
  div(class = "cause-card",
      div(class = "cause-title", bs_icon("shield-plus"), " ", item$nome),
      div(class = "cause-block",
          tags$span(class = "cause-label", "O que é"), tags$p(item$oque)),
      div(class = "cause-block",
          tags$span(class = "cause-label prev", "O que fazer para evitar"),
          tags$p(item$fazer)))
}
sim_prio_cid <- read.csv(text = '
faixa,prio,cid,nome,n,pct
Neonatal (0–27 dias),Ação Prioritária (Prevenção/Tratamento),A50,Sífilis congênita,1749,59.1
Neonatal (0–27 dias),Ação Prioritária (Prevenção/Tratamento),J18,Pneumonia não especificada,414,14.0
Neonatal (0–27 dias),Ação Prioritária (Prevenção/Tratamento),A09,Diarreia e gastroenterite infecciosas,163,5.5
Neonatal (0–27 dias),Ação Prioritária (Prevenção/Tratamento),B34,Infecção viral não especificada,99,3.3
Neonatal (0–27 dias),Ação Prioritária (Prevenção/Tratamento),J21,Bronquiolite aguda,89,3.0
Neonatal (0–27 dias),Ação Prioritária (Prevenção/Tratamento),J15,Pneumonia bacteriana,67,2.3
Neonatal (0–27 dias),Ação Prioritária (Prevenção/Tratamento),B99,Outras doenças infecciosas,62,2.1
Neonatal (0–27 dias),Ação Prioritária (Prevenção/Tratamento),J81,Edema pulmonar,31,1.0
Neonatal (0–27 dias),Ação Prioritária (Prevenção/Tratamento),A48,Outras doenças bacterianas,29,1.0
Neonatal (0–27 dias),Ação Prioritária (Prevenção/Tratamento),A04,Outras infecções intestinais bacterianas,18,0.6
Neonatal (0–27 dias),Ação Prioritária (Prevenção/Tratamento),A39,Doença meningocócica,13,0.4
Neonatal (0–27 dias),Ação Prioritária (Prevenção/Tratamento),J12,Pneumonia viral,13,0.4
Neonatal (0–27 dias),Atenção à Gestação e Parto,P36,Sepse neonatal,23959,13.4
Neonatal (0–27 dias),Atenção à Gestação e Parto,P00,RN afetado por afecções maternas,20625,11.6
Neonatal (0–27 dias),Atenção à Gestação e Parto,P22,Desconforto respiratório do RN,20512,11.5
Neonatal (0–27 dias),Atenção à Gestação e Parto,P01,RN afetado por complicações da gravidez,14796,8.3
Neonatal (0–27 dias),Atenção à Gestação e Parto,P07,Prematuridade e baixo peso ao nascer,14264,8.0
Neonatal (0–27 dias),Atenção à Gestação e Parto,P02,RN afetado por complicações da placenta/cordão,13384,7.5
Neonatal (0–27 dias),Atenção à Gestação e Parto,P21,Asfixia ao nascer,8324,4.7
Neonatal (0–27 dias),Atenção à Gestação e Parto,P96,Outras afecções perinatais,7252,4.1
Neonatal (0–27 dias),Atenção à Gestação e Parto,P24,Aspiração neonatal,6991,3.9
Neonatal (0–27 dias),Atenção à Gestação e Parto,P28,Outras afecções respiratórias do RN,6367,3.6
Neonatal (0–27 dias),Atenção à Gestação e Parto,P29,Transtornos cardiovasculares perinatais,5162,2.9
Neonatal (0–27 dias),Atenção à Gestação e Parto,P20,Hipóxia intrauterina,4679,2.6
Neonatal (0–27 dias),Malformações (Alta Complexidade),Q24,Outras malformações do coração,8458,17.0
Neonatal (0–27 dias),Malformações (Alta Complexidade),Q89,Outras malformações congênitas,6612,13.3
Neonatal (0–27 dias),Malformações (Alta Complexidade),Q79,Malformação do sistema osteomuscular,4436,8.9
Neonatal (0–27 dias),Malformações (Alta Complexidade),Q33,Malformação do pulmão,4305,8.7
Neonatal (0–27 dias),Malformações (Alta Complexidade),Q00,Anencefalia,4110,8.3
Neonatal (0–27 dias),Malformações (Alta Complexidade),Q91,Síndrome de Edwards / Patau,2296,4.6
Neonatal (0–27 dias),Malformações (Alta Complexidade),Q25,Malformação das grandes artérias,2100,4.2
Neonatal (0–27 dias),Malformações (Alta Complexidade),Q23,Malformação das valvas aórtica/mitral,1613,3.2
Neonatal (0–27 dias),Malformações (Alta Complexidade),Q20,Malformação das câmaras cardíacas,1557,3.1
Neonatal (0–27 dias),Malformações (Alta Complexidade),Q04,Malformação congênita do cérebro,1356,2.7
Neonatal (0–27 dias),Malformações (Alta Complexidade),Q60,Agenesia renal,1354,2.7
Neonatal (0–27 dias),Malformações (Alta Complexidade),Q21,Malformação dos septos cardíacos,1269,2.6
Neonatal (0–27 dias),Outras Causas / Difícil Prevenção,R95,Morte súbita na infância (SMSL),556,12.7
Neonatal (0–27 dias),Outras Causas / Difícil Prevenção,R99,Causas mal definidas e desconhecidas,429,9.8
Neonatal (0–27 dias),Outras Causas / Difícil Prevenção,R98,Morte sem assistência,344,7.8
Neonatal (0–27 dias),Outras Causas / Difícil Prevenção,W78,Inalação de conteúdo gástrico,318,7.2
Neonatal (0–27 dias),Outras Causas / Difícil Prevenção,Y09,Agressão por meios não especificados,262,6.0
Neonatal (0–27 dias),Outras Causas / Difícil Prevenção,W84,Obstrução respiratória não especificada,238,5.4
Neonatal (0–27 dias),Outras Causas / Difícil Prevenção,W79,Engasgo por alimento (obstrução das vias aéreas),200,4.6
Neonatal (0–27 dias),Outras Causas / Difícil Prevenção,Y34,Evento não especificado (intenção indeterminada),168,3.8
Neonatal (0–27 dias),Outras Causas / Difícil Prevenção,E87,Distúrbio hidroeletrolítico,133,3.0
Neonatal (0–27 dias),Outras Causas / Difícil Prevenção,I42,Cardiomiopatia,105,2.4
Neonatal (0–27 dias),Outras Causas / Difícil Prevenção,W75,Sufocação acidental na cama,83,1.9
Neonatal (0–27 dias),Outras Causas / Difícil Prevenção,G00,Meningite bacteriana,79,1.8
Pós-neonatal (28 d–<1 ano),Ação Prioritária (Prevenção/Tratamento),J18,Pneumonia não especificada,6069,23.0
Pós-neonatal (28 d–<1 ano),Ação Prioritária (Prevenção/Tratamento),A41,Septicemia (sepse),5174,19.6
Pós-neonatal (28 d–<1 ano),Ação Prioritária (Prevenção/Tratamento),A09,Diarreia e gastroenterite infecciosas,2970,11.2
Pós-neonatal (28 d–<1 ano),Ação Prioritária (Prevenção/Tratamento),J21,Bronquiolite aguda,2610,9.9
Pós-neonatal (28 d–<1 ano),Ação Prioritária (Prevenção/Tratamento),J15,Pneumonia bacteriana,1403,5.3
Pós-neonatal (28 d–<1 ano),Ação Prioritária (Prevenção/Tratamento),B34,Infecção viral não especificada,1099,4.2
Pós-neonatal (28 d–<1 ano),Ação Prioritária (Prevenção/Tratamento),J69,Pneumonite por aspiração,990,3.7
Pós-neonatal (28 d–<1 ano),Ação Prioritária (Prevenção/Tratamento),J96,Insuficiência respiratória,545,2.1
Pós-neonatal (28 d–<1 ano),Ação Prioritária (Prevenção/Tratamento),J98,Outros transtornos respiratórios,491,1.9
Pós-neonatal (28 d–<1 ano),Ação Prioritária (Prevenção/Tratamento),A04,Outras infecções intestinais bacterianas,425,1.6
Pós-neonatal (28 d–<1 ano),Ação Prioritária (Prevenção/Tratamento),J12,Pneumonia viral,319,1.2
Pós-neonatal (28 d–<1 ano),Ação Prioritária (Prevenção/Tratamento),A50,Sífilis congênita,315,1.2
Pós-neonatal (28 d–<1 ano),Atenção à Gestação e Parto,P36,Sepse neonatal,3137,20.2
Pós-neonatal (28 d–<1 ano),Atenção à Gestação e Parto,P77,Enterocolite necrosante,2225,14.3
Pós-neonatal (28 d–<1 ano),Atenção à Gestação e Parto,P00,RN afetado por afecções maternas,1600,10.3
Pós-neonatal (28 d–<1 ano),Atenção à Gestação e Parto,P27,Doença respiratória crônica perinatal,1276,8.2
Pós-neonatal (28 d–<1 ano),Atenção à Gestação e Parto,P24,Aspiração neonatal,828,5.3
Pós-neonatal (28 d–<1 ano),Atenção à Gestação e Parto,P01,RN afetado por complicações da gravidez,673,4.3
Pós-neonatal (28 d–<1 ano),Atenção à Gestação e Parto,P22,Desconforto respiratório do RN,635,4.1
Pós-neonatal (28 d–<1 ano),Atenção à Gestação e Parto,P21,Asfixia ao nascer,617,4.0
Pós-neonatal (28 d–<1 ano),Atenção à Gestação e Parto,P29,Transtornos cardiovasculares perinatais,526,3.4
Pós-neonatal (28 d–<1 ano),Atenção à Gestação e Parto,P02,RN afetado por complicações da placenta/cordão,514,3.3
Pós-neonatal (28 d–<1 ano),Atenção à Gestação e Parto,P28,Outras afecções respiratórias do RN,434,2.8
Pós-neonatal (28 d–<1 ano),Atenção à Gestação e Parto,P23,Pneumonia congênita,408,2.6
Pós-neonatal (28 d–<1 ano),Malformações (Alta Complexidade),Q24,Outras malformações do coração,6377,22.2
Pós-neonatal (28 d–<1 ano),Malformações (Alta Complexidade),Q21,Malformação dos septos cardíacos,3030,10.5
Pós-neonatal (28 d–<1 ano),Malformações (Alta Complexidade),Q91,Síndrome de Edwards / Patau,2299,8.0
Pós-neonatal (28 d–<1 ano),Malformações (Alta Complexidade),Q25,Malformação das grandes artérias,1340,4.7
Pós-neonatal (28 d–<1 ano),Malformações (Alta Complexidade),Q79,Malformação do sistema osteomuscular,1258,4.4
Pós-neonatal (28 d–<1 ano),Malformações (Alta Complexidade),Q04,Malformação congênita do cérebro,1257,4.4
Pós-neonatal (28 d–<1 ano),Malformações (Alta Complexidade),Q20,Malformação das câmaras cardíacas,1210,4.2
Pós-neonatal (28 d–<1 ano),Malformações (Alta Complexidade),Q90,Síndrome de Down,1140,4.0
Pós-neonatal (28 d–<1 ano),Malformações (Alta Complexidade),Q23,Malformação das valvas aórtica/mitral,1130,3.9
Pós-neonatal (28 d–<1 ano),Malformações (Alta Complexidade),Q03,Hidrocefalia congênita,1048,3.6
Pós-neonatal (28 d–<1 ano),Malformações (Alta Complexidade),Q89,Outras malformações congênitas,845,2.9
Pós-neonatal (28 d–<1 ano),Malformações (Alta Complexidade),Q22,Malformação das valvas pulmonar/tricúspide,826,2.9
Pós-neonatal (28 d–<1 ano),Outras Causas / Difícil Prevenção,R99,Causas mal definidas e desconhecidas,4395,13.4
Pós-neonatal (28 d–<1 ano),Outras Causas / Difícil Prevenção,W78,Inalação de conteúdo gástrico,2000,6.1
Pós-neonatal (28 d–<1 ano),Outras Causas / Difícil Prevenção,W84,Obstrução respiratória não especificada,1602,4.9
Pós-neonatal (28 d–<1 ano),Outras Causas / Difícil Prevenção,W79,Engasgo por alimento (obstrução das vias aéreas),1522,4.6
Pós-neonatal (28 d–<1 ano),Outras Causas / Difícil Prevenção,R95,Morte súbita na infância (SMSL),925,2.8
Pós-neonatal (28 d–<1 ano),Outras Causas / Difícil Prevenção,G93,Outros transtornos do encéfalo,857,2.6
Pós-neonatal (28 d–<1 ano),Outras Causas / Difícil Prevenção,I42,Cardiomiopatia,755,2.3
Pós-neonatal (28 d–<1 ano),Outras Causas / Difícil Prevenção,G91,Hidrocefalia,737,2.2
Pós-neonatal (28 d–<1 ano),Outras Causas / Difícil Prevenção,K56,Íleo/obstrução intestinal,650,2.0
Pós-neonatal (28 d–<1 ano),Outras Causas / Difícil Prevenção,E43,Desnutrição grave,610,1.9
Pós-neonatal (28 d–<1 ano),Outras Causas / Difícil Prevenção,E46,Desnutrição não especificada,608,1.9
Pós-neonatal (28 d–<1 ano),Outras Causas / Difícil Prevenção,Y34,Evento não especificado (intenção indeterminada),588,1.8
1 a 6 anos,Ação Prioritária (Prevenção/Tratamento),J18,Pneumonia não especificada,5284,30.8
1 a 6 anos,Ação Prioritária (Prevenção/Tratamento),A41,Septicemia (sepse),1792,10.4
1 a 6 anos,Ação Prioritária (Prevenção/Tratamento),A09,Diarreia e gastroenterite infecciosas,1536,8.9
1 a 6 anos,Ação Prioritária (Prevenção/Tratamento),J15,Pneumonia bacteriana,1253,7.3
1 a 6 anos,Ação Prioritária (Prevenção/Tratamento),B34,Infecção viral não especificada,897,5.2
1 a 6 anos,Ação Prioritária (Prevenção/Tratamento),J45,Asma,402,2.3
1 a 6 anos,Ação Prioritária (Prevenção/Tratamento),J96,Insuficiência respiratória,383,2.2
1 a 6 anos,Ação Prioritária (Prevenção/Tratamento),J69,Pneumonite por aspiração,365,2.1
1 a 6 anos,Ação Prioritária (Prevenção/Tratamento),J98,Outros transtornos respiratórios,328,1.9
1 a 6 anos,Ação Prioritária (Prevenção/Tratamento),A39,Doença meningocócica,306,1.8
1 a 6 anos,Ação Prioritária (Prevenção/Tratamento),J21,Bronquiolite aguda,298,1.7
1 a 6 anos,Ação Prioritária (Prevenção/Tratamento),B55,Leishmaniose,250,1.5
1 a 6 anos,Atenção à Gestação e Parto,P21,Asfixia ao nascer,98,29.1
1 a 6 anos,Atenção à Gestação e Parto,P27,Doença respiratória crônica perinatal,81,24.0
1 a 6 anos,Atenção à Gestação e Parto,P20,Hipóxia intrauterina,20,5.9
1 a 6 anos,Atenção à Gestação e Parto,P36,Sepse neonatal,18,5.3
1 a 6 anos,Atenção à Gestação e Parto,P37,Infecções congênitas,18,5.3
1 a 6 anos,Atenção à Gestação e Parto,P22,Desconforto respiratório do RN,15,4.5
1 a 6 anos,Atenção à Gestação e Parto,P77,Enterocolite necrosante,13,3.9
1 a 6 anos,Atenção à Gestação e Parto,P35,Doenças virais congênitas,12,3.6
1 a 6 anos,Atenção à Gestação e Parto,P57,Kernicterus,9,2.7
1 a 6 anos,Atenção à Gestação e Parto,P94,Transtornos do tônus muscular do RN,8,2.4
1 a 6 anos,Atenção à Gestação e Parto,P23,Pneumonia congênita,7,2.1
1 a 6 anos,Atenção à Gestação e Parto,P28,Outras afecções respiratórias do RN,6,1.8
1 a 6 anos,Malformações (Alta Complexidade),Q24,Outras malformações do coração,1546,18.5
1 a 6 anos,Malformações (Alta Complexidade),Q21,Malformação dos septos cardíacos,1087,13.0
1 a 6 anos,Malformações (Alta Complexidade),Q03,Hidrocefalia congênita,654,7.8
1 a 6 anos,Malformações (Alta Complexidade),Q04,Malformação congênita do cérebro,614,7.3
1 a 6 anos,Malformações (Alta Complexidade),Q02,Microcefalia,557,6.7
1 a 6 anos,Malformações (Alta Complexidade),Q90,Síndrome de Down,539,6.4
1 a 6 anos,Malformações (Alta Complexidade),Q87,Síndromes de malformações múltiplas,307,3.7
1 a 6 anos,Malformações (Alta Complexidade),Q20,Malformação das câmaras cardíacas,302,3.6
1 a 6 anos,Malformações (Alta Complexidade),Q91,Síndrome de Edwards / Patau,235,2.8
1 a 6 anos,Malformações (Alta Complexidade),Q22,Malformação das valvas pulmonar/tricúspide,229,2.7
1 a 6 anos,Malformações (Alta Complexidade),Q33,Malformação do pulmão,206,2.5
1 a 6 anos,Malformações (Alta Complexidade),Q25,Malformação das grandes artérias,183,2.2
1 a 6 anos,Outras Causas / Difícil Prevenção,R99,Causas mal definidas e desconhecidas,2757,6.2
1 a 6 anos,Outras Causas / Difícil Prevenção,W74,Afogamento não especificado,2014,4.5
1 a 6 anos,Outras Causas / Difícil Prevenção,C71,Câncer do encéfalo,1674,3.7
1 a 6 anos,Outras Causas / Difícil Prevenção,G80,Paralisia cerebral,1569,3.5
1 a 6 anos,Outras Causas / Difícil Prevenção,C91,Leucemia linfoide,1556,3.5
1 a 6 anos,Outras Causas / Difícil Prevenção,G93,Outros transtornos do encéfalo,1385,3.1
1 a 6 anos,Outras Causas / Difícil Prevenção,W69,Afogamento em águas naturais,1244,2.8
1 a 6 anos,Outras Causas / Difícil Prevenção,G40,Epilepsia,1070,2.4
1 a 6 anos,Outras Causas / Difícil Prevenção,G91,Hidrocefalia,998,2.2
1 a 6 anos,Outras Causas / Difícil Prevenção,W67,Afogamento em piscina,776,1.7
1 a 6 anos,Outras Causas / Difícil Prevenção,C74,Câncer da suprarrenal (neuroblastoma),727,1.6
1 a 6 anos,Outras Causas / Difícil Prevenção,C92,Leucemia mieloide,695,1.6
', stringsAsFactors = FALSE, check.names = FALSE, encoding = 'UTF-8')
sim_capitulo <- read.csv(text = '
faixa,cap,nome,n,pct
Total 0-6 anos,XVI,Afecções originadas no período perinatal,194111,47.4
Total 0-6 anos,XVII,Malformações congênitas e anomalias cromossômicas,86855,21.2
Total 0-6 anos,XX,Causas externas de morbidade e mortalidade,26519,6.5
Total 0-6 anos,X,Doenças do aparelho respiratório,25696,6.3
Total 0-6 anos,I,Algumas doenças infecciosas e parasitárias,20850,5.1
Total 0-6 anos,XVIII,Sintomas e achados anormais mal definidos,12245,3.0
Total 0-6 anos,VI,Doenças do sistema nervoso,11301,2.8
Total 0-6 anos,II,Neoplasias (tumores),8826,2.2
Total 0-6 anos,IX,Doenças do aparelho circulatório,5721,1.4
Total 0-6 anos,IV,"Doenças endócrinas, nutricionais e metabólicas",5555,1.4
Total 0-6 anos,XI,Doenças do aparelho digestivo,4814,1.2
Total 0-6 anos,III,Doenças do sangue e órgãos hematopoéticos,3425,0.8
Total 0-6 anos,XIV,Doenças do aparelho geniturinário,2340,0.6
Total 0-6 anos,XII,Doenças da pele e tecido subcutâneo,475,0.1
Total 0-6 anos,XIII,Doenças do sistema osteomuscular,337,0.1
Total 0-6 anos,VIII,Doenças do ouvido,197,0.0
Total 0-6 anos,V,Transtornos mentais e comportamentais,60,0.0
Total 0-6 anos,VII,Doenças do olho e anexos,25,0.0
Neonatal Precoce (0-6 dias),XVI,Afecções originadas no período perinatal,136926,77.5
Neonatal Precoce (0-6 dias),XVII,Malformações congênitas e anomalias cromossômicas,35831,20.3
Neonatal Precoce (0-6 dias),I,Algumas doenças infecciosas e parasitárias,1489,0.8
Neonatal Precoce (0-6 dias),XVIII,Sintomas e achados anormais mal definidos,941,0.5
Neonatal Precoce (0-6 dias),XX,Causas externas de morbidade e mortalidade,785,0.4
Neonatal Precoce (0-6 dias),IV,"Doenças endócrinas, nutricionais e metabólicas",165,0.1
Neonatal Precoce (0-6 dias),IX,Doenças do aparelho circulatório,130,0.1
Neonatal Precoce (0-6 dias),II,Neoplasias (tumores),119,0.1
Neonatal Precoce (0-6 dias),X,Doenças do aparelho respiratório,69,0.0
Neonatal Precoce (0-6 dias),III,Doenças do sangue e órgãos hematopoéticos,54,0.0
Neonatal Precoce (0-6 dias),VI,Doenças do sistema nervoso,43,0.0
Neonatal Precoce (0-6 dias),XI,Doenças do aparelho digestivo,31,0.0
Neonatal Precoce (0-6 dias),XIV,Doenças do aparelho geniturinário,21,0.0
Neonatal Precoce (0-6 dias),VII,Doenças do olho e anexos,14,0.0
Neonatal Precoce (0-6 dias),XIII,Doenças do sistema osteomuscular,6,0.0
Neonatal Precoce (0-6 dias),XII,Doenças da pele e tecido subcutâneo,6,0.0
Neonatal Precoce (0-6 dias),V,Transtornos mentais e comportamentais,3,0.0
Neonatal Tardia (7-27 dias),XVI,Afecções originadas no período perinatal,41294,70.4
Neonatal Tardia (7-27 dias),XVII,Malformações congênitas e anomalias cromossômicas,13923,23.7
Neonatal Tardia (7-27 dias),XX,Causas externas de morbidade e mortalidade,914,1.6
Neonatal Tardia (7-27 dias),I,Algumas doenças infecciosas e parasitárias,756,1.3
Neonatal Tardia (7-27 dias),X,Doenças do aparelho respiratório,646,1.1
Neonatal Tardia (7-27 dias),XVIII,Sintomas e achados anormais mal definidos,543,0.9
Neonatal Tardia (7-27 dias),IV,"Doenças endócrinas, nutricionais e metabólicas",199,0.3
Neonatal Tardia (7-27 dias),VI,Doenças do sistema nervoso,130,0.2
Neonatal Tardia (7-27 dias),IX,Doenças do aparelho circulatório,88,0.1
Neonatal Tardia (7-27 dias),II,Neoplasias (tumores),67,0.1
Neonatal Tardia (7-27 dias),XI,Doenças do aparelho digestivo,52,0.1
Neonatal Tardia (7-27 dias),III,Doenças do sangue e órgãos hematopoéticos,46,0.1
Neonatal Tardia (7-27 dias),XII,Doenças da pele e tecido subcutâneo,13,0.0
Neonatal Tardia (7-27 dias),XIII,Doenças do sistema osteomuscular,8,0.0
Neonatal Tardia (7-27 dias),XIV,Doenças do aparelho geniturinário,8,0.0
Neonatal Tardia (7-27 dias),VIII,Doenças do ouvido,1,0.0
Pós-Neonatal (28 dias a <1 ano),XVII,Malformações congênitas e anomalias cromossômicas,28726,27.8
Pós-Neonatal (28 dias a <1 ano),XVI,Afecções originadas no período perinatal,15554,15.0
Pós-Neonatal (28 dias a <1 ano),X,Doenças do aparelho respiratório,14360,13.9
Pós-Neonatal (28 dias a <1 ano),I,Algumas doenças infecciosas e parasitárias,12059,11.7
Pós-Neonatal (28 dias a <1 ano),XX,Causas externas de morbidade e mortalidade,8917,8.6
Pós-Neonatal (28 dias a <1 ano),XVIII,Sintomas e achados anormais mal definidos,6810,6.6
Pós-Neonatal (28 dias a <1 ano),VI,Doenças do sistema nervoso,3819,3.7
Pós-Neonatal (28 dias a <1 ano),IV,"Doenças endócrinas, nutricionais e metabólicas",2985,2.9
Pós-Neonatal (28 dias a <1 ano),XI,Doenças do aparelho digestivo,2970,2.9
Pós-Neonatal (28 dias a <1 ano),IX,Doenças do aparelho circulatório,2954,2.9
Pós-Neonatal (28 dias a <1 ano),III,Doenças do sangue e órgãos hematopoéticos,1556,1.5
Pós-Neonatal (28 dias a <1 ano),XIV,Doenças do aparelho geniturinário,1275,1.2
Pós-Neonatal (28 dias a <1 ano),II,Neoplasias (tumores),1044,1.0
Pós-Neonatal (28 dias a <1 ano),XII,Doenças da pele e tecido subcutâneo,244,0.2
Pós-Neonatal (28 dias a <1 ano),XIII,Doenças do sistema osteomuscular,94,0.1
Pós-Neonatal (28 dias a <1 ano),VIII,Doenças do ouvido,83,0.1
Pós-Neonatal (28 dias a <1 ano),V,Transtornos mentais e comportamentais,12,0.0
Pós-Neonatal (28 dias a <1 ano),VII,Doenças do olho e anexos,4,0.0
1 a 6 Anos,XX,Causas externas de morbidade e mortalidade,15903,22.5
1 a 6 Anos,X,Doenças do aparelho respiratório,10621,15.1
1 a 6 Anos,XVII,Malformações congênitas e anomalias cromossômicas,8375,11.9
1 a 6 Anos,II,Neoplasias (tumores),7596,10.8
1 a 6 Anos,VI,Doenças do sistema nervoso,7309,10.4
1 a 6 Anos,I,Algumas doenças infecciosas e parasitárias,6546,9.3
1 a 6 Anos,XVIII,Sintomas e achados anormais mal definidos,3951,5.6
1 a 6 Anos,IX,Doenças do aparelho circulatório,2549,3.6
1 a 6 Anos,IV,"Doenças endócrinas, nutricionais e metabólicas",2206,3.1
1 a 6 Anos,III,Doenças do sangue e órgãos hematopoéticos,1769,2.5
1 a 6 Anos,XI,Doenças do aparelho digestivo,1761,2.5
1 a 6 Anos,XIV,Doenças do aparelho geniturinário,1036,1.5
1 a 6 Anos,XVI,Afecções originadas no período perinatal,337,0.5
1 a 6 Anos,XIII,Doenças do sistema osteomuscular,229,0.3
1 a 6 Anos,XII,Doenças da pele e tecido subcutâneo,212,0.3
1 a 6 Anos,VIII,Doenças do ouvido,113,0.2
1 a 6 Anos,V,Transtornos mentais e comportamentais,45,0.1
1 a 6 Anos,VII,Doenças do olho e anexos,7,0.0
', stringsAsFactors = FALSE, check.names = FALSE, encoding = 'UTF-8')
sim_conc <- read.csv(text = '
escopo,cid,nome,n,pct,cum
Geral,P36,Sepse neonatal,27114,6.6,6.6
Geral,P00,RN afetado por afecções maternas,22227,5.4,12.1
Geral,P22,Desconforto respiratório do RN,21162,5.2,17.2
Geral,Q24,Outras malformações do coração,16381,4.0,21.2
Geral,P01,RN afetado por complicações da gravidez,15470,3.8,25.0
Geral,P07,Prematuridade e baixo peso ao nascer,14452,3.5,28.5
Geral,P02,RN afetado por complicações da placenta/cordão,13899,3.4,31.9
Geral,J18,Pneumonia não especificada,11767,2.9,34.8
Geral,P21,Asfixia ao nascer,9039,2.2,37.0
Geral,P24,Aspiração neonatal,7824,1.9,38.9
Geral,Q89,Outras malformações congênitas,7622,1.9,40.8
Geral,R99,Causas mal definidas e desconhecidas,7581,1.9,42.6
Geral,P96,Outras afecções perinatais,7268,1.8,44.4
Geral,A41,Septicemia (sepse),6971,1.7,46.1
Geral,P28,Outras afecções respiratórias do RN,6807,1.7,47.8
Geral,P77,Enterocolite necrosante,6776,1.7,49.4
Geral,Q79,Malformação do sistema osteomuscular,5774,1.4,50.8
Geral,P29,Transtornos cardiovasculares perinatais,5692,1.4,52.2
Geral,Q21,Malformação dos septos cardíacos,5386,1.3,53.6
Geral,Q33,Malformação do pulmão,5259,1.3,54.8
Outras,R99,Causas mal definidas e desconhecidas,7581,9.3,9.3
Outras,W78,Inalação de conteúdo gástrico,2752,3.4,12.6
Outras,G93,Outros transtornos do encéfalo,2261,2.8,15.4
Outras,W84,Obstrução respiratória não especificada,2232,2.7,18.1
Outras,W79,Engasgo por alimento (obstrução das vias aéreas),2131,2.6,20.7
Outras,W74,Afogamento não especificado,2106,2.6,23.3
Outras,G80,Paralisia cerebral,1928,2.4,25.6
Outras,C71,Câncer do encéfalo,1881,2.3,27.9
Outras,G91,Hidrocefalia,1737,2.1,30.1
Outras,C91,Leucemia linfoide,1697,2.1,32.1
Outras,R95,Morte súbita na infância (SMSL),1499,1.8,34.0
Outras,I42,Cardiomiopatia,1453,1.8,35.8
Outras,G40,Epilepsia,1374,1.7,37.4
Outras,R98,Morte sem assistência,1302,1.6,39.0
Outras,W69,Afogamento em águas naturais,1291,1.6,40.6
Outras,Y34,Evento não especificado (intenção indeterminada),1249,1.5,42.1
Outras,G00,Meningite bacteriana,1088,1.3,43.5
Outras,E43,Desnutrição grave,1047,1.3,44.7
Outras,E46,Desnutrição não especificada,982,1.2,45.9
Outras,K56,Íleo/obstrução intestinal,945,1.2,47.1
', stringsAsFactors = FALSE, check.names = FALSE, encoding = 'UTF-8')
sim_agrup <- read.csv(text = '
faixa,grupo,n,pct
Neonatal (0–27 dias),Outras Causas,2066,47.1
Neonatal (0–27 dias),Causas Externas,1699,38.7
Neonatal (0–27 dias),Doenças Metabólicas/Genéticas,263,6.0
Neonatal (0–27 dias),Neoplasias (Oncologia),186,4.2
Neonatal (0–27 dias),Doenças do Sistema Nervoso,173,3.9
Pós-neonatal (28 d–<1 ano),Outras Causas,17682,54.0
Pós-neonatal (28 d–<1 ano),Causas Externas,8917,27.2
Pós-neonatal (28 d–<1 ano),Doenças do Sistema Nervoso,3819,11.7
Pós-neonatal (28 d–<1 ano),Doenças Metabólicas/Genéticas,1305,4.0
Pós-neonatal (28 d–<1 ano),Neoplasias (Oncologia),1044,3.2
1 a 6 anos,Causas Externas,15903,35.6
1 a 6 anos,Outras Causas,12885,28.8
1 a 6 anos,Neoplasias (Oncologia),7596,17.0
1 a 6 anos,Doenças do Sistema Nervoso,7309,16.4
1 a 6 anos,Doenças Metabólicas/Genéticas,993,2.2
Total 0–6 anos,Outras Causas,32633,39.9
Total 0–6 anos,Causas Externas,26519,32.4
Total 0–6 anos,Doenças do Sistema Nervoso,11301,13.8
Total 0–6 anos,Neoplasias (Oncologia),8826,10.8
Total 0–6 anos,Doenças Metabólicas/Genéticas,2561,3.1
', stringsAsFactors = FALSE, check.names = FALSE, encoding = 'UTF-8')
sim_uf <- read.csv(text = '
sigla,uf,regiao,nv,nmr,imr,u5mr,sdg_nmr,sdg_imr,sdg_u5mr,ipea_nmr,ipea_imr,ipea_u5mr
RO,Rondônia,Norte,258917,8.51,12.96,15.1,1,1,1,0,0,0
AC,Acre,Norte,154832,9.71,16.39,19.69,1,0,1,0,0,0
AM,Amazonas,Norte,753586,10.15,15.76,19.07,1,0,1,0,0,0
RR,Roraima,Norte,128436,11.25,19.18,24.19,1,0,1,0,0,0
PA,Pará,Norte,1342967,10.39,15.08,17.91,1,1,1,0,0,0
AP,Amapá,Norte,146406,12.04,18.67,22.09,0,0,1,0,0,0
TO,Tocantins,Norte,238934,8.44,12.46,15.01,1,1,1,0,0,0
MA,Maranhão,Nordeste,1073603,10.12,14.6,17.21,1,1,1,0,0,0
PI,Piauí,Nordeste,457508,9.97,14.88,17.11,1,1,1,0,0,0
CE,Ceará,Nordeste,1218257,8.34,11.95,13.86,1,1,1,0,0,0
RN,Rio Grande do Norte,Nordeste,435875,8.61,12.2,14.19,1,1,1,0,0,0
PB,Paraíba,Nordeste,554798,8.81,12.73,14.82,1,1,1,0,0,0
PE,Pernambuco,Nordeste,1282496,8.91,12.75,14.86,1,1,1,0,0,0
AL,Alagoas,Nordeste,487958,9.09,13.38,15.67,1,1,1,0,0,0
SE,Sergipe,Nordeste,316019,11.4,15.97,18.22,1,0,1,0,0,0
BA,Bahia,Nordeste,1891721,11.1,15.12,17.25,1,1,1,0,0,0
MG,Minas Gerais,Sudeste,2481450,7.97,11.19,13.08,1,1,1,0,0,0
ES,Espírito Santo,Sudeste,537959,7.6,11.05,13.1,1,1,1,0,0,0
RJ,Rio de Janeiro,Sudeste,2017027,8.65,12.95,15.05,1,1,1,0,0,0
SP,São Paulo,Sudeste,5601469,7.56,10.87,12.55,1,1,1,0,0,0
PR,Paraná,Sul,1483151,7.3,10.3,12.06,1,1,1,0,0,0
SC,Santa Catarina,Sul,971718,6.92,9.55,11.05,1,1,1,0,0,0
RS,Rio Grande do Sul,Sul,1315073,7.11,9.94,11.58,1,1,1,0,0,0
MS,Mato Grosso do Sul,Centro-Oeste,421398,7.64,11.8,14.17,1,1,1,0,0,0
MT,Mato Grosso,Centro-Oeste,571886,8.75,13.2,16.14,1,1,1,0,0,0
GO,Goiás,Centro-Oeste,942902,8.78,12.36,14.46,1,1,1,0,0,0
DF,Distrito Federal,Centro-Oeste,402804,7.54,10.24,11.86,1,1,1,0,0,0
', stringsAsFactors = FALSE, check.names = FALSE, encoding = 'UTF-8')
sim_macro <- read.csv(text = '
regiao,nv,nmr,imr,u5mr,sdg_nmr,sdg_imr,sdg_u5mr,ipea_nmr,ipea_imr,ipea_u5mr
Norte,3024078,10.1,15.27,18.29,1,1,1,0,0,0
Nordeste,7718235,9.68,13.73,15.9,1,1,1,0,0,0
Sudeste,10637905,7.86,11.35,13.18,1,1,1,0,0,0
Sul,3769942,7.14,9.98,11.63,1,1,1,0,0,0
Centro-Oeste,2338990,8.35,12.1,14.37,1,1,1,0,0,0
', stringsAsFactors = FALSE, check.names = FALSE, encoding = 'UTF-8')
sim_ufano <- read.csv(text = '
sigla,ano,nmr,imr,u5mr
RO,2015,9.89,14.51,15.98
RO,2016,9.25,13.42,15.75
RO,2017,8.36,12.83,14.73
RO,2018,8.15,12.71,14.31
RO,2019,7.36,11.51,13.58
RO,2020,9.07,13.02,14.81
RO,2021,8.14,12.34,14.51
RO,2022,8.63,13.41,16.18
RO,2023,7.27,12.25,14.84
RO,2024,8.89,13.58,16.67
AC,2015,10.42,17.14,19.49
AC,2016,8.24,15.15,18.89
AC,2017,8.86,13.63,16.63
AC,2018,10.7,16.5,19.46
AC,2019,8.05,15.91,19.41
AC,2020,9.77,16.05,18.76
AC,2021,11.72,17.84,20.96
AC,2022,9.11,17.19,20.64
AC,2023,10.02,16.93,20.66
AC,2024,10.3,18.01,22.75
AM,2015,10.09,15.54,18.7
AM,2016,9.93,15.97,19.27
AM,2017,10.68,16.54,19.7
AM,2018,10.69,16.05,19.34
AM,2019,10.58,15.99,19.17
AM,2020,9.24,13.9,16.63
AM,2021,10.06,14.8,17.97
AM,2022,9.4,15.69,19.46
AM,2023,10.11,17.09,21.21
AM,2024,10.71,16.15,19.51
RR,2015,10.52,16.74,19.1
RR,2016,10.99,18.46,23.29
RR,2017,9.97,17.89,21.39
RR,2018,13.11,19.93,23.98
RR,2019,9.78,18.06,22.44
RR,2020,12.65,19.19,22.46
RR,2021,10.79,19.35,25.68
RR,2022,9.32,18.79,24.6
RR,2023,13.66,23.88,33.35
RR,2024,11.58,19.11,24.81
PA,2015,10.69,14.97,17.52
PA,2016,11.06,15.67,18.83
PA,2017,11.07,15.39,18.2
PA,2018,10.43,15.04,17.57
PA,2019,10.47,15.14,18.01
PA,2020,10.26,14.86,17.57
PA,2021,10.22,14.73,17.02
PA,2022,9.77,14.66,17.76
PA,2023,10.09,15.04,18.14
PA,2024,9.69,15.27,18.55
AP,2015,10.98,16.76,19.37
AP,2016,12.18,18.3,21.52
AP,2017,12.27,19.61,23.83
AP,2018,12.8,18.53,21.05
AP,2019,11.85,18.82,22.6
AP,2020,12.85,18.04,20.77
AP,2021,12.94,19.94,23.08
AP,2022,11.68,18.07,21.81
AP,2023,11.89,20.93,25.95
AP,2024,10.71,17.85,21.42
TO,2015,8.72,13.02,15.61
TO,2016,9.05,12.48,15.33
TO,2017,8.42,12.39,14.84
TO,2018,8.71,12.68,15.27
TO,2019,7.48,11.7,14.11
TO,2020,7.25,10.62,13.15
TO,2021,8.63,12.59,15.29
TO,2022,8.29,12.33,15.48
TO,2023,8.43,12.7,14.43
TO,2024,9.49,14.19,16.74
MA,2015,10.96,15.22,17.56
MA,2016,10.29,14.99,17.56
MA,2017,11.21,15.82,18.54
MA,2018,9.72,14.07,16.87
MA,2019,10.04,14.36,16.94
MA,2020,9.95,13.74,16.19
MA,2021,9.53,13.53,15.97
MA,2022,10.19,15.32,18.16
MA,2023,9.76,14.84,17.74
MA,2024,9.35,14.01,16.52
PI,2015,10.8,14.82,17.01
PI,2016,11.81,16.24,18.64
PI,2017,10.36,15.59,17.82
PI,2018,10.26,14.85,17.28
PI,2019,9.47,14.65,16.75
PI,2020,9.31,13.88,16.03
PI,2021,9.4,13.75,15.62
PI,2022,9.82,15.76,18.58
PI,2023,9.28,14.95,17.06
PI,2024,8.79,14.26,16.25
CE,2015,8.63,12.06,13.7
CE,2016,8.78,12.64,14.83
CE,2017,9.19,13.2,15.29
CE,2018,8.62,12.12,14.11
CE,2019,8.34,12.23,14.21
CE,2020,8.33,11.62,13.33
CE,2021,7.29,10.7,12.46
CE,2022,8.03,11.73,13.86
CE,2023,8.37,11.72,13.54
CE,2024,7.57,11.19,13.02
RN,2015,9.82,13.85,15.64
RN,2016,8.68,12.81,15.17
RN,2017,8.5,12.31,14.54
RN,2018,8.46,11.72,13.76
RN,2019,8.27,12.42,14.33
RN,2020,8.57,11.3,12.75
RN,2021,8.59,12.23,13.45
RN,2022,7.77,11.06,13.96
RN,2023,7.51,11.16,13.13
RN,2024,9.83,12.81,14.91
PB,2015,8.31,11.64,13.84
PB,2016,8.51,12.64,14.89
PB,2017,9.55,13.29,15.18
PB,2018,8.01,11.68,13.6
PB,2019,8.98,13.03,15.13
PB,2020,8.82,12.68,14.1
PB,2021,9.28,12.63,14.49
PB,2022,9.98,14.72,17.57
PB,2023,8.54,12.96,15.29
PB,2024,8.24,12.31,14.44
PE,2015,9.4,13.0,14.98
PE,2016,9.78,13.93,16.25
PE,2017,8.67,12.12,14.26
PE,2018,8.71,12.39,14.51
PE,2019,8.77,12.25,14.1
PE,2020,8.44,11.62,13.43
PE,2021,8.91,12.42,14.18
PE,2022,8.91,13.27,15.78
PE,2023,8.44,13.21,15.8
PE,2024,9.01,13.47,15.66
AL,2015,10.45,14.64,16.71
AL,2016,9.7,14.31,17.0
AL,2017,8.89,13.4,15.94
AL,2018,8.65,12.53,14.71
AL,2019,8.98,13.23,15.68
AL,2020,8.4,11.98,13.65
AL,2021,9.18,13.36,15.45
AL,2022,8.61,12.83,15.7
AL,2023,8.85,13.56,15.86
AL,2024,9.09,13.93,16.04
SE,2015,10.83,15.01,17.67
SE,2016,11.02,15.36,17.63
SE,2017,11.84,15.38,17.21
SE,2018,12.7,16.81,18.89
SE,2019,12.2,17.28,19.21
SE,2020,11.7,15.89,17.87
SE,2021,10.22,14.04,16.31
SE,2022,11.85,17.63,20.37
SE,2023,11.93,18.48,21.41
SE,2024,9.4,13.98,15.83
BA,2015,11.39,15.32,17.33
BA,2016,11.92,15.99,18.27
BA,2017,11.47,15.1,17.21
BA,2018,11.02,15.19,17.16
BA,2019,10.94,15.06,17.15
BA,2020,10.81,14.34,16.05
BA,2021,11.08,14.93,16.71
BA,2022,11.03,15.35,17.89
BA,2023,10.56,14.81,17.32
BA,2024,10.58,15.05,17.41
MG,2015,8.13,11.44,13.18
MG,2016,7.95,11.49,13.49
MG,2017,8.15,11.43,13.36
MG,2018,7.94,10.96,12.77
MG,2019,8.12,11.45,13.4
MG,2020,7.84,10.44,11.99
MG,2021,7.72,10.68,12.18
MG,2022,8.07,11.37,13.57
MG,2023,7.92,11.27,13.45
MG,2024,7.8,11.31,13.49
ES,2015,7.83,11.42,13.33
ES,2016,8.01,11.68,14.17
ES,2017,7.7,10.67,12.64
ES,2018,7.39,10.58,12.39
ES,2019,7.14,10.65,12.62
ES,2020,6.96,9.76,11.4
ES,2021,8.0,11.24,13.26
ES,2022,7.17,10.79,13.51
ES,2023,7.76,11.59,13.87
ES,2024,8.09,12.22,13.98
RJ,2015,8.39,12.57,14.41
RJ,2016,8.73,13.64,15.77
RJ,2017,8.57,12.4,14.47
RJ,2018,8.4,12.66,14.71
RJ,2019,8.79,13.16,15.29
RJ,2020,8.81,12.6,14.34
RJ,2021,8.71,12.73,14.58
RJ,2022,8.48,13.16,15.71
RJ,2023,8.98,13.45,15.99
RJ,2024,8.72,13.36,15.66
SP,2015,7.58,10.8,12.2
SP,2016,7.67,11.08,12.77
SP,2017,7.68,10.92,12.53
SP,2018,7.44,10.77,12.44
SP,2019,7.78,11.05,12.8
SP,2020,7.02,9.88,11.12
SP,2021,7.27,10.37,11.95
SP,2022,7.7,11.31,13.33
SP,2023,7.87,11.36,13.27
SP,2024,7.57,11.28,13.38
PR,2015,7.87,10.92,12.38
PR,2016,7.35,10.51,12.44
PR,2017,7.49,10.36,11.79
PR,2018,7.55,10.33,12.23
PR,2019,7.33,10.31,12.15
PR,2020,6.77,9.3,10.73
PR,2021,6.6,9.46,10.95
PR,2022,7.1,10.32,12.45
PR,2023,7.5,10.81,12.55
PR,2024,7.33,10.67,13.03
SC,2015,7.24,9.93,11.36
SC,2016,6.33,8.75,10.34
SC,2017,7.38,9.93,11.39
SC,2018,6.91,9.54,10.77
SC,2019,7.06,9.61,10.89
SC,2020,7.16,9.32,10.51
SC,2021,6.7,9.23,10.57
SC,2022,6.83,9.79,11.54
SC,2023,6.29,9.07,10.67
SC,2024,7.24,10.36,12.48
RS,2015,7.19,10.12,11.69
RS,2016,7.06,10.18,11.78
RS,2017,6.97,10.07,11.92
RS,2018,7.1,9.8,11.35
RS,2019,7.62,10.62,12.08
RS,2020,6.66,8.64,9.84
RS,2021,7.15,9.59,11.46
RS,2022,7.32,10.49,12.2
RS,2023,6.82,9.68,11.46
RS,2024,7.24,10.15,12.1
MS,2015,8.02,12.03,14.27
MS,2016,8.22,12.91,15.93
MS,2017,7.75,10.57,12.65
MS,2018,7.61,11.32,13.53
MS,2019,7.23,11.1,13.11
MS,2020,7.6,10.92,12.83
MS,2021,6.9,10.69,12.64
MS,2022,6.84,12.35,15.12
MS,2023,8.25,13.55,16.26
MS,2024,7.94,12.87,15.8
MT,2015,9.25,13.78,16.94
MT,2016,9.25,13.81,17.15
MT,2017,8.59,12.57,15.45
MT,2018,8.06,12.14,14.65
MT,2019,8.34,12.69,15.12
MT,2020,8.01,12.08,14.39
MT,2021,8.78,12.67,15.42
MT,2022,9.21,14.08,17.48
MT,2023,9.24,14.04,17.32
MT,2024,8.82,14.19,17.66
GO,2015,8.98,12.23,14.06
GO,2016,9.36,13.01,14.98
GO,2017,8.47,11.87,13.71
GO,2018,9.04,12.48,14.62
GO,2019,9.51,13.11,15.2
GO,2020,8.27,11.36,12.96
GO,2021,8.88,12.08,14.23
GO,2022,8.5,12.66,15.3
GO,2023,8.42,12.46,14.62
GO,2024,8.25,12.33,14.99
DF,2015,8.15,10.58,12.19
DF,2016,7.8,10.31,11.86
DF,2017,8.19,11.08,12.81
DF,2018,7.92,10.25,11.54
DF,2019,6.29,8.53,10.28
DF,2020,7.22,9.68,10.65
DF,2021,7.73,10.54,11.96
DF,2022,7.24,10.08,12.19
DF,2023,7.76,10.86,12.74
DF,2024,6.85,10.49,12.65
', stringsAsFactors = FALSE, check.names = FALSE, encoding = 'UTF-8')
sim_nacano <- read.csv(text = '
ano,nmr,imr,u5mr
2015,8.78,12.43,14.28
2016,8.79,12.72,14.89
2017,8.76,12.39,14.41
2018,8.54,12.18,14.17
2019,8.6,12.39,14.43
2020,8.27,11.51,13.19
2021,8.39,11.9,13.77
2022,8.47,12.59,15.04
2023,8.5,12.62,14.96
2024,8.42,12.56,14.92
', stringsAsFactors = FALSE, check.names = FALSE, encoding = 'UTF-8')

META_SDG  <- c(nmr = 12.0, imr = 15.7, u5mr = 25.0)
META_IPEA <- c(nmr = 5.3,  imr = 7.7,  u5mr = 8.3)
IND_LABEL <- c(nmr = 'Neonatal (0–27 dias)', imr = 'Infantil (< 1 ano)', u5mr = 'Menores de 5 anos')
IND_ORDER <- c('nmr','imr','u5mr')
PAINEL_FX <- c(neo = 'Neonatal (0–27 dias)', pos = 'Pós-neonatal (28 d–<1 ano)', inf = '1 a 6 anos')
BRASIL_RATE <- c(nmr = 8.56, imr = 12.32, u5mr = 14.39)
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ============================================================================
# 1e. DADOS DE CID (SIH — internações) — pré-computados dos microdados 2015–2024
# ============================================================================
sih_prio_cid <- read.csv(text = '
faixa,prio,cid,nome,n,pct
1 a 6 anos,Atenção à Gestação e Parto,P83,Afecções do tegumento do RN,1147,17.6
1 a 6 anos,Atenção à Gestação e Parto,P23,Pneumonia congênita,1061,16.3
1 a 6 anos,Atenção à Gestação e Parto,P28,Outras afecções respiratórias do RN,1038,15.9
1 a 6 anos,Atenção à Gestação e Parto,P24,Aspiração neonatal,620,9.5
1 a 6 anos,Atenção à Gestação e Parto,P96,Outras afecções perinatais,574,8.8
1 a 6 anos,Atenção à Gestação e Parto,P59,Icterícia neonatal por outras causas,271,4.2
1 a 6 anos,Atenção à Gestação e Parto,P22,Desconforto respiratório do RN,201,3.1
1 a 6 anos,Atenção à Gestação e Parto,P74,Distúrbio metabólico/eletrolítico do RN,154,2.4
1 a 6 anos,Atenção à Gestação e Parto,P07,Prematuridade e baixo peso ao nascer,133,2.0
1 a 6 anos,Atenção à Gestação e Parto,P27,Doença respiratória crônica perinatal,129,2.0
1 a 6 anos,Atenção à Gestação e Parto,P39,Infecções do período perinatal,125,1.9
1 a 6 anos,Atenção à Gestação e Parto,P91,Distúrbios cerebrais do RN,100,1.5
1 a 6 anos,Ação Prioritária (Prevenção/Tratamento),J18,Pneumonia não especificada,788331,23.1
1 a 6 anos,Ação Prioritária (Prevenção/Tratamento),A09,Diarreia e gastroenterite infecciosas,358524,10.5
1 a 6 anos,Ação Prioritária (Prevenção/Tratamento),J45,Asma,333199,9.8
1 a 6 anos,Ação Prioritária (Prevenção/Tratamento),J15,Pneumonia bacteriana,316048,9.3
1 a 6 anos,Ação Prioritária (Prevenção/Tratamento),J35,Doenças crônicas das amígdalas e adenoides,219616,6.4
1 a 6 anos,Ação Prioritária (Prevenção/Tratamento),A49,Infecção bacteriana não especificada,159795,4.7
1 a 6 anos,Ação Prioritária (Prevenção/Tratamento),J21,Bronquiolite aguda,136651,4.0
1 a 6 anos,Ação Prioritária (Prevenção/Tratamento),A04,Outras infecções intestinais bacterianas,132537,3.9
1 a 6 anos,Ação Prioritária (Prevenção/Tratamento),A08,Infecções intestinais virais,107362,3.1
1 a 6 anos,Ação Prioritária (Prevenção/Tratamento),J06,Infecção aguda das vias aéreas superiores,74462,2.2
1 a 6 anos,Ação Prioritária (Prevenção/Tratamento),J12,Pneumonia viral,71800,2.1
1 a 6 anos,Ação Prioritária (Prevenção/Tratamento),J96,Insuficiência respiratória,54338,1.6
1 a 6 anos,Malformações (Alta Complexidade),Q53,Testículo não descido (criptorquidia),51217,22.2
1 a 6 anos,Malformações (Alta Complexidade),Q54,Hipospádia,20404,8.8
1 a 6 anos,Malformações (Alta Complexidade),Q21,Malformação dos septos cardíacos,16863,7.3
1 a 6 anos,Malformações (Alta Complexidade),Q66,Deformidades congênitas do pé,16682,7.2
1 a 6 anos,Malformações (Alta Complexidade),Q37,Fenda labiopalatina,13878,6.0
1 a 6 anos,Malformações (Alta Complexidade),Q35,Fenda palatina,9487,4.1
1 a 6 anos,Malformações (Alta Complexidade),Q25,Malformação das grandes artérias,6754,2.9
1 a 6 anos,Malformações (Alta Complexidade),Q89,Outras malformações congênitas,6444,2.8
1 a 6 anos,Malformações (Alta Complexidade),Q18,Malformações da face e do pescoço,6352,2.7
1 a 6 anos,Malformações (Alta Complexidade),Q69,Polidactilia,5601,2.4
1 a 6 anos,Malformações (Alta Complexidade),Q38,"Malformações da língua, boca e faringe",5475,2.4
1 a 6 anos,Malformações (Alta Complexidade),Q78,Outras osteocondrodisplasias,5093,2.2
1 a 6 anos,Outras Causas / Difícil Prevenção,N47,Prepúcio redundante e fimose,188657,6.9
1 a 6 anos,Outras Causas / Difícil Prevenção,K40,Hérnia inguinal,126136,4.6
1 a 6 anos,Outras Causas / Difícil Prevenção,N39,Transtornos do trato urinário,120188,4.4
1 a 6 anos,Outras Causas / Difícil Prevenção,G40,Epilepsia,107202,3.9
1 a 6 anos,Outras Causas / Difícil Prevenção,K42,Hérnia umbilical,98863,3.6
1 a 6 anos,Outras Causas / Difícil Prevenção,L03,Celulite (flegmão),98410,3.6
1 a 6 anos,Outras Causas / Difícil Prevenção,S52,Fratura do antebraço,83181,3.0
1 a 6 anos,Outras Causas / Difícil Prevenção,S42,Fratura do ombro e do braço,79374,2.9
1 a 6 anos,Outras Causas / Difícil Prevenção,C91,Leucemia linfoide,77487,2.8
1 a 6 anos,Outras Causas / Difícil Prevenção,S06,Traumatismo intracraniano,63243,2.3
1 a 6 anos,Outras Causas / Difícil Prevenção,K35,Apendicite aguda,57190,2.1
1 a 6 anos,Outras Causas / Difícil Prevenção,K92,Outras doenças do aparelho digestivo,49085,1.8
Neonatal (0–27 dias),Atenção à Gestação e Parto,P59,Icterícia neonatal por outras causas,672000,23.4
Neonatal (0–27 dias),Atenção à Gestação e Parto,P22,Desconforto respiratório do RN,538814,18.8
Neonatal (0–27 dias),Atenção à Gestação e Parto,P07,Prematuridade e baixo peso ao nascer,484361,16.9
Neonatal (0–27 dias),Atenção à Gestação e Parto,P96,Outras afecções perinatais,185372,6.5
Neonatal (0–27 dias),Atenção à Gestação e Parto,P39,Infecções do período perinatal,164734,5.7
Neonatal (0–27 dias),Atenção à Gestação e Parto,P70,Distúrbio de carboidratos (RN),101355,3.5
Neonatal (0–27 dias),Atenção à Gestação e Parto,P28,Outras afecções respiratórias do RN,95780,3.3
Neonatal (0–27 dias),Atenção à Gestação e Parto,P36,Sepse neonatal,77091,2.7
Neonatal (0–27 dias),Atenção à Gestação e Parto,P05,Crescimento fetal retardado / desnutrição fetal,76713,2.7
Neonatal (0–27 dias),Atenção à Gestação e Parto,P92,Problemas de alimentação do RN,70937,2.5
Neonatal (0–27 dias),Atenção à Gestação e Parto,P58,Icterícia neonatal por hemólise,52732,1.8
Neonatal (0–27 dias),Atenção à Gestação e Parto,P00,RN afetado por afecções maternas,48451,1.7
Neonatal (0–27 dias),Ação Prioritária (Prevenção/Tratamento),A50,Sífilis congênita,168145,41.1
Neonatal (0–27 dias),Ação Prioritária (Prevenção/Tratamento),A41,Septicemia (sepse),60009,14.7
Neonatal (0–27 dias),Ação Prioritária (Prevenção/Tratamento),J21,Bronquiolite aguda,31257,7.6
Neonatal (0–27 dias),Ação Prioritária (Prevenção/Tratamento),J18,Pneumonia não especificada,26497,6.5
Neonatal (0–27 dias),Ação Prioritária (Prevenção/Tratamento),A49,Infecção bacteriana não especificada,23472,5.7
Neonatal (0–27 dias),Ação Prioritária (Prevenção/Tratamento),J96,Insuficiência respiratória,11039,2.7
Neonatal (0–27 dias),Ação Prioritária (Prevenção/Tratamento),J15,Pneumonia bacteriana,8755,2.1
Neonatal (0–27 dias),Ação Prioritária (Prevenção/Tratamento),A48,Outras doenças bacterianas,6187,1.5
Neonatal (0–27 dias),Ação Prioritária (Prevenção/Tratamento),A31,Infecção por outras micobactérias,6151,1.5
Neonatal (0–27 dias),Ação Prioritária (Prevenção/Tratamento),B34,Infecção viral não especificada,5432,1.3
Neonatal (0–27 dias),Ação Prioritária (Prevenção/Tratamento),A53,Sífilis não especificada,4757,1.2
Neonatal (0–27 dias),Ação Prioritária (Prevenção/Tratamento),A09,Diarreia e gastroenterite infecciosas,4203,1.0
Neonatal (0–27 dias),Malformações (Alta Complexidade),Q24,Outras malformações do coração,8664,8.6
Neonatal (0–27 dias),Malformações (Alta Complexidade),Q25,Malformação das grandes artérias,7196,7.1
Neonatal (0–27 dias),Malformações (Alta Complexidade),Q21,Malformação dos septos cardíacos,6795,6.7
Neonatal (0–27 dias),Malformações (Alta Complexidade),Q79,Malformação do sistema osteomuscular,6199,6.1
Neonatal (0–27 dias),Malformações (Alta Complexidade),Q05,Espinha bífida,6135,6.1
Neonatal (0–27 dias),Malformações (Alta Complexidade),Q89,Outras malformações congênitas,5239,5.2
Neonatal (0–27 dias),Malformações (Alta Complexidade),Q20,Malformação das câmaras cardíacas,4325,4.3
Neonatal (0–27 dias),Malformações (Alta Complexidade),Q39,Malformação do esôfago,3844,3.8
Neonatal (0–27 dias),Malformações (Alta Complexidade),Q03,Hidrocefalia congênita,3442,3.4
Neonatal (0–27 dias),Malformações (Alta Complexidade),Q42,Atresia do intestino grosso,3271,3.2
Neonatal (0–27 dias),Malformações (Alta Complexidade),Q22,Malformação das valvas pulmonar/tricúspide,3184,3.1
Neonatal (0–27 dias),Malformações (Alta Complexidade),Q38,"Malformações da língua, boca e faringe",3098,3.1
Neonatal (0–27 dias),Outras Causas / Difícil Prevenção,Z03,Observação por suspeita de doença,17977,8.4
Neonatal (0–27 dias),Outras Causas / Difícil Prevenção,N39,Transtornos do trato urinário,11033,5.2
Neonatal (0–27 dias),Outras Causas / Difícil Prevenção,K56,Íleo/obstrução intestinal,9848,4.6
Neonatal (0–27 dias),Outras Causas / Difícil Prevenção,Z37,Resultado do parto,9351,4.4
Neonatal (0–27 dias),Outras Causas / Difícil Prevenção,G91,Hidrocefalia,6308,2.9
Neonatal (0–27 dias),Outras Causas / Difícil Prevenção,Z38,Nascidos vivos (local de nascimento),6040,2.8
Neonatal (0–27 dias),Outras Causas / Difícil Prevenção,Z76,Contato com serviços de saúde em outras circunstâncias,4998,2.3
Neonatal (0–27 dias),Outras Causas / Difícil Prevenção,R50,Febre de origem desconhecida,3943,1.8
Neonatal (0–27 dias),Outras Causas / Difícil Prevenção,R69,Causas de morbidade desconhecidas,3938,1.8
Neonatal (0–27 dias),Outras Causas / Difícil Prevenção,L01,Impetigo,3749,1.8
Neonatal (0–27 dias),Outras Causas / Difícil Prevenção,K63,Outras doenças do intestino,3335,1.6
Neonatal (0–27 dias),Outras Causas / Difícil Prevenção,G40,Epilepsia,3035,1.4
Pós-neonatal (28 d–<1 ano),Atenção à Gestação e Parto,P07,Prematuridade e baixo peso ao nascer,31476,29.7
Pós-neonatal (28 d–<1 ano),Atenção à Gestação e Parto,P28,Outras afecções respiratórias do RN,19484,18.4
Pós-neonatal (28 d–<1 ano),Atenção à Gestação e Parto,P22,Desconforto respiratório do RN,14204,13.4
Pós-neonatal (28 d–<1 ano),Atenção à Gestação e Parto,P59,Icterícia neonatal por outras causas,5842,5.5
Pós-neonatal (28 d–<1 ano),Atenção à Gestação e Parto,P36,Sepse neonatal,4615,4.4
Pós-neonatal (28 d–<1 ano),Atenção à Gestação e Parto,P96,Outras afecções perinatais,4000,3.8
Pós-neonatal (28 d–<1 ano),Atenção à Gestação e Parto,P39,Infecções do período perinatal,3877,3.7
Pós-neonatal (28 d–<1 ano),Atenção à Gestação e Parto,P05,Crescimento fetal retardado / desnutrição fetal,3105,2.9
Pós-neonatal (28 d–<1 ano),Atenção à Gestação e Parto,P27,Doença respiratória crônica perinatal,2348,2.2
Pós-neonatal (28 d–<1 ano),Atenção à Gestação e Parto,P92,Problemas de alimentação do RN,1938,1.8
Pós-neonatal (28 d–<1 ano),Atenção à Gestação e Parto,P21,Asfixia ao nascer,1673,1.6
Pós-neonatal (28 d–<1 ano),Atenção à Gestação e Parto,P24,Aspiração neonatal,1505,1.4
Pós-neonatal (28 d–<1 ano),Ação Prioritária (Prevenção/Tratamento),J21,Bronquiolite aguda,410952,24.6
Pós-neonatal (28 d–<1 ano),Ação Prioritária (Prevenção/Tratamento),J18,Pneumonia não especificada,398404,23.8
Pós-neonatal (28 d–<1 ano),Ação Prioritária (Prevenção/Tratamento),J15,Pneumonia bacteriana,155147,9.3
Pós-neonatal (28 d–<1 ano),Ação Prioritária (Prevenção/Tratamento),A09,Diarreia e gastroenterite infecciosas,110507,6.6
Pós-neonatal (28 d–<1 ano),Ação Prioritária (Prevenção/Tratamento),A49,Infecção bacteriana não especificada,59670,3.6
Pós-neonatal (28 d–<1 ano),Ação Prioritária (Prevenção/Tratamento),J96,Insuficiência respiratória,54165,3.2
Pós-neonatal (28 d–<1 ano),Ação Prioritária (Prevenção/Tratamento),J45,Asma,51853,3.1
Pós-neonatal (28 d–<1 ano),Ação Prioritária (Prevenção/Tratamento),A41,Septicemia (sepse),50148,3.0
Pós-neonatal (28 d–<1 ano),Ação Prioritária (Prevenção/Tratamento),J12,Pneumonia viral,36705,2.2
Pós-neonatal (28 d–<1 ano),Ação Prioritária (Prevenção/Tratamento),A04,Outras infecções intestinais bacterianas,31714,1.9
Pós-neonatal (28 d–<1 ano),Ação Prioritária (Prevenção/Tratamento),J06,Infecção aguda das vias aéreas superiores,30314,1.8
Pós-neonatal (28 d–<1 ano),Ação Prioritária (Prevenção/Tratamento),A08,Infecções intestinais virais,26853,1.6
Pós-neonatal (28 d–<1 ano),Malformações (Alta Complexidade),Q21,Malformação dos septos cardíacos,19152,14.4
Pós-neonatal (28 d–<1 ano),Malformações (Alta Complexidade),Q66,Deformidades congênitas do pé,15330,11.5
Pós-neonatal (28 d–<1 ano),Malformações (Alta Complexidade),Q25,Malformação das grandes artérias,9364,7.0
Pós-neonatal (28 d–<1 ano),Malformações (Alta Complexidade),Q37,Fenda labiopalatina,8561,6.4
Pós-neonatal (28 d–<1 ano),Malformações (Alta Complexidade),Q24,Outras malformações do coração,6022,4.5
Pós-neonatal (28 d–<1 ano),Malformações (Alta Complexidade),Q03,Hidrocefalia congênita,5224,3.9
Pós-neonatal (28 d–<1 ano),Malformações (Alta Complexidade),Q69,Polidactilia,4818,3.6
Pós-neonatal (28 d–<1 ano),Malformações (Alta Complexidade),Q20,Malformação das câmaras cardíacas,4655,3.5
Pós-neonatal (28 d–<1 ano),Malformações (Alta Complexidade),Q40,Malformações do trato digestivo superior,3881,2.9
Pós-neonatal (28 d–<1 ano),Malformações (Alta Complexidade),Q36,Fenda labial,3860,2.9
Pós-neonatal (28 d–<1 ano),Malformações (Alta Complexidade),Q22,Malformação das valvas pulmonar/tricúspide,3632,2.7
Pós-neonatal (28 d–<1 ano),Malformações (Alta Complexidade),Q38,"Malformações da língua, boca e faringe",3472,2.6
Pós-neonatal (28 d–<1 ano),Outras Causas / Difícil Prevenção,N39,Transtornos do trato urinário,85654,12.8
Pós-neonatal (28 d–<1 ano),Outras Causas / Difícil Prevenção,K40,Hérnia inguinal,40035,6.0
Pós-neonatal (28 d–<1 ano),Outras Causas / Difícil Prevenção,G40,Epilepsia,34590,5.2
Pós-neonatal (28 d–<1 ano),Outras Causas / Difícil Prevenção,G91,Hidrocefalia,27550,4.1
Pós-neonatal (28 d–<1 ano),Outras Causas / Difícil Prevenção,S06,Traumatismo intracraniano,24056,3.6
Pós-neonatal (28 d–<1 ano),Outras Causas / Difícil Prevenção,L03,Celulite (flegmão),19404,2.9
Pós-neonatal (28 d–<1 ano),Outras Causas / Difícil Prevenção,K56,Íleo/obstrução intestinal,14228,2.1
Pós-neonatal (28 d–<1 ano),Outras Causas / Difícil Prevenção,E46,Desnutrição não especificada,12726,1.9
Pós-neonatal (28 d–<1 ano),Outras Causas / Difícil Prevenção,K92,Outras doenças do aparelho digestivo,12344,1.8
Pós-neonatal (28 d–<1 ano),Outras Causas / Difícil Prevenção,K52,Gastroenterite e colite não infecciosas,11446,1.7
Pós-neonatal (28 d–<1 ano),Outras Causas / Difícil Prevenção,E86,Depleção de volume (desidratação),11252,1.7
Pós-neonatal (28 d–<1 ano),Outras Causas / Difícil Prevenção,R50,Febre de origem desconhecida,10559,1.6
', stringsAsFactors = FALSE, check.names = FALSE, encoding = 'UTF-8')
sih_capitulo <- read.csv(text = '
faixa,cap,nome,n,pct
1 a 6 anos,X,Doenças do aparelho respiratório,2350094,36.7
1 a 6 anos,I,Algumas doenças infecciosas e parasitárias,1063287,16.6
1 a 6 anos,XIX,"Lesões, envenenamentos e causas externas",539541,8.4
1 a 6 anos,XI,Doenças do aparelho digestivo,536857,8.4
1 a 6 anos,XIV,Doenças do aparelho geniturinário,434322,6.8
1 a 6 anos,XII,Doenças da pele e tecido subcutâneo,247149,3.9
1 a 6 anos,XVII,Malformações congênitas e anomalias cromossômicas,231213,3.6
1 a 6 anos,II,Neoplasias (tumores),215381,3.4
1 a 6 anos,VI,Doenças do sistema nervoso,189924,3.0
1 a 6 anos,XVIII,Sintomas e achados anormais mal definidos,114760,1.8
1 a 6 anos,IV,"Doenças endócrinas, nutricionais e metabólicas",101337,1.6
1 a 6 anos,III,Doenças do sangue e órgãos hematopoéticos,92309,1.4
1 a 6 anos,XXI,Fatores que influenciam o estado de saúde,82669,1.3
1 a 6 anos,IX,Doenças do aparelho circulatório,55930,0.9
1 a 6 anos,VIII,Doenças do ouvido,53170,0.8
1 a 6 anos,XIII,Doenças do sistema osteomuscular,53016,0.8
1 a 6 anos,VII,Doenças do olho e anexos,24453,0.4
1 a 6 anos,XVI,Afecções originadas no período perinatal,6508,0.1
1 a 6 anos,V,Transtornos mentais e comportamentais,4413,0.1
1 a 6 anos,XXII,Códigos para propósitos especiais,421,0.0
1 a 6 anos,XV,"Gravidez, parto e puerpério",405,0.0
1 a 6 anos,XX,Causas externas de morbidade e mortalidade,208,0.0
Neonatal (0–27 dias),XVI,Afecções originadas no período perinatal,2870790,79.9
Neonatal (0–27 dias),I,Algumas doenças infecciosas e parasitárias,304633,8.5
Neonatal (0–27 dias),X,Doenças do aparelho respiratório,104241,2.9
Neonatal (0–27 dias),XVII,Malformações congênitas e anomalias cromossômicas,101169,2.8
Neonatal (0–27 dias),XXI,Fatores que influenciam o estado de saúde,47318,1.3
Neonatal (0–27 dias),XI,Doenças do aparelho digestivo,38846,1.1
Neonatal (0–27 dias),XVIII,Sintomas e achados anormais mal definidos,23299,0.6
Neonatal (0–27 dias),XIV,Doenças do aparelho geniturinário,19080,0.5
Neonatal (0–27 dias),IV,"Doenças endócrinas, nutricionais e metabólicas",16853,0.5
Neonatal (0–27 dias),XII,Doenças da pele e tecido subcutâneo,13476,0.4
Neonatal (0–27 dias),XIX,"Lesões, envenenamentos e causas externas",13385,0.4
Neonatal (0–27 dias),IX,Doenças do aparelho circulatório,13056,0.4
Neonatal (0–27 dias),VI,Doenças do sistema nervoso,12928,0.4
Neonatal (0–27 dias),III,Doenças do sangue e órgãos hematopoéticos,3972,0.1
Neonatal (0–27 dias),II,Neoplasias (tumores),3488,0.1
Neonatal (0–27 dias),XV,"Gravidez, parto e puerpério",2539,0.1
Neonatal (0–27 dias),XIII,Doenças do sistema osteomuscular,2330,0.1
Neonatal (0–27 dias),VII,Doenças do olho e anexos,2187,0.1
Neonatal (0–27 dias),VIII,Doenças do ouvido,895,0.0
Neonatal (0–27 dias),V,Transtornos mentais e comportamentais,260,0.0
Neonatal (0–27 dias),XXII,Códigos para propósitos especiais,62,0.0
Neonatal (0–27 dias),XX,Causas externas de morbidade e mortalidade,8,0.0
Pós-neonatal (28 d–<1 ano),X,Doenças do aparelho respiratório,1278307,49.5
Pós-neonatal (28 d–<1 ano),I,Algumas doenças infecciosas e parasitárias,394846,15.3
Pós-neonatal (28 d–<1 ano),XI,Doenças do aparelho digestivo,134367,5.2
Pós-neonatal (28 d–<1 ano),XVII,Malformações congênitas e anomalias cromossômicas,133377,5.2
Pós-neonatal (28 d–<1 ano),XIV,Doenças do aparelho geniturinário,116595,4.5
Pós-neonatal (28 d–<1 ano),XVI,Afecções originadas no período perinatal,106021,4.1
Pós-neonatal (28 d–<1 ano),VI,Doenças do sistema nervoso,80351,3.1
Pós-neonatal (28 d–<1 ano),XIX,"Lesões, envenenamentos e causas externas",61923,2.4
Pós-neonatal (28 d–<1 ano),XII,Doenças da pele e tecido subcutâneo,61299,2.4
Pós-neonatal (28 d–<1 ano),IV,"Doenças endócrinas, nutricionais e metabólicas",50685,2.0
Pós-neonatal (28 d–<1 ano),XVIII,Sintomas e achados anormais mal definidos,39914,1.5
Pós-neonatal (28 d–<1 ano),III,Doenças do sangue e órgãos hematopoéticos,29785,1.2
Pós-neonatal (28 d–<1 ano),IX,Doenças do aparelho circulatório,28623,1.1
Pós-neonatal (28 d–<1 ano),II,Neoplasias (tumores),19767,0.8
Pós-neonatal (28 d–<1 ano),VIII,Doenças do ouvido,14838,0.6
Pós-neonatal (28 d–<1 ano),XXI,Fatores que influenciam o estado de saúde,14785,0.6
Pós-neonatal (28 d–<1 ano),VII,Doenças do olho e anexos,6868,0.3
Pós-neonatal (28 d–<1 ano),XIII,Doenças do sistema osteomuscular,6302,0.2
Pós-neonatal (28 d–<1 ano),V,Transtornos mentais e comportamentais,496,0.0
Pós-neonatal (28 d–<1 ano),XV,"Gravidez, parto e puerpério",455,0.0
Pós-neonatal (28 d–<1 ano),XXII,Códigos para propósitos especiais,362,0.0
Pós-neonatal (28 d–<1 ano),XX,Causas externas de morbidade e mortalidade,29,0.0
Total 0-6 anos,X,Doenças do aparelho respiratório,3732642,29.7
Total 0-6 anos,XVI,Afecções originadas no período perinatal,2983319,23.7
Total 0-6 anos,I,Algumas doenças infecciosas e parasitárias,1762766,14.0
Total 0-6 anos,XI,Doenças do aparelho digestivo,710070,5.6
Total 0-6 anos,XIX,"Lesões, envenenamentos e causas externas",614849,4.9
Total 0-6 anos,XIV,Doenças do aparelho geniturinário,569997,4.5
Total 0-6 anos,XVII,Malformações congênitas e anomalias cromossômicas,465759,3.7
Total 0-6 anos,XII,Doenças da pele e tecido subcutâneo,321924,2.6
Total 0-6 anos,VI,Doenças do sistema nervoso,283203,2.3
Total 0-6 anos,II,Neoplasias (tumores),238636,1.9
Total 0-6 anos,XVIII,Sintomas e achados anormais mal definidos,177973,1.4
Total 0-6 anos,IV,"Doenças endócrinas, nutricionais e metabólicas",168875,1.3
Total 0-6 anos,XXI,Fatores que influenciam o estado de saúde,144772,1.2
Total 0-6 anos,III,Doenças do sangue e órgãos hematopoéticos,126066,1.0
Total 0-6 anos,IX,Doenças do aparelho circulatório,97609,0.8
Total 0-6 anos,VIII,Doenças do ouvido,68903,0.5
Total 0-6 anos,XIII,Doenças do sistema osteomuscular,61648,0.5
Total 0-6 anos,VII,Doenças do olho e anexos,33508,0.3
Total 0-6 anos,V,Transtornos mentais e comportamentais,5169,0.0
Total 0-6 anos,XV,"Gravidez, parto e puerpério",3399,0.0
Total 0-6 anos,XXII,Códigos para propósitos especiais,845,0.0
Total 0-6 anos,XX,Causas externas de morbidade e mortalidade,245,0.0
', stringsAsFactors = FALSE, check.names = FALSE, encoding = 'UTF-8')
sih_conc <- read.csv(text = '
escopo,cid,nome,n,pct,cum
Geral,J18,Pneumonia não especificada,1213232,9.7,9.7
Geral,P59,Icterícia neonatal por outras causas,678113,5.4,15.1
Geral,J21,Bronquiolite aguda,578860,4.6,19.7
Geral,P22,Desconforto respiratório do RN,553219,4.4,24.1
Geral,P07,Prematuridade e baixo peso ao nascer,515970,4.1,28.2
Geral,J15,Pneumonia bacteriana,479950,3.8,32.0
Geral,A09,Diarreia e gastroenterite infecciosas,473234,3.8,35.8
Geral,J45,Asma,386823,3.1,38.9
Geral,A49,Infecção bacteriana não especificada,242937,1.9,40.8
Geral,J35,Doenças crônicas das amígdalas e adenoides,219865,1.7,42.5
Geral,N39,Transtornos do trato urinário,216875,1.7,44.2
Geral,N47,Prepúcio redundante e fimose,191878,1.5,45.7
Geral,P96,Outras afecções perinatais,189946,1.5,47.2
Geral,A50,Sífilis congênita,172720,1.4,48.6
Geral,K40,Hérnia inguinal,168894,1.3,49.9
Geral,P39,Infecções do período perinatal,168736,1.3,51.2
Geral,A04,Outras infecções intestinais bacterianas,165996,1.3,52.5
Geral,G40,Epilepsia,144827,1.2,53.7
Geral,A41,Septicemia (sepse),142746,1.1,54.8
Geral,A08,Infecções intestinais virais,135709,1.1,55.9
Outras,N39,Transtornos do trato urinário,216875,6.0,6.0
Outras,N47,Prepúcio redundante e fimose,191878,5.3,11.3
Outras,K40,Hérnia inguinal,168894,4.7,16.0
Outras,G40,Epilepsia,144827,4.0,20.0
Outras,L03,Celulite (flegmão),120538,3.3,23.3
Outras,K42,Hérnia umbilical,102233,2.8,26.1
Outras,S06,Traumatismo intracraniano,90147,2.5,28.6
Outras,S52,Fratura do antebraço,84522,2.3,30.9
Outras,S42,Fratura do ombro e do braço,80401,2.2,33.1
Outras,C91,Leucemia linfoide,79490,2.2,35.3
Outras,K92,Outras doenças do aparelho digestivo,63900,1.8,37.1
Outras,K35,Apendicite aguda,58197,1.6,38.7
Outras,G91,Hidrocefalia,55804,1.5,40.2
Outras,Z03,Observação por suspeita de doença,52722,1.5,41.7
Outras,E86,Depleção de volume (desidratação),50434,1.4,43.1
Outras,L98,Outras afecções da pele,47156,1.3,44.4
Outras,K52,Gastroenterite e colite não infecciosas,45685,1.3,45.7
Outras,L02,"Abscesso, furúnculo e antraz cutâneo",44284,1.2,46.9
Outras,K56,Íleo/obstrução intestinal,43141,1.2,48.1
Outras,D57,Anemia falciforme,32689,0.9,49.0
', stringsAsFactors = FALSE, check.names = FALSE, encoding = 'UTF-8')
sih_agrup <- read.csv(text = '
faixa,grupo,n,pct
1 a 6 anos,Causas Externas,208,0.0
1 a 6 anos,Doenças Metabólicas/Genéticas,68740,2.5
1 a 6 anos,Doenças do Sistema Nervoso,189924,6.9
1 a 6 anos,Neoplasias (Oncologia),215381,7.8
1 a 6 anos,Outras Causas,2272012,82.7
Neonatal (0–27 dias),Causas Externas,8,0.0
Neonatal (0–27 dias),Doenças Metabólicas/Genéticas,8895,4.2
Neonatal (0–27 dias),Doenças do Sistema Nervoso,12928,6.0
Neonatal (0–27 dias),Neoplasias (Oncologia),3488,1.6
Neonatal (0–27 dias),Outras Causas,188663,88.2
Pós-neonatal (28 d–<1 ano),Causas Externas,29,0.0
Pós-neonatal (28 d–<1 ano),Doenças Metabólicas/Genéticas,23281,3.5
Pós-neonatal (28 d–<1 ano),Doenças do Sistema Nervoso,80351,12.0
Pós-neonatal (28 d–<1 ano),Neoplasias (Oncologia),19767,3.0
Pós-neonatal (28 d–<1 ano),Outras Causas,544016,81.5
Total 0–6 anos,Causas Externas,245,0.0
Total 0–6 anos,Doenças Metabólicas/Genéticas,100916,2.8
Total 0–6 anos,Doenças do Sistema Nervoso,283203,7.8
Total 0–6 anos,Neoplasias (Oncologia),238636,6.6
Total 0–6 anos,Outras Causas,3004691,82.8
', stringsAsFactors = FALSE, check.names = FALSE, encoding = 'UTF-8')
CID_DISTINTOS <- list(mort = c(Geral = 1069, Outras = 762), int = c(Geral = 1743, Outras = 1363))

# ============================================================================
# 1f. MOTOR DE CLASSIFICAÇÃO CID-10
#     (a) por SISTEMA DO ORGANISMO — regra determinística por faixa de código,
#         válida para qualquer CID-10, inclusive códigos ainda não vistos;
#     (b) por GRUPO DE PATOLOGIA — consolidação de condições clínicas
#         equivalentes (cardiopatias congênitas, sepse, afogamento, ...);
#     (c) marcação de códigos GENÉRICOS / mal definidos / administrativos,
#         que é o que permite responder "entrou genérico e saiu genérico?".
#     Revisão de nomenclatura conforme a reunião de 27/07 (inclui W84).
# ============================================================================

# --- (a) Capítulo e sistema por faixa de código -----------------------------
CID_CAPITULOS <- data.frame(
  cap    = c("I","II","III","IV","V","VI","VII","VIII","IX","X","XI","XII",
             "XIII","XIV","XV","XVI","XVII","XVIII","XIX","XX","XXI","XXII"),
  letra0 = c("A","C","D","E","F","G","H","H","I","J","K","L",
             "M","N","O","P","Q","R","S","V","Z","U"),
  ini    = c("A00","C00","D50","E00","F00","G00","H00","H60","I00","J00","K00","L00",
             "M00","N00","O00","P00","Q00","R00","S00","V01","Z00","U00"),
  fim    = c("B99","D48","D89","E90","F99","G99","H59","H95","I99","J99","K93","L99",
             "M99","N99","O99","P96","Q99","R99","T98","Y98","Z99","U99"),
  nome   = c("Algumas doenças infecciosas e parasitárias",
             "Neoplasias (tumores)",
             "Doenças do sangue e dos órgãos hematopoéticos",
             "Doenças endócrinas, nutricionais e metabólicas",
             "Transtornos mentais e comportamentais",
             "Doenças do sistema nervoso",
             "Doenças do olho e anexos",
             "Doenças do ouvido e da apófise mastoide",
             "Doenças do aparelho circulatório",
             "Doenças do aparelho respiratório",
             "Doenças do aparelho digestivo",
             "Doenças da pele e do tecido subcutâneo",
             "Doenças do sistema osteomuscular e do tecido conjuntivo",
             "Doenças do aparelho geniturinário",
             "Gravidez, parto e puerpério",
             "Afecções originadas no período perinatal",
             "Malformações congênitas e anomalias cromossômicas",
             "Sintomas, sinais e achados anormais mal definidos",
             "Lesões, envenenamentos e outras consequências de causas externas",
             "Causas externas de morbidade e mortalidade",
             "Contato com os serviços de saúde",
             "Códigos para propósitos especiais (inclui COVID-19)"),
  sistema = c("Doenças infecciosas e parasitárias",
              "Neoplasias (oncologia)",
              "Sangue, órgãos hematopoéticos e imunidade",
              "Endócrino, nutricional e metabólico",
              "Saúde mental e comportamento",
              "Sistema nervoso",
              "Olho e anexos",
              "Ouvido",
              "Sistema cardiovascular",
              "Sistema respiratório",
              "Sistema digestório",
              "Pele e tecido subcutâneo",
              "Sistema osteomuscular",
              "Sistema geniturinário",
              "Gestação, parto e puerpério",
              "Afecções perinatais",
              "Malformações congênitas e cromossômicas",
              "Causas mal definidas",
              "Lesões e traumatismos",
              "Causas externas",
              "Registro administrativo (não-diagnóstico)",
              "Códigos especiais (COVID-19 e afins)"),
  stringsAsFactors = FALSE
)

# Converte "Q24" em um inteiro ordenável (A=0 ... Z=25) → letra*100 + dígitos.
cid_rank <- function(x) {
  x <- toupper(substr(gsub("[^A-Za-z0-9]", "", as.character(x)), 1, 3))
  l <- match(substr(x, 1, 1), LETTERS) - 1L
  d <- suppressWarnings(as.integer(substr(x, 2, 3)))
  ifelse(is.na(l) | is.na(d), NA_integer_, l * 100L + d)
}

CID_CAPITULOS$r_ini <- cid_rank(CID_CAPITULOS$ini)
CID_CAPITULOS$r_fim <- cid_rank(CID_CAPITULOS$fim)

# Classificação por faixa (vetorizada). Devolve capítulo, nome e sistema.
cid_capitulo_de <- function(cid) {
  r <- cid_rank(cid)
  idx <- vapply(r, function(v) {
    if (is.na(v)) return(NA_integer_)
    hit <- which(v >= CID_CAPITULOS$r_ini & v <= CID_CAPITULOS$r_fim)
    if (length(hit) == 0) NA_integer_ else hit[1]
  }, integer(1))
  data.frame(cap     = CID_CAPITULOS$cap[idx],
             cap_nome = CID_CAPITULOS$nome[idx],
             sistema  = CID_CAPITULOS$sistema[idx],
             stringsAsFactors = FALSE)
}

# --- (b) Dicionário revisado dos códigos presentes nas bases ---------------
# nome     : nomenclatura CID-10 conferida (correções da reunião aplicadas)
# grupo    : agrupamento de patologias semelhantes (insumo das análises)
# generico : 1 = código inespecífico / mal definido / "outras" / administrativo
# nota     : registro da revisão, exibido na aba Metodologia
cid_dic <- read.csv(text = '
cid|nome|grupo|generico|nota
A04|Outras infecções intestinais bacterianas|Diarreia e gastroenterite|0|
A08|Infecções intestinais virais e outras especificadas|Diarreia e gastroenterite|0|
A09|Diarreia e gastroenterite de origem infecciosa presumível|Diarreia e gastroenterite|0|
A31|Infecções por outras micobactérias|Sepse e infecções sistêmicas|0|
A39|Infecção meningocócica|Meningites e infecções do SNC|0|Nomenclatura ajustada: CID-10 A39 é "Infecção meningocócica".
A41|Outras septicemias (sepse) — agente não especificado|Sepse e infecções sistêmicas|1|Sepse sem identificação do agente: código inespecífico.
A48|Outras doenças bacterianas não classificadas em outra parte|Sepse e infecções sistêmicas|1|
A49|Infecção bacteriana de localização não especificada|Sepse e infecções sistêmicas|1|
A50|Sífilis congênita|Infecções congênitas (sífilis e TORCH)|0|
A53|Sífilis não especificada e outras sífilis|Infecções congênitas (sífilis e TORCH)|1|
B34|Doenças por vírus de localização não especificada|Sepse e infecções sistêmicas|1|Nomenclatura ajustada; código inespecífico.
B55|Leishmaniose|Doenças tropicais negligenciadas|0|
B99|Doenças infecciosas outras e as não especificadas|Sepse e infecções sistêmicas|1|
C71|Neoplasia maligna do encéfalo|Tumores do sistema nervoso central|0|
C74|Neoplasia maligna da glândula suprarrenal|Tumores sólidos extracranianos|0|Rótulo anterior citava neuroblastoma; C74 é a topografia suprarrenal.
C91|Leucemia linfoide|Leucemias|0|
C92|Leucemia mieloide|Leucemias|0|
D57|Transtornos falciformes|Hemoglobinopatias|0|Nomenclatura ajustada: D57 cobre todos os transtornos falciformes.
E43|Desnutrição proteico-calórica grave não especificada|Desnutrição|1|
E46|Desnutrição proteico-calórica não especificada|Desnutrição|1|
E86|Depleção de volume (desidratação)|Desidratação e distúrbios hidroeletrolíticos|0|
E87|Outros transtornos do equilíbrio hidroeletrolítico e ácido-base|Desidratação e distúrbios hidroeletrolíticos|0|
G00|Meningite bacteriana não classificada em outra parte|Meningites e infecções do SNC|0|
G40|Epilepsia|Epilepsia e encefalopatias|0|
G80|Paralisia cerebral infantil|Epilepsia e encefalopatias|0|
G91|Hidrocefalia|Epilepsia e encefalopatias|0|
G93|Outros transtornos do encéfalo|Epilepsia e encefalopatias|1|
I42|Cardiomiopatia|Cardiomiopatias|0|
J06|Infecções agudas das vias aéreas superiores de localizações múltiplas ou não especificadas|Infecções respiratórias altas|1|
J12|Pneumonia viral não classificada em outra parte|Infecções respiratórias baixas|0|
J15|Pneumonia bacteriana não classificada em outra parte|Infecções respiratórias baixas|0|
J18|Pneumonia por microrganismo não especificado|Infecções respiratórias baixas|1|Pneumonia sem agente identificado: código inespecífico de alto volume.
J21|Bronquiolite aguda|Infecções respiratórias baixas|0|
J35|Doenças crônicas das amígdalas e das adenoides|Infecções respiratórias altas|0|
J45|Asma|Asma e sibilância|0|
J69|Pneumonite devida a sólidos e líquidos (aspiração)|Infecções respiratórias baixas|0|
J81|Edema pulmonar|Insuficiência e outros transtornos respiratórios|0|
J96|Insuficiência respiratória não classificada em outra parte|Insuficiência e outros transtornos respiratórios|1|
J98|Outros transtornos respiratórios|Insuficiência e outros transtornos respiratórios|1|
K35|Apendicite aguda|Afecções cirúrgicas do abdome|0|
K40|Hérnia inguinal|Afecções cirúrgicas do abdome|0|
K42|Hérnia umbilical|Afecções cirúrgicas do abdome|0|
K52|Outras gastroenterites e colites não infecciosas|Diarreia e gastroenterite|0|
K56|Íleo paralítico e obstrução intestinal sem hérnia|Afecções cirúrgicas do abdome|0|
K63|Outras doenças do intestino|Afecções cirúrgicas do abdome|1|
K92|Outras doenças do aparelho digestivo|Afecções cirúrgicas do abdome|1|
L01|Impetigo|Infecções de pele e partes moles|0|
L02|Abscesso cutâneo, furúnculo e antraz|Infecções de pele e partes moles|0|
L03|Celulite (flegmão)|Infecções de pele e partes moles|0|
L98|Outras afecções da pele e do tecido subcutâneo|Infecções de pele e partes moles|1|
N39|Outros transtornos do trato urinário|Infecções e transtornos do trato urinário|1|
N47|Prepúcio redundante, fimose e parafimose|Procedimentos urológicos eletivos|0|
P00|Recém-nascido afetado por afecções maternas|Complicações maternas e placentárias|0|
P01|Recém-nascido afetado por complicações maternas da gravidez|Complicações maternas e placentárias|0|
P02|Recém-nascido afetado por complicações da placenta, do cordão e das membranas|Complicações maternas e placentárias|0|
P05|Crescimento fetal retardado e desnutrição fetal|Prematuridade e baixo peso|0|
P07|Transtornos relacionados com gestação de curta duração e baixo peso ao nascer|Prematuridade e baixo peso|0|
P20|Hipóxia intrauterina|Asfixia e hipóxia perinatal|0|
P21|Asfixia ao nascer|Asfixia e hipóxia perinatal|0|
P22|Desconforto respiratório do recém-nascido|Insuficiência e outros transtornos respiratórios|0|
P23|Pneumonia congênita|Infecções respiratórias baixas|0|
P24|Síndromes de aspiração neonatal|Aspiração neonatal|0|
P27|Doença respiratória crônica originada no período perinatal|Insuficiência e outros transtornos respiratórios|0|
P28|Outras afecções respiratórias do recém-nascido|Insuficiência e outros transtornos respiratórios|1|
P29|Transtornos cardiovasculares originados no período perinatal|Transtornos cardiovasculares perinatais|0|
P35|Doenças virais congênitas|Infecções congênitas (sífilis e TORCH)|0|
P36|Septicemia bacteriana do recém-nascido|Sepse e infecções sistêmicas|0|
P37|Outras doenças infecciosas e parasitárias congênitas|Infecções congênitas (sífilis e TORCH)|0|
P39|Outras infecções próprias do período perinatal|Sepse e infecções sistêmicas|1|
P57|Kernicterus|Icterícia e distúrbios hematológicos neonatais|0|
P58|Icterícia neonatal devida a outras hemólises excessivas|Icterícia e distúrbios hematológicos neonatais|0|
P59|Icterícia neonatal devida a outras causas e às não especificadas|Icterícia e distúrbios hematológicos neonatais|1|
P70|Transtornos transitórios do metabolismo dos carboidratos do recém-nascido|Distúrbios metabólicos neonatais|0|
P74|Outros distúrbios metabólicos e eletrolíticos transitórios do recém-nascido|Distúrbios metabólicos neonatais|0|
P77|Enterocolite necrotizante do feto e do recém-nascido|Enterocolite necrotizante|0|
P83|Outras afecções comprometendo o tegumento do feto e do recém-nascido|Outras afecções perinatais|1|
P91|Outros distúrbios da função cerebral do recém-nascido|Asfixia e hipóxia perinatal|1|
P92|Problemas de alimentação do recém-nascido|Outras afecções perinatais|0|
P94|Transtornos do tônus muscular do recém-nascido|Outras afecções perinatais|0|
P96|Outras afecções originadas no período perinatal|Outras afecções perinatais|1|
Q00|Anencefalia e malformações similares|Malformações do sistema nervoso central|0|
Q02|Microcefalia|Malformações do sistema nervoso central|0|
Q03|Hidrocefalia congênita|Malformações do sistema nervoso central|0|
Q04|Outras malformações congênitas do cérebro|Malformações do sistema nervoso central|1|
Q05|Espinha bífida|Malformações do sistema nervoso central|0|
Q18|Outras malformações congênitas da face e do pescoço|Malformações osteoarticulares e craniofaciais|1|
Q20|Malformações congênitas das câmaras e das comunicações cardíacas|Cardiopatias congênitas|0|
Q21|Malformações congênitas dos septos cardíacos|Cardiopatias congênitas|0|
Q22|Malformações congênitas das valvas pulmonar e tricúspide|Cardiopatias congênitas|0|
Q23|Malformações congênitas das valvas aórtica e mitral|Cardiopatias congênitas|0|
Q24|Outras malformações congênitas do coração|Cardiopatias congênitas|1|Maior volume do bloco cardíaco, porém é o código "outras": limita a leitura clínica.
Q25|Malformações congênitas das grandes artérias|Cardiopatias congênitas|0|
Q33|Malformações congênitas do pulmão|Malformações respiratórias|0|
Q35|Fenda palatina|Fendas orofaciais|0|
Q36|Fenda labial|Fendas orofaciais|0|
Q37|Fenda labial com fenda palatina|Fendas orofaciais|0|
Q38|Outras malformações congênitas da língua, da boca e da faringe|Malformações do trato digestivo|1|
Q39|Malformações congênitas do esôfago|Malformações do trato digestivo|0|
Q40|Outras malformações congênitas do trato digestivo superior|Malformações do trato digestivo|1|
Q42|Ausência, atresia e estenose congênitas do intestino grosso|Malformações do trato digestivo|0|
Q53|Testículo não descido (criptorquidia)|Malformações genitais|0|
Q54|Hipospádia|Malformações genitais|0|
Q60|Agenesia renal e outras deficiências de redução do rim|Malformações do trato urinário|0|
Q66|Deformidades congênitas do pé|Malformações osteoarticulares e craniofaciais|0|
Q69|Polidactilia|Malformações osteoarticulares e craniofaciais|0|
Q78|Outras osteocondrodisplasias|Malformações osteoarticulares e craniofaciais|1|
Q79|Malformações congênitas do sistema osteomuscular NCOP (hérnia diafragmática, onfalocele, gastrosquise)|Malformações da parede abdominal e do diafragma|0|Reclassificado: no 0–6 anos os óbitos de Q79 são majoritariamente hérnia diafragmática (Q79.0) e defeitos de parede abdominal (Q79.2–Q79.3), não doença osteomuscular.
Q87|Outras síndromes com malformações congênitas múltiplas|Síndromes genéticas e cromossômicas|0|
Q89|Outras malformações congênitas não classificadas em outra parte|Malformações múltiplas e inespecíficas|1|
Q90|Síndrome de Down|Síndromes genéticas e cromossômicas|0|
Q91|Síndrome de Edwards e síndrome de Patau|Síndromes genéticas e cromossômicas|0|
R50|Febre de origem desconhecida|Causas mal definidas|1|
R69|Causas desconhecidas e não especificadas de morbidade|Causas mal definidas|1|
R95|Síndrome da morte súbita na infância|Morte súbita do lactente|0|
R98|Morte sem assistência|Causas mal definidas|1|
R99|Outras causas mal definidas e as não especificadas de mortalidade|Causas mal definidas|1|
S06|Traumatismo intracraniano|Traumatismos|0|
S42|Fratura do ombro e do braço|Traumatismos|0|
S52|Fratura do antebraço|Traumatismos|0|
W67|Afogamento e submersão em piscina|Afogamento|0|
W69|Afogamento e submersão em águas naturais|Afogamento|0|
W74|Afogamento e submersão não especificados|Afogamento|1|
W75|Sufocação e estrangulamento acidentais na cama|Sufocação, aspiração e engasgo|0|
W78|Inalação do conteúdo gástrico|Sufocação, aspiração e engasgo|0|
W79|Inalação e ingestão de alimentos causando obstrução do trato respiratório|Sufocação, aspiração e engasgo|0|
W84|Obstrução não especificada da respiração|Sufocação, aspiração e engasgo|1|CORRIGIDO. O rótulo anterior ("obstrução respiratória não especificada") invertia o sentido do código. W84 é um garbage code de causa externa: registra que houve obstrução da respiração sem dizer o mecanismo.
Y09|Agressão por meios não especificados|Causas mal definidas|1|Violência sem mecanismo especificado: entra como causa externa mal definida.
Y34|Fato ou evento não especificado e intenção não determinada|Causas mal definidas|1|
Z03|Observação e avaliação médicas por suspeita de doenças|Registros administrativos (não-diagnóstico)|1|Código administrativo do SIH: não é diagnóstico. Deve ser excluído de análises de causa.
Z37|Resultado do parto|Registros administrativos (não-diagnóstico)|1|Código administrativo do SIH: não é diagnóstico.
Z38|Nascidos vivos, segundo o local de nascimento|Registros administrativos (não-diagnóstico)|1|Código administrativo do SIH: marca o nascimento, não uma doença. Infla o volume de internações neonatais.
Z76|Pessoas em contato com os serviços de saúde em outras circunstâncias|Registros administrativos (não-diagnóstico)|1|Código administrativo do SIH: não é diagnóstico.
', sep = "|", stringsAsFactors = FALSE, quote = "", check.names = FALSE,
   encoding = "UTF-8", colClasses = "character")

cid_dic$generico <- as.integer(cid_dic$generico)
cid_dic$nota[is.na(cid_dic$nota)] <- ""
cid_dic <- cbind(cid_dic, cid_capitulo_de(cid_dic$cid))

# Padrões de texto que denunciam código inespecífico, usados como rede de
# segurança para códigos fora do dicionário (microdados, novas exportações).
RX_GENERICO <- paste0("n[aã]o especificad|nao especificad|inespec[ií]fic|",
                      "mal definid|desconhecid|outras? |outros |NCOP|",
                      "sem assist|intenç[aã]o n[aã]o determinad")

# Faixas de "garbage code" reconhecidas na literatura (GBD/RIPSA): capítulo
# XVIII inteiro, códigos administrativos (XXI), intenção indeterminada
# (Y10–Y34), sequelas sem causa (Y86–Y89), exposição a fator não especificado
# (X59), meio de transporte não especificado (V99) e obstrução respiratória
# sem mecanismo (W84).
CID_GARBAGE_FAIXAS <- rbind(
  data.frame(ini = "Y10", fim = "Y34"), data.frame(ini = "Y86", fim = "Y89"),
  data.frame(ini = "X59", fim = "X59"), data.frame(ini = "V99", fim = "V99"),
  data.frame(ini = "W84", fim = "W84"), data.frame(ini = "Y09", fim = "Y09"))
CID_GARBAGE_FAIXAS$r_ini <- cid_rank(CID_GARBAGE_FAIXAS$ini)
CID_GARBAGE_FAIXAS$r_fim <- cid_rank(CID_GARBAGE_FAIXAS$fim)

eh_garbage_faixa <- function(cid) {
  r <- cid_rank(cid)
  vapply(r, function(v) {
    if (is.na(v)) return(FALSE)
    any(v >= CID_GARBAGE_FAIXAS$r_ini & v <= CID_GARBAGE_FAIXAS$r_fim)
  }, logical(1))
}

# API única de classificação — serve ao app e ao ETL de microdados.
cid_classify <- function(cid, nome = NULL) {
  key <- toupper(substr(gsub("[^A-Za-z0-9]", "", as.character(cid)), 1, 3))
  key[!nzchar(key)] <- NA_character_
  i   <- match(key, cid_dic$cid)
  cp  <- cid_capitulo_de(key)
  nm_in <- if (is.null(nome)) rep(NA_character_, length(key)) else as.character(nome)
  nm  <- ifelse(!is.na(i), cid_dic$nome[i], nm_in)
  nm  <- ifelse(is.na(nm) & !is.na(key), key, nm)
  gp  <- ifelse(!is.na(i), cid_dic$grupo[i],
                ifelse(is.na(key), NA_character_,
                       paste0("Não classificado (", cp$sistema, ")")))
  gen <- ifelse(!is.na(i), cid_dic$generico[i],
                as.integer(grepl(RX_GENERICO, ifelse(is.na(nm), "", nm), ignore.case = TRUE) |
                             cp$cap %in% c("XVIII", "XXI") |
                             eh_garbage_faixa(key)))
  gen[is.na(gen)] <- 0L
  data.frame(cid = key, nome = nm, cap = cp$cap, cap_nome = cp$cap_nome,
             sistema = cp$sistema, grupo = gp, generico = gen,
             especificidade = ifelse(gen == 1, "Genérico / mal definido", "Específico"),
             stringsAsFactors = FALSE)
}

# Aplica a nomenclatura revisada às tabelas pré-computadas do app.
aplica_revisao_cid <- function(df) {
  if (is.null(df) || !"cid" %in% names(df)) return(df)
  cl <- cid_classify(df$cid, df$nome)
  df$nome           <- ifelse(is.na(cl$nome), df$nome, cl$nome)
  df$sistema        <- cl$sistema
  df$grupo_pat      <- cl$grupo
  df$generico       <- cl$generico
  df$especificidade <- cl$especificidade
  df$cap_cid        <- cl$cap
  df
}

pal_sistema <- c(
  "Afecções perinatais"                       = "#004B87",
  "Malformações congênitas e cromossômicas"   = "#E67E22",
  "Sistema respiratório"                      = "#16A085",
  "Doenças infecciosas e parasitárias"        = "#2E86C1",
  "Causas externas"                           = "#D9534F",
  "Sistema nervoso"                           = "#00A3A1",
  "Neoplasias (oncologia)"                    = "#8E44AD",
  "Sistema cardiovascular"                    = "#C0392B",
  "Endócrino, nutricional e metabólico"       = "#F4A261",
  "Sistema digestório"                        = "#A0522D",
  "Sangue, órgãos hematopoéticos e imunidade" = "#B03A2E",
  "Sistema geniturinário"                     = "#5D6D7E",
  "Pele e tecido subcutâneo"                  = "#D2B4DE",
  "Sistema osteomuscular"                     = "#7D6608",
  "Causas mal definidas"                      = "#6C757D",
  "Lesões e traumatismos"                     = "#922B21",
  "Registro administrativo (não-diagnóstico)" = "#95A5A6",
  "Ouvido"                                    = "#AAB7B8",
  "Olho e anexos"                             = "#AEB6BF",
  "Saúde mental e comportamento"              = "#85929E",
  "Gestação, parto e puerpério"               = "#CA6F1E")

cor_sistema <- function(x) {
  co <- unname(pal_sistema[as.character(x)])
  co[is.na(co)] <- pal$gray_2
  co
}

# ============================================================================
# 1g. DENOMINADORES — nascidos vivos (<1 ano) e população da faixa (1–6 anos)
#     Ata de 27/07: "Calcular todas as taxas na população por nascidos vivos,
#     até 1 ano, para poder comparar regiões diferentes. Para as outras taxas
#     usar as populações das faixas etárias."
#
#       • Faixas neonatal e pós-neonatal  → denominador = nascidos vivos (SINASC)
#       • Faixa 1–6 anos                  → denominador = população da faixa
#
#     A população de 1 a 6 anos vem de `populacao_uf_faixa.csv`, gerado pelo
#     script `01_baixar_populacao.R` a partir do POPSVS/DATASUS (IBGE).
#     SEM ESSE ARQUIVO O APP NÃO ESTIMA NADA: as taxas da faixa de 1 a 6 anos
#     ficam em branco e o painel indica qual script rodar.
# ============================================================================
POP_ARQ <- "populacao_uf_faixa.csv"

pop_raw <- if (file.exists(POP_ARQ))
  tryCatch(read.csv(POP_ARQ, stringsAsFactors = FALSE, fileEncoding = "UTF-8"),
           error = function(e) NULL) else NULL

POP_REAL <- !is.null(pop_raw) &&
  all(c("sigla", "ano", "faixa", "populacao") %in% names(pop_raw))

POP_FONTE <- if (POP_REAL)
  "POPSVS/DATASUS (IBGE) — populacao_uf_faixa.csv" else
  "não carregada"

ANOS_PERIODO <- max(1L, ano_max - ano_min + 1L)

# Nascidos vivos por UF: dado real do SINASC, já presente nas tabelas do app.
denom_uf <- sim_uf[, c("sigla", "uf", "regiao", "nv", "imr")]
names(denom_uf)[names(denom_uf) == "nv"] <- "nv_periodo"

# Crianças de 1 a 6 anos somadas ano a ano no período, por UF. Dividir os
# óbitos do período por essa soma dá a média ANUAL por 1.000 crianças.
# Só existe se o arquivo oficial estiver presente.
pop_1a6_ano <- if (POP_REAL) {
  p <- pop_raw
  p <- p[grepl("1", p$faixa) & grepl("6", p$faixa), ]
  if (nrow(p) == 0) NULL else
    p %>% group_by(sigla, ano) %>%
      summarise(pop = sum(populacao, na.rm = TRUE), .groups = "drop")
} else NULL

# Soma das crianças de 1 a 6 anos em cada ano do intervalo pedido.
pa_1a6_periodo <- function(y0, y1) {
  if (is.null(pop_1a6_ano)) return(NULL)
  pop_1a6_ano %>% filter(ano >= y0, ano <= y1) %>%
    group_by(sigla) %>% summarise(pa = sum(pop, na.rm = TRUE), .groups = "drop")
}

denom_uf$pa_1a6 <- if (!is.null(pop_1a6_ano)) {
  ag <- pa_1a6_periodo(ano_min, ano_max)
  ag$pa[match(denom_uf$sigla, ag$sigla)]
} else NA_real_

# Crianças de 0 a 6 anos somadas ano a ano — denominador correto para as taxas
# que cobrem a coorte inteira (mapas, ranking estadual, macrorregiões).
pop_0a6_ano <- if (POP_REAL) {
  pop_raw %>% group_by(sigla, ano) %>%
    summarise(pop = sum(populacao, na.rm = TRUE), .groups = "drop")
} else NULL

pa_0a6_periodo <- function(y0, y1) {
  if (is.null(pop_0a6_ano)) return(NULL)
  pop_0a6_ano %>% filter(ano >= y0, ano <= y1) %>%
    group_by(sigla) %>% summarise(pa = sum(pop, na.rm = TRUE), .groups = "drop")
}

denom_macro <- denom_uf %>%
  group_by(regiao) %>%
  summarise(nv_periodo = sum(nv_periodo, na.rm = TRUE),
            pa_1a6 = if (all(is.na(pa_1a6))) NA_real_ else sum(pa_1a6, na.rm = TRUE),
            .groups = "drop")

DENOM_LBL <- c(nv  = "por 1.000 nascidos vivos",
               pop = "por 1.000 crianças da faixa (média anual)")

# ============================================================================
# 1h. TRAJETÓRIA HOSPITALAR E TRANSIÇÃO DE CID (SIH ↔ SIM)
#       T0 = entrada hospitalar (SIH)
#       T1 = seguimento hospitalar → transferência
#       T2 = desfecho (alta, óbito, transferência) → SIM
#
#     A base é produzida por `00_link_sih_sim.R`, que baixa os microdados
#     reais do SIH e do SIM no DATASUS, faz o linkage e grava
#     `sih_sim_linkado.rds`.
#
#     NÃO HÁ BASE SUBSTITUTA. Sem o arquivo, os painéis de trajetória e de
#     transição de CID ficam vazios e mostram a instrução de como gerá-los.
#     Nenhum número é inventado em nenhuma circunstância.
# ============================================================================
TRAJ_ARQ <- "sih_sim_linkado.rds"
TRAJ_COLS <- c("ano", "regiao", "uf", "faixa", "sexo", "raca_cor",
               "cid_entrada", "cid_obito", "cid_secundario", "desfecho",
               "n_transferencias", "dias_internacao", "linkado")

traj_raw <- if (file.exists(TRAJ_ARQ))
  tryCatch(readRDS(TRAJ_ARQ), error = function(e) NULL) else NULL

TRAJ_REAL <- !is.null(traj_raw) && is.data.frame(traj_raw) && nrow(traj_raw) > 0 &&
  all(c("cid_entrada", "cid_obito", "regiao", "ano") %in% names(traj_raw))

# Estrutura vazia com o esquema correto: os painéis renderizam o estado
# "sem dados" sem quebrar, e nenhum valor fictício entra no lugar.
traj_vazia <- function() {
  d <- data.frame(ano = integer(0), regiao = character(0), uf = character(0),
                  faixa = character(0), sexo = character(0), raca_cor = character(0),
                  cid_entrada = character(0), cid_obito = character(0),
                  cid_secundario = character(0), desfecho = character(0),
                  n_transferencias = integer(0), dias_internacao = integer(0),
                  linkado = logical(0), stringsAsFactors = FALSE)
  d
}

traj <- if (TRAJ_REAL) traj_raw else traj_vazia()
for (cc in TRAJ_COLS) if (!cc %in% names(traj)) traj[[cc]] <- NA

# ---- Enriquecimento: sistema, grupo e especificidade nas duas pontas -------
enriquece_traj <- function(df) {
  if (nrow(df) == 0) {
    for (cc in c("sis_entrada","sis_obito","grp_entrada","grp_obito",
                 "nm_entrada","nm_obito","transicao"))
      df[[cc]] <- character(0)
    df$gen_entrada <- integer(0); df$gen_obito <- integer(0)
    df$mudou_cid <- logical(0);   df$mudou_sis <- logical(0)
    return(df)
  }
  ce <- cid_classify(df$cid_entrada)
  co <- cid_classify(df$cid_obito)
  df$sis_entrada <- ce$sistema;  df$sis_obito <- co$sistema
  df$grp_entrada <- ce$grupo;    df$grp_obito <- co$grupo
  df$gen_entrada <- ce$generico; df$gen_obito <- co$generico
  df$nm_entrada  <- ce$nome;     df$nm_obito  <- co$nome
  df$cid_entrada <- ce$cid;      df$cid_obito <- co$cid

  mudou_cid <- !is.na(df$cid_obito) & df$cid_entrada != df$cid_obito
  mudou_sis <- !is.na(df$sis_obito) & df$sis_entrada != df$sis_obito

  df$transicao <- dplyr::case_when(
    is.na(df$cid_obito)                        ~ "Sem óbito registrado",
    !mudou_cid & df$gen_obito == 1             ~ "Mesmo CID, genérico (causa nunca esclarecida)",
    !mudou_cid                                 ~ "Mesmo CID, específico",
    df$gen_entrada == 1 & df$gen_obito == 1    ~ "Genérico → outro genérico (causa nunca esclarecida)",
    df$gen_entrada == 1 & df$gen_obito == 0    ~ "Genérico → Específico (ganho diagnóstico)",
    df$gen_entrada == 0 & df$gen_obito == 1    ~ "Específico → Genérico (perda diagnóstica)",
    mudou_sis                                  ~ "Específico → Específico, outro sistema",
    TRUE                                       ~ "Específico → Específico, mesmo sistema")
  df$mudou_cid <- mudou_cid
  df$mudou_sis <- mudou_sis
  df
}
traj <- enriquece_traj(traj)

# Cobertura efetiva do linkage carregado — exibida na aba Metodologia.
TRAJ_INFO <- if (TRAJ_REAL) {
  list(n = nrow(traj),
       anos = paste0(min(traj$ano, na.rm = TRUE), "–", max(traj$ano, na.rm = TRUE)),
       ufs = length(unique(traj$uf[!is.na(traj$uf)])),
       linkados = sum(!is.na(traj$cid_obito)))
} else NULL

# ---- v3 · FLUXO EM 5 ETAPAS (proposta de linkage SIM ↔ SIH) ----------------
# Colunas novas gravadas por 00_link_sih_sim_v3.R: metodo (Exato /
# Probabilístico), escore, dist_data, teve_prev_30/45/60, gap_prev,
# desfecho_prev, cid_prev. Sem elas, os painéis novos mostram a instrução de
# rodar o script — nenhum número é estimado.
LINK_V3 <- TRAJ_REAL && all(c("metodo", "teve_prev_30") %in% names(traj))
for (cc in c("metodo", "desfecho_prev", "cid_prev"))
  if (!cc %in% names(traj)) traj[[cc]] <- rep(NA_character_, nrow(traj))
for (cc in c("teve_prev_30", "teve_prev_45", "teve_prev_60"))
  if (!cc %in% names(traj)) traj[[cc]] <- rep(NA, nrow(traj))
if (!"escore"   %in% names(traj)) traj$escore   <- rep(NA_integer_, nrow(traj))
if (!"gap_prev" %in% names(traj)) traj$gap_prev <- rep(NA_integer_, nrow(traj))

QUAL_ARQ <- "linkage_qualidade.rds"
qual <- if (file.exists(QUAL_ARQ))
  tryCatch(readRDS(QUAL_ARQ), error = function(e) NULL) else NULL
QUAL_REAL <- is.list(qual) && !is.null(qual$funil) && nrow(qual$funil) > 0

BAIXA_ARQ <- "sih_nao_obito_agregado.rds"
baixa_raw <- if (file.exists(BAIXA_ARQ))
  tryCatch(readRDS(BAIXA_ARQ), error = function(e) NULL) else NULL
BAIXA_REAL <- is.data.frame(baixa_raw) && nrow(baixa_raw) > 0 &&
  all(c("ano", "uf", "sistema", "n") %in% names(baixa_raw))

TRANS_NIVEIS <- c(
  "Mesmo CID, específico",
  "Específico → Específico, mesmo sistema",
  "Específico → Específico, outro sistema",
  "Genérico → Específico (ganho diagnóstico)",
  "Específico → Genérico (perda diagnóstica)",
  "Mesmo CID, genérico (causa nunca esclarecida)",
  "Genérico → outro genérico (causa nunca esclarecida)")

TRANS_COR <- c(
  "Mesmo CID, específico"                                = "#1E7B4F",
  "Específico → Específico, mesmo sistema"               = "#00A3A1",
  "Específico → Específico, outro sistema"               = "#004B87",
  "Genérico → Específico (ganho diagnóstico)"            = "#F4A261",
  "Específico → Genérico (perda diagnóstica)"            = "#C0392B",
  "Mesmo CID, genérico (causa nunca esclarecida)"        = "#8C95A3",
  "Genérico → outro genérico (causa nunca esclarecida)"  = "#5D6D7E")

# As duas últimas categorias juntas respondem "a causa nunca foi esclarecida".
TRANS_NUNCA_ESCLARECIDA <- c(
  "Mesmo CID, genérico (causa nunca esclarecida)",
  "Genérico → outro genérico (causa nunca esclarecida)")

REG_ORDEM <- c("Sul", "Sudeste", "Centro-Oeste", "Nordeste", "Norte")

# ----------------------------------------------------------------------------
# 2. CAMADA ESPACIAL (sf) + CHAVES DE JUNÇÃO ROBUSTAS
# ----------------------------------------------------------------------------
uf_sf <- tryCatch(readRDS("uf_sf_simplified.rds"), error = function(e) NULL)

nome2sigla <- c(
  "Acre"="AC","Alagoas"="AL","Amapá"="AP","Amazonas"="AM","Bahia"="BA",
  "Ceará"="CE","Distrito Federal"="DF","Espírito Santo"="ES","Goiás"="GO",
  "Maranhão"="MA","Mato Grosso"="MT","Mato Grosso do Sul"="MS","Minas Gerais"="MG",
  "Paraná"="PR","Paraíba"="PB","Pará"="PA","Pernambuco"="PE","Piauí"="PI",
  "Rio Grande do Norte"="RN","Rio Grande do Sul"="RS","Rio de Janeiro"="RJ",
  "Rondônia"="RO","Roraima"="RR","Santa Catarina"="SC","Sergipe"="SE",
  "São Paulo"="SP","Tocantins"="TO")

strip_accents <- function(x) {
  y <- iconv(x, to = "ASCII//TRANSLIT")
  y[is.na(y)] <- x[is.na(y)]
  gsub("[^A-Za-z]", "", y)
}
norm_key <- function(x) toupper(strip_accents(x))

map_ready <- !is.null(uf_sf) &&
  all(c("abbrev_state", "name_region") %in% names(uf_sf))

if (map_ready) uf_sf <- uf_sf %>% mutate(reg_key = norm_key(name_region))

join_uf_map <- function(taxa_uf_df, tot_col) {
  d <- taxa_uf_df %>%
    mutate(SIGLA = unname(nome2sigla[NOME_ESTADO]), total = .data[[tot_col]])
  uf_sf %>%
    left_join(d %>% select(SIGLA, NOME_ESTADO, total, nascidos_vivos, taxa = taxa_estado),
              by = c("abbrev_state" = "SIGLA"))
}

join_macro_map <- function(taxa_macro_df, tot_col) {
  d <- taxa_macro_df %>%
    mutate(reg_key = norm_key(MACRORREGIAO), total = .data[[tot_col]])
  uf_sf %>%
    left_join(d %>% select(reg_key, MACRORREGIAO, total, nascidos_vivos, taxa = taxa_macro),
              by = "reg_key")
}

# ---- Taxas da coorte 0–6 anos com o denominador correto -------------------
# Regra da ata de 27/07: nascidos vivos só valem como denominador ATÉ 1 ANO.
# As tabelas 7 e 8 cobrem a coorte inteira de 0 a 6, então a taxa passa a ser
# por 1.000 crianças de 0 a 6 anos (média anual) sempre que a população oficial
# estiver carregada. O numerador é acumulado no período, então o denominador
# também usa o período completo — daí o selo "período fixo" nesses cards.
taxa_uf_coorte <- function(fonte = "mort") {
  base <- if (identical(fonte, "int")) data_int$taxa_uf else data_mort$taxa_uf
  if (is.null(base)) return(NULL)
  tot <- if (identical(fonte, "int")) col_tot_int else col_tot_mort
  d <- data.frame(NOME_ESTADO = base$NOME_ESTADO,
                  sigla = unname(nome2sigla[base$NOME_ESTADO]),
                  total = base[[tot]],
                  nascidos_vivos = base$nascidos_vivos,
                  stringsAsFactors = FALSE)
  ag <- pa_0a6_periodo(ano_min, ano_max)
  if (!is.null(ag)) {
    d$denom   <- ag$pa[match(d$sigla, ag$sigla)]
    d$unidade <- "por 1.000 crianças de 0 a 6 anos (média anual)"
    d$oficial <- TRUE
  } else {
    d$denom   <- d$nascidos_vivos
    d$unidade <- "por 1.000 NV — denominador provisório, carregue a população"
    d$oficial <- FALSE
  }
  d$taxa <- d$total / d$denom * 1000
  d
}

taxa_macro_coorte <- function(fonte = "mort") {
  base <- if (identical(fonte, "int")) data_int$taxa_macro else data_mort$taxa_macro
  if (is.null(base)) return(NULL)
  tot <- if (identical(fonte, "int")) col_tot_int else col_tot_mort
  d <- data.frame(MACRORREGIAO = base$MACRORREGIAO,
                  total = base[[tot]],
                  nascidos_vivos = base$nascidos_vivos,
                  stringsAsFactors = FALSE)
  ag <- pa_0a6_periodo(ano_min, ano_max)
  if (!is.null(ag)) {
    ag  <- merge(ag, denom_uf[, c("sigla", "regiao")], by = "sigla")
    agr <- stats::aggregate(pa ~ regiao, data = ag, FUN = sum)
    d$denom   <- agr$pa[match(d$MACRORREGIAO, agr$regiao)]
    d$unidade <- "por 1.000 crianças de 0 a 6 anos (média anual)"
    d$oficial <- TRUE
  } else {
    d$denom   <- d$nascidos_vivos
    d$unidade <- "por 1.000 NV — denominador provisório, carregue a população"
    d$oficial <- FALSE
  }
  d$taxa <- d$total / d$denom * 1000
  d
}

join_uf_coorte <- function(d) {
  uf_sf %>% left_join(d[, c("sigla", "NOME_ESTADO", "total", "denom", "taxa")],
                      by = c("abbrev_state" = "sigla"))
}
join_macro_coorte <- function(d) {
  d$reg_key <- norm_key(d$MACRORREGIAO)
  uf_sf %>% left_join(d[, c("reg_key", "MACRORREGIAO", "total", "denom", "taxa")],
                      by = "reg_key")
}

# ----------------------------------------------------------------------------
# 3. HELPERS DE FORMATAÇÃO E PLOTAGEM
# ----------------------------------------------------------------------------
fmt_br  <- function(x, d = 0) {
  if (length(x) == 0 || all(is.na(x))) return("—")
  format(round(x, d), big.mark = ".", decimal.mark = ",", nsmall = d, scientific = FALSE)
}
fmt_pct <- function(x, d = 1) paste0(fmt_br(100 * x, d), "%")

empty_plot <- function(msg = "Dados indisponíveis") {
  plot_ly() %>%
    layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
           annotations = list(text = msg, showarrow = FALSE,
                              font = list(size = 15, color = pal$gray_2)),
           plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)") %>%
    config(displayModeBar = FALSE)
}

clean_plotly <- function(p, legend_pos = "bottom") {
  lp <- switch(legend_pos,
               "bottom" = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2),
               "right"  = list(orientation = "v", x = 1.02, y = 0.5),
               "none"   = list(visible = FALSE))
  p %>% layout(
    plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
    font = list(family = "Inter, system-ui, sans-serif", color = pal$gray_1, size = 13),
    xaxis = list(showgrid = FALSE, zeroline = FALSE),
    yaxis = list(gridcolor = "#EEF1F6", zeroline = FALSE),
    legend = lp, margin = list(l = 50, r = 20, t = 30, b = 60),
    hoverlabel = list(bgcolor = "white", font = list(family = "Inter", size = 13))
  ) %>% config(displaylogo = FALSE,
               modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
}

theme_map <- function() {
  theme_void(base_family = "sans") +
    theme(plot.title    = element_text(face = "bold", size = 15, hjust = .5, color = pal$dark),
          plot.subtitle = element_text(size = 11, hjust = .5, color = pal$gray_2,
                                       margin = margin(b = 6)),
          legend.position = "bottom",
          legend.title  = element_text(size = 10, color = pal$gray_1),
          legend.text   = element_text(size = 9,  color = pal$gray_1),
          plot.margin   = margin(4, 4, 4, 4),
          plot.background  = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA))
}

gg_choropleth <- function(sf_obj, lo, hi, title, sub, leg = "por 1.000 NV") {
  ggplot(sf_obj) +
    geom_sf(aes(fill = taxa), color = "white", linewidth = .18) +
    coord_sf(expand = FALSE, datum = NA) +
    scale_fill_gradient(low = lo, high = hi, na.value = "grey90", name = leg,
                        labels = label_number(accuracy = 1),
                        guide = guide_colorbar(direction = "horizontal",
                                               title.position = "top", title.hjust = .5,
                                               barwidth = 16, barheight = .6)) +
    labs(title = title, subtitle = sub) + theme_map()
}

# Selo padronizado para cards cuja tabela de origem não tem dimensão anual.
selo_periodo <- function(txt = NULL) {
  tags$span(class = "periodo-badge", bs_icon("calendar-range"), " ",
            txt %||% paste0("Período fixo ", PERIODO_FIXO, " · tabela sem dimensão anual"))
}
selo_ok <- function() {
  tags$span(class = "periodo-badge ok", bs_icon("calendar-check"), " Responde ao filtro de período")
}
# Estado "sem dados" dos painéis que dependem do linkage SIH↔SIM.
# Não existe base substituta: ou o arquivo real está presente, ou o painel
# fica vazio com a instrução de como gerá-lo.
banner_sem_dados <- function() {
  if (TRAJ_REAL) return(NULL)
  div(class = "nodata-banner",
      bs_icon("database-exclamation"), tags$b(" SEM DADOS CARREGADOS. "),
      "Este painel depende de ", tags$code(TRAJ_ARQ),
      ", que não está incluído nesta publicação. Os gráficos abaixo ficam vazios: ",
      tags$b("nenhum número é estimado ou simulado."))
}

selo_linkage_ok <- function() {
  if (!TRAJ_REAL) return(NULL)
  div(class = "ok-banner",
      bs_icon("database-check"), tags$b(" Linkage real carregado. "),
      fmt_br(TRAJ_INFO$n), " episódios hospitalares · ", TRAJ_INFO$anos, " · ",
      TRAJ_INFO$ufs, " UF · ", fmt_br(TRAJ_INFO$linkados), " pareados com o SIM.")
}

MSG_SEM_V3 <- "Dados do linkage em 5 etapas indisponíveis nesta publicação"
banner_sem_v3 <- function(o_que = c("link", "qual", "baixa")) {
  falta <- switch(o_que[1],
                  qual  = !QUAL_REAL,
                  baixa = !BAIXA_REAL,
                  !LINK_V3)
  if (!falta) return(NULL)
  div(class = "nodata-banner",
      bs_icon("database-exclamation"), tags$b(" SEM DADOS DO FLUXO v3. "),
      "Este painel depende das saídas de ", tags$code("00_link_sih_sim_v3.R"),
      " (fluxo em 5 etapas: alvo no SIM → linkage exato + probabilístico → ",
      "histórico de internações), que não está incluído nesta publicação. ",
      tags$b("Nenhum número é estimado ou simulado."))
}

MSG_SEM_LINKAGE <- paste0("Sem dados: gere ", TRAJ_ARQ, " com 00_link_sih_sim_v3.R")
MSG_SEM_POP <- paste0("Sem dados: gere ", POP_ARQ, " com 01_baixar_populacao.R")

# ----------------------------------------------------------------------------
# 3b. HELPERS — CID DETALHADO, CAPÍTULOS, CONCENTRAÇÃO E METAS ODS/IPEA
# ----------------------------------------------------------------------------
pal_grupo <- c(
  "Causas Externas"                  = "#D9534F",
  "Neoplasias (Oncologia)"           = "#8E44AD",
  "Doenças do Sistema Nervoso"       = "#00A3A1",
  "Doenças Metabólicas/Genéticas"    = "#F4A261",
  "Outras Causas"                    = "#6C757D",
  "Doenças Infecciosas"              = "#2E86C1",
  "Aparelho Respiratório"            = "#16A085",
  "Cardiopatias Congênitas"          = "#C0392B",
  "Malformações Congênitas (outras)" = "#E67E22",
  "Afecções Perinatais"              = "#004B87")

cor_prio <- c(
  "Ação Prioritária (Prevenção/Tratamento)" = pal$alert,
  "Atenção à Gestação e Parto"              = pal$primary,
  "Malformações (Alta Complexidade)"        = pal$accent,
  "Outras Causas / Difícil Prevenção"       = pal$gray_2)

# Nomenclatura revisada aplicada às tabelas pré-computadas.
sim_prio_cid <- aplica_revisao_cid(sim_prio_cid)
sih_prio_cid <- aplica_revisao_cid(sih_prio_cid)
sim_conc     <- aplica_revisao_cid(sim_conc)
sih_conc     <- aplica_revisao_cid(sih_conc)

tab_prio_cid <- function(fonte) if (identical(fonte, "int")) sih_prio_cid else sim_prio_cid
tab_capitulo <- function(fonte) if (identical(fonte, "int")) sih_capitulo else sim_capitulo
tab_conc     <- function(fonte) if (identical(fonte, "int")) sih_conc     else sim_conc
tab_agrup    <- function(fonte) if (identical(fonte, "int")) sih_agrup    else sim_agrup
FONTE_LBL <- c(mort = "óbitos", int = "internações")
FONTE_TIT <- c(mort = "Óbitos", int = "Internações")

# Barra horizontal dos CIDs nomeados de uma causa numa faixa -----------------
plot_cid_bar <- function(faixa_lbl, prio_lbl, fonte = "mort", so_genericos = FALSE,
                         base = NULL) {
  if (is.null(base)) base <- tab_prio_cid(fonte)
  d <- base[base$faixa == faixa_lbl & base$prio == prio_lbl, ]
  if (so_genericos) d <- d[d$generico == 1, ]
  if (nrow(d) == 0) return(empty_plot("Sem CIDs para esta combinação"))
  d <- d[order(d$pct), ]
  d$rotulo <- paste0(d$cid, " · ", d$nome)
  d$rotulo <- factor(d$rotulo, levels = d$rotulo)
  cor <- unname(cor_prio[prio_lbl]); if (is.na(cor)) cor <- pal$primary
  cores <- ifelse(d$generico == 1, pal$gray_2, cor)
  plot_ly(d, x = ~pct, y = ~rotulo, type = "bar", orientation = "h",
          marker = list(color = cores, line = list(color = "white", width = 1)),
          text = ~paste0(format(pct, decimal.mark = ",", nsmall = 1), "%"),
          textposition = "outside", textfont = list(size = 11, color = pal$dark),
          hovertext = ~paste0("<b>", cid, " · ", nome, "</b><br>",
                              format(pct, decimal.mark = ",", nsmall = 1), "% da causa<br>",
                              FONTE_TIT[fonte], ": ", fmt_br(n), "<br>",
                              "Sistema: ", sistema, "<br>Grupo: ", grupo_pat,
                              "<br>Especificidade: ", especificidade),
          hoverinfo = "text") %>%
    layout(xaxis = list(title = "% dentro da causa", ticksuffix = "%",
                        range = c(0, max(d$pct, na.rm = TRUE) * 1.22)),
           yaxis = list(title = "", automargin = TRUE, tickfont = list(size = 11))) %>%
    clean_plotly(legend_pos = "none")
}

plot_agrup_outras <- function(faixa_lbl, fonte = "mort") {
  base <- tab_agrup(fonte)
  d <- base[base$faixa == faixa_lbl, ]
  if (nrow(d) == 0) return(empty_plot("Sem dados"))
  d <- d[order(d$pct), ]
  d$grupo <- factor(d$grupo, levels = d$grupo)
  cores <- unname(pal_grupo[as.character(d$grupo)]); cores[is.na(cores)] <- pal$gray_2
  plot_ly(d, x = ~pct, y = ~grupo, type = "bar", orientation = "h",
          marker = list(color = cores, line = list(color = "white", width = 1)),
          text = ~paste0(format(pct, decimal.mark = ",", nsmall = 1), "%"),
          textposition = "outside", textfont = list(size = 11, color = pal$dark),
          hovertemplate = paste0("<b>%{y}</b><br>%{x:.1f}% das outras causas<br>",
                                 FONTE_TIT[fonte], ": %{customdata:,.0f}<extra></extra>"),
          customdata = ~n) %>%
    layout(xaxis = list(title = "% das \"Outras Causas / Difícil Prevenção\"", ticksuffix = "%",
                        range = c(0, max(d$pct, na.rm = TRUE) * 1.22)),
           yaxis = list(title = "", automargin = TRUE)) %>%
    clean_plotly(legend_pos = "none")
}

plot_capitulo <- function(faixa_lbl, fonte = "mort", topn = 10) {
  base <- tab_capitulo(fonte)
  d <- base[base$faixa == faixa_lbl, ]
  if (nrow(d) == 0) return(empty_plot("Sem dados"))
  d <- d[order(-d$pct), ]; if (nrow(d) > topn) d <- d[seq_len(topn), ]
  d <- d[order(d$pct), ]
  d$lbl <- factor(paste0("Cap. ", d$cap, " · ", d$nome),
                  levels = paste0("Cap. ", d$cap, " · ", d$nome))
  paleta <- colorRampPalette(c(pal$gray_3, pal$secondary, pal$primary))(nrow(d))
  plot_ly(d, x = ~pct, y = ~lbl, type = "bar", orientation = "h",
          marker = list(color = paleta[rank(d$pct)], line = list(color = "white", width = 1)),
          text = ~paste0(format(pct, decimal.mark = ",", nsmall = 1), "%"),
          textposition = "outside", textfont = list(size = 11, color = pal$dark),
          hovertemplate = paste0("<b>%{y}</b><br>%{x:.1f}%<br>",
                                 FONTE_TIT[fonte], ": %{customdata:,.0f}<extra></extra>"),
          customdata = ~n) %>%
    layout(xaxis = list(title = paste0("% dos ", FONTE_LBL[fonte]), ticksuffix = "%",
                        range = c(0, max(d$pct, na.rm = TRUE) * 1.2)),
           yaxis = list(title = "", automargin = TRUE, tickfont = list(size = 11))) %>%
    clean_plotly(legend_pos = "none")
}

plot_concentracao <- function(escopo_lbl, fonte = "mort", base = NULL) {
  if (is.null(base)) base <- tab_conc(fonte)
  d <- base[base$escopo == escopo_lbl, ]
  if (nrow(d) == 0) return(empty_plot("Sem dados"))
  d$lbl <- factor(paste0(d$cid, " · ", d$nome), levels = paste0(d$cid, " · ", d$nome))
  cores <- ifelse(d$generico == 1, pal$gray_2, pal$primary)
  plot_ly(d) %>%
    add_bars(x = ~lbl, y = ~pct, marker = list(color = cores), name = "% do total",
             hovertext = ~paste0("<b>", cid, " · ", nome, "</b><br>",
                                 format(pct, decimal.mark = ",", nsmall = 1), "% do total<br>",
                                 "Acumulado: ", format(cum, decimal.mark = ",", nsmall = 1), "%<br>",
                                 "Sistema: ", sistema, "<br>", especificidade),
             hoverinfo = "text") %>%
    add_trace(x = ~lbl, y = ~cum, type = "scatter", mode = "lines+markers", yaxis = "y2",
              line = list(color = pal$alert, width = 3), marker = list(color = pal$alert, size = 7),
              name = "% acumulado",
              hovertemplate = "Acumulado: %{y:.1f}%<extra></extra>") %>%
    layout(xaxis = list(title = "", tickangle = -40, tickfont = list(size = 10)),
           yaxis = list(title = "% do total", ticksuffix = "%", gridcolor = "#EEF1F6"),
           yaxis2 = list(title = "% acumulado", overlaying = "y", side = "right",
                         range = c(0, 100), ticksuffix = "%", showgrid = FALSE),
           legend = list(orientation = "h", x = .5, xanchor = "center", y = -.35),
           plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
           font = list(family = "Inter", color = pal$gray_1, size = 12),
           annotations = list(list(x = 1, y = 1.06, xref = "paper", yref = "paper",
                                   xanchor = "right", showarrow = FALSE,
                                   text = "Barras cinza = código genérico / mal definido",
                                   font = list(size = 11, color = pal$gray_2))),
           margin = list(l = 55, r = 60, t = 34, b = 130)) %>%
    config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d","select2d","autoScale2d"))
}

STATUS_LBL <- c(ipea = "Atinge meta IPEA (mais rígida)",
                sdg  = "Atinge meta SDG",
                nao  = "Não atinge a meta SDG")
STATUS_COR <- c("Atinge meta IPEA (mais rígida)" = "#1E7B4F",
                "Atinge meta SDG"                 = "#F4A261",
                "Não atinge a meta SDG"           = "#D9534F")
meta_status <- function(df, ind) {
  s_ipea <- df[[paste0("ipea_", ind)]] == 1
  s_sdg  <- df[[paste0("sdg_",  ind)]] == 1
  ifelse(s_ipea, STATUS_LBL["ipea"], ifelse(s_sdg, STATUS_LBL["sdg"], STATUS_LBL["nao"]))
}

# Taxa média do período por UF a partir da série anual, respeitando o filtro.
uf_rate_periodo <- function(ind, y0, y1) {
  d <- sim_ufano[sim_ufano$ano >= y0 & sim_ufano$ano <= y1, ]
  if (nrow(d) == 0) return(sim_uf[, c("sigla", "uf", "regiao", ind)] %>%
                             setNames(c("sigla", "uf", "regiao", "val")))
  ag <- d %>% group_by(sigla) %>% summarise(val = mean(.data[[ind]], na.rm = TRUE), .groups = "drop")
  sim_uf %>% select(sigla, uf, regiao) %>% left_join(ag, by = "sigla")
}

plot_estados_meta <- function(ind, y0, y1) {
  d <- uf_rate_periodo(ind, y0, y1)
  ref <- sim_uf
  d$sdg  <- as.integer(d$val <= META_SDG[[ind]])
  d$ipea <- as.integer(d$val <= META_IPEA[[ind]])
  d$status <- ifelse(d$ipea == 1, STATUS_LBL["ipea"],
                     ifelse(d$sdg == 1, STATUS_LBL["sdg"], STATUS_LBL["nao"]))
  d <- d[order(d$val), ]
  d$sigla <- factor(d$sigla, levels = d$sigla)
  msdg <- unname(META_SDG[ind]); mipea <- unname(META_IPEA[ind])
  xmax <- max(d$val, msdg, na.rm = TRUE) * 1.12
  plot_ly(d, x = ~val, y = ~sigla, type = "bar", orientation = "h",
          marker = list(color = unname(STATUS_COR[d$status]),
                        line = list(color = "white", width = .6)),
          text = ~format(round(val, 1), decimal.mark = ",", nsmall = 1),
          textposition = "outside", textfont = list(size = 10, color = pal$dark),
          hovertext = ~paste0(uf, "<br>", format(round(val, 1), decimal.mark = ",", nsmall = 1),
                              " / 1.000 NV<br>", status, "<br>Média ", y0, "–", y1),
          hoverinfo = "text") %>%
    layout(xaxis = list(title = paste0(IND_LABEL[ind], " — óbitos por 1.000 NV (média ", y0, "–", y1, ")"),
                        gridcolor = "#EEF1F6", zeroline = FALSE, range = c(0, xmax)),
           yaxis = list(title = "", tickfont = list(size = 10), automargin = TRUE),
           shapes = list(
             list(type = "line", x0 = msdg,  x1 = msdg,  y0 = -.5, y1 = nrow(d) - .5,
                  line = list(color = pal$dark, width = 2, dash = "dash")),
             list(type = "line", x0 = mipea, x1 = mipea, y0 = -.5, y1 = nrow(d) - .5,
                  line = list(color = pal$green, width = 2, dash = "dot"))),
           annotations = list(
             list(x = msdg,  y = nrow(d) - .5, yanchor = "bottom", xanchor = "left",
                  text = paste0("Meta SDG ", format(msdg, decimal.mark = ",")),
                  showarrow = FALSE, font = list(size = 11, color = pal$dark)),
             list(x = mipea, y = nrow(d) - .5, yanchor = "bottom", xanchor = "right",
                  text = paste0("Meta IPEA ", format(mipea, decimal.mark = ",")),
                  showarrow = FALSE, font = list(size = 11, color = pal$green))),
           plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
           font = list(family = "Inter", color = pal$gray_1, size = 12),
           showlegend = FALSE, margin = list(l = 55, r = 55, t = 20, b = 55)) %>%
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = c("lasso2d","select2d","autoScale2d","zoom2d","pan2d"))
}

plot_evolucao_meta <- function(ind, siglas, y0, y1) {
  msdg <- unname(META_SDG[ind]); mipea <- unname(META_IPEA[ind])
  nac <- sim_nacano[sim_nacano$ano >= y0 & sim_nacano$ano <= y1, ]
  if (nrow(nac) == 0) return(empty_plot("Período sem dados"))
  p <- plot_ly() %>%
    add_trace(data = nac, x = ~ano, y = nac[[ind]], type = "scatter", mode = "lines",
              name = "Brasil", line = list(color = pal$dark, width = 3),
              hovertemplate = "Brasil %{x}: %{y:.1f}<extra></extra>")
  if (length(siglas) > 0) {
    cores <- colorRampPalette(c(pal$primary, pal$secondary, pal$accent, pal$alert))(length(siglas))
    for (i in seq_along(siglas)) {
      dd <- sim_ufano[sim_ufano$sigla == siglas[i] & sim_ufano$ano >= y0 & sim_ufano$ano <= y1, ]
      if (nrow(dd) == 0) next
      p <- p %>% add_trace(data = dd, x = ~ano, y = dd[[ind]], type = "scatter",
                           mode = "lines+markers", name = siglas[i],
                           line = list(color = cores[i], width = 2.4),
                           marker = list(color = cores[i], size = 6),
                           hovertemplate = paste0(siglas[i], " %{x}: %{y:.1f}<extra></extra>"))
    }
  }
  p %>% layout(
    xaxis = list(title = "", dtick = 1, showgrid = FALSE),
    yaxis = list(title = paste0(IND_LABEL[ind], " / 1.000 NV"), gridcolor = "#EEF1F6",
                 rangemode = "tozero"),
    shapes = list(
      list(type = "line", x0 = min(nac$ano), x1 = max(nac$ano), y0 = msdg,  y1 = msdg,
           line = list(color = pal$alert, width = 2, dash = "dash")),
      list(type = "line", x0 = min(nac$ano), x1 = max(nac$ano), y0 = mipea, y1 = mipea,
           line = list(color = pal$green, width = 2, dash = "dot"))),
    annotations = list(
      list(x = max(nac$ano), y = msdg, xanchor = "right", yanchor = "bottom",
           text = paste0("Meta SDG ", format(msdg, decimal.mark = ",")),
           showarrow = FALSE, font = list(color = pal$alert, size = 11)),
      list(x = max(nac$ano), y = mipea, xanchor = "right", yanchor = "bottom",
           text = paste0("Meta IPEA ", format(mipea, decimal.mark = ",")),
           showarrow = FALSE, font = list(color = pal$green, size = 11))),
    plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
    font = list(family = "Inter", color = pal$gray_1, size = 13),
    legend = list(orientation = "h", x = .5, xanchor = "center", y = -.16),
    margin = list(l = 55, r = 30, t = 20, b = 55)) %>%
    config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d","select2d","autoScale2d"))
}

map_macro_meta <- function(ind) {
  if (!map_ready) return(NULL)
  dm <- sim_macro
  dm$status  <- meta_status(dm, ind)
  dm$reg_key <- norm_key(dm$regiao)
  sfm <- uf_sf %>% left_join(dm[, c("reg_key", "regiao", "status")], by = "reg_key")
  sfm$status <- factor(sfm$status, levels = names(STATUS_COR))
  ggplot(sfm) +
    geom_sf(aes(fill = status), color = "white", linewidth = .18) +
    coord_sf(expand = FALSE, datum = NA) +
    scale_fill_manual(values = STATUS_COR, na.value = "grey85", name = NULL, drop = FALSE) +
    labs(title = paste0(IND_LABEL[ind], " — atingimento das metas por macrorregião"),
         subtitle = paste0("Verde: meta IPEA · Laranja: meta SDG · Vermelho: acima da meta SDG · ",
                           PERIODO_FIXO)) +
    theme_map() + theme(legend.position = "bottom")
}

# ----------------------------------------------------------------------------
# 3c. HELPERS NOVOS — SISTEMAS, TRAJETÓRIA E TRANSIÇÃO DE CID
# ----------------------------------------------------------------------------
# Distribuição por sistema do organismo a partir das tabelas de CID nomeado.
tab_sistema <- function(fonte = "mort", faixa_lbl = NULL, por = c("sistema", "grupo_pat"),
                        base = NULL) {
  por  <- match.arg(por)
  if (is.null(base)) base <- tab_prio_cid(fonte)
  if (!is.null(faixa_lbl) && faixa_lbl != "Todas as faixas")
    base <- base[base$faixa == faixa_lbl, ]
  if (nrow(base) == 0) return(NULL)
  base %>%
    group_by(chave = .data[[por]]) %>%
    summarise(n = sum(n, na.rm = TRUE), k = dplyr::n(), .groups = "drop") %>%
    mutate(pct = 100 * n / sum(n)) %>%
    arrange(desc(n))
}

plot_sistema_bar <- function(fonte, faixa_lbl, por = "sistema", topn = 14, base = NULL) {
  d <- tab_sistema(fonte, faixa_lbl, por, base)
  if (is.null(d)) return(empty_plot("Sem dados para esta combinação"))
  if (nrow(d) > topn) d <- d[seq_len(topn), ]
  d <- d[order(d$pct), ]
  d$chave <- factor(d$chave, levels = d$chave)
  cores <- if (por == "sistema") cor_sistema(d$chave)
           else colorRampPalette(c(pal$gray_3, pal$secondary, pal$primary))(nrow(d))
  plot_ly(d, x = ~pct, y = ~chave, type = "bar", orientation = "h",
          marker = list(color = cores, line = list(color = "white", width = 1)),
          text = ~paste0(format(round(pct, 1), decimal.mark = ",", nsmall = 1), "%"),
          textposition = "outside", textfont = list(size = 11, color = pal$dark),
          hovertemplate = paste0("<b>%{y}</b><br>%{x:.1f}% dos ", FONTE_LBL[fonte],
                                 " classificados<br>Casos: %{customdata[0]:,.0f}",
                                 "<br>Códigos CID distintos: %{customdata[1]}<extra></extra>"),
          customdata = ~cbind(n, k)) %>%
    layout(xaxis = list(title = paste0("% dos ", FONTE_LBL[fonte], " com CID nomeado"),
                        ticksuffix = "%", range = c(0, max(d$pct, na.rm = TRUE) * 1.25)),
           yaxis = list(title = "", automargin = TRUE, tickfont = list(size = 11))) %>%
    clean_plotly(legend_pos = "none")
}

# --- Transição de CID entrada (SIH) → óbito (SIM) ---------------------------
traj_filtrada <- function(y0, y1, faixa = "Todas as faixas", regiao = "Brasil") {
  d <- traj[traj$ano >= y0 & traj$ano <= y1, ]
  if (!identical(faixa, "Todas as faixas")) d <- d[d$faixa == faixa, ]
  if (!identical(regiao, "Brasil"))         d <- d[d$regiao == regiao, ]
  d
}

resumo_transicao <- function(d) {
  d <- d[!is.na(d$cid_obito), ]
  if (nrow(d) == 0) return(NULL)
  d %>% group_by(transicao) %>% summarise(n = dplyr::n(), .groups = "drop") %>%
    mutate(transicao = factor(transicao, levels = TRANS_NIVEIS),
           pct = 100 * n / sum(n)) %>%
    arrange(transicao)
}

plot_transicao_barras <- function(d) {
  if (!TRAJ_REAL) return(empty_plot(MSG_SEM_LINKAGE))
  r <- resumo_transicao(d)
  if (is.null(r)) return(empty_plot("Nenhum par entrada→óbito no recorte"))
  r <- r[order(r$pct), ]
  r$transicao <- factor(as.character(r$transicao), levels = as.character(r$transicao))
  plot_ly(r, x = ~pct, y = ~transicao, type = "bar", orientation = "h",
          marker = list(color = unname(TRANS_COR[as.character(r$transicao)]),
                        line = list(color = "white", width = 1)),
          text = ~paste0(format(round(pct, 1), decimal.mark = ",", nsmall = 1), "%"),
          textposition = "outside", textfont = list(size = 11, color = pal$dark),
          hovertemplate = "<b>%{y}</b><br>%{x:.1f}% dos óbitos linkados<br>n = %{customdata:,.0f}<extra></extra>",
          customdata = ~n) %>%
    layout(xaxis = list(title = "% dos óbitos hospitalares linkados", ticksuffix = "%",
                        range = c(0, max(r$pct, na.rm = TRUE) * 1.3)),
           yaxis = list(title = "", automargin = TRUE, tickfont = list(size = 11))) %>%
    clean_plotly(legend_pos = "none")
}

# Δ entrada/óbito por região.
# Δ = % de óbitos hospitalares em que o CID de entrada difere do CID de óbito.
delta_por_regiao <- function(d) {
  d <- d[!is.na(d$cid_obito), ]
  if (nrow(d) == 0) return(NULL)
  d %>% group_by(regiao) %>%
    summarise(n = dplyr::n(),
              delta_cid  = 100 * mean(mudou_cid, na.rm = TRUE),
              delta_sis  = 100 * mean(mudou_sis, na.rm = TRUE),
              gen_gen    = 100 * mean(transicao %in% TRANS_NUNCA_ESCLARECIDA),
              perda      = 100 * mean(transicao == "Específico → Genérico (perda diagnóstica)"),
              ganho      = 100 * mean(transicao == "Genérico → Específico (ganho diagnóstico)"),
              .groups = "drop") %>%
    mutate(regiao = factor(regiao, levels = REG_ORDEM)) %>%
    arrange(regiao)
}

plot_delta_regiao <- function(d, metrica = "delta_cid") {
  if (!TRAJ_REAL) return(empty_plot(MSG_SEM_LINKAGE))
  r <- delta_por_regiao(d)
  if (is.null(r)) return(empty_plot("Sem pares entrada→óbito no recorte"))
  lbl <- c(delta_cid = "Δ CID entrada → óbito (troca de código)",
           delta_sis = "Δ sistema do organismo (troca de sistema)",
           gen_gen   = "Causa nunca esclarecida (genérico nas duas pontas)",
           perda     = "Específico → Genérico (perda diagnóstica)",
           ganho     = "Genérico → Específico (ganho diagnóstico)")[metrica]
  r$val <- r[[metrica]]
  cores <- colorRampPalette(c(pal$secondary, pal$primary, pal$alert))(nrow(r))
  plot_ly(r, x = ~regiao, y = ~val, type = "bar",
          marker = list(color = cores[rank(r$val)], line = list(color = "white", width = 1)),
          text = ~paste0(format(round(val, 1), decimal.mark = ",", nsmall = 1), "%"),
          textposition = "outside", textfont = list(size = 12, color = pal$dark),
          hovertemplate = "<b>%{x}</b><br>%{y:.1f}%<br>Óbitos linkados: %{customdata:,.0f}<extra></extra>",
          customdata = ~n) %>%
    layout(xaxis = list(title = "Local (região)", tickfont = list(size = 12)),
           yaxis = list(title = unname(lbl), ticksuffix = "%",
                        range = c(0, max(r$val, na.rm = TRUE) * 1.25)),
           margin = list(l = 60, r = 20, t = 30, b = 60)) %>%
    clean_plotly(legend_pos = "none")
}

# Sankey entrada → óbito, agregado por sistema do organismo.
plot_sankey_sistema <- function(d, minimo = 25) {
  if (!TRAJ_REAL) return(empty_plot(MSG_SEM_LINKAGE))
  d <- d[!is.na(d$cid_obito) & !is.na(d$sis_entrada) & !is.na(d$sis_obito), ]
  if (nrow(d) == 0) return(empty_plot("Sem pares entrada→óbito no recorte"))
  fl <- d %>% count(sis_entrada, sis_obito, name = "n") %>% filter(n >= minimo)
  if (nrow(fl) == 0) return(empty_plot("Fluxos abaixo do mínimo exibível"))
  ent <- sort(unique(fl$sis_entrada)); sai <- sort(unique(fl$sis_obito))
  rot <- c(paste0(ent, "  (entrada)"), paste0(sai, "  (óbito)"))
  cor <- c(cor_sistema(ent), cor_sistema(sai))
  src <- match(fl$sis_entrada, ent) - 1
  tgt <- length(ent) + match(fl$sis_obito, sai) - 1
  rgb_ent  <- grDevices::col2rgb(cor_sistema(fl$sis_entrada))
  cor_link <- sprintf("rgba(%d,%d,%d,0.38)", rgb_ent[1, ], rgb_ent[2, ], rgb_ent[3, ])
  plot_ly(type = "sankey", arrangement = "snap",
          node = list(label = rot, color = cor, pad = 14, thickness = 16,
                      line = list(color = "white", width = .5)),
          link = list(source = src, target = tgt, value = fl$n, color = cor_link,
                      hovertemplate = "%{source.label} → %{target.label}<br>%{value:,.0f} óbitos<extra></extra>")) %>%
    layout(font = list(family = "Inter", size = 12, color = pal$gray_1),
           paper_bgcolor = "rgba(0,0,0,0)", margin = list(l = 10, r = 10, t = 10, b = 10)) %>%
    config(displaylogo = FALSE)
}

# Matriz entrada × óbito nos códigos mais frequentes.
plot_matriz_cid <- function(d, topn = 15) {
  if (!TRAJ_REAL) return(empty_plot(MSG_SEM_LINKAGE))
  d <- d[!is.na(d$cid_obito), ]
  if (nrow(d) == 0) return(empty_plot("Sem pares entrada→óbito no recorte"))
  te <- names(sort(table(d$cid_entrada), decreasing = TRUE))[seq_len(min(topn, length(unique(d$cid_entrada))))]
  to <- names(sort(table(d$cid_obito),   decreasing = TRUE))[seq_len(min(topn, length(unique(d$cid_obito))))]
  m <- d %>% filter(cid_entrada %in% te, cid_obito %in% to) %>%
    count(cid_entrada, cid_obito, name = "n") %>%
    tidyr::complete(cid_entrada = te, cid_obito = to, fill = list(n = 0))
  rot <- function(x) paste0(x, " · ", substr(cid_classify(x)$nome, 1, 32))
  m$le <- rot(m$cid_entrada); m$lo <- rot(m$cid_obito)
  plot_ly(m, x = ~lo, y = ~le, z = ~n, type = "heatmap",
          colorscale = list(list(0, "#F7FBFF"), list(.5, "#6BAED6"), list(1, "#08306B")),
          hovertemplate = "Entrada <b>%{y}</b><br>Óbito <b>%{x}</b><br>%{z:,.0f} casos<extra></extra>",
          colorbar = list(title = "Casos", thickness = 12)) %>%
    layout(xaxis = list(title = "CID do óbito (SIM)", tickangle = -45,
                        tickfont = list(size = 10), showgrid = FALSE),
           yaxis = list(title = "CID de entrada (SIH)", tickfont = list(size = 10), showgrid = FALSE),
           plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
           font = list(family = "Inter", color = pal$gray_1, size = 12),
           margin = list(l = 200, r = 20, t = 20, b = 160)) %>%
    config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d","select2d","autoScale2d"))
}

# Trajetória T0 → T1 → T2 (funil de desfechos).
plot_trajetoria <- function(d) {
  if (!TRAJ_REAL) return(empty_plot(MSG_SEM_LINKAGE))
  if (nrow(d) == 0) return(empty_plot("Sem registros no recorte"))
  t0 <- nrow(d)
  t1 <- sum(d$n_transferencias > 0, na.rm = TRUE)
  ob <- sum(d$desfecho == "Óbito", na.rm = TRUE)
  tr <- sum(d$desfecho == "Transferência", na.rm = TRUE)
  lk <- sum(!is.na(d$cid_obito))
  et <- data.frame(
    etapa = factor(c("T₀ · entrada hospitalar (SIH)",
                     "T₁ · seguimento com transferência",
                     "T₂ · desfecho óbito",
                     "T₂ · desfecho transferência",
                     "Linkado ao SIM (CID de óbito)"),
                   levels = rev(c("T₀ · entrada hospitalar (SIH)",
                                  "T₁ · seguimento com transferência",
                                  "T₂ · desfecho óbito",
                                  "T₂ · desfecho transferência",
                                  "Linkado ao SIM (CID de óbito)"))),
    n = c(t0, t1, ob, tr, lk))
  et$pct <- 100 * et$n / t0
  cores <- c(pal$primary, pal$secondary, pal$alert, pal$accent, pal$purple)
  plot_ly(et, x = ~pct, y = ~etapa, type = "bar", orientation = "h",
          marker = list(color = rev(cores), line = list(color = "white", width = 1)),
          text = ~paste0(format(round(pct, 1), decimal.mark = ",", nsmall = 1), "%"),
          textposition = "outside", textfont = list(size = 12, color = pal$dark),
          hovertemplate = "<b>%{y}</b><br>%{x:.1f}% das internações<br>n = %{customdata:,.0f}<extra></extra>",
          customdata = ~n) %>%
    layout(xaxis = list(title = "% das internações de base (T₀ = 100%)", ticksuffix = "%",
                        range = c(0, 118)),
           yaxis = list(title = "", automargin = TRUE)) %>%
    clean_plotly(legend_pos = "none")
}

# Qualidade de preenchimento: % de campo ausente e % de CID genérico por região.
qualidade_regiao <- function(d) {
  if (nrow(d) == 0) return(NULL)
  d %>% group_by(regiao) %>%
    summarise(
      n = dplyr::n(),
      na_raca   = 100 * mean(is.na(raca_cor)),
      na_cidobt = 100 * mean(is.na(cid_obito) & desfecho == "Óbito"),
      gen_ent   = 100 * mean(gen_entrada == 1, na.rm = TRUE),
      gen_obt   = 100 * mean(gen_obito == 1, na.rm = TRUE),
      .groups = "drop") %>%
    mutate(regiao = factor(regiao, levels = REG_ORDEM)) %>% arrange(regiao)
}

plot_qualidade <- function(d) {
  if (!TRAJ_REAL) return(empty_plot(MSG_SEM_LINKAGE))
  q <- qualidade_regiao(d)
  if (is.null(q)) return(empty_plot("Sem registros no recorte"))
  ind <- data.frame(
    regiao = rep(q$regiao, 4),
    ind = rep(c("Raça/cor ausente", "CID de óbito ausente",
                "CID de entrada genérico", "CID de óbito genérico"), each = nrow(q)),
    val = c(q$na_raca, q$na_cidobt, q$gen_ent, q$gen_obt))
  cores <- c("Raça/cor ausente" = pal$gray_2, "CID de óbito ausente" = pal$accent,
             "CID de entrada genérico" = pal$secondary, "CID de óbito genérico" = pal$alert)
  plot_ly(ind, x = ~regiao, y = ~val, color = ~ind, colors = cores, type = "bar",
          hovertemplate = "<b>%{fullData.name}</b><br>%{x}: %{y:.1f}%<extra></extra>") %>%
    layout(barmode = "group", xaxis = list(title = ""),
           yaxis = list(title = "% dos registros", ticksuffix = "%")) %>%
    clean_plotly()
}

# Causas secundárias (diagnóstico secundário do SIH / linha adicional do SIM).
plot_secundarias <- function(d, topn = 15) {
  if (!TRAJ_REAL) return(empty_plot(MSG_SEM_LINKAGE))
  d <- d[!is.na(d$cid_secundario), ]
  if (nrow(d) == 0) return(empty_plot("Sem causa secundária registrada no recorte"))
  cl <- cid_classify(d$cid_secundario)
  r <- data.frame(cid = cl$cid, nome = cl$nome, sistema = cl$sistema,
                  gen = cl$generico, stringsAsFactors = FALSE) %>%
    count(cid, nome, sistema, gen, name = "n") %>%
    mutate(pct = 100 * n / sum(n)) %>% arrange(desc(n))
  if (nrow(r) > topn) r <- r[seq_len(topn), ]
  r <- r[order(r$pct), ]
  r$lbl <- factor(paste0(r$cid, " · ", r$nome), levels = paste0(r$cid, " · ", r$nome))
  plot_ly(r, x = ~pct, y = ~lbl, type = "bar", orientation = "h",
          marker = list(color = cor_sistema(r$sistema), line = list(color = "white", width = 1)),
          text = ~paste0(format(round(pct, 1), decimal.mark = ",", nsmall = 1), "%"),
          textposition = "outside", textfont = list(size = 11, color = pal$dark),
          hovertext = ~paste0("<b>", cid, " · ", nome, "</b><br>",
                              format(round(pct, 1), decimal.mark = ",", nsmall = 1),
                              "% das causas secundárias<br>n = ", fmt_br(n),
                              "<br>Sistema: ", sistema),
          hoverinfo = "text") %>%
    layout(xaxis = list(title = "% das causas secundárias registradas", ticksuffix = "%",
                        range = c(0, max(r$pct, na.rm = TRUE) * 1.25)),
           yaxis = list(title = "", automargin = TRUE, tickfont = list(size = 11))) %>%
    clean_plotly(legend_pos = "none")
}

# ----------------------------------------------------------------------------
# 3c. HELPERS — LINKAGE v3: QUALIDADE, DESCRITIVA, GUIA DE CIDs E ETAPA 5
# ----------------------------------------------------------------------------
PAL_METODO <- c("Exato" = "#004B87", "Probabilístico" = "#F4A261")

# ---- Reconciliação: duas trilhas, dois universos ---------------------------
# O SIM conta ÓBITOS; o SIH conta EPISÓDIOS de internação. Nenhum é
# subconjunto do outro — os dois só se encontram no número de pareados.
# Empilhar as duas contagens num único funil descendente sugeria que
# 374.604 "caía" para 323.802, o que não descreve nenhum filtro real.
COR_TRILHA <- c("SIM \u00b7 \u00f3bitos" = "#004B87",
                "SIH \u00b7 epis\u00f3dios" = "#00A3A1",
                "Encontro das duas bases" = "#F4A261")

reconciliacao <- function() {
  if (!QUAL_REAL) return(NULL)
  # Preferir as trilhas gravadas pelo script; se o arquivo for de uma
  # execução anterior, derivar o que dá dos números disponíveis.
  if (!is.null(qual$trilha_sim) && !is.null(qual$trilha_sih)) {
    sim <- qual$trilha_sim; sih <- qual$trilha_sih
  } else {
    f <- qual$funil
    g <- function(rx) { v <- f$n[grepl(rx, f$etapa)]; if (length(v)) v[1] else NA_real_ }
    alvo <- g("alvo"); par <- g("Pareados")
    sim <- data.frame(
      etapa = c("\u00d3bitos de 0 a 6 anos no SIM",
                "\u00d3bitos hospitalares (alvo)",
                "Alvo capturado na AIH (pareados)",
                "Alvo n\u00e3o capturado"),
      n = c(g("no SIM$"), alvo, par, alvo - par), stringsAsFactors = FALSE)
    nob <- sum(traj$desfecho == "\u00d3bito", na.rm = TRUE)
    ntr <- sum(traj$desfecho == "Transfer\u00eancia", na.rm = TRUE)
    sih <- data.frame(
      etapa = c("Epis\u00f3dios com desfecho \u00f3bito",
                "Epis\u00f3dios com desfecho transfer\u00eancia",
                "Base anal\u00edtica (\u00f3bito + transfer\u00eancia)",
                "\u00d3bitos pareados com o SIM",
                "\u00d3bitos sem par no SIM"),
      n = c(nob, ntr, nob + ntr, par, nob - par), stringsAsFactors = FALSE)
  }
  list(sim = sim, sih = sih)
}

# Duas trilhas lado a lado, com o ponto de encontro destacado.
plot_funil_etapas <- function() {
  if (!QUAL_REAL) return(empty_plot(MSG_SEM_V3))
  rc <- reconciliacao()
  if (is.null(rc)) return(empty_plot(MSG_SEM_V3))
  eh_encontro <- function(x) grepl("pareado|capturado na AIH", x, ignore.case = TRUE)
  d <- rbind(
    data.frame(trilha = "SIM \u00b7 \u00f3bitos", etapa = rc$sim$etapa, n = rc$sim$n),
    data.frame(trilha = "SIH \u00b7 epis\u00f3dios", etapa = rc$sih$etapa, n = rc$sih$n))
  d <- d[!is.na(d$n), ]
  if (!nrow(d)) return(empty_plot("Sem dados de reconcilia\u00e7\u00e3o"))
  d$grupo <- ifelse(eh_encontro(d$etapa), "Encontro das duas bases", d$trilha)
  d$rot   <- paste0(d$etapa, "   ")
  d$rot   <- factor(d$rot, levels = rev(d$rot))
  plot_ly(d, x = ~n, y = ~rot, type = "bar", orientation = "h",
          color = ~grupo, colors = COR_TRILHA,
          marker = list(line = list(color = "white", width = 1)),
          text = ~fmt_br(n), textposition = "outside",
          textfont = list(size = 11, color = pal$dark),
          hovertemplate = paste0("<b>%{y}</b><br>%{x:,.0f}<br>",
                                 "<i>%{fullData.name}</i><extra></extra>")) %>%
    layout(xaxis = list(title = "Registros (unidades diferentes por trilha \u2014 ver tabela abaixo)",
                        range = c(0, max(d$n, na.rm = TRUE) * 1.25)),
           yaxis = list(title = "", automargin = TRUE, tickfont = list(size = 11)),
           margin = list(l = 10, r = 20, t = 10, b = 60)) %>%
    clean_plotly()
}

# Tabela que fecha a conta e nomeia a unidade de cada número.
tab_reconciliacao <- function() {
  rc <- reconciliacao()
  if (is.null(rc)) return(NULL)
  rbind(
    data.frame(Trilha = "SIM \u2014 \u00f3bitos", Etapa = rc$sim$etapa,
               n = rc$sim$n, Unidade = "\u00f3bito (Declara\u00e7\u00e3o de \u00d3bito)"),
    data.frame(Trilha = "SIH \u2014 interna\u00e7\u00f5es", Etapa = rc$sih$etapa,
               n = rc$sih$n, Unidade = "epis\u00f3dio (AIHs encadeadas)"))
}

# Cobertura por ano: barras exato × probabilístico + linha de cobertura do alvo.
plot_qual_ano <- function() {
  if (!QUAL_REAL || is.null(qual$por_ano)) return(empty_plot(MSG_SEM_V3))
  a <- as.data.frame(qual$por_ano)
  plot_ly(a) %>%
    add_bars(x = ~ano, y = ~exato, name = "Match exato",
             marker = list(color = PAL_METODO[["Exato"]]),
             hovertemplate = "%{x}: %{y:,.0f} exatos<extra></extra>") %>%
    add_bars(x = ~ano, y = ~prob, name = "Match probabilístico",
             marker = list(color = PAL_METODO[["Probabilístico"]]),
             hovertemplate = "%{x}: %{y:,.0f} probabilísticos<extra></extra>") %>%
    add_lines(x = ~ano, y = ~cobertura_alvo, name = "% do alvo capturado",
              yaxis = "y2", line = list(color = pal$alert, width = 3),
              marker = list(color = pal$alert),
              hovertemplate = "%{x}: %{y:.1f}% do alvo<extra></extra>") %>%
    layout(barmode = "stack",
           xaxis = list(title = "Ano do óbito"),
           yaxis = list(title = "Óbitos pareados"),
           yaxis2 = list(overlaying = "y", side = "right", ticksuffix = "%",
                         title = "% do alvo (óbitos hospitalares do SIM)",
                         rangemode = "tozero", showgrid = FALSE)) %>%
    clean_plotly() %>%
    layout(margin = list(r = 70))
}

# Cobertura do alvo por UF.
plot_qual_uf <- function() {
  if (!QUAL_REAL || is.null(qual$por_uf)) return(empty_plot(MSG_SEM_V3))
  u <- as.data.frame(qual$por_uf)
  u <- u[!is.na(u$cobertura_alvo), ]
  u <- u[order(u$cobertura_alvo), ]
  u$uf <- factor(u$uf, levels = u$uf)
  cores <- colorRampPalette(c(pal$alert, pal$secondary, pal$primary))(nrow(u))
  plot_ly(u, x = ~cobertura_alvo, y = ~uf, type = "bar", orientation = "h",
          marker = list(color = cores, line = list(color = "white", width = .5)),
          text = ~paste0(format(round(cobertura_alvo, 1), decimal.mark = ","), "%"),
          textposition = "outside", textfont = list(size = 10, color = pal$dark),
          hovertemplate = paste0("<b>%{y}</b> · %{x:.1f}% do alvo<br>",
                                 "pareados: %{customdata:,.0f}<extra></extra>"),
          customdata = ~pareados) %>%
    layout(xaxis = list(title = "% dos óbitos hospitalares do SIM capturados",
                        ticksuffix = "%",
                        range = c(0, max(u$cobertura_alvo, na.rm = TRUE) * 1.18)),
           yaxis = list(title = "", tickfont = list(size = 10))) %>%
    clean_plotly(legend_pos = "none")
}

# Sensibilidade da janela de retroação (Etapa 3).
plot_sens_janela <- function() {
  if (!QUAL_REAL || is.null(qual$sensibilidade)) return(empty_plot(MSG_SEM_V3))
  sj <- as.data.frame(qual$sensibilidade)
  sj$lbl <- paste0(sj$janela, " dias")
  sj$lbl <- factor(sj$lbl, levels = sj$lbl)
  plot_ly(sj, x = ~lbl, y = ~pct, type = "bar",
          marker = list(color = c(pal$primary, pal$secondary, pal$accent)[seq_len(nrow(sj))],
                        line = list(color = "white", width = 1)),
          text = ~paste0(format(round(pct, 1), decimal.mark = ","), "%"),
          textposition = "outside", textfont = list(size = 12, color = pal$dark),
          hovertemplate = paste0("<b>Janela de %{x}</b><br>%{y:.1f}% dos óbitos ",
                                 "pareados<br>n = %{customdata:,.0f}<extra></extra>"),
          customdata = ~com_prev) %>%
    layout(xaxis = list(title = "Janela de retroação antes da internação-índice"),
           yaxis = list(title = "% com internação anterior identificada",
                        ticksuffix = "%",
                        range = c(0, max(sj$pct, na.rm = TRUE) * 1.3))) %>%
    clean_plotly(legend_pos = "none")
}

# ---- Descritiva dos óbitos linkados ----------------------------------------
desc_por_ano <- function(d) {
  if (!TRAJ_REAL) return(empty_plot(MSG_SEM_LINKAGE))
  d <- d[!is.na(d$cid_obito), ]
  if (nrow(d) == 0) return(empty_plot("Sem óbitos linkados no recorte"))
  if (LINK_V3 && any(!is.na(d$metodo))) {
    r <- d %>% count(ano, metodo, name = "n")
    plot_ly(r, x = ~ano, y = ~n, color = ~metodo, colors = PAL_METODO,
            type = "bar",
            hovertemplate = "<b>%{x}</b> · %{fullData.name}: %{y:,.0f}<extra></extra>") %>%
      layout(barmode = "stack", xaxis = list(title = "Ano"),
             yaxis = list(title = "Óbitos linkados")) %>%
      clean_plotly()
  } else {
    r <- d %>% count(ano, name = "n")
    plot_ly(r, x = ~ano, y = ~n, type = "bar",
            marker = list(color = pal$primary),
            hovertemplate = "<b>%{x}</b>: %{y:,.0f}<extra></extra>") %>%
      layout(xaxis = list(title = "Ano"),
             yaxis = list(title = "Óbitos linkados")) %>%
      clean_plotly(legend_pos = "none")
  }
}

desc_por_uf <- function(d) {
  if (!TRAJ_REAL) return(empty_plot(MSG_SEM_LINKAGE))
  d <- d[!is.na(d$cid_obito) & !is.na(d$uf), ]
  if (nrow(d) == 0) return(empty_plot("Sem óbitos linkados no recorte"))
  r <- d %>% count(uf, regiao, name = "n") %>% arrange(n)
  r$uf <- factor(r$uf, levels = r$uf)
  reg_cor <- c("Norte" = "#C0392B", "Nordeste" = "#E67E22",
               "Centro-Oeste" = "#F4A261", "Sudeste" = "#00A3A1", "Sul" = "#004B87")
  plot_ly(r, x = ~n, y = ~uf, type = "bar", orientation = "h",
          marker = list(color = unname(reg_cor[r$regiao]),
                        line = list(color = "white", width = .5)),
          text = ~fmt_br(n), textposition = "outside",
          textfont = list(size = 10, color = pal$dark),
          hovertemplate = "<b>%{y}</b> (%{customdata})<br>%{x:,.0f} óbitos linkados<extra></extra>",
          customdata = ~regiao) %>%
    layout(xaxis = list(title = "Óbitos linkados",
                        range = c(0, max(r$n, na.rm = TRUE) * 1.18)),
           yaxis = list(title = "", tickfont = list(size = 10))) %>%
    clean_plotly(legend_pos = "none")
}

desc_faixa_sexo <- function(d) {
  if (!TRAJ_REAL) return(empty_plot(MSG_SEM_LINKAGE))
  d <- d[!is.na(d$cid_obito) & !is.na(d$faixa) & !is.na(d$sexo), ]
  if (nrow(d) == 0) return(empty_plot("Sem óbitos linkados no recorte"))
  r <- d %>% count(faixa, sexo, name = "n")
  r$faixa <- factor(r$faixa, levels = c("Neonatal (0–27 dias)",
                                        "Pós-neonatal (28 d–<1 ano)", "1 a 6 anos"))
  plot_ly(r, x = ~faixa, y = ~n, color = ~sexo,
          colors = c("Masculino" = pal$primary, "Feminino" = "#8E44AD"),
          type = "bar",
          hovertemplate = "<b>%{x}</b> · %{fullData.name}: %{y:,.0f}<extra></extra>") %>%
    layout(barmode = "group", xaxis = list(title = ""),
           yaxis = list(title = "Óbitos linkados")) %>%
    clean_plotly()
}

desc_resumo_uf <- function(d) {
  d <- d[!is.na(d$cid_obito), ]
  if (nrow(d) == 0) return(NULL)
  d %>% group_by(regiao, uf) %>%
    summarise(
      `Óbitos linkados`  = dplyr::n(),
      `% match exato`    = if (LINK_V3) 100 * mean(metodo == "Exato", na.rm = TRUE) else NA_real_,
      `% com internação anterior (30 d)` =
        if (LINK_V3) 100 * mean(teve_prev_30 %in% TRUE) else NA_real_,
      `Permanência mediana (dias)` = stats::median(dias_internacao, na.rm = TRUE),
      `% com transferência` = 100 * mean(n_transferencias > 0, na.rm = TRUE),
      .groups = "drop") %>%
    arrange(regiao, dplyr::desc(`Óbitos linkados`))
}

# ---- Guia de CIDs ----------------------------------------------------------
guia_cids_tab <- function(d) {
  base <- cid_dic[, c("cid", "nome", "grupo", "generico", "nota")]
  cl <- cid_classify(base$cid)
  base$sistema  <- cl$sistema
  base$capitulo <- cl$cap
  base$especificidade <- ifelse(base$generico == 1, "Genérico / mal definido",
                                "Específico")
  if (TRAJ_REAL && nrow(d)) {
    ne <- table(d$cid_entrada)
    no <- table(d$cid_obito[!is.na(d$cid_obito)])
    base$n_entrada <- as.integer(ne[base$cid]); base$n_entrada[is.na(base$n_entrada)] <- 0L
    base$n_obito   <- as.integer(no[base$cid]); base$n_obito[is.na(base$n_obito)]     <- 0L
  } else {
    base$n_entrada <- NA_integer_; base$n_obito <- NA_integer_
  }
  base[order(-base$n_obito, base$cid),
       c("cid", "nome", "sistema", "grupo", "capitulo", "especificidade",
         "n_entrada", "n_obito", "nota")]
}

# ---- Etapa 5: internações sem óbito associado ------------------------------
baixa_sistema <- function(b, topn = 12) {
  if (!BAIXA_REAL) return(empty_plot(MSG_SEM_V3))
  if (nrow(b) == 0) return(empty_plot("Sem registros no recorte"))
  r <- b %>% filter(!is.na(sistema)) %>%
    group_by(sistema) %>% summarise(n = sum(n), .groups = "drop") %>%
    arrange(desc(n))
  if (nrow(r) > topn) r <- r[seq_len(topn), ]
  r <- r[order(r$n), ]
  r$sistema <- factor(r$sistema, levels = r$sistema)
  plot_ly(r, x = ~n, y = ~sistema, type = "bar", orientation = "h",
          marker = list(color = cor_sistema(as.character(r$sistema)),
                        line = list(color = "white", width = .5)),
          text = ~fmt_br(n), textposition = "outside",
          textfont = list(size = 10, color = pal$dark),
          hovertemplate = "<b>%{y}</b><br>%{x:,.0f} internações<extra></extra>") %>%
    layout(xaxis = list(title = "Internações",
                        range = c(0, max(r$n, na.rm = TRUE) * 1.2)),
           yaxis = list(title = "", automargin = TRUE, tickfont = list(size = 10))) %>%
    clean_plotly(legend_pos = "none")
}

baixa_tempo <- function(b, topn = 12) {
  if (!BAIXA_REAL) return(empty_plot(MSG_SEM_V3))
  if (nrow(b) == 0) return(empty_plot("Sem registros no recorte"))
  r <- b %>% filter(!is.na(sistema)) %>%
    group_by(sistema) %>%
    summarise(dias = stats::weighted.mean(dias_medio, w = n, na.rm = TRUE),
              n = sum(n),
              .groups = "drop") %>%
    arrange(desc(n))
  if (nrow(r) > topn) r <- r[seq_len(topn), ]
  r <- r[order(r$dias), ]
  r$sistema <- factor(r$sistema, levels = r$sistema)
  plot_ly(r, x = ~dias, y = ~sistema, type = "bar", orientation = "h",
          marker = list(color = cor_sistema(as.character(r$sistema)),
                        line = list(color = "white", width = .5)),
          text = ~format(round(dias, 1), decimal.mark = ","),
          textposition = "outside", textfont = list(size = 10, color = pal$dark),
          hovertemplate = paste0("<b>%{y}</b><br>permanência média %{x:.1f} dias<br>",
                                 "n = %{customdata:,.0f}<extra></extra>"),
          customdata = ~n) %>%
    layout(xaxis = list(title = "Permanência média (dias)",
                        range = c(0, max(r$dias, na.rm = TRUE) * 1.25)),
           yaxis = list(title = "", automargin = TRUE, tickfont = list(size = 10))) %>%
    clean_plotly(legend_pos = "none")
}

baixa_serie <- function(b) {
  if (!BAIXA_REAL) return(empty_plot(MSG_SEM_V3))
  if (nrow(b) == 0) return(empty_plot("Sem registros no recorte"))
  b <- b[!is.na(b$desfecho), ]
  if (nrow(b) == 0) return(empty_plot("Sem registros no recorte"))
  r <- b %>% group_by(ano, desfecho) %>% summarise(n = sum(n), .groups = "drop")
  plot_ly(r, x = ~ano, y = ~n, color = ~desfecho,
          colors = c("Alta" = pal$secondary, "Transferência" = pal$accent),
          type = "bar",
          hovertemplate = "<b>%{x}</b> · %{fullData.name}: %{y:,.0f}<extra></extra>") %>%
    layout(barmode = "stack", xaxis = list(title = "Ano"),
           yaxis = list(title = "Internações sem óbito associado")) %>%
    clean_plotly()
}

# ----------------------------------------------------------------------------
# 4. CSS
# ----------------------------------------------------------------------------
custom_css <- "
  body { background-color:#F4F6FA; }
  .navbar { background:#FFF !important; border-bottom:3px solid #004B87;
            box-shadow:0 2px 6px rgba(0,0,0,.06); }
  .navbar-brand { font-weight:700 !important; color:#004B87 !important; }
  .nav-link { color:#495057 !important; font-weight:500; margin:0 6px; }
  .nav-link.active { color:#004B87 !important;
                     border-bottom:3px solid #00A3A1; font-weight:700; }
  .card { border:1px solid #E3E7EE; border-radius:10px;
          box-shadow:0 2px 10px rgba(0,0,0,.04); background:#FFF; }
  .card:hover { box-shadow:0 6px 18px rgba(0,0,0,.08); }
  .card-header { background:linear-gradient(90deg,#FFF,#F4F6FA);
                 font-weight:700; text-transform:uppercase; font-size:.72rem;
                 color:#495057; letter-spacing:1.4px;
                 border-bottom:1px solid #E3E7EE; padding:12px 18px; }
  .header-subtle { font-weight:400; color:#8C95A3; text-transform:none;
                    font-size:.72rem; margin-left:4px; letter-spacing:.3px; }
  .value-box { border-radius:10px !important; border:1px solid #E3E7EE;
               box-shadow:0 2px 10px rgba(0,0,0,.04); }
  .value-box .value-box-title { font-size:.7rem !important;
                                 letter-spacing:1.2px; text-transform:uppercase;
                                 font-weight:600; }
  .value-box .value-box-value { font-size:1.7rem !important; font-weight:700; }
  .sidebar, .bslib-sidebar-layout > .sidebar {
    background:#FFF !important; border-right:1px solid #E3E7EE !important; }
  .section-lead { color:#495057; font-size:.95rem; margin-bottom:16px;
                  padding:12px 18px; background:#FFF;
                  border-left:4px solid #004B87; border-radius:6px;
                  box-shadow:0 1px 4px rgba(0,0,0,.03); }
  .section-lead b { color:#004B87; }
  .meta-card h5 { color:#004B87; font-weight:700; margin-top:14px; }
  .meta-card ul { margin-bottom:8px; }
  .gap-badge { display:inline-block; background:#FFF5F5; color:#D9534F;
               border:1px solid #f1c9c7; border-radius:6px; padding:1px 8px;
               font-size:.72rem; font-weight:600; margin-left:6px; }
  footer.app-footer { padding:18px; text-align:center; color:#6c757d;
                       font-size:.8rem; border-top:1px solid #E3E7EE;
                       background:#FFF; margin-top:30px; }
  .faixa-hero { background:linear-gradient(120deg,#004B87,#00A3A1);
                color:#FFF; border-radius:12px; padding:18px 22px; margin-bottom:16px;
                box-shadow:0 4px 14px rgba(0,75,135,.18); }
  .faixa-hero .fx-tag { display:inline-block; background:rgba(255,255,255,.18);
                border-radius:20px; padding:2px 12px; font-size:.7rem;
                font-weight:700; letter-spacing:1px; text-transform:uppercase; }
  .faixa-hero h3 { margin:8px 0 4px; font-weight:800; }
  .faixa-hero p { margin:0; opacity:.94; font-size:.92rem; max-width:1000px; }
  .cause-card { border:1px solid #E3E7EE; border-left:4px solid #F4A261;
                border-radius:10px; background:#FFF; padding:14px 16px;
                margin-bottom:12px; box-shadow:0 1px 6px rgba(0,0,0,.04); }
  .cause-title { font-weight:700; color:#004B87; font-size:1rem; margin-bottom:8px; }
  .cause-block { margin-bottom:6px; }
  .cause-label { display:inline-block; font-size:.66rem; font-weight:700;
                 letter-spacing:.8px; text-transform:uppercase; color:#8C6D3F;
                 background:#FFF6E9; border-radius:5px; padding:1px 7px; margin-bottom:2px; }
  .cause-label.prev { color:#1E6F5C; background:#E7F6F0; }
  .cause-block p { margin:3px 0 0; font-size:.9rem; color:#495057; }
  /* --- selos de período --- */
  .periodo-badge { display:inline-block; background:#FFF6E9; color:#8C6D3F;
                   border:1px solid #F0DCC0; border-radius:6px; padding:1px 8px;
                   font-size:.66rem; font-weight:600; letter-spacing:.3px;
                   text-transform:none; margin-left:8px; }
  .periodo-badge.ok { background:#E7F6F0; color:#1E6F5C; border-color:#BFE6D6; }
  /* --- banner de ausência de dados --- */
  .nodata-banner { background:#FFF5F5; border:1px solid #F1C9C7; border-left:5px solid #D9534F;
                 color:#7B241C; border-radius:8px; padding:12px 16px; margin-bottom:16px;
                 font-size:.88rem; }
  .nodata-banner code { background:#FFE9E7; color:#7B241C; padding:1px 5px; border-radius:4px; }
  .ok-banner { background:#E7F6F0; border:1px solid #BFE6D6; border-left:5px solid #1E7B4F;
               color:#14543F; border-radius:8px; padding:12px 16px; margin-bottom:16px;
               font-size:.88rem; }
  /* --- caixa de revisão de CID --- */
  .rev-item { border-left:3px solid #00A3A1; background:#F7FBFC; border-radius:6px;
              padding:8px 12px; margin-bottom:8px; font-size:.86rem; }
  .rev-item b { color:#004B87; }
  .kpi-note { font-size:.72rem; color:#6c757d; }
"

# ----------------------------------------------------------------------------
# 4b. UI — construtor de painel de faixa etária (reutilizável)
# ----------------------------------------------------------------------------
faixa_panel <- function(fx) {
  id <- fx$id
  detalhe <- if (id %in% c("pos", "inf")) {
    layout_columns(
      col_widths = 12,
      card(full_screen = TRUE,
           card_header(sprintf("Outras causas / difícil prevenção — %s · explicado para leigos", fx$curto)),
           div(style = "padding:14px 18px;",
               tags$p(class = "text-muted", style = "font-size:.85rem;margin-bottom:12px;",
                      "Grupo de causas que não se evita com uma única política simples. ",
                      "Abaixo, o que cada uma significa e o que reduz mortes na prática. ",
                      tags$b("Conteúdo explicativo em linguagem simples; não substitui orientação clínica.")),
               div(class = "row",
                   lapply(causas_dificeis[[id]], function(it)
                     div(class = "col-md-6", card_causa(it))))))
    )
  } else NULL

  prios_disp <- unique(sim_prio_cid$prio[sim_prio_cid$faixa == PAINEL_FX[[id]]])
  prios_ord  <- c("Outras Causas / Difícil Prevenção", "Ação Prioritária (Prevenção/Tratamento)",
                  "Atenção à Gestação e Parto", "Malformações (Alta Complexidade)")
  prios_disp <- prios_ord[prios_ord %in% prios_disp]

  cid_block <- layout_columns(
    col_widths = c(7, 5),
    card(full_screen = TRUE,
         card_header(
           div(class = "d-flex justify-content-between align-items-center flex-wrap",
               tags$span("Destrinchando a causa por CID (nomeado)", selo_periodo()),
               div(style = "min-width:290px;",
                   selectInput(paste0("fx_", id, "_causa"), NULL,
                               choices = prios_disp, selected = prios_disp[1], width = "100%")))),
         div(style = "padding:2px 6px;",
             tags$p(class = "text-muted", style = "font-size:.82rem;margin:6px 12px 2px;",
                    "Dos ", tags$b(textOutput(paste0("fx_", id, "_causa_pct"), inline = TRUE)),
                    " que a causa representa nesta faixa, veja quanto é cada CID (CID-10, 3 caracteres). ",
                    tags$span(style = "color:#6c757d;", "Barras cinza = código genérico / mal definido.")),
             plotlyOutput(paste0("fx_", id, "_cid"), height = "430px"))),
    card(full_screen = TRUE,
         card_header("Sistema do organismo acometido", selo_periodo()),
         div(style = "padding:2px 6px;",
             tags$p(class = "text-muted", style = "font-size:.82rem;margin:6px 12px 2px;",
                    "Classificação CID-10 → capítulo → sistema, aplicada a todos os códigos ",
                    "nomeados desta faixa."),
             plotlyOutput(paste0("fx_", id, "_sistema"), height = "430px"))))

  agrup_block <- layout_columns(
    col_widths = c(6, 6),
    card(full_screen = TRUE,
         card_header("O que há dentro das \"Outras Causas / Difícil Prevenção\"", selo_periodo()),
         plotlyOutput(paste0("fx_", id, "_agrup"), height = "400px")),
    card(full_screen = TRUE,
         card_header("Agrupamento de patologias semelhantes", selo_periodo()),
         div(style = "padding:2px 6px;",
             tags$p(class = "text-muted", style = "font-size:.82rem;margin:6px 12px 2px;",
                    "Consolidação de CIDs que representam a mesma condição clínica ",
                    "(cardiopatias congênitas, sepse, afogamento, prematuridade, ...)."),
             plotlyOutput(paste0("fx_", id, "_grupo"), height = "400px"))))

  nav_panel(
    title = paste0(fx$n, ". ", fx$curto),
    div(class = "faixa-hero",
        tags$span(class = "fx-tag", paste0("Faixa ", fx$n, " de 3")),
        tags$h3(fx$titulo),
        tags$p(fx$lead)),
    layout_columns(
      fill = FALSE, col_widths = c(3, 3, 3, 3),
      value_box(title = "Óbitos na faixa (período)",
                value = textOutput(paste0("fx_", id, "_obitos"), inline = TRUE),
                showcase = bs_icon("person-dash"),
                theme = value_box_theme(bg = pal$white, fg = pal$primary),
                p(tags$small(class = "kpi-note", "Responde ao filtro de período"))),
      value_box(title = "% do total 0–6 anos",
                value = textOutput(paste0("fx_", id, "_share"), inline = TRUE),
                showcase = bs_icon("pie-chart"),
                theme = value_box_theme(bg = pal$white, fg = pal$secondary)),
      value_box(title = "Variação no período",
                value = textOutput(paste0("fx_", id, "_var"), inline = TRUE),
                showcase = bs_icon("graph-down"),
                theme = value_box_theme(bg = pal$white, fg = pal$accent)),
      value_box(title = "Taxa da faixa",
                value = textOutput(paste0("fx_", id, "_taxa"), inline = TRUE),
                showcase = bs_icon("speedometer"),
                theme = value_box_theme(bg = pal$white, fg = pal$primary),
                p(tags$small(class = "kpi-note",
                             unname(DENOM_LBL[fx$denom]))))
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(full_screen = TRUE,
           card_header("Série anual — óbitos e internações na faixa", selo_ok()),
           plotlyOutput(paste0("fx_", id, "_serie"), height = "420px")),
      card(full_screen = TRUE,
           card_header("Priorização de causas nesta faixa", selo_periodo()),
           plotlyOutput(paste0("fx_", id, "_prio"), height = "420px"))
    ),
    cid_block,
    agrup_block,
    detalhe
  )
}

# ----------------------------------------------------------------------------
# 5. UI
# ----------------------------------------------------------------------------
ui <- page_navbar(
  title = tags$span(bs_icon("activity", class = "me-2"),
                    "Observatório de Saúde Infantil — Brasil"),
  window_title = "Observatório de Saúde Infantil — Brasil",
  theme = bs_theme(version = 5, primary = pal$primary,
                   # local = FALSE: o navegador carrega a fonte via <link>; o
                   # servidor NAO baixa nada (o download era bloqueado na rede
                   # do INSPER e derrubava o sass com std::runtime_error).
                   base_font = font_google("Inter", local = FALSE),
                   heading_font = font_google("Inter", local = FALSE),
                   "font-size-base" = "0.92rem") |> bs_add_rules(custom_css),
  underline = TRUE, fillable = FALSE,

  sidebar = sidebar(
    title = tagList(bs_icon("sliders"), " Filtros Globais"),
    width = 290, open = "open",
    sliderInput("filtro_ano", "Período de análise:",
                min = ano_min, max = ano_max, value = c(ano_min, ano_max),
                step = 1, sep = "", ticks = FALSE),
    radioButtons("faixa_modo", "Recorte etário (séries):",
                 choices = c("Detalhado (4 faixas)" = "det",
                             "Agregado (<1 ano × 1–6)" = "agg"),
                 selected = "det"),
    checkboxInput("excluir_admin",
                  tagList("Excluir códigos administrativos ",
                          tags$small(class = "text-muted", "(Z37, Z38, Z03, Z76)")),
                  value = TRUE),
    hr(),
    helpText(tags$small(
      tags$b("Leitura dos selos:"), tags$br(),
      tags$span(class = "periodo-badge ok", "responde ao período"), tags$br(),
      tags$span(class = "periodo-badge", "período fixo"),
      " — a tabela de origem é acumulada e não tem coluna de ano.")),
    hr(),
    helpText(tags$small(
      tags$b("Fontes:"), tags$br(),
      "• Mortalidade: SIM/DATASUS", tags$br(),
      "• Internações: SIH-SUS/AIH", tags$br(),
      "• Nascidos vivos: SINASC", tags$br(),
      "• População 1–6: ", POP_FONTE, tags$br(), tags$br(),
      tags$b("Linkage SIM↔SIH: "),
      if (LINK_V3) "fluxo em 5 etapas carregado"
      else if (TRAJ_REAL) "carregado (versão anterior do fluxo)"
      else "sem dados carregados",
      tags$br(), tags$br(),
      tags$b("Coorte: "), "0–6 anos", tags$br(),
      tags$b("Período: "), PERIODO_FIXO, tags$br(),
      tags$b("Taxas: "), "por 1.000 nascidos vivos até 1 ano; ",
      "por 1.000 crianças da faixa de 1 a 6"))
  ),

  # ===================== PANORAMA =====================
  nav_panel(
    title = "Panorama", icon = bs_icon("speedometer2"),
    div(class = "section-lead",
        tags$b("Visão executiva. "),
        "Indicadores consolidados, distribuição espacial, série histórica ",
        "óbitos × internações e ranking de macrorregiões. Taxas de mortalidade ",
        "de menores de 1 ano usam nascidos vivos; a faixa de 1 a 6 anos usa ",
        "as próprias crianças daquela faixa como denominador."),

    layout_columns(
      fill = FALSE, col_widths = c(2, 2, 2, 2, 2, 2),
      value_box(title = "Óbitos no período",
                value = textOutput("kpi_mort_total", inline = TRUE),
                showcase = bs_icon("person-dash"),
                theme = value_box_theme(bg = pal$white, fg = pal$primary),
                p(textOutput("kpi_mort_var", inline = TRUE))),
      value_box(title = "Internações no período",
                value = textOutput("kpi_int_total", inline = TRUE),
                showcase = bs_icon("hospital"),
                theme = value_box_theme(bg = pal$white, fg = pal$secondary),
                p(textOutput("kpi_int_var", inline = TRUE))),
      value_box(title = "TMM5 Brasil",
                value = textOutput("kpi_taxa_mort", inline = TRUE),
                showcase = bs_icon("graph-down"),
                theme = value_box_theme(bg = pal$white, fg = pal$primary),
                p(tags$small(class = "kpi-note", "Óbitos <5 anos / 1.000 NV"))),
      value_box(title = "Internações 1–6 anos",
                value = textOutput("kpi_taxa_int", inline = TRUE),
                showcase = bs_icon("graph-up"),
                theme = value_box_theme(bg = pal$white, fg = pal$secondary),
                p(tags$small(class = "kpi-note", "por 1.000 crianças de 1 a 6, ao ano"))),
      value_box(title = "UF com maior taxa (óbitos)",
                value = textOutput("kpi_uf_critica", inline = TRUE),
                showcase = bs_icon("exclamation-triangle"),
                theme = value_box_theme(bg = "#FFF5F5", fg = pal$alert),
                p(tags$small(class = "kpi-note", "Média do período / 1.000 NV"))),
      value_box(title = "CID genérico nos óbitos",
                value = textOutput("kpi_generico", inline = TRUE),
                showcase = bs_icon("question-octagon"),
                theme = value_box_theme(bg = pal$white, fg = pal$accent),
                p(tags$small(class = "kpi-note", "% em código mal definido")))
    ),

    layout_columns(
      col_widths = c(7, 5),
      card(full_screen = TRUE,
           card_header(
             div(class = "d-flex justify-content-between align-items-center",
                 tags$span("Mapa coroplético — taxa por UF", selo_periodo()),
                 div(radioButtons("mapa_metrica", NULL,
                                  choices = c("Mortalidade" = "mort", "Internações" = "int"),
                                  selected = "mort", inline = TRUE)))),
           plotOutput("plot_mapa_uf", height = "560px")),
      card(full_screen = TRUE,
           card_header("Ranking estadual", selo_periodo()),
           plotlyOutput("plot_barras_uf", height = "560px"))
    ),

    layout_columns(
      col_widths = c(7, 5),
      card(full_screen = TRUE,
           card_header("Série histórica — óbitos × internações", selo_ok()),
           plotlyOutput("plot_tendencia_geral", height = "460px")),
      card(full_screen = TRUE,
           card_header("Taxa por macrorregião", selo_periodo()),
           plotlyOutput("plot_macro_geral", height = "460px"))
    )
  ),

  # ===================== POR FAIXA ETÁRIA =====================
  nav_menu(
    title = "Por Faixa Etária", icon = bs_icon("people"),
    faixa_panel(faixas$neo),
    faixa_panel(faixas$pos),
    faixa_panel(faixas$inf)
  ),

  # ===================== SISTEMAS & PATOLOGIAS =====================
  nav_panel(
    title = "Sistemas & Patologias", icon = bs_icon("body-text"),
    div(class = "section-lead",
        tags$b("Classificação por sistema do organismo e por grupo de patologia. "),
        "Todo código CID-10 é mapeado para o capítulo correspondente e, deste, ",
        "para o sistema acometido; em paralelo, CIDs que representam a mesma ",
        "condição clínica são consolidados em grupos (cardiopatias congênitas, ",
        "sepse, afogamento, prematuridade e assim por diante). Vale para óbitos ",
        "(SIM) e internações (SIH)."),
    layout_columns(
      fill = FALSE, col_widths = c(4, 2, 2, 2, 2),
      card(card_header("Filtros"),
           div(style = "padding:10px 14px;",
               radioButtons("sis_fonte", "Fonte:",
                            choices = c("Mortalidade (SIM)" = "mort", "Internações (SIH)" = "int"),
                            selected = "mort", inline = TRUE),
               selectInput("sis_faixa", "Faixa etária:",
                           choices = c("Todas as faixas", "Neonatal (0–27 dias)",
                                       "Pós-neonatal (28 d–<1 ano)", "1 a 6 anos")))),
      value_box(title = "Sistemas com casos", value = textOutput("sis_kpi_k", inline = TRUE),
                showcase = bs_icon("diagram-3"),
                theme = value_box_theme(bg = pal$white, fg = pal$primary)),
      value_box(title = "Sistema predominante", value = textOutput("sis_kpi_top", inline = TRUE),
                showcase = bs_icon("trophy"),
                theme = value_box_theme(bg = pal$white, fg = pal$secondary)),
      value_box(title = "Grupos de patologia", value = textOutput("sis_kpi_grp", inline = TRUE),
                showcase = bs_icon("collection"),
                theme = value_box_theme(bg = pal$white, fg = pal$accent)),
      value_box(title = "Em código genérico", value = textOutput("sis_kpi_gen", inline = TRUE),
                showcase = bs_icon("question-octagon"),
                theme = value_box_theme(bg = "#FFF5F5", fg = pal$alert))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(full_screen = TRUE,
           card_header("Sistema do organismo acometido", selo_periodo()),
           plotlyOutput("sis_bar", height = "540px")),
      card(full_screen = TRUE,
           card_header("Grupos de patologias semelhantes", selo_periodo()),
           plotlyOutput("sis_grupo", height = "540px"))
    ),
    layout_columns(
      col_widths = 12,
      card(full_screen = TRUE,
           card_header("Tabela de classificação — CID, sistema, grupo e especificidade"),
           DTOutput("sis_tab")))
  ),

  # ===================== TRAJETÓRIA & TRANSIÇÃO DE CID =====================
  nav_menu(
    title = "Linkage & Trajetória", icon = bs_icon("arrow-left-right"),
    nav_panel(
      "Qualidade do linkage",
      banner_sem_v3("qual"),
      div(class = "section-lead",
          tags$b("Duas bases, duas unidades de contagem. "),
          "O SIM conta ", tags$b("óbitos"), " e o SIH conta ", tags$b("episódios de internação"),
          " (AIHs do mesmo paciente encadeadas). Nenhuma das duas é subconjunto ",
          "da outra: elas só se encontram no número de registros pareados. ",
          "Por isso o total do SIM e o total do SIH não devem ser lidos como ",
          "uma sequência decrescente — a tabela de reconciliação abaixo fecha ",
          "a conta de cada lado separadamente."),
      div(class = "section-lead",
          tags$b("Fluxo em 5 etapas. "),
          "Etapa 1: os óbitos hospitalares do SIM formam o alvo — como a AIH ",
          "não separa público de privado, todo óbito hospitalar é candidato. ",
          "Etapa 2: linkage com as AIHs de desfecho óbito por data de ",
          "nascimento, sexo, data da alta/óbito, CNES e município de ",
          "residência — ", tags$b("match exato"), " exige data idêntica e ",
          "chaves concordantes; o ", tags$b("probabilístico"),
          " tolera até ±3 dias com escore mínimo. Etapa 3: retroação de 30 ",
          "dias (45 e 60 na sensibilidade) em busca de internações anteriores ",
          "com desfecho alta ou transferência."),
      layout_columns(
        fill = FALSE, col_widths = c(2, 2, 2, 2, 2, 2),
        value_box(title = "Alvo · óbitos hospitalares (SIM)",
                  value = textOutput("ql_kpi_alvo", inline = TRUE),
                  showcase = bs_icon("bullseye"),
                  theme = value_box_theme(bg = pal$white, fg = pal$dark)),
        value_box(title = "Pareados · encontro das bases",
                  value = textOutput("ql_kpi_par", inline = TRUE),
                  showcase = bs_icon("link-45deg"),
                  theme = value_box_theme(bg = pal$white, fg = pal$primary)),
        value_box(title = "% do alvo capturado",
                  value = textOutput("ql_kpi_cob", inline = TRUE),
                  showcase = bs_icon("crosshair"),
                  theme = value_box_theme(bg = pal$white, fg = pal$alert)),
        value_box(title = "Match exato",
                  value = textOutput("ql_kpi_exato", inline = TRUE),
                  showcase = bs_icon("check2-circle"),
                  theme = value_box_theme(bg = pal$white, fg = pal$primary)),
        value_box(title = "Match probabilístico",
                  value = textOutput("ql_kpi_prob", inline = TRUE),
                  showcase = bs_icon("percent"),
                  theme = value_box_theme(bg = pal$white, fg = pal$accent)),
        value_box(title = "Com internação anterior (30 d)",
                  value = textOutput("ql_kpi_prev", inline = TRUE),
                  showcase = bs_icon("clock-history"),
                  theme = value_box_theme(bg = pal$white, fg = pal$secondary),
                  p(tags$small(class = "kpi-note", "% dos óbitos pareados")))
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(full_screen = TRUE,
             card_header("Reconciliação: trilha do SIM × trilha do SIH",
                         selo_periodo("Período completo do arquivo de linkage")),
             plotlyOutput("ql_funil", height = "440px"),
             div(style = "padding:0 16px 12px;",
                 tags$small(class = "text-muted",
                            "As barras azuis contam óbitos; as verdes contam ",
                            "episódios de internação. A barra âmbar é o mesmo ",
                            "conjunto visto pelas duas bases."))),
        card(full_screen = TRUE,
             card_header("Sensibilidade da janela de retroação (Etapa 3)",
                         selo_periodo("Período completo do arquivo de linkage")),
             plotlyOutput("ql_sens", height = "440px"))),
      layout_columns(
        col_widths = c(6, 6),
        card(full_screen = TRUE,
             card_header("Cobertura por ano · exato × probabilístico",
                         selo_periodo("Período completo do arquivo de linkage")),
             plotlyOutput("ql_ano", height = "460px")),
        card(full_screen = TRUE,
             card_header("Cobertura do alvo por UF",
                         selo_periodo("Período completo do arquivo de linkage")),
             plotlyOutput("ql_uf", height = "460px"))),
      layout_columns(
        col_widths = 12,
        card(full_screen = TRUE,
             card_header("Como cada número se relaciona",
                         selo_periodo("Período completo do arquivo de linkage")),
             DTOutput("ql_recon")))
    ),
    nav_panel(
      "Descritiva dos linkados",
      banner_sem_dados(), selo_linkage_ok(),
      div(class = "section-lead",
          tags$b("Quem são os óbitos pareados. "),
          "Distribuição dos óbitos hospitalares linkados por ano, UF, faixa ",
          "etária e sexo, com o método de pareamento quando o fluxo v3 está ",
          "carregado. Responde aos filtros abaixo e ao período global."),
      layout_columns(
        fill = FALSE, col_widths = c(4, 8),
        card(card_header("Recorte"),
             div(style = "padding:10px 14px;",
                 selectInput("dsc_faixa", "Faixa etária:",
                             choices = c("Todas as faixas", "Neonatal (0–27 dias)",
                                         "Pós-neonatal (28 d–<1 ano)", "1 a 6 anos")),
                 selectInput("dsc_regiao", "Região:", choices = c("Brasil", REG_ORDEM)),
                 selectInput("dsc_sexo", "Sexo:",
                             choices = c("Ambos", "Masculino", "Feminino")))),
        card(full_screen = TRUE,
             card_header("Óbitos linkados por ano", selo_ok()),
             plotlyOutput("dsc_ano", height = "360px"))
      ),
      layout_columns(
        col_widths = c(5, 7),
        card(full_screen = TRUE,
             card_header("Por faixa etária e sexo", selo_ok()),
             plotlyOutput("dsc_faixa_sexo", height = "420px")),
        card(full_screen = TRUE,
             card_header("Por UF", selo_ok()),
             plotlyOutput("dsc_uf", height = "620px"))),
      layout_columns(
        col_widths = 12,
        card(full_screen = TRUE,
             card_header("Resumo por UF", selo_ok()),
             DTOutput("dsc_tab")))
    ),
    nav_panel(
      "Guia de CIDs",
      div(class = "section-lead",
          tags$b("Dicionário curado de códigos CID-10. "),
          "Os ", fmt_br(nrow(cid_dic)), " códigos revisados pela equipe, com ",
          "sistema do organismo, grupo de patologia, marcação de código ",
          "genérico/mal definido e as notas de revisão. As duas últimas ",
          "colunas contam quantas vezes cada código aparece no linkage ",
          "carregado — como CID de entrada (SIH) e como causa básica de ",
          "óbito (SIM)."),
      layout_columns(
        fill = FALSE, col_widths = c(3, 3, 3, 3),
        value_box(title = "Códigos no dicionário",
                  value = fmt_br(nrow(cid_dic)),
                  showcase = bs_icon("journal-medical"),
                  theme = value_box_theme(bg = pal$white, fg = pal$primary)),
        value_box(title = "Marcados como genéricos",
                  value = paste0(fmt_br(sum(cid_dic$generico == 1)), " (",
                                 fmt_pct(mean(cid_dic$generico == 1)), ")"),
                  showcase = bs_icon("question-octagon"),
                  theme = value_box_theme(bg = pal$white, fg = pal$alert)),
        value_box(title = "Com nota de revisão",
                  value = fmt_br(sum(nzchar(trimws(cid_dic$nota)))),
                  showcase = bs_icon("pencil-square"),
                  theme = value_box_theme(bg = pal$white, fg = pal$secondary)),
        value_box(title = "Grupos de patologia",
                  value = fmt_br(length(unique(cid_dic$grupo))),
                  showcase = bs_icon("collection"),
                  theme = value_box_theme(bg = pal$white, fg = pal$accent))
      ),
      layout_columns(
        col_widths = 12,
        card(full_screen = TRUE,
             card_header("Guia de CIDs — busca por código, nome, sistema ou grupo",
                         selo_periodo("Contagens sobre o período completo do linkage")),
             DTOutput("guia_tab")))
    ),
    nav_panel(
      "Trajetória hospitalar (T₀→T₂)",
      banner_sem_dados(), selo_linkage_ok(),
      div(class = "section-lead",
          tags$b("Linha do tempo do paciente. "),
          "T₀ = entrada hospitalar (SIH) · T₁ = seguimento e transferências · ",
          "T₂ = desfecho (alta, óbito ou transferência), com o óbito confirmado ",
          "no SIM. A base é restrita a internações que terminaram em óbito ou ",
          "transferência, recorte necessário para reconstruir a trajetória até o óbito."),
      layout_columns(
        fill = FALSE, col_widths = c(4, 2, 2, 2, 2),
        card(card_header("Recorte"),
             div(style = "padding:10px 14px;",
                 selectInput("tj_faixa", "Faixa etária:",
                             choices = c("Todas as faixas", "Neonatal (0–27 dias)",
                                         "Pós-neonatal (28 d–<1 ano)", "1 a 6 anos")),
                 selectInput("tj_regiao", "Região:",
                             choices = c("Brasil", REG_ORDEM)))),
        value_box(title = "Episódios (óbito + transf.)", value = textOutput("tj_kpi_t0", inline = TRUE),
                  showcase = bs_icon("box-arrow-in-right"),
                  theme = value_box_theme(bg = pal$white, fg = pal$primary)),
        value_box(title = "Com transferência (T₁)", value = textOutput("tj_kpi_t1", inline = TRUE),
                  showcase = bs_icon("arrow-left-right"),
                  theme = value_box_theme(bg = pal$white, fg = pal$secondary)),
        value_box(title = "Óbitos (T₂)", value = textOutput("tj_kpi_t2", inline = TRUE),
                  showcase = bs_icon("person-dash"),
                  theme = value_box_theme(bg = pal$white, fg = pal$alert)),
        value_box(title = "Permanência mediana", value = textOutput("tj_kpi_dias", inline = TRUE),
                  showcase = bs_icon("clock-history"),
                  theme = value_box_theme(bg = pal$white, fg = pal$accent),
                  p(tags$small(class = "kpi-note", "dias de T₀ a T₂")))
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(full_screen = TRUE,
             card_header("Funil T₀ → T₁ → T₂", selo_ok()),
             plotlyOutput("tj_funil", height = "440px")),
        card(full_screen = TRUE,
             card_header("Qualidade de preenchimento por região", selo_ok()),
             plotlyOutput("tj_qualidade", height = "440px")))
    ),
    nav_panel(
      "Transição de CID (entrada → óbito)",
      banner_sem_dados(), selo_linkage_ok(),
      div(class = "section-lead",
          tags$b("O paciente entra com um CID no SIH e morre com outro no SIM. "),
          "Cada internação linkada é classificada em um de seis desfechos: mesmo ",
          "CID; troca dentro do mesmo sistema; troca de sistema; ",
          tags$b("genérico → específico"), " (o hospital esclareceu a causa); ",
          tags$b("específico → genérico"), " (perdeu-se a informação); e ",
          tags$b("genérico → genérico"), " — entra com uma causa vaga e morre com ",
          "uma causa vaga de mortalidade, sem que a causa exata seja identificada."),
      layout_columns(
        fill = FALSE, col_widths = c(4, 2, 2, 2, 2),
        card(card_header("Recorte"),
             div(style = "padding:10px 14px;",
                 selectInput("tr_faixa", "Faixa etária:",
                             choices = c("Todas as faixas", "Neonatal (0–27 dias)",
                                         "Pós-neonatal (28 d–<1 ano)", "1 a 6 anos")),
                 selectInput("tr_regiao", "Região:", choices = c("Brasil", REG_ORDEM)))),
        value_box(title = "Óbitos linkados", value = textOutput("tr_kpi_n", inline = TRUE),
                  showcase = bs_icon("link-45deg"),
                  theme = value_box_theme(bg = pal$white, fg = pal$primary)),
        value_box(title = "Trocaram de CID", value = textOutput("tr_kpi_delta", inline = TRUE),
                  showcase = bs_icon("shuffle"),
                  theme = value_box_theme(bg = pal$white, fg = pal$secondary)),
        value_box(title = "Trocaram de sistema", value = textOutput("tr_kpi_sis", inline = TRUE),
                  showcase = bs_icon("diagram-2"),
                  theme = value_box_theme(bg = pal$white, fg = pal$accent)),
        value_box(title = "Causa nunca esclarecida", value = textOutput("tr_kpi_gg", inline = TRUE),
                  showcase = bs_icon("question-octagon"),
                  theme = value_box_theme(bg = "#FFF5F5", fg = pal$alert),
                  p(tags$small(class = "kpi-note", "genérico na entrada e no óbito")))
      ),
      layout_columns(
        col_widths = c(5, 7),
        card(full_screen = TRUE,
             card_header("Tipos de transição", selo_ok()),
             plotlyOutput("tr_barras", height = "460px")),
        card(full_screen = TRUE,
             card_header("Fluxo entrada → óbito por sistema do organismo", selo_ok()),
             plotlyOutput("tr_sankey", height = "460px"))
      ),
      layout_columns(
        col_widths = 12,
        card(full_screen = TRUE,
             card_header("Matriz CID de entrada × CID de óbito (códigos mais frequentes)", selo_ok()),
             plotlyOutput("tr_matriz", height = "620px"))),
      layout_columns(
        col_widths = 12,
        card(full_screen = TRUE,
             card_header("Pares entrada → óbito mais frequentes"),
             DTOutput("tr_tab")))
    ),
    nav_panel(
      "Δ entrada/óbito por região",
      banner_sem_dados(), selo_linkage_ok(),
      div(class = "section-lead",
          tags$b("Variação do diagnóstico entre a entrada e o óbito. "),
          "No eixo horizontal a região; no vertical, o Δ entre o CID de entrada e ",
          "o CID de óbito. Regiões com Δ alto e muita transição genérico→genérico ",
          "indicam menor capacidade de esclarecer a causa da morte — não ",
          "necessariamente mais gravidade clínica."),
      layout_columns(
        fill = FALSE, col_widths = c(4, 8),
        card(card_header("Recorte e métrica"),
             div(style = "padding:10px 14px;",
                 selectInput("dl_metrica", "Métrica no eixo vertical:",
                             choices = c("Δ CID entrada → óbito" = "delta_cid",
                                         "Δ sistema do organismo" = "delta_sis",
                                         "Causa nunca esclarecida (genérico → genérico)" = "gen_gen",
                                         "Específico → Genérico (perda diagnóstica)" = "perda",
                                         "Genérico → Específico (ganho diagnóstico)" = "ganho")),
                 selectInput("dl_faixa", "Faixa etária:",
                             choices = c("Todas as faixas", "Neonatal (0–27 dias)",
                                         "Pós-neonatal (28 d–<1 ano)", "1 a 6 anos")))),
        card(full_screen = TRUE,
             card_header("Δ entrada/óbito por região", selo_ok()),
             plotlyOutput("dl_bar", height = "440px"))
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(full_screen = TRUE,
             card_header("Composição das transições por região (100%)", selo_ok()),
             plotlyOutput("dl_stack", height = "460px")),
        card(full_screen = TRUE,
             card_header("Tabela regional"), DTOutput("dl_tab")))
    ),
    nav_panel(
      "Causas secundárias",
      banner_sem_dados(), selo_linkage_ok(),
      div(class = "section-lead",
          tags$b("Causas secundárias e comorbidades. "),
          "Diagnóstico secundário do SIH e causas associadas do SIM. É onde ",
          "aparecem malformações, prematuridade e doenças raras que não entram ",
          "como causa básica, mas explicam o desfecho."),
      layout_columns(
        fill = FALSE, col_widths = c(4, 4, 4),
        card(card_header("Recorte"),
             div(style = "padding:10px 14px;",
                 selectInput("cs_faixa", "Faixa etária:",
                             choices = c("Todas as faixas", "Neonatal (0–27 dias)",
                                         "Pós-neonatal (28 d–<1 ano)", "1 a 6 anos")),
                 selectInput("cs_regiao", "Região:", choices = c("Brasil", REG_ORDEM)))),
        value_box(title = "Com causa secundária", value = textOutput("cs_kpi_pct", inline = TRUE),
                  showcase = bs_icon("list-check"),
                  theme = value_box_theme(bg = pal$white, fg = pal$primary)),
        value_box(title = "Sistema secundário mais comum", value = textOutput("cs_kpi_sis", inline = TRUE),
                  showcase = bs_icon("diagram-3"),
                  theme = value_box_theme(bg = pal$white, fg = pal$secondary))
      ),
      layout_columns(
        col_widths = 12,
        card(full_screen = TRUE,
             card_header("Principais causas secundárias registradas", selo_ok()),
             plotlyOutput("cs_bar", height = "560px")))
    ),
    nav_panel(
      "Internações sem óbito (Etapa 5)",
      banner_sem_v3("baixa"),
      div(class = "section-lead",
          tags$b("O outro lado do funil. "),
          "Excluídas as AIHs de episódios que terminaram em óbito e as ",
          "internações anteriores identificadas na Etapa 3, sobra o grupo de ",
          "internações \u201cde baixa mortalidade\u201d: causas, desfecho e tempo de ",
          "internação. ", tags$b("Atenção à unidade: "), "aqui a contagem é de ",
          tags$b("AIHs"), " individuais, e não de episódios como nas abas de ",
          "trajetória — por isso o total é maior. Agregado sem microdado."),
      layout_columns(
        fill = FALSE, col_widths = c(4, 2, 3, 3),
        card(card_header("Recorte"),
             div(style = "padding:10px 14px;",
                 selectInput("bx_faixa", "Faixa etária:",
                             choices = c("Todas as faixas", "Neonatal (0–27 dias)",
                                         "Pós-neonatal (28 d–<1 ano)", "1 a 6 anos")),
                 selectInput("bx_regiao", "Região:", choices = c("Brasil", REG_ORDEM)))),
        value_box(title = "AIHs no recorte",
                  value = textOutput("bx_kpi_n", inline = TRUE),
                  showcase = bs_icon("hospital"),
                  theme = value_box_theme(bg = pal$white, fg = pal$primary),
                  p(tags$small(class = "kpi-note", "unidade: AIH, não episódio"))),
        value_box(title = "Permanência média",
                  value = textOutput("bx_kpi_dias", inline = TRUE),
                  showcase = bs_icon("clock-history"),
                  theme = value_box_theme(bg = pal$white, fg = pal$secondary),
                  p(tags$small(class = "kpi-note", "dias, ponderada pelo volume"))),
        value_box(title = "Desfecho alta",
                  value = textOutput("bx_kpi_alta", inline = TRUE),
                  showcase = bs_icon("box-arrow-right"),
                  theme = value_box_theme(bg = pal$white, fg = pal$accent))
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(full_screen = TRUE,
             card_header("Principais sistemas do organismo (CID de entrada)", selo_ok()),
             plotlyOutput("bx_sistema", height = "480px")),
        card(full_screen = TRUE,
             card_header("Permanência média por sistema", selo_ok()),
             plotlyOutput("bx_tempo", height = "480px"))),
      layout_columns(
        col_widths = 12,
        card(full_screen = TRUE,
             card_header("Série anual por desfecho", selo_ok()),
             plotlyOutput("bx_serie", height = "380px")))
    )
  ),

  # ===================== METAS ODS/IPEA =====================
  nav_panel(
    title = "Metas ODS/IPEA", icon = bs_icon("bullseye"),
    div(class = "section-lead",
        tags$b("Estados e macrorregiões frente às metas de mortalidade na infância. "),
        "Taxas por 1.000 nascidos vivos (SIM/SINASC) comparadas às metas ",
        tags$b("SDG"), " (neonatal 12,0 · infantil 15,7 · menores de 5 anos 25,0) e às metas ",
        tags$b("IPEA"), " ajustadas para o Brasil (5,3 · 7,7 · 8,3). ",
        tags$small(class = "text-muted", "Referência: Lopes et al., Lancet Reg Health Am, 2025.")),
    layout_columns(
      fill = FALSE, col_widths = c(3, 3, 3, 3),
      value_box(title = "Indicador em foco", value = textOutput("meta_kpi_ind", inline = TRUE),
                showcase = bs_icon("bullseye"), theme = value_box_theme(bg = pal$white, fg = pal$primary)),
      value_box(title = "Taxa Brasil (período)", value = textOutput("meta_kpi_br", inline = TRUE),
                showcase = bs_icon("flag"), theme = value_box_theme(bg = pal$white, fg = pal$secondary),
                p(tags$small(class = "kpi-note", "média do período filtrado / 1.000 NV"))),
      value_box(title = "UFs que atingem a meta SDG", value = textOutput("meta_kpi_sdg", inline = TRUE),
                showcase = bs_icon("check-circle"), theme = value_box_theme(bg = pal$white, fg = pal$accent)),
      value_box(title = "UFs que atingem a meta IPEA", value = textOutput("meta_kpi_ipea", inline = TRUE),
                showcase = bs_icon("award"), theme = value_box_theme(bg = "#F2FBF6", fg = pal$green))
    ),
    div(style = "margin:6px 4px 14px;",
        radioButtons("meta_ind", "Indicador:",
                     choices = c("Neonatal (0–27 dias)" = "nmr",
                                 "Infantil (< 1 ano)" = "imr",
                                 "Menores de 5 anos" = "u5mr"),
                     selected = "u5mr", inline = TRUE)),
    layout_columns(
      col_widths = c(6, 6),
      card(full_screen = TRUE,
           card_header("Estados × metas SDG e IPEA", selo_ok()),
           plotlyOutput("meta_estados", height = "620px")),
      card(full_screen = TRUE,
           card_header("Macrorregiões — atingimento da meta", selo_periodo()),
           plotOutput("meta_mapa", height = "620px"))
    ),
    layout_columns(
      col_widths = 12,
      card(full_screen = TRUE,
           card_header(
             div(class = "d-flex justify-content-between align-items-center flex-wrap",
                 tags$span("Evolução no período com linhas de meta", selo_ok()),
                 div(style = "min-width:340px;",
                     selectizeInput("meta_evo_uf", NULL, choices = sort(sim_uf$sigla),
                                    selected = c("RR", "SC"), multiple = TRUE,
                                    options = list(placeholder = "Selecione estados para comparar",
                                                   plugins = list("remove_button")))))),
           plotlyOutput("meta_evo", height = "460px"))
    ),
    layout_columns(
      col_widths = 12,
      card(full_screen = TRUE,
           card_header("Tabela — taxas e atingimento por estado", selo_ok()),
           DTOutput("meta_tabela")))
  ),

  # ===================== CID & CAPÍTULOS =====================
  nav_menu(
    title = "CID & Capítulos", icon = bs_icon("diagram-3"),
    nav_panel("Detalhamento por CID",
              div(class = "section-lead",
                  tags$b("O que vem de CID, nomeado. "),
                  "Escolha a fonte (mortalidade SIM ou internações SIH). Para cada faixa e causa ",
                  "prioritária, os principais códigos CID-10 (3 caracteres), quanto representam ",
                  "dentro da causa, o sistema acometido e se o código é específico ou genérico."),
              layout_columns(
                fill = FALSE, col_widths = c(4, 2, 2, 2, 2),
                card(card_header("Filtros"),
                     div(style = "padding:10px 14px;",
                         radioButtons("cid_fonte", "Fonte:",
                                      choices = c("Mortalidade (SIM)" = "mort", "Internações (SIH)" = "int"),
                                      selected = "mort", inline = TRUE),
                         selectInput("cid_faixa", "Faixa etária:",
                                     choices = c("Neonatal (0–27 dias)", "Pós-neonatal (28 d–<1 ano)", "1 a 6 anos")),
                         selectInput("cid_causa", "Causa prioritária:",
                                     choices = c("Outras Causas / Difícil Prevenção",
                                                 "Ação Prioritária (Prevenção/Tratamento)",
                                                 "Atenção à Gestação e Parto",
                                                 "Malformações (Alta Complexidade)")),
                         checkboxInput("cid_so_gen", "Mostrar apenas códigos genéricos", FALSE))),
                value_box(title = "Casos exibidos", value = textOutput("cid_kpi_n", inline = TRUE),
                          showcase = bs_icon("list-ol"), theme = value_box_theme(bg = pal$white, fg = pal$primary)),
                value_box(title = "CIDs distintos", value = textOutput("cid_kpi_k", inline = TRUE),
                          showcase = bs_icon("upc-scan"), theme = value_box_theme(bg = pal$white, fg = pal$secondary)),
                value_box(title = "Em código genérico", value = textOutput("cid_kpi_gen", inline = TRUE),
                          showcase = bs_icon("question-octagon"), theme = value_box_theme(bg = "#FFF5F5", fg = pal$alert)),
                value_box(title = "Sistemas envolvidos", value = textOutput("cid_kpi_sis", inline = TRUE),
                          showcase = bs_icon("diagram-3"), theme = value_box_theme(bg = pal$white, fg = pal$accent))
              ),
              layout_columns(
                col_widths = c(7, 5),
                card(full_screen = TRUE, card_header("Principais CIDs da causa (nomeados)", selo_periodo()),
                     plotlyOutput("cid_bar", height = "470px")),
                card(full_screen = TRUE, card_header("Tabela de CIDs"),
                     DTOutput("cid_tab")))
    ),
    nav_panel("Grandes Capítulos",
              div(class = "section-lead",
                  tags$b("Estatística por grandes capítulos da CID-10. "),
                  "Distribuição pelos capítulos (I a XXII) da classificação, por faixa etária."),
              layout_columns(
                col_widths = c(3, 9),
                card(card_header("Filtros"),
                     div(style = "padding:10px 14px;",
                         radioButtons("cap_fonte", "Fonte:",
                                      choices = c("Mortalidade (SIM)" = "mort", "Internações (SIH)" = "int"),
                                      selected = "mort", inline = TRUE),
                         selectInput("cap_faixa", "Recorte:",
                                     choices = c("Total 0-6 anos", "Neonatal Precoce (0-6 dias)",
                                                 "Neonatal Tardia (7-27 dias)", "Pós-Neonatal (28 dias a <1 ano)",
                                                 "1 a 6 Anos")))),
                card(full_screen = TRUE, card_header("Distribuição por capítulo CID-10", selo_periodo()),
                     plotlyOutput("cap_bar", height = "520px")))
    ),
    nav_panel("Concentração (2 dígitos)",
              div(class = "section-lead",
                  tags$b("Concentração nos primeiros dígitos da CID. "),
                  "Categorias de 3 caracteres ordenadas por frequência, com curva de acumulado ",
                  "(Pareto). Barras cinza marcam códigos genéricos ou mal definidos."),
              layout_columns(
                fill = FALSE, col_widths = c(4, 4, 4),
                card(card_header("Filtros"),
                     div(style = "padding:10px 14px;",
                         radioButtons("conc_fonte", "Fonte:",
                                      choices = c("Mortalidade (SIM)" = "mort", "Internações (SIH)" = "int"),
                                      selected = "mort", inline = TRUE),
                         radioButtons("conc_escopo", "Escopo:",
                                      choices = c("Todos os casos" = "Geral",
                                                  "Só \"Outras Causas / Difícil Prevenção\"" = "Outras"),
                                      selected = "Geral"))),
                value_box(title = "Categorias CID distintas", value = textOutput("conc_kpi_dist", inline = TRUE),
                          showcase = bs_icon("diagram-2"), theme = value_box_theme(bg = pal$white, fg = pal$primary)),
                value_box(title = "Cobertura do Top 20", value = textOutput("conc_kpi_cob", inline = TRUE),
                          showcase = bs_icon("stack"), theme = value_box_theme(bg = pal$white, fg = pal$accent))
              ),
              layout_columns(col_widths = 12,
                             card(full_screen = TRUE, card_header("Pareto de categorias CID-10", selo_periodo()),
                                  plotlyOutput("conc_bar", height = "540px")))
    ),
    nav_panel("Revisão de nomenclatura",
              div(class = "section-lead",
                  tags$b("Registro das correções aplicadas. "),
                  "Cada linha documenta uma revisão de nomenclatura, reclassificação ou ",
                  "marcação de código genérico, para o documento de critérios."),
              layout_columns(
                fill = FALSE, col_widths = c(4, 4, 4),
                value_box(title = "Códigos no dicionário", value = textOutput("rev_kpi_n", inline = TRUE),
                          showcase = bs_icon("journal-check"),
                          theme = value_box_theme(bg = pal$white, fg = pal$primary)),
                value_box(title = "Marcados como genéricos", value = textOutput("rev_kpi_gen", inline = TRUE),
                          showcase = bs_icon("question-octagon"),
                          theme = value_box_theme(bg = "#FFF5F5", fg = pal$alert)),
                value_box(title = "Com nota de revisão", value = textOutput("rev_kpi_nota", inline = TRUE),
                          showcase = bs_icon("pencil-square"),
                          theme = value_box_theme(bg = pal$white, fg = pal$secondary))
              ),
              layout_columns(col_widths = 12,
                             card(full_screen = TRUE, card_header("Notas de revisão"),
                                  div(style = "padding:14px 18px;", uiOutput("rev_notas")))),
              layout_columns(col_widths = 12,
                             card(full_screen = TRUE, card_header("Dicionário CID-10 completo"),
                                  DTOutput("rev_tab")))
    )
  ),

  # ===================== MORTALIDADE =====================
  nav_menu(
    title = "Mortalidade", icon = bs_icon("heart-pulse"),
    nav_panel("Temporal & Causas",
              div(class = "section-lead",
                  "Evolução anual dos óbitos por faixa etária e por grupo de causa. ",
                  "Os gráficos empilhados são sempre proporcionais e somam 100%; ",
                  "os valores absolutos aparecem no hover."),
              layout_columns(col_widths = c(6, 6),
                             card(full_screen = TRUE,
                                  card_header("Curva por faixa etária", selo_ok()),
                                  plotlyOutput("mort_plot_idade", height = "470px")),
                             card(full_screen = TRUE,
                                  card_header("Composição das causas (100%)", selo_ok()),
                                  plotlyOutput("mort_plot_causas", height = "470px"))),
              layout_columns(col_widths = 12,
                             card(full_screen = TRUE,
                                  card_header("Priorização — causas evitáveis por faixa etária (100%)", selo_periodo()),
                                  plotlyOutput("mort_plot_prio", height = "560px")))),
    nav_panel("Mapas & Desigualdades",
              div(class = "section-lead",
                  "Distribuição espacial da taxa de mortalidade e heterogeneidade regional."),
              layout_columns(col_widths = c(6, 6),
                             card(full_screen = TRUE,
                                  card_header("Taxa por estado", selo_periodo()),
                                  plotOutput("mort_mapa_uf", height = "560px")),
                             card(full_screen = TRUE,
                                  card_header("Taxa por macrorregião", selo_periodo()),
                                  plotOutput("mort_mapa_macro", height = "560px")))),
    nav_panel("Fluxos & Polos",
              div(class = "section-lead",
                  "Evasão (intermunicipal e interestadual), polos de absorção e ",
                  "matriz de fluxo entre UFs (residência → ocorrência)."),
              layout_columns(col_widths = c(6, 6),
                             card(full_screen = TRUE,
                                  card_header(
                                    div(class = "d-flex justify-content-between align-items-center",
                                        tags$span("Evasão por UF", selo_periodo()),
                                        div(radioButtons("mort_evasao_tipo", NULL,
                                                         c("Fora do município" = "mun", "Fora da UF" = "uf"),
                                                         "mun", inline = TRUE)))),
                                  plotlyOutput("mort_plot_evasao", height = "620px")),
                             card(full_screen = TRUE,
                                  card_header("Top 30 polos de absorção (municípios)", selo_periodo()),
                                  plotlyOutput("mort_plot_polos", height = "620px"))),
              layout_columns(col_widths = 12,
                             card(full_screen = TRUE,
                                  card_header("Matriz de fluxo inter-UF (top 50 pares)", selo_periodo()),
                                  plotlyOutput("mort_plot_heatuf", height = "640px"))))
  ),

  # ===================== INTERNAÇÕES =====================
  nav_menu(
    title = "Internações", icon = bs_icon("hospital"),
    nav_panel("Temporal & Causas",
              div(class = "section-lead",
                  "AIH do SUS na coorte 0–6 anos. Atenção: o SIH registra ",
                  tags$b("diagnóstico principal"), ", de modo que causas externas aparecem ",
                  "pela lesão (capítulo XIX) e não pelo mecanismo (capítulo XX)."),
              layout_columns(col_widths = c(6, 6),
                             card(full_screen = TRUE,
                                  card_header("Curva por faixa etária", selo_ok()),
                                  plotlyOutput("int_plot_idade", height = "470px")),
                             card(full_screen = TRUE,
                                  card_header("Composição das causas (100%)", selo_ok()),
                                  plotlyOutput("int_plot_causas", height = "470px"))),
              layout_columns(col_widths = 12,
                             card(full_screen = TRUE,
                                  card_header("Priorização — causas evitáveis por faixa etária (100%)", selo_periodo()),
                                  plotlyOutput("int_plot_prio", height = "560px")))),
    nav_panel("Mapas & Desigualdades",
              div(class = "section-lead",
                  "Distribuição espacial da taxa de internação e heterogeneidade regional."),
              layout_columns(col_widths = c(6, 6),
                             card(full_screen = TRUE,
                                  card_header("Taxa por estado", selo_periodo()),
                                  plotOutput("int_mapa_uf", height = "560px")),
                             card(full_screen = TRUE,
                                  card_header("Taxa por macrorregião", selo_periodo()),
                                  plotOutput("int_mapa_macro", height = "560px")))),
    nav_panel("Fluxos & Polos",
              div(class = "section-lead",
                  "Rede hospitalar: evasão, polos de atendimento e fluxo entre UFs."),
              layout_columns(col_widths = c(6, 6),
                             card(full_screen = TRUE,
                                  card_header(
                                    div(class = "d-flex justify-content-between align-items-center",
                                        tags$span("Evasão por UF", selo_periodo()),
                                        div(radioButtons("int_evasao_tipo", NULL,
                                                         c("Fora do município" = "mun", "Fora da UF" = "uf"),
                                                         "mun", inline = TRUE)))),
                                  plotlyOutput("int_plot_evasao", height = "620px")),
                             card(full_screen = TRUE,
                                  card_header("Top 30 polos de atendimento (municípios)", selo_periodo()),
                                  plotlyOutput("int_plot_polos", height = "620px"))),
              layout_columns(col_widths = 12,
                             card(full_screen = TRUE,
                                  card_header("Matriz de fluxo inter-UF (top 50 pares)", selo_periodo()),
                                  plotlyOutput("int_plot_heatuf", height = "640px"))))
  ),

  # ===================== METODOLOGIA =====================
  nav_panel(
    title = "Metodologia", icon = bs_icon("journal-text"),
    div(class = "section-lead",
        tags$b("Nota metodológica e proveniência dos dados. "),
        "Projeto reprodutível baseado em microdados públicos do DATASUS."),
    layout_columns(
      col_widths = c(6, 6),
      card(class = "meta-card", full_screen = TRUE,
           card_header("Desenho, fontes e denominadores"),
           div(style = "padding:6px 18px 14px;",
               tags$h5("Coorte e período"),
               tags$ul(
                 tags$li("Crianças de 0 a 6 anos, Brasil, ", PERIODO_FIXO, "."),
                 tags$li("Recortes: neonatal precoce (0–6 d), neonatal tardia (7–27 d), pós-neonatal (28 d a <1 ano) e 1 a 6 anos.")),
               tags$h5("Fontes oficiais"),
               tags$ul(
                 tags$li(tags$b("SIM"), " — Sistema de Informações sobre Mortalidade."),
                 tags$li(tags$b("SIH/SUS"), " — Sistema de Informações Hospitalares (AIH)."),
                 tags$li(tags$b("SINASC"), " — nascidos vivos, denominador das taxas até 1 ano."),
                 tags$li(tags$b("População por faixa etária"), " — ", POP_FONTE, ".")),
               tags$h5("Denominadores das taxas"),
               tags$ul(
                 tags$li("Mortalidade neonatal, pós-neonatal e infantil: ",
                         tags$b("por 1.000 nascidos vivos"), " — comparável entre regiões e anos."),
                 tags$li("Faixa de 1 a 6 anos: ", tags$b("por 1.000 crianças de 1 a 6 anos"),
                         ", porque nascidos vivos deixam de ser denominador adequado depois do primeiro ano."),
                 tags$li("O painel ", tags$b("não estima população"), ": o denominador da faixa de ",
                         "1 a 6 anos vem das projeções oficiais do IBGE (POPSVS/DATASUS), ",
                         "por UF, ano e idade simples."),
                 tags$li(tags$b("Taxas de internação por 1.000 nascidos vivos não são usadas para a faixa de 1 a 6 anos"),
                         ", por não serem denominador adequado após o primeiro ano de vida.")),
               tags$h5("Filtros temporais"),
               tags$ul(
                 tags$li("Cards com selo verde respondem ao slider de período."),
                 tags$li("Cards com selo âmbar vêm de tabelas consolidadas do período completo, ",
                         "sem recorte anual.")),
               tags$h5("Mapas"),
               tags$ul(
                 tags$li("Malha de UF (SIRGAS 2000) carregada de ", tags$code("uf_sf_simplified.rds"),
                         " — execução offline, sem dependência do pacote geobr."))))
      ,
      card(class = "meta-card", full_screen = TRUE,
           card_header("Classificação de CID e linkage SIH↔SIM"),
           div(style = "padding:6px 18px 14px;",
               tags$h5("Classificação por sistema do organismo"),
               tags$ul(
                 tags$li("Todo código é mapeado por faixa determinística CID-10 → capítulo → sistema, ",
                         "de modo que códigos novos são classificados sem edição manual."),
                 tags$li("Sobre essa base, um dicionário revisado consolida patologias semelhantes ",
                         "e corrige nomenclaturas — ver aba ", tags$b("CID & Capítulos → Revisão de nomenclatura"), ".")),
               tags$h5("Códigos genéricos e mal definidos"),
               tags$ul(
                 tags$li("Marcamos como genéricos os códigos inespecíficos (\"não especificado\", ",
                         "\"outras\", NCOP), o capítulo XVIII inteiro e as causas externas sem mecanismo, ",
                         "como ", tags$code("W84"), "."),
                 tags$li("Códigos administrativos do SIH (", tags$code("Z37"), ", ", tags$code("Z38"),
                         ", ", tags$code("Z03"), ", ", tags$code("Z76"), ") não são diagnóstico e podem ",
                         "ser excluídos pelo filtro global.")),
               tags$h5("Linkage SIM ↔ SIH — fluxo em 5 etapas (v3)"),
               tags$ol(
                 tags$li(tags$b("Alvo (Etapa 1). "), "Óbitos hospitalares do SIM (LOCOCOR 1–2). ",
                         "Como a AIH não separa público de privado, todo óbito hospitalar é candidato."),
                 tags$li(tags$b("Linkage (Etapa 2). "), "Pareamento com as AIHs de desfecho óbito por ",
                         "data de nascimento, sexo, data da alta/óbito, CNES e município de residência; ",
                         "raça/cor entra como verificação. ", tags$b("Exato"), " = data idêntica e chaves ",
                         "concordantes; ", tags$b("probabilístico"), " = buffer de ±3 dias com escore mínimo. ",
                         "Cada AIH pareia com no máximo um óbito e vice-versa."),
                 tags$li(tags$b("Histórico (Etapa 3). "), "Retroação de 30 dias (45/60 na sensibilidade) ",
                         "antes da internação-índice em busca de internações anteriores com desfecho ",
                         "alta ou transferência."),
                 tags$li(tags$b("Análises (Etapa 4). "), "Trajetória T₀→T₂, transição de CID ",
                         "entrada→óbito e histórico do paciente."),
                 tags$li(tags$b("Baixa mortalidade (Etapa 5). "), "As AIHs fora do circuito de óbito ",
                         "são agregadas por ano, UF, faixa, sexo e sistema (causas e permanência).")),
               tags$h5("Contrato de colunas de ", tags$code(TRAJ_ARQ)),
               tags$pre(style = "font-size:.78rem;white-space:pre-wrap;",
                        paste(TRAJ_COLS, collapse = " · ")),
               tags$h5("Status atual"),
               if (TRAJ_REAL)
                 tags$p(tags$span(class = "periodo-badge ok", "Linkage real carregado"), " ",
                        tags$code(TRAJ_ARQ), " · ", format(TRAJ_INFO$n, big.mark = "."),
                        " episódios · ", TRAJ_INFO$anos, " · ", TRAJ_INFO$ufs, " UF · ",
                        format(TRAJ_INFO$linkados, big.mark = "."), " pareados com o SIM.")
               else
                 tags$p(tags$span(class = "gap-badge", "Sem dados"), " ",
                        tags$code(TRAJ_ARQ), " ausente. Os painéis de trajetória e transição ",
                        "ficam vazios — o app não gera dado substituto, simulado ou estimado ",
                        "em nenhuma hipótese."),
               tags$h5("Reprodutibilidade"),
               tags$p(tags$small("Todo o processamento — download da população oficial, linkage ",
                                 "SIM↔SIH em cinco etapas e agregações — é feito por scripts em R ",
                                 "a partir dos microdados públicos do DATASUS, sem nenhum dado ",
                                 "estimado ou simulado.")),
               tags$hr(),
               tags$p(tags$small(tags$b("Repositório: "),
                                 "fmdsocial/sim-sih-mortalidade-internacoes-0-6-anos — Felipe Delpino · Licença MIT."))))
    ),
  ),

  # ===================== DADOS =====================
  nav_panel(
    title = "Dados", icon = bs_icon("database"),
    div(class = "section-lead", "Tabelas executivas para auditoria e exportação (CSV/Excel)."),
    layout_columns(
      col_widths = c(3, 9),
      card(card_header("Seleção"),
           selectInput("data_source", "Base:",
                       choices = c("Mortalidade (SIM)" = "mort",
                                   "Internações (SIH-SUS)" = "int",
                                   "Dicionário CID-10 revisado" = "dic",
                                   "Denominadores por UF" = "den")),
           selectInput("sheet_sel", "Tabela:",
                       choices = c("1 — Evolução por idade" = "idade",
                                   "2 — Evolução por causas" = "causas",
                                   "3 — Priorização" = "prio",
                                   "4 — Fluxo por UF" = "fluxo_uf",
                                   "5 — Polos" = "polos",
                                   "6 — Fluxo inter-UF" = "inter_uf",
                                   "7 — Taxa por UF" = "taxa_uf",
                                   "8 — Taxa por macrorregião" = "taxa_macro"))),
      card(full_screen = TRUE,
           card_header(textOutput("tabela_titulo", inline = TRUE)),
           DTOutput("tabela_mestra")))
  ),

  nav_spacer(),
  nav_item(tags$span(style = "color:#6c757d;font-size:.78rem;padding-right:14px;font-weight:500;",
                     "INSPER")),

  footer = tags$footer(class = "app-footer",
                       "Observatório de Saúde Infantil — Brasil · ",
                       tags$span(style = "color:#004B87;font-weight:600;", "INSPER"),
                       " · Dados: DATASUS (SIM, SIH-SUS, SINASC) · ", PERIODO_FIXO)
)

# ----------------------------------------------------------------------------
# 6. SERVER
# ----------------------------------------------------------------------------
server <- function(input, output, session) {

  rng <- reactive({ req(input$filtro_ano); input$filtro_ano })
  n_anos_sel <- reactive(max(1L, rng()[2] - rng()[1] + 1L))
  # Fração do período total coberta pelo filtro — usada para escalar
  # denominadores acumulados de forma proporcional.
  frac_periodo <- reactive(n_anos_sel() / ANOS_PERIODO)

  CID_ADMIN <- cid_dic$cid[cid_dic$grupo == "Registros administrativos (não-diagnóstico)"]
  sem_admin <- function(df) {
    if (is.null(df) || !isTRUE(input$excluir_admin) || !"cid" %in% names(df)) return(df)
    df[!df$cid %in% CID_ADMIN, , drop = FALSE]
  }
  prio_cid_f <- function(fonte) sem_admin(tab_prio_cid(fonte))
  conc_f     <- function(fonte) sem_admin(tab_conc(fonte))

  has_idade_m <- !is.null(data_mort$idade) && "ANO" %in% names(data_mort$idade)
  has_idade_i <- !is.null(data_int$idade)  && "ANO" %in% names(data_int$idade)

  filt_idade_m  <- reactive(if (has_idade_m) data_mort$idade %>% filter(ANO >= rng()[1], ANO <= rng()[2]) else NULL)
  filt_idade_i  <- reactive(if (has_idade_i) data_int$idade  %>% filter(ANO >= rng()[1], ANO <= rng()[2]) else NULL)
  filt_causas_m <- reactive(if (!is.null(data_mort$causas)) data_mort$causas %>% filter(ANO >= rng()[1], ANO <= rng()[2]) else NULL)
  filt_causas_i <- reactive(if (!is.null(data_int$causas))  data_int$causas  %>% filter(ANO >= rng()[1], ANO <= rng()[2]) else NULL)

  reshape_idade <- function(df) {
    if (is.null(df)) return(NULL)
    if (identical(input$faixa_modo, "agg")) {
      neo <- intersect(c("Neonatal Precoce (0-6 dias)", "Neonatal Tardia (7-27 dias)",
                         "Pós-Neonatal (28 dias a <1 ano)"), names(df))
      col_16 <- names(df)[grepl("1 ?a ?6", names(df), ignore.case = TRUE)]
      if (length(neo) == 0 || length(col_16) == 0) return(df)
      df %>% transmute(ANO,
                       `Menores de 1 ano` = rowSums(across(all_of(neo)), na.rm = TRUE),
                       `1 a 6 Anos`       = rowSums(across(all_of(col_16)), na.rm = TRUE))
    } else df
  }

  sum_num <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(NA_real_)
    num <- df %>% select(-any_of("ANO")) %>% select(where(is.numeric))
    if (ncol(num) == 0) return(NA_real_)
    sum(as.matrix(num), na.rm = TRUE)
  }

  # ---- KPIs do Panorama ----
  output$kpi_mort_total <- renderText(fmt_br(sum_num(filt_idade_m())))
  output$kpi_int_total  <- renderText(fmt_br(sum_num(filt_idade_i())))

  var_periodo <- function(df) {
    if (is.null(df) || nrow(df) < 2) return("Período insuficiente")
    num <- df %>% select(where(is.numeric))
    ini <- sum(as.matrix(num[df$ANO == min(df$ANO), setdiff(names(num), "ANO"), drop = FALSE]), na.rm = TRUE)
    fim <- sum(as.matrix(num[df$ANO == max(df$ANO), setdiff(names(num), "ANO"), drop = FALSE]), na.rm = TRUE)
    if (!is.finite(ini) || ini == 0) return("—")
    v <- fim / ini - 1
    paste0(ifelse(v >= 0, "▲ +", "▼ "), fmt_pct(abs(v)), " no período")
  }
  output$kpi_mort_var <- renderText(var_periodo(filt_idade_m()))
  output$kpi_int_var  <- renderText(var_periodo(filt_idade_i()))

  # TMM5 = média das taxas anuais nacionais no período filtrado.
  output$kpi_taxa_mort <- renderText({
    d <- sim_nacano[sim_nacano$ano >= rng()[1] & sim_nacano$ano <= rng()[2], ]
    if (nrow(d) == 0) return("—")
    fmt_br(mean(d$u5mr, na.rm = TRUE), 1)
  })

  # Crianças de 1 a 6 anos somadas nos anos filtrados — só existe com o arquivo
  # oficial de população. Sem ele, nada é estimado: a taxa fica em branco.
  pa_sel <- reactive({
    ag <- pa_1a6_periodo(rng()[1], rng()[2])
    if (is.null(ag)) return(NA_real_)
    sum(ag$pa, na.rm = TRUE)
  })

  # Internações de 1 a 6 anos por 1.000 crianças da faixa (média anual).
  output$kpi_taxa_int <- renderText({
    s <- faixa_serie(data_int$idade, faixas$inf$regex, rng()[1], rng()[2])
    pa <- pa_sel()
    if (nrow(s) == 0 || !is.finite(pa) || pa == 0) return("—")
    fmt_br(sum(s$Valor, na.rm = TRUE) / pa * 1000, 1)
  })

  output$kpi_uf_critica <- renderText({
    d <- uf_rate_periodo("u5mr", rng()[1], rng()[2])
    d <- d[order(-d$val), ][1, ]
    paste0(d$uf, " (", fmt_br(d$val, 1), ")")
  })

  output$kpi_generico <- renderText({
    d <- prio_cid_f("mort")
    if (is.null(d) || nrow(d) == 0) return("—")
    fmt_pct(sum(d$n[d$generico == 1], na.rm = TRUE) / sum(d$n, na.rm = TRUE))
  })

  # ---- POR FAIXA ETÁRIA ----
  prio_faixa <- function(prio_df, regex) {
    if (is.null(prio_df) || !"GRUPO_ETARIO" %in% names(prio_df)) return(NULL)
    d <- prio_df %>% filter(grepl(regex, GRUPO_ETARIO, ignore.case = TRUE, perl = TRUE))
    if (nrow(d) == 0) return(NULL)
    # A faixa neonatal reúne precoce + tardia: consolida antes de plotar,
    # senão o gráfico repete as mesmas categorias duas vezes.
    d %>% group_by(CAUSA_PRIORITARIA) %>%
      summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
      mutate(Perc = n / sum(n))
  }

  build_faixa_outputs <- function(fx) {
    id <- fx$id; regex <- fx$regex

    serie_m <- reactive(faixa_serie(data_mort$idade, regex, rng()[1], rng()[2]))
    serie_i <- reactive(faixa_serie(data_int$idade,  regex, rng()[1], rng()[2]))

    output[[paste0("fx_", id, "_obitos")]] <- renderText({
      s <- serie_m(); if (nrow(s) == 0) return("—"); fmt_br(sum(s$Valor, na.rm = TRUE))
    })
    output[[paste0("fx_", id, "_share")]] <- renderText({
      s <- serie_m(); tot <- sum_num(filt_idade_m())
      if (nrow(s) == 0 || is.na(tot) || tot == 0) return("—")
      fmt_pct(sum(s$Valor, na.rm = TRUE) / tot)
    })
    output[[paste0("fx_", id, "_var")]] <- renderText({
      s <- serie_m()
      if (nrow(s) < 2) return("—")
      ini <- s$Valor[which.min(s$ANO)]; fim <- s$Valor[which.max(s$ANO)]
      if (!is.finite(ini) || ini == 0) return("—")
      v <- fim / ini - 1
      paste0(ifelse(v >= 0, "▲ +", "▼ "), fmt_pct(abs(v)))
    })
    # Taxa da faixa com o denominador correto (NV até 1 ano; crianças de 1 a 6).
    # Se o denominador real não estiver carregado, mostra "—" em vez de estimar.
    output[[paste0("fx_", id, "_taxa")]] <- renderText({
      s <- serie_m(); if (nrow(s) == 0) return("—")
      den <- if (identical(fx$denom, "pop")) pa_sel()
             else sum(denom_uf$nv_periodo, na.rm = TRUE) * frac_periodo()
      if (!is.finite(den) || den == 0) return("—")
      fmt_br(sum(s$Valor, na.rm = TRUE) / den * 1000, 2)
    })

    output[[paste0("fx_", id, "_serie")]] <- renderPlotly({
      s <- serie_m(); si <- serie_i()
      if (nrow(s) == 0) return(empty_plot("Sem colunas correspondentes na aba 'idade'"))
      p <- plot_ly()
      if (nrow(si) > 0)
        p <- p %>% add_trace(data = si, x = ~ANO, y = ~Valor, yaxis = "y2",
                             name = "Internações (dir.)", type = "scatter", mode = "lines+markers",
                             line = list(color = pal$secondary, width = 2.6, shape = "spline"),
                             marker = list(color = pal$secondary, size = 7,
                                           line = list(color = "white", width = 1.4)),
                             hovertemplate = "<b>Internações</b><br>Ano %{x}: %{y:,.0f}<extra></extra>")
      p %>% add_trace(data = s, x = ~ANO, y = ~Valor, name = "Óbitos (esq.)",
                      type = "scatter", mode = "lines+markers",
                      line = list(color = pal$primary, width = 3.2, shape = "spline"),
                      marker = list(color = pal$primary, size = 9,
                                    line = list(color = "white", width = 1.6)),
                      fill = "tozeroy", fillcolor = "rgba(0,75,135,0.10)",
                      hovertemplate = "<b>Óbitos</b><br>Ano %{x}: %{y:,.0f}<extra></extra>") %>%
        layout(xaxis = list(title = "", dtick = 1, showgrid = FALSE),
               yaxis = list(title = list(text = "Óbitos", font = list(color = pal$primary, size = 12)),
                            tickformat = ",.0f", gridcolor = "#EEF1F6",
                            tickfont = list(color = pal$primary)),
               yaxis2 = list(title = list(text = "Internações", font = list(color = pal$secondary, size = 12)),
                             overlaying = "y", side = "right", tickformat = ",.0f",
                             showgrid = FALSE, tickfont = list(color = pal$secondary)),
               hovermode = "x unified",
               plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
               font = list(family = "Inter", color = pal$gray_1, size = 13),
               legend = list(orientation = "h", x = .5, xanchor = "center", y = -.18),
               margin = list(l = 65, r = 65, t = 30, b = 60)) %>%
        config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d","select2d","autoScale2d"))
    })

    output[[paste0("fx_", id, "_prio")]] <- renderPlotly({
      d <- prio_faixa(data_mort$prio, regex)
      if (is.null(d)) return(empty_plot("Priorização indisponível para esta faixa"))
      d <- d %>% arrange(Perc) %>%
        mutate(CAUSA_PRIORITARIA = factor(CAUSA_PRIORITARIA, levels = unique(CAUSA_PRIORITARIA)))
      plot_ly(d, x = ~Perc, y = ~CAUSA_PRIORITARIA, type = "bar", orientation = "h",
              marker = list(color = unname(cor_prio[as.character(d$CAUSA_PRIORITARIA)]),
                            line = list(color = "white", width = 1)),
              text = ~paste0(fmt_br(100 * Perc, 1), "%"), textposition = "outside",
              textfont = list(size = 12, color = pal$dark),
              hovertemplate = "<b>%{y}</b><br>%{x:.1%} da faixa<br>n = %{customdata:,.0f}<extra></extra>",
              customdata = ~n) %>%
        layout(xaxis = list(title = "Proporção na faixa", tickformat = ".0%",
                            range = c(0, min(1, max(d$Perc, na.rm = TRUE) * 1.25))),
               yaxis = list(title = "", automargin = TRUE)) %>%
        clean_plotly(legend_pos = "none")
    })

    faixa_lbl <- unname(PAINEL_FX[[id]])
    output[[paste0("fx_", id, "_cid")]] <- renderPlotly({
      pr <- input[[paste0("fx_", id, "_causa")]]; req(pr)
      plot_cid_bar(faixa_lbl, pr, "mort", base = prio_cid_f("mort"))
    })
    output[[paste0("fx_", id, "_causa_pct")]] <- renderText({
      pr <- input[[paste0("fx_", id, "_causa")]]; req(pr)
      d <- prio_faixa(data_mort$prio, regex)
      if (is.null(d)) return("—")
      tot <- sum(d$n, na.rm = TRUE)
      v <- sum(d$n[d$CAUSA_PRIORITARIA == pr], na.rm = TRUE)
      if (!is.finite(tot) || tot == 0) "—" else fmt_pct(v / tot)
    })
    output[[paste0("fx_", id, "_sistema")]] <- renderPlotly(
      plot_sistema_bar("mort", faixa_lbl, "sistema", topn = 12, base = prio_cid_f("mort")))
    output[[paste0("fx_", id, "_grupo")]] <- renderPlotly(
      plot_sistema_bar("mort", faixa_lbl, "grupo_pat", topn = 12, base = prio_cid_f("mort")))
    output[[paste0("fx_", id, "_agrup")]] <- renderPlotly(plot_agrup_outras(faixa_lbl))
  }
  invisible(lapply(faixas, build_faixa_outputs))

  # ---- SISTEMAS & PATOLOGIAS ----
  sis_base <- reactive({
    req(input$sis_fonte, input$sis_faixa)
    b <- prio_cid_f(input$sis_fonte)
    if (!identical(input$sis_faixa, "Todas as faixas")) b <- b[b$faixa == input$sis_faixa, ]
    b
  })
  output$sis_kpi_k   <- renderText({ d <- sis_base(); if (nrow(d) == 0) "—" else as.character(dplyr::n_distinct(d$sistema)) })
  output$sis_kpi_grp <- renderText({ d <- sis_base(); if (nrow(d) == 0) "—" else as.character(dplyr::n_distinct(d$grupo_pat)) })
  output$sis_kpi_top <- renderText({
    d <- sis_base(); if (nrow(d) == 0) return("—")
    ag <- d %>% group_by(sistema) %>% summarise(n = sum(n), .groups = "drop") %>% arrange(desc(n))
    paste0(ag$sistema[1], " (", fmt_pct(ag$n[1] / sum(ag$n)), ")")
  })
  output$sis_kpi_gen <- renderText({
    d <- sis_base(); if (nrow(d) == 0) return("—")
    fmt_pct(sum(d$n[d$generico == 1], na.rm = TRUE) / sum(d$n, na.rm = TRUE))
  })
  output$sis_bar   <- renderPlotly(plot_sistema_bar(input$sis_fonte %||% "mort", NULL,
                                                    "sistema", 14, base = sis_base()))
  output$sis_grupo <- renderPlotly(plot_sistema_bar(input$sis_fonte %||% "mort", NULL,
                                                    "grupo_pat", 14, base = sis_base()))
  output$sis_tab <- renderDT({
    d <- sis_base()
    if (nrow(d) == 0) return(datatable(data.frame(Aviso = "Sem dados"), rownames = FALSE))
    tb <- d %>%
      transmute(CID = cid, Descrição = nome, Capítulo = cap_cid, Sistema = sistema,
                `Grupo de patologia` = grupo_pat, Especificidade = especificidade,
                Faixa = faixa, `Causa prioritária` = prio, Casos = n) %>%
      arrange(desc(Casos))
    datatable(tb, rownames = FALSE, class = "cell-border stripe hover compact",
              filter = "top", extensions = "Buttons",
              options = list(pageLength = 15, dom = "Bfrtip", scrollX = TRUE,
                             buttons = list(list(extend = "csv", text = "CSV"),
                                            list(extend = "excel", text = "Excel"))))
  })

  # ---- TRAJETÓRIA ----
  tj_dados <- reactive(traj_filtrada(rng()[1], rng()[2],
                                     input$tj_faixa %||% "Todas as faixas",
                                     input$tj_regiao %||% "Brasil"))
  output$tj_kpi_t0 <- renderText(fmt_br(nrow(tj_dados())))
  output$tj_kpi_t1 <- renderText({
    d <- tj_dados(); if (nrow(d) == 0) return("—")
    fmt_pct(mean(d$n_transferencias > 0, na.rm = TRUE))
  })
  output$tj_kpi_t2 <- renderText({
    d <- tj_dados(); if (nrow(d) == 0) return("—")
    fmt_br(sum(d$desfecho == "Óbito", na.rm = TRUE))
  })
  output$tj_kpi_dias <- renderText({
    d <- tj_dados(); if (nrow(d) == 0) return("—")
    fmt_br(stats::median(d$dias_internacao, na.rm = TRUE), 0)
  })
  output$tj_funil     <- renderPlotly(plot_trajetoria(tj_dados()))
  output$tj_qualidade <- renderPlotly(plot_qualidade(traj_filtrada(rng()[1], rng()[2],
                                                                   input$tj_faixa %||% "Todas as faixas",
                                                                   "Brasil")))

  # ---- TRANSIÇÃO DE CID ----
  tr_dados <- reactive(traj_filtrada(rng()[1], rng()[2],
                                     input$tr_faixa %||% "Todas as faixas",
                                     input$tr_regiao %||% "Brasil"))
  tr_link <- reactive({ d <- tr_dados(); d[!is.na(d$cid_obito), ] })

  output$tr_kpi_n <- renderText(fmt_br(nrow(tr_link())))
  output$tr_kpi_delta <- renderText({
    d <- tr_link(); if (nrow(d) == 0) return("—"); fmt_pct(mean(d$mudou_cid, na.rm = TRUE))
  })
  output$tr_kpi_sis <- renderText({
    d <- tr_link(); if (nrow(d) == 0) return("—"); fmt_pct(mean(d$mudou_sis, na.rm = TRUE))
  })
  output$tr_kpi_gg <- renderText({
    d <- tr_link(); if (nrow(d) == 0) return("—")
    fmt_pct(mean(d$transicao %in% TRANS_NUNCA_ESCLARECIDA))
  })
  output$tr_barras <- renderPlotly(plot_transicao_barras(tr_dados()))
  output$tr_sankey <- renderPlotly(plot_sankey_sistema(tr_dados()))
  output$tr_matriz <- renderPlotly(plot_matriz_cid(tr_dados()))
  output$tr_tab <- renderDT({
    d <- tr_link()
    if (nrow(d) == 0) return(datatable(data.frame(Aviso = "Sem pares no recorte"), rownames = FALSE))
    tb <- d %>%
      count(cid_entrada, nm_entrada, sis_entrada, cid_obito, nm_obito, sis_obito,
            transicao, name = "Casos") %>%
      arrange(desc(Casos)) %>%
      transmute(`CID entrada` = cid_entrada, `Descrição entrada` = nm_entrada,
                `Sistema entrada` = sis_entrada, `CID óbito` = cid_obito,
                `Descrição óbito` = nm_obito, `Sistema óbito` = sis_obito,
                Transição = transicao, Casos,
                `% dos linkados` = round(100 * Casos / nrow(d), 2))
    datatable(tb, rownames = FALSE, class = "cell-border stripe hover compact",
              filter = "top", extensions = "Buttons",
              options = list(pageLength = 15, dom = "Bfrtip", scrollX = TRUE,
                             buttons = list(list(extend = "csv", text = "CSV"),
                                            list(extend = "excel", text = "Excel"))))
  })

  # ---- Δ ENTRADA/ÓBITO POR REGIÃO ----
  dl_dados <- reactive(traj_filtrada(rng()[1], rng()[2],
                                     input$dl_faixa %||% "Todas as faixas", "Brasil"))
  output$dl_bar <- renderPlotly(plot_delta_regiao(dl_dados(), input$dl_metrica %||% "delta_cid"))
  output$dl_stack <- renderPlotly({
    d <- dl_dados(); d <- d[!is.na(d$cid_obito), ]
    if (nrow(d) == 0) return(empty_plot("Sem pares entrada→óbito no recorte"))
    r <- d %>% count(regiao, transicao, name = "n") %>%
      group_by(regiao) %>% mutate(pct = n / sum(n)) %>% ungroup() %>%
      mutate(regiao = factor(regiao, levels = REG_ORDEM),
             transicao = factor(transicao, levels = TRANS_NIVEIS))
    plot_ly(r, x = ~regiao, y = ~pct, color = ~transicao, colors = TRANS_COR,
            type = "bar",
            hovertemplate = "<b>%{fullData.name}</b><br>%{x}: %{y:.1%}<br>n = %{customdata:,.0f}<extra></extra>",
            customdata = ~n) %>%
      layout(barmode = "stack", xaxis = list(title = "Local (região)"),
             yaxis = list(title = "Composição", tickformat = ".0%", range = c(0, 1)),
             legend = list(orientation = "h", x = .5, xanchor = "center", y = -.32,
                           font = list(size = 10))) %>%
      clean_plotly(legend_pos = "bottom")
  })
  output$dl_tab <- renderDT({
    r <- delta_por_regiao(dl_dados())
    if (is.null(r)) return(datatable(data.frame(Aviso = "Sem dados"), rownames = FALSE))
    tb <- r %>% transmute(Região = as.character(regiao), `Óbitos linkados` = n,
                          `Δ CID (%)` = round(delta_cid, 1),
                          `Δ sistema (%)` = round(delta_sis, 1),
                          `Gen→Gen (%)` = round(gen_gen, 1),
                          `Esp→Gen (%)` = round(perda, 1),
                          `Gen→Esp (%)` = round(ganho, 1))
    datatable(tb, rownames = FALSE, class = "cell-border stripe hover compact",
              options = list(pageLength = 6, dom = "t", scrollX = TRUE))
  })

  # ---- CAUSAS SECUNDÁRIAS ----
  cs_dados <- reactive(traj_filtrada(rng()[1], rng()[2],
                                     input$cs_faixa %||% "Todas as faixas",
                                     input$cs_regiao %||% "Brasil"))
  output$cs_kpi_pct <- renderText({
    d <- cs_dados(); if (nrow(d) == 0) return("—")
    fmt_pct(mean(!is.na(d$cid_secundario)))
  })
  output$cs_kpi_sis <- renderText({
    d <- cs_dados(); d <- d[!is.na(d$cid_secundario), ]
    if (nrow(d) == 0) return("—")
    s <- cid_classify(d$cid_secundario)$sistema
    tb <- sort(table(s), decreasing = TRUE)
    paste0(names(tb)[1], " (", fmt_pct(tb[[1]] / sum(tb)), ")")
  })
  output$cs_bar <- renderPlotly(plot_secundarias(cs_dados()))

  # ---- 5f. LINKAGE v3 · QUALIDADE (estático, período completo do arquivo) --
  output$ql_kpi_alvo <- renderText({
    if (!QUAL_REAL) return("—")
    f <- qual$funil
    fmt_br(f$n[grepl("alvo", f$etapa)][1])
  })
  output$ql_kpi_par <- renderText({
    if (!QUAL_REAL) return("—")
    f <- qual$funil
    fmt_br(f$n[grepl("Pareados", f$etapa)][1])
  })
  output$ql_kpi_cob <- renderText({
    if (!QUAL_REAL) return("—")
    f <- qual$funil
    alvo <- f$n[grepl("alvo", f$etapa)][1]
    par  <- f$n[grepl("Pareados", f$etapa)][1]
    if (is.na(alvo) || alvo == 0) return("—")
    fmt_pct(par / alvo)
  })
  output$ql_kpi_exato <- renderText({
    if (!QUAL_REAL) return("—")
    f <- qual$funil
    par <- f$n[grepl("Pareados", f$etapa)][1]
    ex  <- f$n[grepl("exato", f$etapa)][1]
    if (is.na(par) || par == 0) return("—")
    paste0(fmt_br(ex), " (", fmt_pct(ex / par), ")")
  })
  output$ql_kpi_prob <- renderText({
    if (!QUAL_REAL) return("—")
    f <- qual$funil
    par <- f$n[grepl("Pareados", f$etapa)][1]
    pr  <- f$n[grepl("probabil", f$etapa)][1]
    if (is.na(par) || par == 0) return("—")
    paste0(fmt_br(pr), " (", fmt_pct(pr / par), ")")
  })
  output$ql_kpi_prev <- renderText({
    if (!QUAL_REAL) return("—")
    f <- qual$funil
    par  <- f$n[grepl("Pareados", f$etapa)][1]
    prev <- f$n[grepl("anterior", f$etapa)][1]
    if (is.na(par) || par == 0) return("—")
    fmt_pct(prev / par)
  })
  output$ql_funil <- renderPlotly(plot_funil_etapas())
  output$ql_recon <- renderDT({
    r <- tab_reconciliacao()
    if (is.null(r)) return(datatable(data.frame(aviso = MSG_SEM_V3),
                                     rownames = FALSE, options = list(dom = "t")))
    r$n <- fmt_br(r$n)
    datatable(r, rownames = FALSE, extensions = "Buttons",
              options = list(pageLength = 15, dom = "Bt", ordering = FALSE,
                             buttons = c("copy", "csv", "excel"),
                             columnDefs = list(list(className = "dt-right", targets = 2)))) %>%
      formatStyle("Trilha", fontWeight = "bold",
                  color = styleEqual(c("SIM \u2014 \u00f3bitos", "SIH \u2014 interna\u00e7\u00f5es"),
                                     c("#004B87", "#00A3A1")))
  })
  output$ql_sens  <- renderPlotly(plot_sens_janela())
  output$ql_ano   <- renderPlotly(plot_qual_ano())
  output$ql_uf    <- renderPlotly(plot_qual_uf())

  # ---- 5g. LINKAGE v3 · DESCRITIVA DOS LINKADOS ----------------------------
  dsc_dados <- reactive({
    d <- traj_filtrada(rng()[1], rng()[2],
                       input$dsc_faixa %||% "Todas as faixas",
                       input$dsc_regiao %||% "Brasil")
    sx <- input$dsc_sexo %||% "Ambos"
    if (sx != "Ambos") d <- d[!is.na(d$sexo) & d$sexo == sx, ]
    d
  })
  output$dsc_ano        <- renderPlotly(desc_por_ano(dsc_dados()))
  output$dsc_uf         <- renderPlotly(desc_por_uf(dsc_dados()))
  output$dsc_faixa_sexo <- renderPlotly(desc_faixa_sexo(dsc_dados()))
  output$dsc_tab <- renderDT({
    r <- desc_resumo_uf(dsc_dados())
    if (is.null(r)) return(datatable(data.frame(aviso = MSG_SEM_LINKAGE),
                                     rownames = FALSE, options = list(dom = "t")))
    datatable(r, rownames = FALSE, extensions = "Buttons",
              options = list(pageLength = 27, dom = "Bft", scrollY = "420px",
                             buttons = c("copy", "csv", "excel"))) %>%
      formatRound(c("% match exato", "% com internação anterior (30 d)",
                    "% com transferência"), 1) %>%
      formatRound("Permanência mediana (dias)", 0)
  })

  # ---- 5h. LINKAGE v3 · GUIA DE CIDs ---------------------------------------
  output$guia_tab <- renderDT({
    g <- guia_cids_tab(traj)
    names(g) <- c("CID", "Nome revisado", "Sistema do organismo",
                  "Grupo de patologia", "Capítulo", "Especificidade",
                  "n como CID de entrada (SIH)", "n como causa de óbito (SIM)",
                  "Nota da revisão")
    datatable(g, rownames = FALSE, filter = "top", extensions = "Buttons",
              options = list(pageLength = 15, dom = "Bfrtip", scrollX = TRUE,
                             buttons = c("copy", "csv", "excel"))) %>%
      formatStyle("Especificidade",
                  color = styleEqual(c("Genérico / mal definido", "Específico"),
                                     c(pal$alert, pal$primary)))
  })

  # ---- 5i. LINKAGE v3 · ETAPA 5 (internações sem óbito associado) ----------
  bx_dados <- reactive({
    if (!BAIXA_REAL) return(NULL)
    b <- baixa_raw
    b <- b[!is.na(b$ano) & b$ano >= rng()[1] & b$ano <= rng()[2], ]
    fx <- input$bx_faixa %||% "Todas as faixas"
    if (fx != "Todas as faixas") b <- b[!is.na(b$faixa) & b$faixa == fx, ]
    rg <- input$bx_regiao %||% "Brasil"
    if (rg != "Brasil") b <- b[!is.na(b$regiao) & b$regiao == rg, ]
    b
  })
  output$bx_kpi_n <- renderText({
    b <- bx_dados(); if (is.null(b) || nrow(b) == 0) return("—")
    fmt_br(sum(b$n))
  })
  output$bx_kpi_dias <- renderText({
    b <- bx_dados(); if (is.null(b) || nrow(b) == 0) return("—")
    fmt_br(stats::weighted.mean(b$dias_medio, w = b$n, na.rm = TRUE), 1)
  })
  output$bx_kpi_alta <- renderText({
    b <- bx_dados(); if (is.null(b) || nrow(b) == 0) return("—")
    fmt_pct(sum(b$n[b$desfecho == "Alta"], na.rm = TRUE) / sum(b$n))
  })
  output$bx_sistema <- renderPlotly({
    b <- bx_dados()
    if (is.null(b)) return(empty_plot(MSG_SEM_V3))
    baixa_sistema(b)
  })
  output$bx_tempo <- renderPlotly({
    b <- bx_dados()
    if (is.null(b)) return(empty_plot(MSG_SEM_V3))
    baixa_tempo(b)
  })
  output$bx_serie <- renderPlotly({
    b <- bx_dados()
    if (is.null(b)) return(empty_plot(MSG_SEM_V3))
    baixa_serie(b)
  })

  # ---- METAS ODS/IPEA ----
  output$meta_kpi_ind <- renderText(unname(IND_LABEL[input$meta_ind %||% "u5mr"]))
  output$meta_kpi_br  <- renderText({
    ind <- input$meta_ind %||% "u5mr"
    d <- sim_nacano[sim_nacano$ano >= rng()[1] & sim_nacano$ano <= rng()[2], ]
    if (nrow(d) == 0) return("—")
    fmt_br(mean(d[[ind]], na.rm = TRUE), 1)
  })
  output$meta_kpi_sdg <- renderText({
    ind <- input$meta_ind %||% "u5mr"
    d <- uf_rate_periodo(ind, rng()[1], rng()[2])
    paste0(sum(d$val <= META_SDG[[ind]], na.rm = TRUE), " de 27")
  })
  output$meta_kpi_ipea <- renderText({
    ind <- input$meta_ind %||% "u5mr"
    d <- uf_rate_periodo(ind, rng()[1], rng()[2])
    paste0(sum(d$val <= META_IPEA[[ind]], na.rm = TRUE), " de 27")
  })
  output$meta_estados <- renderPlotly(
    plot_estados_meta(input$meta_ind %||% "u5mr", rng()[1], rng()[2]))
  output$meta_mapa <- renderPlot({
    g <- map_macro_meta(input$meta_ind %||% "u5mr")
    if (is.null(g)) { plot.new(); text(.5, .5, "Malha indisponível."); return(invisible()) }
    g
  }, bg = "transparent")
  output$meta_evo <- renderPlotly(
    plot_evolucao_meta(input$meta_ind %||% "u5mr", input$meta_evo_uf, rng()[1], rng()[2]))
  output$meta_tabela <- renderDT({
    ind <- input$meta_ind %||% "u5mr"
    y0 <- rng()[1]; y1 <- rng()[2]
    d <- sim_ufano[sim_ufano$ano >= y0 & sim_ufano$ano <= y1, ] %>%
      group_by(sigla) %>%
      summarise(nmr = mean(nmr, na.rm = TRUE), imr = mean(imr, na.rm = TRUE),
                u5mr = mean(u5mr, na.rm = TRUE), .groups = "drop")
    d <- sim_uf %>% select(uf, sigla, regiao, nv) %>% left_join(d, by = "sigla")
    tb <- data.frame(d$uf, d$sigla, d$regiao, d$nv, d$nmr, d$imr, d$u5mr,
                     ifelse(d[[ind]] <= META_SDG[[ind]],  "Atinge", "Não atinge"),
                     ifelse(d[[ind]] <= META_IPEA[[ind]], "Atinge", "Não atinge"),
                     check.names = FALSE, stringsAsFactors = FALSE)
    names(tb) <- c("UF", "Sigla", "Região", "Nascidos vivos (período completo)",
                   "Neonatal", "Infantil", "Menores de 5", "Meta SDG (foco)", "Meta IPEA (foco)")
    ord <- c(nmr = "Neonatal", imr = "Infantil", u5mr = "Menores de 5")[[ind]]
    tb <- tb[order(tb[[ord]]), ]
    datatable(tb, rownames = FALSE, class = "cell-border stripe hover compact",
              extensions = "Buttons",
              options = list(pageLength = 27, dom = "Bt", scrollX = TRUE,
                             buttons = list(list(extend = "csv", text = "CSV"),
                                            list(extend = "excel", text = "Excel")))) %>%
      formatRound(c("Neonatal", "Infantil", "Menores de 5"), 1) %>%
      formatCurrency("Nascidos vivos (período completo)", currency = "", digits = 0, mark = ".")
  })

  # ---- CID: detalhamento ----
  cid_sel <- reactive({
    req(input$cid_faixa, input$cid_causa)
    base <- prio_cid_f(input$cid_fonte %||% "mort")
    d <- base[base$faixa == input$cid_faixa & base$prio == input$cid_causa, ]
    if (isTRUE(input$cid_so_gen)) d <- d[d$generico == 1, ]
    d
  })
  output$cid_kpi_n   <- renderText({ d <- cid_sel(); if (nrow(d) == 0) "—" else fmt_br(sum(d$n)) })
  output$cid_kpi_k   <- renderText({ d <- cid_sel(); as.character(nrow(d)) })
  output$cid_kpi_gen <- renderText({
    d <- cid_sel(); if (nrow(d) == 0) return("—")
    fmt_pct(sum(d$n[d$generico == 1], na.rm = TRUE) / sum(d$n, na.rm = TRUE))
  })
  output$cid_kpi_sis <- renderText({
    d <- cid_sel(); if (nrow(d) == 0) "—" else as.character(dplyr::n_distinct(d$sistema))
  })
  output$cid_bar <- renderPlotly({
    req(input$cid_faixa, input$cid_causa)
    fo <- input$cid_fonte %||% "mort"
    plot_cid_bar(input$cid_faixa, input$cid_causa, fo,
                 so_genericos = isTRUE(input$cid_so_gen), base = prio_cid_f(fo))
  })
  output$cid_tab <- renderDT({
    d <- cid_sel()
    if (nrow(d) == 0) return(datatable(data.frame(Aviso = "Sem dados"), rownames = FALSE))
    lbl <- unname(FONTE_TIT[input$cid_fonte %||% "mort"])
    tb <- data.frame(d$cid, d$nome, d$sistema, d$grupo_pat, d$especificidade, d$n, d$pct,
                     check.names = FALSE, stringsAsFactors = FALSE)
    names(tb) <- c("CID", "Descrição", "Sistema", "Grupo de patologia",
                   "Especificidade", lbl, "% da causa")
    tb <- tb[order(-tb[[lbl]]), ]
    datatable(tb, rownames = FALSE, class = "cell-border stripe hover compact",
              extensions = "Buttons",
              options = list(pageLength = 12, dom = "Bt", scrollX = TRUE,
                             buttons = list(list(extend = "csv", text = "CSV"),
                                            list(extend = "excel", text = "Excel")))) %>%
      formatRound("% da causa", 1)
  })

  # ---- CID: grandes capítulos ----
  observeEvent(input$cap_fonte, {
    ch <- if (identical(input$cap_fonte, "int"))
      c("Total 0-6 anos", "Neonatal (0–27 dias)", "Pós-neonatal (28 d–<1 ano)", "1 a 6 anos")
    else
      c("Total 0-6 anos", "Neonatal Precoce (0-6 dias)", "Neonatal Tardia (7-27 dias)",
        "Pós-Neonatal (28 dias a <1 ano)", "1 a 6 Anos")
    updateSelectInput(session, "cap_faixa", choices = ch)
  }, ignoreInit = TRUE)
  output$cap_bar <- renderPlotly({
    req(input$cap_faixa); plot_capitulo(input$cap_faixa, input$cap_fonte %||% "mort")
  })

  # ---- CID: concentração (Pareto) ----
  output$conc_kpi_dist <- renderText({
    fmt_br(unname(CID_DISTINTOS[[input$conc_fonte %||% "mort"]][input$conc_escopo %||% "Geral"]))
  })
  output$conc_kpi_cob <- renderText({
    d <- conc_f(input$conc_fonte %||% "mort")
    d <- d[d$escopo == (input$conc_escopo %||% "Geral"), ]
    if (nrow(d) == 0) "—" else paste0(fmt_br(max(d$cum), 1), "%")
  })
  output$conc_bar <- renderPlotly({
    fo <- input$conc_fonte %||% "mort"
    plot_concentracao(input$conc_escopo %||% "Geral", fo, base = conc_f(fo))
  })

  # ---- CID: revisão de nomenclatura ----
  output$rev_kpi_n    <- renderText(as.character(nrow(cid_dic)))
  output$rev_kpi_gen  <- renderText(paste0(sum(cid_dic$generico == 1), " de ", nrow(cid_dic)))
  output$rev_kpi_nota <- renderText(as.character(sum(nzchar(cid_dic$nota))))
  output$rev_notas <- renderUI({
    d <- cid_dic[nzchar(cid_dic$nota), ]
    if (nrow(d) == 0) return(tags$p("Nenhuma nota registrada."))
    tagList(lapply(seq_len(nrow(d)), function(i)
      div(class = "rev-item",
          tags$b(paste0(d$cid[i], " · ", d$nome[i])), tags$br(), d$nota[i])))
  })
  output$rev_tab <- renderDT({
    tb <- cid_dic %>%
      transmute(CID = cid, Descrição = nome, Capítulo = cap, `Nome do capítulo` = cap_nome,
                Sistema = sistema, `Grupo de patologia` = grupo,
                Especificidade = ifelse(generico == 1, "Genérico / mal definido", "Específico"),
                `Nota de revisão` = nota)
    datatable(tb, rownames = FALSE, class = "cell-border stripe hover compact",
              filter = "top", extensions = "Buttons",
              options = list(pageLength = 20, dom = "Bfrtip", scrollX = TRUE,
                             buttons = list(list(extend = "csv", text = "CSV"),
                                            list(extend = "excel", text = "Excel"))))
  })

  # ---- PANORAMA: tendência e macrorregião ----
  output$plot_tendencia_geral <- renderPlotly({
    dm <- filt_idade_m(); di <- filt_idade_i()
    if (is.null(dm) || is.null(di) || nrow(dm) == 0) return(empty_plot())
    df_m <- dm %>% transmute(ANO, Total = rowSums(across(-ANO), na.rm = TRUE))
    df_i <- di %>% transmute(ANO, Total = rowSums(across(-ANO), na.rm = TRUE))
    plot_ly() %>%
      add_trace(data = df_i, x = ~ANO, y = ~Total, yaxis = "y2",
                name = "Internações (dir.)", type = "scatter", mode = "lines+markers",
                line = list(color = pal$secondary, width = 3, shape = "spline"),
                marker = list(color = pal$secondary, size = 9, line = list(color = "white", width = 1.5)),
                fill = "tozeroy", fillcolor = "rgba(0,163,161,0.12)",
                hovertemplate = "<b>Internações</b><br>Ano %{x}: %{y:,.0f}<extra></extra>") %>%
      add_trace(data = df_m, x = ~ANO, y = ~Total, name = "Óbitos (esq.)",
                type = "scatter", mode = "lines+markers",
                line = list(color = pal$primary, width = 3.5, shape = "spline"),
                marker = list(color = pal$primary, size = 10, line = list(color = "white", width = 2)),
                hovertemplate = "<b>Óbitos</b><br>Ano %{x}: %{y:,.0f}<extra></extra>") %>%
      layout(xaxis = list(title = "", dtick = 1, showgrid = FALSE, zeroline = FALSE),
             yaxis = list(title = list(text = "Óbitos", font = list(color = pal$primary, size = 12)),
                          tickformat = ",.0f", gridcolor = "#EEF1F6",
                          tickfont = list(color = pal$primary), zeroline = FALSE),
             yaxis2 = list(title = list(text = "Internações", font = list(color = pal$secondary, size = 12)),
                           overlaying = "y", side = "right", tickformat = ",.0f",
                           showgrid = FALSE, tickfont = list(color = pal$secondary), zeroline = FALSE),
             hovermode = "x unified",
             plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
             font = list(family = "Inter", color = pal$gray_1, size = 13),
             legend = list(orientation = "h", x = .5, xanchor = "center", y = -.18),
             margin = list(l = 70, r = 70, t = 30, b = 60)) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d","select2d","autoScale2d"))
  })

  output$plot_macro_geral <- renderPlotly({
    df <- taxa_macro_coorte("mort")
    if (is.null(df) || !nrow(df)) return(empty_plot())
    df <- df %>% arrange(taxa) %>%
      mutate(MACRORREGIAO = factor(MACRORREGIAO, levels = MACRORREGIAO),
             taxa_macro = taxa, nascidos_vivos = denom, obitos_total = total,
             cor_grad = colorRampPalette(c("#7FB3D5", pal$primary, pal$alert))(dplyr::n())[rank(taxa)],
             rotulo = paste0(format(round(taxa, 1), decimal.mark = ","), " /1.000"))
    plot_ly(df, x = ~taxa_macro, y = ~MACRORREGIAO, type = "bar", orientation = "h",
            marker = list(color = ~cor_grad, line = list(color = "white", width = 1)),
            text = ~rotulo, textposition = "outside",
            textfont = list(size = 13, color = pal$dark, family = "Inter"),
            hovertemplate = paste0("<b>%{y}</b><br>Taxa: %{x:.1f}<br>",
                                   "Óbitos: %{customdata[0]:,.0f}<br>Denominador: %{customdata[1]:,.0f}<extra></extra>"),
            customdata = ~cbind(obitos_total, nascidos_vivos)) %>%
      layout(xaxis = list(title = paste0("Óbitos ", df$unidade[1]), gridcolor = "#EEF1F6", zeroline = FALSE,
                          range = c(0, max(df$taxa_macro, na.rm = TRUE) * 1.18)),
             yaxis = list(title = "", tickfont = list(size = 13, color = pal$dark)),
             plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
             font = list(family = "Inter", color = pal$gray_1, size = 13),
             showlegend = FALSE, margin = list(l = 100, r = 50, t = 20, b = 55)) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("lasso2d","select2d","autoScale2d","zoom2d","pan2d"))
  })

  # ---- PANORAMA: mapa + ranking ----
  dados_uf <- reactive({
    req(input$mapa_metrica)
    d <- taxa_uf_coorte(input$mapa_metrica)
    req(!is.null(d))
    lbl <- if (identical(input$mapa_metrica, "mort")) "Óbitos" else "Internações"
    d %>% transmute(uf = NOME_ESTADO, valor = taxa, total = total,
                    rotulo = paste0(lbl, " ", unidade))
  })

  output$plot_mapa_uf <- renderPlot({
    if (!map_ready) { plot.new(); text(.5, .5, "Malha geográfica indisponível.", cex = 1.2); return(invisible()) }
    d <- taxa_uf_coorte(input$mapa_metrica); req(!is.null(d))
    lbl <- if (identical(input$mapa_metrica, "mort")) "Óbitos" else "Internações"
    lo <- if (identical(input$mapa_metrica, "mort")) pal$map_mort_lo else pal$map_int_lo
    hi <- if (identical(input$mapa_metrica, "mort")) pal$map_mort_hi else pal$map_int_hi
    gg_choropleth(join_uf_coorte(d), lo, hi, "Taxa por estado (0 a 6 anos)",
                  paste0(lbl, " ", d$unidade[1], " · acumulado ", PERIODO_FIXO),
                  leg = d$unidade[1])
  }, bg = "transparent")

  output$plot_barras_uf <- renderPlotly({
    d <- dados_uf()
    if (is.null(d) || nrow(d) == 0) return(empty_plot())
    d <- d %>% arrange(valor) %>% mutate(uf = factor(uf, levels = uf))
    cor_base <- if (input$mapa_metrica == "mort") "#E6550D" else "#2171B5"
    cor_max  <- if (input$mapa_metrica == "mort") "#A63603" else "#08306B"
    d$cor <- colorRampPalette(c(pal$gray_3, cor_base, cor_max))(nrow(d))[rank(d$valor)]
    plot_ly(d, x = ~valor, y = ~uf, type = "bar", orientation = "h",
            marker = list(color = ~cor, line = list(color = "white", width = .5)),
            text = ~format(round(valor, 1), decimal.mark = ",", big.mark = "."),
            textposition = "outside", textfont = list(size = 10, color = pal$dark, family = "Inter"),
            hovertemplate = "<b>%{y}</b><br>Taxa: %{x:.1f}<br>Total: %{customdata:,.0f}<extra></extra>",
            customdata = ~total) %>%
      layout(xaxis = list(title = unique(d$rotulo), gridcolor = "#EEF1F6", zeroline = FALSE,
                          range = c(0, max(d$valor, na.rm = TRUE) * 1.18)),
             yaxis = list(title = "", tickfont = list(size = 10, color = pal$dark), automargin = TRUE),
             plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
             font = list(family = "Inter", color = pal$gray_1, size = 12),
             showlegend = FALSE, margin = list(l = 130, r = 60, t = 20, b = 55)) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("lasso2d","select2d","autoScale2d","zoom2d","pan2d"))
  })

  # ---- Renderizadores genéricos ----
  render_age <- function(df_r) {
    renderPlotly({
      dfi <- reshape_idade(df_r())
      if (is.null(dfi) || nrow(dfi) == 0) return(empty_plot())
      df <- dfi %>% pivot_longer(-ANO, names_to = "Faixa", values_to = "Valor")
      cores <- c("#004B87", "#00A3A1", "#F4A261", "#D9534F", "#6C757D")[seq_len(dplyr::n_distinct(df$Faixa))]
      plot_ly(df, x = ~ANO, y = ~Valor, color = ~Faixa, colors = cores,
              type = "scatter", mode = "lines+markers",
              line = list(width = 2.5), marker = list(size = 7),
              hovertemplate = "<b>%{fullData.name}</b><br>Ano %{x}: %{y:,.0f}<extra></extra>") %>%
        layout(xaxis = list(title = "", dtick = 1),
               yaxis = list(title = "", tickformat = ",.0f")) %>%
        clean_plotly()
    })
  }

  # Empilhado sempre proporcional (100%); absoluto apenas no hover.
  render_causes <- function(df_r) {
    renderPlotly({
      if (is.null(df_r()) || nrow(df_r()) == 0) return(empty_plot())
      df <- df_r() %>% pivot_longer(-ANO, names_to = "Causa", values_to = "Valor") %>%
        group_by(ANO) %>% mutate(Total = sum(Valor, na.rm = TRUE),
                                 Prop = ifelse(Total > 0, Valor / Total, 0)) %>% ungroup()
      cores <- c("#00A3A1", "#F4A261", "#6C757D", "#D9534F", "#8E44AD", "#F1C40F")[seq_len(dplyr::n_distinct(df$Causa))]
      plot_ly(df, x = ~ANO, y = ~Prop, color = ~Causa, colors = cores,
              type = "scatter", mode = "none", stackgroup = "one",
              hovertemplate = "<b>%{fullData.name}</b><br>Ano %{x}: %{y:.1%}<br>n = %{customdata:,.0f}<extra></extra>",
              customdata = ~Valor) %>%
        layout(xaxis = list(title = "", dtick = 1),
               yaxis = list(title = "Composição", tickformat = ".0%", range = c(0, 1))) %>%
        clean_plotly()
    })
  }

  render_prio <- function(df) {
    renderPlotly({
      if (is.null(df) || !all(c("GRUPO_ETARIO", "Perc", "CAUSA_PRIORITARIA") %in% names(df)))
        return(empty_plot())
      d <- df %>% group_by(GRUPO_ETARIO) %>%
        mutate(Perc = n / sum(n, na.rm = TRUE),
               ord = sum(Perc * (CAUSA_PRIORITARIA == "Ação Prioritária (Prevenção/Tratamento)"), na.rm = TRUE)) %>%
        ungroup() %>% arrange(ord) %>%
        mutate(GRUPO_ETARIO = factor(GRUPO_ETARIO, levels = unique(GRUPO_ETARIO)))
      plot_ly(d, x = ~Perc, y = ~GRUPO_ETARIO, color = ~CAUSA_PRIORITARIA, colors = cor_prio,
              type = "bar", orientation = "h",
              text = ~ifelse(Perc >= .05, paste0(fmt_br(100 * Perc, 1), "%"), ""),
              textposition = "inside", insidetextfont = list(color = "white", size = 11),
              hovertemplate = "<b>%{fullData.name}</b><br>%{y}<br>%{x:.1%} (n=%{customdata:,.0f})<extra></extra>",
              customdata = ~n) %>%
        layout(barmode = "stack",
               xaxis = list(title = "Composição da faixa", tickformat = ".0%", range = c(0, 1)),
               yaxis = list(title = "", automargin = TRUE)) %>%
        clean_plotly()
    })
  }

  render_evasao <- function(df, cor, tipo_r) {
    renderPlotly({
      if (is.null(df)) return(empty_plot())
      tipo <- tipo_r() %||% "mun"
      pcol <- if (tipo == "uf") "perc_fora_uf" else "perc_fora_mun"
      ncol <- if (tipo == "uf") "fora_uf" else "fora_mun"
      lab  <- if (tipo == "uf") "% fora da UF" else "% fora do município"
      if (!pcol %in% names(df)) return(empty_plot("Coluna de evasão indisponível"))
      d <- df %>% arrange(desc(.data[[pcol]])) %>% slice_head(n = 27) %>%
        arrange(.data[[pcol]]) %>% mutate(NOME_ESTADO = factor(NOME_ESTADO, levels = NOME_ESTADO))
      d$val <- d[[pcol]]
      d$cas <- if (ncol %in% names(d)) d[[ncol]] else NA_real_
      plot_ly(d, x = ~val, y = ~NOME_ESTADO, type = "bar", orientation = "h",
              marker = list(color = cor, line = list(color = "white", width = .5)),
              text = ~paste0(fmt_br(100 * val, 1), "%"), textposition = "outside",
              textfont = list(size = 10, color = pal$dark),
              hovertemplate = paste0("<b>%{y}</b><br>", lab,
                                     ": %{x:.1%}<br>Casos: %{customdata:,.0f}<extra></extra>"),
              customdata = ~cas) %>%
        layout(xaxis = list(title = lab, tickformat = ".0%",
                            range = c(0, max(d$val, na.rm = TRUE) * 1.18)),
               yaxis = list(title = "", tickfont = list(size = 10))) %>%
        clean_plotly(legend_pos = "none")
    })
  }

  render_polos <- function(df, cor, col) {
    renderPlotly({
      if (is.null(df) || !col %in% names(df) || !"label" %in% names(df)) return(empty_plot())
      d <- df %>% arrange(desc(.data[[col]])) %>% slice_head(n = 30) %>%
        arrange(.data[[col]]) %>% mutate(label = factor(label, levels = label))
      d$val <- d[[col]]
      plot_ly(d, x = ~val, y = ~label, type = "bar", orientation = "h",
              marker = list(color = cor, line = list(color = "white", width = .5)),
              hovertemplate = "<b>%{y}</b><br>Casos recebidos: %{x:,.0f}<extra></extra>") %>%
        layout(xaxis = list(title = "Casos recebidos (não residentes)", tickformat = ",.0f"),
               yaxis = list(title = "", tickfont = list(size = 9.5))) %>%
        clean_plotly(legend_pos = "none")
    })
  }

  render_heatuf <- function(df, col, escala) {
    renderPlotly({
      if (is.null(df) || !all(c("NM_RES", "NM_OCOR", col) %in% names(df))) return(empty_plot())
      d <- df; d$v <- d[[col]]
      # Ordena por volume total de fluxo, não por número de aparições.
      vol <- d %>% tidyr::pivot_longer(c(NM_RES, NM_OCOR), values_to = "uf") %>%
        group_by(uf) %>% summarise(tot = sum(v, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(tot))
      ord <- vol$uf
      d <- d %>% mutate(NM_RES = factor(NM_RES, levels = rev(ord)),
                        NM_OCOR = factor(NM_OCOR, levels = ord))
      plot_ly(d, x = ~NM_OCOR, y = ~NM_RES, z = ~v, type = "heatmap",
              colorscale = escala, reversescale = FALSE,
              hovertemplate = "<b>%{y} → %{x}</b><br>Casos: %{z:,.0f}<extra></extra>",
              colorbar = list(title = "Casos", thickness = 12)) %>%
        layout(xaxis = list(title = "UF de ocorrência", tickangle = -45,
                            tickfont = list(size = 10), showgrid = FALSE),
               yaxis = list(title = "UF de residência", tickfont = list(size = 10), showgrid = FALSE),
               plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
               font = list(family = "Inter", color = pal$gray_1, size = 12),
               margin = list(l = 130, r = 20, t = 20, b = 110)) %>%
        config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d","select2d","autoScale2d"))
    })
  }

  # ---- Saídas: MORTALIDADE ----
  output$mort_plot_idade  <- render_age(filt_idade_m)
  output$mort_plot_causas <- render_causes(filt_causas_m)
  output$mort_plot_prio   <- render_prio(data_mort$prio)
  output$mort_plot_evasao <- render_evasao(data_mort$fluxo_uf, pal$alert, reactive(input$mort_evasao_tipo))
  output$mort_plot_polos  <- render_polos(data_mort$polos, pal$primary, col_polos_mort)
  output$mort_plot_heatuf <- render_heatuf(data_mort$inter_uf, col_iuf_mort,
                                           list(list(0, "#FFF3D6"), list(.5, "#F4A261"), list(1, "#7F0000")))
  output$mort_mapa_uf <- renderPlot({
    d <- taxa_uf_coorte("mort")
    if (!map_ready || is.null(d)) { plot.new(); text(.5, .5, "Malha indisponível."); return(invisible()) }
    gg_choropleth(join_uf_coorte(d), pal$map_mort_lo, pal$map_mort_hi,
                  "Mortalidade por estado (0 a 6 anos)",
                  paste0("Óbitos ", d$unidade[1], " · acumulado ", PERIODO_FIXO), leg = d$unidade[1])
  }, bg = "transparent")
  output$mort_mapa_macro <- renderPlot({
    d <- taxa_macro_coorte("mort")
    if (!map_ready || is.null(d)) { plot.new(); text(.5, .5, "Malha indisponível."); return(invisible()) }
    gg_choropleth(join_macro_coorte(d), pal$map_mort_lo, pal$map_mort_hi,
                  "Mortalidade por macrorregião (0 a 6 anos)",
                  paste0("Óbitos ", d$unidade[1], " · acumulado ", PERIODO_FIXO), leg = d$unidade[1])
  }, bg = "transparent")

  # ---- Saídas: INTERNAÇÕES ----
  output$int_plot_idade  <- render_age(filt_idade_i)
  output$int_plot_causas <- render_causes(filt_causas_i)
  output$int_plot_prio   <- render_prio(data_int$prio)
  output$int_plot_evasao <- render_evasao(data_int$fluxo_uf, pal$secondary, reactive(input$int_evasao_tipo))
  output$int_plot_polos  <- render_polos(data_int$polos, pal$secondary, col_polos_int)
  output$int_plot_heatuf <- render_heatuf(data_int$inter_uf, col_iuf_int,
                                          list(list(0, "#EAF3FB"), list(.5, "#4292C6"), list(1, "#08306B")))
  output$int_mapa_uf <- renderPlot({
    d <- taxa_uf_coorte("int")
    if (!map_ready || is.null(d)) { plot.new(); text(.5, .5, "Malha indisponível."); return(invisible()) }
    gg_choropleth(join_uf_coorte(d), pal$map_int_lo, pal$map_int_hi,
                  "Internações por estado (0 a 6 anos)",
                  paste0("Internações ", d$unidade[1], " · acumulado ", PERIODO_FIXO), leg = d$unidade[1])
  }, bg = "transparent")
  output$int_mapa_macro <- renderPlot({
    d <- taxa_macro_coorte("int")
    if (!map_ready || is.null(d)) { plot.new(); text(.5, .5, "Malha indisponível."); return(invisible()) }
    gg_choropleth(join_macro_coorte(d), pal$map_int_lo, pal$map_int_hi,
                  "Internações por macrorregião (0 a 6 anos)",
                  paste0("Internações ", d$unidade[1], " · acumulado ", PERIODO_FIXO), leg = d$unidade[1])
  }, bg = "transparent")

  # ---- DADOS ----
  observeEvent(input$data_source, {
    if (input$data_source %in% c("mort", "int")) {
      updateSelectInput(session, "sheet_sel",
                        choices = c("1 — Evolução por idade" = "idade",
                                    "2 — Evolução por causas" = "causas",
                                    "3 — Priorização" = "prio",
                                    "4 — Fluxo por UF" = "fluxo_uf",
                                    "5 — Polos" = "polos",
                                    "6 — Fluxo inter-UF" = "inter_uf",
                                    "7 — Taxa por UF" = "taxa_uf",
                                    "8 — Taxa por macrorregião" = "taxa_macro"))
    } else if (identical(input$data_source, "dic")) {
      updateSelectInput(session, "sheet_sel",
                        choices = c("Dicionário CID-10 revisado" = "dicionario",
                                    "CIDs por faixa e causa (SIM)" = "prio_sim",
                                    "CIDs por faixa e causa (SIH)" = "prio_sih"))
    } else {
      updateSelectInput(session, "sheet_sel",
                        choices = c("Denominadores por UF" = "den_uf",
                                    "Denominadores por macrorregião" = "den_macro"))
    }
  }, ignoreInit = FALSE)

  tabela_sel <- reactive({
    req(input$data_source, input$sheet_sel)
    if (input$data_source %in% c("mort", "int")) {
      base <- if (input$data_source == "mort") data_mort else data_int
      df <- base[[input$sheet_sel]]
      if (is.null(df)) return(data.frame(Aviso = "Tabela indisponível nesta base."))
      if (input$sheet_sel %in% c("idade", "causas") && "ANO" %in% names(df))
        df <- df %>% filter(ANO >= rng()[1], ANO <= rng()[2])
      return(df)
    }
    switch(input$sheet_sel,
           dicionario = cid_dic %>% transmute(CID = cid, Descrição = nome, Capítulo = cap,
                                              Sistema = sistema, `Grupo de patologia` = grupo,
                                              Genérico = ifelse(generico == 1, "Sim", "Não"),
                                              `Nota de revisão` = nota),
           prio_sim   = sim_prio_cid,
           prio_sih   = sih_prio_cid,
           den_uf     = denom_uf %>% transmute(Sigla = sigla, UF = uf, Região = regiao,
                                               `Nascidos vivos (SINASC, período)` = round(nv_periodo),
                                               `TMI (/1.000 NV)` = round(imr, 2),
                                               `Crianças de 1 a 6 anos, soma dos anos (IBGE)` = round(pa_1a6)),
           den_macro  = denom_macro %>% transmute(Região = regiao,
                                                  `Nascidos vivos (SINASC, período)` = round(nv_periodo),
                                                  `Crianças de 1 a 6 anos, soma dos anos (IBGE)` = round(pa_1a6)),
           data.frame(Aviso = "Selecione uma tabela."))
  })

  output$tabela_titulo <- renderText({
    if (input$data_source %in% c("mort", "int")) {
      f <- if (input$data_source == "mort") "Mortalidade (SIM)" else "Internações (SIH-SUS)"
      a <- c("idade" = "Evolução por idade", "causas" = "Evolução por causas",
             "prio" = "Priorização", "fluxo_uf" = "Fluxo por UF", "polos" = "Polos",
             "inter_uf" = "Fluxo inter-UF", "taxa_uf" = "Taxa por UF",
             "taxa_macro" = "Taxa por macrorregião")[input$sheet_sel]
      paste0(f, " · ", a %||% "")
    } else if (identical(input$data_source, "dic")) {
      "Classificação CID-10 revisada"
    } else {
      paste0("Denominadores · ", POP_FONTE)
    }
  })

  output$tabela_mestra <- renderDT({
    datatable(tabela_sel(), extensions = "Buttons",
              options = list(pageLength = 15, scrollX = TRUE, dom = "Bfrtip",
                             buttons = list(list(extend = "copy", text = "Copiar"),
                                            list(extend = "csv", text = "CSV"),
                                            list(extend = "excel", text = "Excel")),
                             language = list(search = "Filtrar:",
                                             lengthMenu = "Mostrar _MENU_ registros",
                                             info = "Mostrando _START_ a _END_ de _TOTAL_",
                                             paginate = list(previous = "Anterior", `next` = "Próximo"))),
              rownames = FALSE, class = "cell-border stripe hover compact")
  })
}

shinyApp(ui, server)
