# =============================================================================
# 50_coorte_nascidos_vivos_sim_sih.R — TODOS OS NASCIDOS VIVOS × SIM × SIH
# Observatório de Saúde Infantil | INSPER · Hospital Pequeno Príncipe
#
# Pedido da Isa (12/11/2026): todos os nascidos vivos do SINASC 2015-2024,
# inclusive os que não morreram, e para os óbitos neonatais (0-27 dias) o
# cruzamento SINASC × SIM (e AIH, quando houver).
#
# Uma linha por nascido vivo. Traz:
#   - variáveis do SINASC (harmonizadas como no 20_) + estabelecimento do
#     nascimento (est_nasc_: tipo, natureza jurídica, quem administra, gestão, leitos)
#   - obito_0_6anos / obito_neonatal / obito_neonatal_precoce
#   - para os óbitos neonatais: todas as colunas do Banco 1 (SIM completo sim_,
#     AIH do óbito aih_obito_, episódio ep_, internações anteriores ant_,
#     estabelecimento do óbito est_obito_ e da AIH est_aih_)
#   - para óbitos de 28 dias a 6 anos: id_sim, data, idade e causa básica
#
# ENTRADAS
#   SINASC/Dados/sinasc_BR_AAAA.rds (id_nasc = NV+ano+BR+linha, como no 20_)
#   bases_finais/obitos_linkados/obitos_0_6_linkados_AAAA.csv.gz (20_)
#   bases_finais/banco1_obitos_neonatais/banco1_obitos_neonatais_AAAA.csv.gz (49_)
#   Dash/dados/cnes_bruto/ST_UF_2024.rds e ST_UF_2015.rds, _cadger.rds; Dash/estabelecimentos_dash.csv
# SAÍDA
#   bases_finais/banco1_coorte_nascidos_vivos/coorte_nascidos_vivos_AAAA_REGIAO.csv.gz
#   (ano de nascimento × região de residência; Sudeste e Nordeste em 2 arquivos cada)
# =============================================================================
suppressPackageStartupMessages(library(data.table))
raiz <- c(Sys.getenv("INSPER_RAIZ"), "G:/My Drive/INSPER/Trabalho")
raiz <- raiz[nzchar(raiz) & dir.exists(raiz)][1]
dir_bf <- Sys.getenv("INSPER_BF", file.path(raiz, "Dash/universo_sus/bases_finais"))
dir_ob <- Sys.getenv("INSPER_OBITOS", file.path(dir_bf, "obitos_linkados"))
dir_b1 <- Sys.getenv("INSPER_B1", file.path(dir_bf, "banco1_obitos_neonatais"))
od <- file.path(Sys.getenv("INSPER_OUT", dir_bf), "banco1_coorte_nascidos_vivos")
dir.create(od, recursive = TRUE, showWarnings = FALSE)

data_dm <- function(x) as.IDate(as.character(x), format = "%d%m%Y")
cnes7 <- function(x) { x <- gsub("[^0-9]", "", as.character(x)); x[x == ""] <- NA
  ifelse(is.na(x), NA_character_, formatC(x, width = 7, flag = "0")) }
mun6 <- function(x) { x <- substr(gsub("[^0-9]", "", as.character(x)), 1, 6); x[x == ""] <- NA; x }
num <- function(x) suppressWarnings(as.integer(as.character(x)))
sexo_12 <- function(x) fcase(as.character(x) %chin% c("1","M"), "M", as.character(x) %chin% c("2","F"), "F", default = NA_character_)
raca_12 <- function(x) fcase(x == "1","Branca", x == "2","Preta", x == "3","Amarela", x == "4","Parda", x == "5","Indigena", default = NA_character_)
col <- function(d, v) if (v %in% names(d)) d[[v]] else rep(NA, nrow(d))
chr <- function(d) { for (v in names(d)) if (is.factor(d[[v]])) set(d, j = v, value = as.character(d[[v]])); d }
esc5 <- function(escmae, esc2010) { a <- num(escmae); a[!a %in% 1:5] <- NA
  b <- num(esc2010); b <- c(1L, 2L, 3L, 4L, 5L, 5L)[b + 1L]; fcoalesce(a, b) }
UF_COD <- c("11"="RO","12"="AC","13"="AM","14"="RR","15"="PA","16"="AP","17"="TO",
            "21"="MA","22"="PI","23"="CE","24"="RN","25"="PB","26"="PE","27"="AL",
            "28"="SE","29"="BA","31"="MG","32"="ES","33"="RJ","35"="SP","41"="PR",
            "42"="SC","43"="RS","50"="MS","51"="MT","52"="GO","53"="DF")
UF_REG <- c(RO="Norte",AC="Norte",AM="Norte",RR="Norte",PA="Norte",AP="Norte",TO="Norte",
            MA="Nordeste",PI="Nordeste",CE="Nordeste",RN="Nordeste",PB="Nordeste",PE="Nordeste",
            AL="Nordeste",SE="Nordeste",BA="Nordeste",MG="Sudeste",ES="Sudeste",RJ="Sudeste",SP="Sudeste",
            PR="Sul",SC="Sul",RS="Sul",MS="CentroOeste",MT="CentroOeste",GO="CentroOeste",DF="CentroOeste")

# ---- 1. Estabelecimentos: tipo, natureza jurídica, quem administra, gestão --
fst <- list.files(file.path(raiz, "Dash/dados/cnes_bruto"), "^ST_[A-Z]{2}_(2024|2015)\\.rds$", full.names = TRUE)
st <- rbindlist(lapply(fst, function(f) chr(as.data.table(readRDS(f)))[, .(CNES, TP_UNID, NAT_JUR = if ("NAT_JUR" %in% names(.SD)) NAT_JUR else NA_character_,
  ESFERA_A, TPGESTAO, TP_PREST, VINC_SUS, PF_PJ, NIV_DEP, CLIENTEL, NIV_HIER, COMPETEN = as.character(COMPETEN))]), fill = TRUE)
st <- st[,.(codestab = cnes7(CNES), tp_unid = TP_UNID, nat_jur = NAT_JUR, esfera_adm = ESFERA_A,
             tp_gestao = TPGESTAO, tp_prest = TP_PREST, vinc_sus = VINC_SUS, pf_pj = PF_PJ,
             niv_dep = NIV_DEP, clientela = CLIENTEL, niv_hier = NIV_HIER, competencia_cnes = COMPETEN)]
setorder(st, codestab, -competencia_cnes); st <- unique(st, by = "codestab")
NJ <- c("1015"="Orgao publico federal","1023"="Orgao publico estadual/DF","1031"="Orgao publico municipal",
  "1040"="Orgao publico legislativo federal","1104"="Autarquia federal","1112"="Autarquia estadual/DF",
  "1120"="Autarquia municipal","1139"="Fundacao publica dir. publico federal","1147"="Fundacao publica dir. publico estadual/DF",
  "1155"="Fundacao publica dir. publico municipal","1210"="Consorcio publico dir. publico (assoc. publica)",
  "1228"="Consorcio publico dir. privado","1236"="Estado ou DF","1244"="Municipio",
  "1252"="Fundacao publica dir. privado federal","1260"="Fundacao publica dir. privado estadual/DF",
  "1279"="Fundacao publica dir. privado municipal","2011"="Empresa publica","2038"="Sociedade de economia mista",
  "2046"="S.A. aberta","2054"="S.A. fechada","2062"="Sociedade empresaria limitada","2135"="Empresario individual",
  "2143"="Cooperativa","2232"="Sociedade simples pura","2240"="Sociedade simples limitada","2305"="EIRELI",
  "2313"="EIRELI simples","2321"="Sociedade unipessoal de advocacia","3069"="Fundacao privada",
  "3077"="Servico notarial","3220"="Organizacao religiosa","3301"="Organizacao social (OS)",
  "3999"="Associacao privada","4014"="Empresa individual imobiliaria","4090"="Candidato a cargo politico",
  "4120"="Produtor rural (pessoa fisica)","4000"="Pessoa fisica")
TU <- c("01"="Posto de saude","02"="Centro de saude/UBS","04"="Policlinica","05"="Hospital geral",
  "07"="Hospital especializado","15"="Unidade mista","20"="Pronto socorro geral","21"="Pronto socorro especializado",
  "22"="Consultorio isolado","36"="Clinica/centro de especialidade","39"="Unidade de apoio diagnose e terapia",
  "40"="Unidade movel terrestre","42"="Unidade movel de urgencia","43"="Farmacia","50"="Unidade de vigilancia em saude",
  "62"="Hospital/dia isolado","64"="Central de regulacao","69"="Centro de atencao hemoterapia/hematologica",
  "70"="CAPS","73"="Pronto atendimento (UPA)","74"="Polo academia da saude","75"="Telessaude",
  "76"="Central de regulacao medica das urgencias","77"="Servico de atencao domiciliar","78"="Unidade de atencao indigena",
  "79"="Oficina ortopedica","80"="Laboratorio de saude publica","81"="Central de regulacao do acesso",
  "82"="Central de notificacao/captacao de orgaos","83"="Polo de prevencao de doencas","84"="Central de abastecimento",
  "85"="Centro de imunizacao")
TP <- c("20"="Privada com fins lucrativos","22"="Privada optante pelo Simples","30"="Publica federal",
  "40"="Publica estadual","50"="Publica municipal","60"="Privada sem fins lucrativos",
  "61"="Filantropica com CEBAS","80"="Sindicato")
st[, `:=`(tipo_unidade_cnes = fcoalesce(unname(TU[tp_unid]), paste0("Outro (", tp_unid, ")")),
          natureza_juridica = fcoalesce(unname(NJ[nat_jur]), fifelse(is.na(nat_jur), NA_character_, paste0("Outra (", nat_jur, ")"))),
          natureza_juridica_grupo = fcase(substr(nat_jur, 1, 1) == "1", "Administracao publica",
            substr(nat_jur, 1, 1) == "2", "Entidade empresarial", substr(nat_jur, 1, 1) == "3", "Entidade sem fins lucrativos",
            substr(nat_jur, 1, 1) == "4", "Pessoa fisica", substr(nat_jur, 1, 1) == "5", "Organizacao internacional",
            default = NA_character_),
          gestao = fcase(tp_gestao == "M", "Municipal", tp_gestao == "E", "Estadual", tp_gestao == "D", "Dupla",
                         tp_gestao == "S", "Sem gestao", default = NA_character_),
          tipo_prestador = unname(TP[tp_prest]))]
# quem administra (leitura simples): público / privado sem fins lucrativos / privado com fins lucrativos
st[, quem_administra := fcase(substr(nat_jur, 1, 1) == "1" | nat_jur %chin% c("2011", "2038"), "Publico",
  substr(nat_jur, 1, 1) == "3", "Privado sem fins lucrativos (filantropico/OS/associacao)",
  substr(nat_jur, 1, 1) %chin% c("2", "4"), "Privado com fins lucrativos", default = NA_character_)]
ed <- fread(file.path(raiz, "Dash/estabelecimentos_dash.csv"), encoding = "UTF-8",
            colClasses = list(character = c("codestab", "tipo_cod", "cod_municipio")))
ed[, codestab := cnes7(codestab)]
ed <- unique(ed[, .(codestab, nome, tipo_unidade, natureza_internacao = natureza, perfil_servico, porte,
                    uf_estab = uf, munic_estab = cod_municipio, leitos_total, leitos_sus, leitos_uti_neo,
                    leitos_uti_ped, leitos_uci_neo, leitos_uci_ped, leitos_pediatria,
                    atende_urgencia, atende_hospitalar)], by = "codestab")
cg <- chr(as.data.table(readRDS(file.path(raiz, "Dash/dados/cnes_bruto/_cadger.rds"))))
cg <- unique(cg[, .(codestab = cnes7(CNES), fantasia = as.character(FANTASIA))], by = "codestab")
EST <- merge(merge(cg, st, by = "codestab", all = TRUE), ed, by = "codestab", all = TRUE)
EST[, nome := fcoalesce(nome, fantasia)][, fantasia := NULL]
EST <- EST[!is.na(codestab)]
add_est <- function(d, chave, pref) {
  e <- copy(EST); setnames(e, names(e), paste0(pref, names(e))); setnames(e, paste0(pref, "codestab"), chave)
  merge(d, e, by = chave, all.x = TRUE, sort = FALSE)
}

# variáveis do estabelecimento de nascimento que vão para todos os nascidos
EN <- EST[, .(cnes_nasc = codestab, est_nasc_nome = nome, est_nasc_tipo_unidade_cnes = tipo_unidade_cnes,
              est_nasc_natureza_juridica = natureza_juridica, est_nasc_natureza_juridica_grupo = natureza_juridica_grupo,
              est_nasc_quem_administra = quem_administra, est_nasc_gestao = gestao, est_nasc_tipo_prestador = tipo_prestador,
              est_nasc_vinc_sus = vinc_sus, est_nasc_perfil_servico = perfil_servico, est_nasc_porte = porte,
              est_nasc_leitos_total = leitos_total, est_nasc_leitos_sus = leitos_sus,
              est_nasc_leitos_uti_neo = leitos_uti_neo, est_nasc_leitos_uci_neo = leitos_uci_neo)]

rm(st, ed, cg); gc()

# ---- óbitos 0-6 anos pareados ao SINASC (todas as idades) --------------------
fo <- list.files(dir_ob, "^obitos_0_6_linkados_\\d{4}\\.csv\\.gz$", full.names = TRUE)
ob <- rbindlist(lapply(fo, fread, encoding = "UTF-8",
  select = c("id_sim", "dt_obito", "idade_dias", "faixa", "causabas4", "link_sinasc", "id_nasc"),
  colClasses = list(character = c("id_nasc", "causabas4"))))
ob <- ob[link_sinasc %in% c(TRUE, "TRUE") & !is.na(id_nasc) & id_nasc != ""][, link_sinasc := NULL]
stopifnot(!anyDuplicated(ob$id_nasc))
setnames(ob, c("dt_obito", "idade_dias", "faixa", "causabas4"), c("obito_dt", "obito_idade_dias", "obito_faixa", "obito_causabas"))

# ---- Banco 1 (óbitos neonatais com SIM completo e AIH) -----------------------
b1 <- rbindlist(lapply(list.files(dir_b1, "^banco1_obitos_neonatais_\\d{4}\\.csv\\.gz$", full.names = TRUE),
                       fread, colClasses = list(character = c("id_nasc", "contador", "sim_contador"))), fill = TRUE)
b1 <- b1[!is.na(id_nasc) & id_nasc != ""]
stopifnot(!anyDuplicated(b1$id_nasc))
manter <- setdiff(names(b1), c(grep("^(nv_|est_nasc_)", names(b1), value = TRUE),
                               "hosp_nasc_atende_sus", "dt_nasc", "sexo", "raca_sim", "uf_res", "regiao", "munic_res"))
b1 <- b1[, ..manter]
setnames(b1, setdiff(names(b1), c("id_nasc", "id_sim")),
         ifelse(grepl("^(sim_|aih_obito_|ep_|ant_|est_obito_|est_aih_)", setdiff(names(b1), c("id_nasc", "id_sim"))),
                setdiff(names(b1), c("id_nasc", "id_sim")), paste0("neo_", setdiff(names(b1), c("id_nasc", "id_sim")))))
b1[, id_sim := NULL]

# ---- coorte por ano de nascimento --------------------------------------------
resumo <- list()
for (ano in 2015:2024) {
  d <- as.data.table(readRDS(file.path(raiz, "SINASC/Dados", sprintf("sinasc_BR_%d.rds", ano))))
  h <- data.table(
    id_nasc = sprintf("NV%dBR%07d", ano, seq_len(nrow(d))), ano_nasc = ano,
    contador_sinasc = as.character(col(d, "CONTADOR")),
    dt_nasc = data_dm(col(d, "DTNASC")), hora_nasc = as.character(col(d, "HORANASC")),
    sexo = sexo_12(col(d, "SEXO")), raca_nv = raca_12(as.character(col(d, "RACACOR"))),
    munic_res = mun6(col(d, "CODMUNRES")), munic_nasc = mun6(col(d, "CODMUNNASC")),
    cnes_nasc = cnes7(col(d, "CODESTAB")), locnasc = as.character(col(d, "LOCNASC")),
    peso = num(col(d, "PESO")), semagestac = num(col(d, "SEMAGESTAC")),
    gestacao = as.character(col(d, "GESTACAO")), parto = as.character(col(d, "PARTO")),
    gravidez = as.character(col(d, "GRAVIDEZ")), idademae = num(col(d, "IDADEMAE")),
    escmae = esc5(col(d, "ESCMAE"), col(d, "ESCMAE2010")), escmae_original = as.character(col(d, "ESCMAE")),
    escmae2010 = as.character(col(d, "ESCMAE2010")), escmaeagr1 = as.character(col(d, "ESCMAEAGR1")),
    qtdfilvivo = num(col(d, "QTDFILVIVO")), qtdfilmort = num(col(d, "QTDFILMORT")),
    apgar1 = num(col(d, "APGAR1")), apgar5 = num(col(d, "APGAR5")),
    consultas = as.character(col(d, "CONSULTAS")),
    idanomal = as.character(col(d, "IDANOMAL")), codanomal = as.character(col(d, "CODANOMAL")))
  rm(d); gc()
  h <- h[!is.na(dt_nasc)]
  h[peso %in% c(0L, 9999L), peso := NA]; h[semagestac %in% c(0L, 99L) | semagestac > 45, semagestac := NA]
  h[idademae %in% c(0L, 99L), idademae := NA]
  h[, uf_res := unname(UF_COD[substr(munic_res, 1, 2)])][, regiao_res := unname(UF_REG[uf_res])]
  h <- merge(h, EN, by = "cnes_nasc", all.x = TRUE, sort = FALSE)
  h <- merge(h, ob[substr(id_nasc, 3, 6) == as.character(ano)], by = "id_nasc", all.x = TRUE, sort = FALSE)
  h[, `:=`(obito_0_6anos = as.integer(!is.na(id_sim)),
           obito_neonatal = as.integer(!is.na(obito_idade_dias) & obito_idade_dias <= 27),
           obito_neonatal_precoce = as.integer(!is.na(obito_idade_dias) & obito_idade_dias <= 6),
           fim_seguimento = pmin(as.IDate("2024-12-31"), dt_nasc + 6L * 365L + 1L))]
  h[, arq := fifelse(is.na(regiao_res), "UF_ignorada", regiao_res)]
  h[uf_res == "SP", arq := "Sudeste_SP"][arq == "Sudeste", arq := "Sudeste_MG_ES_RJ"]   # limite de 25 MB por arquivo
  h[uf_res %chin% c("BA", "PE", "AL", "SE"), arq := "Nordeste_BA_PE_AL_SE"][arq == "Nordeste", arq := "Nordeste_MA_PI_CE_RN_PB"]
  bb <- b1[substr(id_nasc, 3, 6) == as.character(ano)]
  pri <- c("id_nasc", "contador_sinasc", "ano_nasc", "dt_nasc", "sexo", "raca_nv", "uf_res", "regiao_res", "munic_res",
           "munic_nasc", "cnes_nasc", "locnasc", "obito_0_6anos", "obito_neonatal", "obito_neonatal_precoce",
           "fim_seguimento", "id_sim", "obito_dt", "obito_idade_dias", "obito_faixa", "obito_causabas")
  rs <- list()
  for (r in unique(h$arq)) {                  # junta o Banco 1 por arquivo (memória)
    x <- merge(h[arq == r, !"arq"], bb, by = "id_nasc", all.x = TRUE, sort = FALSE)
    stopifnot(x[obito_neonatal == 1L, all(!is.na(neo_contador))], x[obito_neonatal == 0L, all(is.na(neo_contador))])
    setcolorder(x, c(pri, setdiff(names(x), pri))); setorder(x, dt_nasc, id_nasc)
    fwrite(x, file.path(od, sprintf("coorte_nascidos_vivos_%d_%s.csv.gz", ano, r)))
    rs[[r]] <- x[, .(nascidos_vivos = .N, obitos_0_6anos = sum(obito_0_6anos), obitos_neonatais = sum(obito_neonatal),
                     obitos_neonatais_precoces = sum(obito_neonatal_precoce),
                     neonatais_com_aih_obito = sum(neo_tem_aih_obito %in% c(TRUE, "TRUE")),
                     uf_ignorada = if (r == "UF_ignorada") .N else 0L)]
    rm(x); gc()
  }
  resumo[[as.character(ano)]] <- cbind(ano_nasc = ano, rbindlist(rs)[, lapply(.SD, sum)])
  message(ano, ": ", nrow(h), " nascidos vivos"); rm(h); gc()
}
resumo <- rbindlist(resumo); fwrite(resumo, file.path(od, "resumo_coorte_nascidos_vivos.csv")); print(resumo)
