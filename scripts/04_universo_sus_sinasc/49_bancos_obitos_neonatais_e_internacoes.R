# =============================================================================
# 49_bancos_obitos_neonatais_e_internacoes.R — DOIS BANCOS PARA ENVIO
# Observatório de Saúde Infantil | INSPER · Hospital Pequeno Príncipe
#
# BANCO 1 · Óbitos neonatais (0-27 dias, óbitos 2015-2024) × SINASC × SIH
#   Uma linha por óbito do SIM. Traz:
#     - TODAS as variáveis originais do SIM (prefixo sim_)
#     - variáveis derivadas do linkage (universo SUS, pareamento SIH, retroação)
#     - TODAS as variáveis originais do SINASC do nascido vivo pareado (prefixo nv_)
#     - SIH da internação que terminou no óbito (prefixo aih_obito_) e o resumo
#       do episódio (ep_) e das internações anteriores (ant_)
#     - tipo de estabelecimento / natureza jurídica / gestão do hospital do
#       nascimento (est_nasc_), do óbito (est_obito_) e da AIH (est_aih_)
#   + banco1_aih_obitos_neonatais.csv.gz: todas as AIHs ligadas a esses óbitos
#
# BANCO 2 · Todas as internações 0-6 anos (SIH 2015-2024) com o desfecho
#   Uma AIH por linha (a base do 47_), mais:
#     - TODAS as variáveis originais do SIM quando a criança morreu (sim_)
#     - tipo de estabelecimento do hospital da AIH (est_hosp_) e do óbito (est_obito_)
#
# ENTRADAS
#   SIM/Dados/sim_brasil_0_a_6_anos_todas_vars_2015_2024.rds (id_sim = linha após o filtro do 20_)
#   SINASC/Dados/sinasc_BR_AAAA.rds                          (id_nasc = NV+ano+BR+linha, como no 20_)
#   bases_finais/obitos_linkados/obitos_0_6_linkados_AAAA.csv.gz (20_, todas as idades)
#   bases_finais/internacoes_sim/internacoes_0_6_sim_AAAA_REGIAO.csv.gz (47_)
#   Dash/dados/cnes_bruto/ST_UF_2024.rds e ST_UF_2015.rds, _cadger.rds; Dash/estabelecimentos_dash.csv
# SAÍDAS (bases_finais/)
#   banco1_obitos_neonatais/banco1_obitos_neonatais_AAAA.csv.gz  (ano do óbito)
#   banco1_obitos_neonatais/banco1_aih_obitos_neonatais.csv.gz
#   banco2_internacoes/banco2_internacoes_0_6_AAAA_REGIAO.csv.gz
# =============================================================================
suppressPackageStartupMessages(library(data.table))
raiz <- c(Sys.getenv("INSPER_RAIZ"), "G:/My Drive/INSPER/Trabalho")
raiz <- raiz[nzchar(raiz) & dir.exists(raiz)][1]
dir_bf  <- Sys.getenv("INSPER_BF", file.path(raiz, "Dash/universo_sus/bases_finais"))
dir_ob  <- Sys.getenv("INSPER_OBITOS", file.path(dir_bf, "obitos_linkados"))
dir_int <- file.path(dir_bf, "internacoes_sim")
dir_out <- Sys.getenv("INSPER_OUT", dir_bf)
o1 <- file.path(dir_out, "banco1_obitos_neonatais"); o2 <- file.path(dir_out, "banco2_internacoes")
dir.create(o1, recursive = TRUE, showWarnings = FALSE); dir.create(o2, recursive = TRUE, showWarnings = FALSE)

data_dm <- function(x) as.IDate(as.character(x), format = "%d%m%Y")
cnes7 <- function(x) { x <- gsub("[^0-9]", "", as.character(x)); x[x == ""] <- NA
  ifelse(is.na(x), NA_character_, formatC(x, width = 7, flag = "0")) }
chr <- function(d) { for (v in names(d)) if (is.factor(d[[v]])) set(d, j = v, value = as.character(d[[v]])); d }

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

# ---- 2. SIM com todas as variáveis (id_sim igual ao do 20_) ------------------
s <- chr(as.data.table(readRDS(file.path(raiz, "SIM/Dados/sim_brasil_0_a_6_anos_todas_vars_2015_2024.rds"))))
dn <- data_dm(s$DTNASC); do <- data_dm(s$DTOBITO)
s <- s[!is.na(dn) & !is.na(do) & year(do) %in% 2015:2024]; rm(dn, do)
s[, id_sim := .I]
setnames(s, setdiff(names(s), "id_sim"), paste0("sim_", tolower(setdiff(names(s), "id_sim"))))

# ---- 3. AIHs (base do 47_) -------------------------------------------------------
fi <- list.files(dir_int, "^internacoes_0_6_sim_\\d{4}_.*\\.csv\\.gz$", full.names = TRUE)
CC <- list(character = c("n_aih", "cnes", "munic_mov", "munic_res", "cid_principal", "cid_secundario", "proc",
                         "car_int", "codestab_sim", "munic_ocor_sim", "contador_sim", "causabas_sim", "lococor_sim"))

# ---- 4. BANCO 1 ---------------------------------------------------------------
fo <- list.files(dir_ob, "^obitos_0_6_linkados_\\d{4}\\.csv\\.gz$", full.names = TRUE)
b1 <- rbindlist(lapply(fo, fread, encoding = "UTF-8",
  colClasses = list(character = c("contador", "id_nasc", "codestab", "munic_res", "munic_ocor", "nv_cnes_nasc",
                                  "nv_munic_nasc", "causabas", "causabas4", "lococor", "cid_ultima_internacao"))), fill = TRUE)
b1 <- b1[idade_dias >= 0 & idade_dias <= 27]
b1[, grep("^nv_", names(b1), value = TRUE) := NULL]          # vem o SINASC completo abaixo
setnames(b1, c("codestab", "nome", "tipo_unidade", "leitos_sus", "pareado", "metodo"),
         c("cnes_obito", "nome_estab_obito", "tipo_unidade_obito", "leitos_sus_obito", "pareado_sih", "metodo_sih"))

# SINASC completo do nascido vivo pareado
anos_n <- sort(unique(as.integer(substr(na.omit(b1$id_nasc), 3, 6))))
nv <- rbindlist(lapply(anos_n, function(y) {
  f <- file.path(raiz, "SINASC/Dados", sprintf("sinasc_BR_%d.rds", y))
  if (!file.exists(f)) { message("falta SINASC ", y); return(NULL) }
  d <- chr(as.data.table(readRDS(f)))
  ids <- b1[substr(id_nasc, 3, 6) == as.character(y), id_nasc]
  r <- as.integer(substr(ids, 9, 15))
  x <- d[r]; rm(d); gc()
  setnames(x, paste0("nv_", tolower(names(x)))); x[, id_nasc := ids]; x
}), fill = TRUE)
b1 <- merge(b1, nv, by = "id_nasc", all.x = TRUE, sort = FALSE); rm(nv); gc()
b1[, nv_cnes_nasc := cnes7(nv_codestab)]

# AIHs ligadas a esses óbitos
ai <- rbindlist(lapply(fi, function(f) { x <- fread(f, colClasses = CC); x[id_sim %in% b1$id_sim] }))
ai[, ordem_link := as.integer(substr(link_obito, 1, 1))]
fwrite(ai[, !"ordem_link"][order(id_sim, dt_inter)], file.path(o1, "banco1_aih_obitos_neonatais.csv.gz"))
ao <- ai[ordem_link %in% 1:2, .(id_sim, aih_obito_id_aih = id_aih, aih_obito_n_aih = n_aih, aih_obito_link = link_obito,
         aih_obito_cnes = cnes, aih_obito_dt_inter = dt_inter, aih_obito_dt_saida = dt_saida, aih_obito_car_int = car_int,
         aih_obito_cid_principal = cid_principal, aih_obito_cid_secundario = cid_secundario, aih_obito_proc = proc,
         aih_obito_dias_uti = dias_uti, aih_obito_val_tot = val_tot, aih_obito_desfecho = desfecho)]
ao <- unique(ao, by = "id_sim")
ep <- ai[ordem_link %in% 1:3, .(ep_n_aih = .N, ep_dt_inter = min(dt_inter), ep_dt_saida = max(dt_saida),
         ep_dias_internado = as.integer(max(dt_saida) - min(dt_inter)), ep_dias_uti = sum(dias_uti, na.rm = TRUE),
         ep_val_tot = round(sum(val_tot, na.rm = TRUE), 2), ep_n_hospitais = uniqueN(cnes),
         ep_procs = paste(unique(proc), collapse = "|"), ep_cids = paste(unique(na.omit(cid_principal)), collapse = "|")), by = id_sim]
an <- ai[ordem_link == 4, .(ant_n_aih = .N, ant_dias_uti = sum(dias_uti, na.rm = TRUE),
         ant_val_tot = round(sum(val_tot, na.rm = TRUE), 2), ant_cids = paste(unique(na.omit(cid_principal)), collapse = "|")), by = id_sim]
sus <- unique(rbindlist(lapply(fi, fread, select = "cnes", colClasses = list(character = "cnes")))$cnes)
b1 <- merge(b1, ao, by = "id_sim", all.x = TRUE); b1 <- merge(b1, ep, by = "id_sim", all.x = TRUE)
b1 <- merge(b1, an, by = "id_sim", all.x = TRUE); rm(ai, ao, ep, an); gc()
b1[, `:=`(tem_aih_obito = !is.na(aih_obito_id_aih), tem_aih_qualquer = !is.na(ep_n_aih) | !is.na(ant_n_aih),
          hosp_nasc_atende_sus = fifelse(is.na(nv_cnes_nasc), NA, nv_cnes_nasc %chin% sus),
          hosp_obito_atende_sus = fifelse(is.na(cnes_obito), NA, cnes_obito %chin% sus))]
b1 <- add_est(b1, "nv_cnes_nasc", "est_nasc_")
b1 <- add_est(b1, "cnes_obito", "est_obito_")
b1 <- add_est(b1, "aih_obito_cnes", "est_aih_")
b1 <- merge(b1, s, by = "id_sim", all.x = TRUE, sort = FALSE)
pri <- c("id_sim", "contador", "id_nasc", "ano", "dt_nasc", "dt_obito", "idade_dias", "faixa", "sexo", "raca_sim",
         "uf_res", "regiao", "munic_res", "cnes_obito", "lococor", "causabas", "causabas4", "universo", "comp_universo",
         "link_sinasc", "pareado_sih", "metodo_sih", "tem_aih_obito", "tem_aih_qualquer")
setcolorder(b1, c(pri, setdiff(names(b1), pri)))
stopifnot(!anyDuplicated(b1$id_sim), all(b1$contador == b1$sim_contador))
setorder(b1, ano, id_sim)
for (a in sort(unique(b1$ano))) fwrite(b1[ano == a], file.path(o1, sprintf("banco1_obitos_neonatais_%d.csv.gz", a)))
r1 <- b1[, .(obitos_0_27d = .N, precoce_0_6d = sum(idade_dias <= 6), com_sinasc = sum(link_sinasc %in% TRUE),
             com_aih_obito = sum(tem_aih_obito), com_alguma_aih = sum(tem_aih_qualquer)), by = ano][order(ano)]
fwrite(r1, file.path(o1, "resumo_banco1.csv")); print(r1)
cat("Banco 1:", nrow(b1), "óbitos x", ncol(b1), "variáveis\n"); rm(b1); gc()

# ---- 5. BANCO 2 ----------------------------------------------------------------
r2 <- list()
for (f in fi) {
  x <- fread(f, colClasses = CC)
  x <- add_est(x, "cnes", "est_hosp_")
  x <- add_est(x, "codestab_sim", "est_obito_")
  x <- merge(x, s, by = "id_sim", all.x = TRUE, sort = FALSE)
  x[, obito_crianca := !is.na(id_sim)]
  setcolorder(x, c("id_aih", "n_aih", "ano_saida", "uf_hosp", "regiao_hosp", "cnes", "est_hosp_nome"))
  setorder(x, uf_hosp, dt_saida, id_aih)
  nm <- sub("^internacoes_0_6_sim_", "banco2_internacoes_0_6_", basename(f))
  if (grepl("Sudeste", nm)) {                       # Sudeste em 2 arquivos (limite de 30 MB por arquivo)
    fwrite(x[uf_hosp == "SP"], file.path(o2, sub("Sudeste", "Sudeste_SP", nm)))
    fwrite(x[uf_hosp != "SP"], file.path(o2, sub("Sudeste", "Sudeste_MG_ES_RJ", nm)))
  } else fwrite(x, file.path(o2, nm))
  r2[[nm]] <- x[, .(arquivo = nm, aih = .N, aih_obito_sih = sum(desfecho == "Obito"),
                    aih_com_sim = sum(obito_crianca), sem_tipo_estab = sum(is.na(est_hosp_tipo_unidade_cnes)))]
  cat(nm, nrow(x), "\n"); rm(x); gc()
}
r2 <- rbindlist(r2); fwrite(r2, file.path(o2, "resumo_banco2.csv")); print(r2[, lapply(.SD, sum), .SDcols = -1])
