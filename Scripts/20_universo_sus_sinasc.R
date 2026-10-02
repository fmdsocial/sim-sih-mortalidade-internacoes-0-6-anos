# ============================================================================
# 20_universo_sus_sinasc.R — UNIVERSO SUS + LINKAGE SIM × SINASC × SIH
# Observatório de Saúde Infantil | INSPER · Hospital Pequeno Príncipe
# Coorte 0 a 6 anos · óbitos 2015–2024 · nascidos 2009–2024
#
# O QUE FAZ (correções de 02/10/2026)
#   1. Novo universo: só óbitos em HOSPITAL (LOCOCOR 1) cujo CNES tem ao menos
#      uma AIH de 0–6 anos no SIH 2015–2024 (estabelecimento que atende SUS).
#      Saem: UPA/PS/UBS (LOCOCOR 2 e unidades de urgência sem internação),
#      hospital sem nenhuma AIH 0–6 (saúde suplementar / não conveniado) e
#      óbito sem CNES na DO.
#   2. Linkage SIM × SIH com as MESMAS regras da v3.3 (exato + probabilístico,
#      buffer ±3 d, escore ≥ 3), agora com dedupe nacional 1:1.
#   3. Retroação: para cada óbito do universo, todas as AIHs da mesma criança
#      (nasc + sexo + município de residência) antes do óbito — AIH aberta no
#      dia do óbito no mesmo hospital com outro motivo de saída, internações
#      anteriores, reinternações, AIH do nascimento.
#   4. Linkage SIM × SINASC (estabelecimento + características do nascimento e
#      da mãe) e SINASC × SIH (nascidos vivos com possíveis internações).
#   5. Tabelas do relatório refeitas no universo novo + bases finais.
#
# PARTES (cada uma grava em disco; a seguinte lê o que a anterior gravou)
#   1 · SIH → AIHs leves, episódios, CNES com AIH
#   2 · SIM → universo SUS + pareamento com o SIH + retroação
#   3 · SINASC → harmoniza (lê SINASC/Dados/sinasc_{UF}_{ano}.rds)
#   4 · Linkage SIM × SINASC
#   5 · SINASC × SIH (nascidos vivos com internação)
#   6 · Tabelas, figura e bases finais
# ============================================================================

RODAR <- if (exists("RODAR_EXTERNO")) RODAR_EXTERNO else 1:6

raiz <- c(Sys.getenv("INSPER_RAIZ"), "G:/My Drive/INSPER/Trabalho",
          "G:/Meu Drive/INSPER/Trabalho")
raiz <- raiz[nzchar(raiz) & dir.exists(raiz)][1]
if (is.na(raiz)) stop("Pasta INSPER/Trabalho nao encontrada.")

CFG <- list(
  dir_sih    = file.path(raiz, "SIH/Dados/Temporarios_UF_Ano"),
  arq_sim    = file.path(raiz, "SIM/Dados/sim_brasil_0_a_6_anos_todas_vars_2015_2024.rds"),
  dir_sinasc = file.path(raiz, "SINASC/Dados"),
  arq_estab  = file.path(raiz, "Dash/estabelecimentos_dash.csv"),
  dir_out    = Sys.getenv("INSPER_SAIDA", file.path(raiz, "Dash/universo_sus")),
  ufs  = c("RO","AC","AM","RR","PA","AP","TO","MA","PI","CE","RN","PB","PE",
           "AL","SE","BA","MG","ES","RJ","SP","PR","SC","RS","MS","MT","GO","DF"),
  anos = 2015:2024, anos_nasc = 2009:2024,
  buffer_dias = 3, escore_minimo = 3, gap_episodio = 2,
  # SIM × SINASC
  escore_sinasc_alta = 10, escore_sinasc_min = 7
)
dir.create(CFG$dir_out, recursive = TRUE, showWarnings = FALSE)
dir_tmp <- file.path(CFG$dir_out, "tmp"); dir.create(dir_tmp, showWarnings = FALSE)
arq <- function(...) file.path(CFG$dir_out, ...)
tmp <- function(...) file.path(dir_tmp, ...)

suppressPackageStartupMessages(library(data.table))
setDTthreads(0)
logf <- arq("relatorio_universo_sus.txt")
say <- function(...) { m <- paste0(...); message(m); cat(m, "\n", file = logf, append = TRUE) }
fmt <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
pct <- function(a, b) sprintf("%.1f", 100 * a / b)

# ---------------------------------------------------------------- funções --
data_sih <- function(x) as.IDate(as.character(x), format = "%Y%m%d")
data_dm  <- function(x) as.IDate(as.character(x), format = "%d%m%Y")
cnes7 <- function(x) { x <- gsub("[^0-9]", "", as.character(x)); x[x == ""] <- NA
  ifelse(is.na(x), NA_character_, formatC(x, width = 7, flag = "0")) }
mun6 <- function(x) { x <- substr(gsub("[^0-9]", "", as.character(x)), 1, 6); x[x == ""] <- NA; x }
cid3 <- function(x) { x <- substr(toupper(gsub("[^A-Za-z0-9]", "", as.character(x))), 1, 3)
  x[!grepl("^[A-Z][0-9]{2}$", x)] <- NA; x }
num <- function(x) suppressWarnings(as.integer(as.character(x)))
sexo_sih <- function(x) fcase(as.character(x) %chin% c("1","M"), "M", as.character(x) %chin% c("3","F"), "F", default = NA_character_)
sexo_12  <- function(x) fcase(as.character(x) %chin% c("1","M"), "M", as.character(x) %chin% c("2","F"), "F", default = NA_character_)
raca_sih <- function(x) fcase(x == "01","Branca", x == "02","Preta", x == "03","Parda", x == "04","Amarela", x == "05","Indigena", default = NA_character_)
raca_12  <- function(x) fcase(x == "1","Branca", x == "2","Preta", x == "3","Amarela", x == "4","Parda", x == "5","Indigena", default = NA_character_)
col <- function(d, v) if (v %in% names(d)) d[[v]] else rep(NA, nrow(d))
UF_COD <- c("11"="RO","12"="AC","13"="AM","14"="RR","15"="PA","16"="AP","17"="TO",
            "21"="MA","22"="PI","23"="CE","24"="RN","25"="PB","26"="PE","27"="AL",
            "28"="SE","29"="BA","31"="MG","32"="ES","33"="RJ","35"="SP","41"="PR",
            "42"="SC","43"="RS","50"="MS","51"="MT","52"="GO","53"="DF")
UF_REG <- c(RO="Norte",AC="Norte",AM="Norte",RR="Norte",PA="Norte",AP="Norte",TO="Norte",
            MA="Nordeste",PI="Nordeste",CE="Nordeste",RN="Nordeste",PB="Nordeste",
            PE="Nordeste",AL="Nordeste",SE="Nordeste",BA="Nordeste",
            MG="Sudeste",ES="Sudeste",RJ="Sudeste",SP="Sudeste",
            PR="Sul",SC="Sul",RS="Sul",MS="Centro-Oeste",MT="Centro-Oeste",
            GO="Centro-Oeste",DF="Centro-Oeste")
faixa4 <- function(d) fcase(is.na(d), NA_character_, d <= 6, "Neonatal precoce (0-6 d)",
  d <= 27, "Neonatal tardio (7-27 d)", d < 365, "Pos-neonatal (28 d-<1 a)",
  default = "1 a 6 anos")

if (1 %in% RODAR || !file.exists(tmp("aih.rds"))) cat("", file = logf)
say("== 20_universo_sus_sinasc.R · ", format(Sys.time(), "%d/%m/%Y %H:%M"), " ==")

# ============================================================================
# PARTE 1 · SIH — AIHs leves, episódios e CNES com AIH 0–6
# ============================================================================
if (1 %in% RODAR) {
  say("\nPARTE 1 · SIH")
  partes <- list()
  for (uf in CFG$ufs) for (ano in CFG$anos) {
    f <- file.path(CFG$dir_sih, sprintf("sih_%s_%d.rds", uf, ano))
    if (!file.exists(f)) { say("  lote ausente: ", basename(f)); next }
    d <- suppressWarnings(readRDS(f))
    x <- data.table(
      uf_hosp = uf,
      cnes = cnes7(col(d, "CNES")), munic_res = mun6(col(d, "MUNIC_RES")),
      dt_nasc = data_sih(col(d, "NASC")), sexo = sexo_sih(col(d, "SEXO")),
      raca_sih = raca_sih(as.character(col(d, "RACA_COR"))),
      dt_inter = data_sih(col(d, "DT_INTER")), dt_saida = data_sih(col(d, "DT_SAIDA")),
      cid_ent = cid3(col(d, "DIAG_PRINC")), cid_sec = cid3(col(d, "DIAG_SECUN")),
      morte = num(col(d, "MORTE")), cob = num(substr(as.character(col(d, "COBRANCA")), 1, 2)),
      proc = as.character(col(d, "PROC_REA")), uti = num(col(d, "UTI_MES_TO")))
    rm(d)
    x <- x[!is.na(dt_nasc) & !is.na(dt_saida)]
    x[, idade_dias := as.integer(dt_saida - dt_nasc)]
    x <- x[idade_dias >= 0 & idade_dias <= 365 * 7]
    partes[[length(partes) + 1]] <- x
  }
  aih <- rbindlist(partes); rm(partes); gc()
  aih[, desfecho := fcase(!is.na(morte) & morte == 1, "Obito", cob %in% 41:43, "Obito",
    cob %in% 31:39, "Transferencia", cob %in% 11:19, "Alta", cob %in% 21:29, "Permanencia",
    cob == 51, "Encerramento adm.", default = "Outro")]
  aih[, c("morte", "cob") := NULL]
  aih[, id_aih := .I]
  aih[, pac := .GRP, by = .(dt_nasc, sexo, munic_res)]          # chave inteira (sem string)
  say("  AIHs 0-6 anos: ", fmt(nrow(aih)))

  # episódios (mesma regra da v3: gap <= 2 d encadeia; óbito tem prioridade)
  setorder(aih, pac, dt_inter, dt_saida)
  aih[, gap := as.integer(dt_inter - shift(dt_saida))]
  aih[, novo := is.na(gap) | gap > CFG$gap_episodio | pac != shift(pac, fill = -1L)]
  aih[, ep_id := cumsum(novo)]
  aih[, c("gap", "novo") := NULL]
  aih[, ord := rowid(ep_id)]
  aih[, `:=`(o1 = fifelse(desfecho == "Obito", ord, 0L), o2 = fifelse(desfecho != "Outro", ord, 0L))]
  ref <- aih[, .(r1 = max(o1), r2 = max(o2), n_aih_ep = .N, t0 = min(dt_inter), cid_ent_t0 = cid_ent[1L]), by = ep_id]
  aih[, c("o1", "o2") := NULL]
  ref[, `:=`(ref_ord = fifelse(r1 > 0L, r1, fifelse(r2 > 0L, r2, n_aih_ep)), tem_obito = r1 > 0L)]
  ref[, c("r1", "r2") := NULL]
  ep <- merge(aih[, .(ep_id, ord, id_aih, uf_hosp, cnes, munic_res, dt_nasc, sexo,
                      raca_sih, dt_saida, cid_sec, desfecho, idade_dias)],
              ref, by.x = c("ep_id", "ord"), by.y = c("ep_id", "ref_ord"))
  rm(ref); gc()
  ep[, desfecho_ep := fifelse(tem_obito, "Obito", desfecho)]
  ep[, ano := year(dt_saida)]
  ep <- ep[ano %in% CFG$anos]
  saveRDS(aih, tmp("aih.rds")); saveRDS(ep, tmp("episodios.rds"))
  say("  episodios: ", fmt(nrow(ep)), " | com obito: ", fmt(sum(ep$desfecho_ep == "Obito")))
  rm(ep); gc()

  cnes_aih <- aih[!is.na(cnes) & year(dt_saida) %in% CFG$anos,
                  .(n_aih = .N, n_aih_obito = sum(desfecho == "Obito"),
                    n_aih_neo = sum(idade_dias <= 27)), by = cnes]
  saveRDS(cnes_aih, tmp("cnes_aih.rds"))
  say("  CNES com ao menos 1 AIH 0-6 (2015-2024): ", fmt(nrow(cnes_aih)))
  rm(aih); gc()
}

# ============================================================================
# PARTE 2 · SIM — universo SUS, pareamento com o SIH e retroação
# ============================================================================
if (2 %in% RODAR) {
  say("\nPARTE 2 · SIM x SIH")
  s <- readRDS(CFG$arq_sim)
  sim <- data.table(
    contador = as.character(col(s, "CONTADOR")),
    dt_nasc = data_dm(col(s, "DTNASC")), dt_obito = data_dm(col(s, "DTOBITO")),
    sexo = sexo_12(col(s, "SEXO")), raca_sim = raca_12(as.character(col(s, "RACACOR"))),
    munic_res = mun6(col(s, "CODMUNRES")), munic_ocor = mun6(col(s, "CODMUNOCOR")),
    codestab = cnes7(col(s, "CODESTAB")), lococor = as.character(col(s, "LOCOCOR")),
    causabas = cid3(col(s, "CAUSABAS")), causabas4 = toupper(as.character(col(s, "CAUSABAS"))),
    idademae = num(col(s, "IDADEMAE")), escmae = as.character(col(s, "ESCMAE")),
    escmaeagr1 = as.character(col(s, "ESCMAEAGR1")),
    gestacao = as.character(col(s, "GESTACAO")), semagestac = num(col(s, "SEMAGESTAC")),
    gravidez = as.character(col(s, "GRAVIDEZ")), parto = as.character(col(s, "PARTO")),
    peso = num(col(s, "PESO")), obitoparto = as.character(col(s, "OBITOPARTO")),
    assistmed = as.character(col(s, "ASSISTMED")), necropsia = as.character(col(s, "NECROPSIA")))
  rm(s); gc()
  sim <- sim[!is.na(dt_nasc) & !is.na(dt_obito) & year(dt_obito) %in% CFG$anos]
  sim[, id_sim := .I]
  sim[, idade_dias := as.integer(dt_obito - dt_nasc)]
  sim[, ano := year(dt_obito)]
  sim[, uf_res := unname(UF_COD[substr(munic_res, 1, 2)])]
  sim[, regiao := unname(UF_REG[uf_res])]
  sim[, faixa := faixa4(idade_dias)]
  say("  obitos 0-6 SIM: ", fmt(nrow(sim)))

  # --- pareamento SIM × SIH (regras v3.3) -----------------------------------
  ep <- readRDS(tmp("episodios.rds"))
  eo <- ep[desfecho_ep == "Obito"]; rm(ep)
  alvo <- sim[lococor %chin% c("1", "2")]
  eo[, bloco := paste(dt_nasc, sexo)]; alvo[, bloco := paste(dt_nasc, sexo)]
  cand <- merge(eo[, .(id_aih, bloco, cnes, munic_res, raca_sih, dt_saida)],
                alvo[, .(id_sim, bloco, dt_obito, codestab, mres = munic_res, raca_sim)],
                by = "bloco", allow.cartesian = TRUE)
  cand[, dist := abs(as.integer(dt_obito - dt_saida))]
  cand <- cand[dist <= CFG$buffer_dias]
  cand[, `:=`(pt = fifelse(dist == 0, 3L, fifelse(dist == 1, 2L, 1L)),
              cok = !is.na(cnes) & !is.na(codestab) & cnes == codestab,
              mok = !is.na(munic_res) & !is.na(mres) & munic_res == mres,
              rok = !is.na(raca_sih) & !is.na(raca_sim) & raca_sih == raca_sim)]
  cand[, escore := pt + 2L * cok + 2L * mok + rok]
  cand[, exato := dist == 0 & mok & (cok | is.na(cnes) | is.na(codestab))]
  cand <- cand[escore >= CFG$escore_minimo | exato]
  setorder(cand, id_aih, -exato, -escore, dist); cand <- cand[, .SD[1L], by = id_aih]
  setorder(cand, id_sim, -exato, -escore, dist); cand <- cand[, .SD[1L], by = id_sim]
  sim <- merge(sim, cand[, .(id_sim, id_aih_obito = id_aih, metodo = fifelse(exato, "Exato", "Probabilistico"),
                             escore_sih = escore, cnes_aih_obito = cnes)], by = "id_sim", all.x = TRUE)
  sim[, pareado := !is.na(id_aih_obito)]
  say("  pareados SIM x SIH (alvo LOCOCOR 1/2): ", fmt(sum(sim$pareado)), " de ", fmt(nrow(alvo)),
      " (", pct(sum(sim$pareado), nrow(alvo)), "%)")
  rm(cand, eo, alvo); gc()

  # --- universo SUS ---------------------------------------------------------
  cnes_aih <- readRDS(tmp("cnes_aih.rds"))
  est <- if (file.exists(CFG$arq_estab)) fread(CFG$arq_estab, colClasses = list(character = c("codestab", "tipo_cod"))) else NULL
  if (!is.null(est)) {
    est[, codestab := cnes7(codestab)]
    est <- unique(est[, .(codestab, nome, tipo_unidade, natureza, perfil_servico,
                          leitos_total, leitos_sus, leitos_uti_neo, leitos_uti_ped)], by = "codestab")
    sim <- merge(sim, est, by = "codestab", all.x = TRUE)
  }
  urg <- c("Pronto atendimento", "Pronto socorro geral", "Pronto socorro especializado", "UPA")
  sim[, sus_cnes := !is.na(codestab) & codestab %chin% cnes_aih$cnes]
  sim[, tipo_urg := !is.na(tipo_unidade) & grepl("Pronto|UPA|Urg", tipo_unidade, ignore.case = TRUE)]
  sim[, comp_universo := fcase(
    !lococor %chin% c("1", "2"), "0 Fora de estabelecimento de saude (domicilio, via publica, outros)",
    lococor == "2", "1 Outro estabelecimento de saude (UPA/PS/UBS) - LOCOCOR 2",
    is.na(codestab), "2 Hospital sem CNES na DO",
    tipo_urg & !sus_cnes, "3 Unidade de urgencia sem internacao (CNES)",
    !sus_cnes, "4 Hospital sem nenhuma AIH 0-6 no periodo (saude suplementar/nao conveniado)",
    tipo_urg, "5 Unidade de urgencia (UPA/PS) com AIH - excluida",
    default = "6 Hospital com AIH 0-6 (universo SUS)")]
  sim[, universo := comp_universo == "6 Hospital com AIH 0-6 (universo SUS)"]
  sim[, dia0 := idade_dias == 0]
  sim[, comp_sem_aih := fifelse(!universo | pareado, NA_character_,
                                fifelse(dia0, "Obito no dia do nascimento", "Obito apos o 1o dia de vida"))]
  say("  universo SUS (LOCOCOR 1 + CNES com AIH): ", fmt(sum(sim$universo)),
      " | pareados ", fmt(sum(sim$universo & sim$pareado)), " (", pct(sum(sim$universo & sim$pareado), sum(sim$universo)), "%)")

  # --- retroação: todas as AIHs da criança até o óbito ----------------------
  aih <- readRDS(tmp("aih.rds"))
  aih[, c("uf_hosp", "raca_sih", "cid_sec", "proc", "uti", "ord") := NULL]; gc()
  chaves <- unique(aih[, .(dt_nasc, sexo, munic_res, pac)])
  u <- sim[universo == TRUE & !is.na(sexo) & !is.na(munic_res), .(id_sim, dt_nasc, sexo, munic_res, dt_obito, codestab, id_aih_obito)]
  u[, n_sim_chave := .N, by = .(dt_nasc, sexo, munic_res)]
  u <- merge(u, chaves, by = c("dt_nasc", "sexo", "munic_res"), all.x = TRUE)
  rm(chaves)
  aih <- aih[pac %in% u$pac, .(id_aih, pac, cnes, dt_inter, dt_saida, desfecho, cid_ent, ep_id, idade_dias)]; gc()
  # n_sim_chave = multiplicidade da chave no SIM (gêmeos / homônimos de chave)
  r <- merge(u[!is.na(pac)], aih, by = "pac", allow.cartesian = TRUE)
  r <- r[dt_inter <= dt_obito + 1L]
  r[, `:=`(mesmo_cnes = !is.na(cnes) & !is.na(codestab) & cnes == codestab,
           cobre_obito = dt_inter <= dt_obito & dt_saida >= dt_obito - 1L,
           eh_obito_par = !is.na(id_aih_obito) & id_aih == id_aih_obito)]
  # episódio do óbito (para separar reinternações de AIHs do mesmo episódio)
  ep_ob <- r[eh_obito_par == TRUE, .(id_sim, ep_obito = ep_id)]
  r <- merge(r, ep_ob, by = "id_sim", all.x = TRUE)
  setorder(r, id_sim, dt_inter)
  ret <- r[, .(
    n_aih_vida = .N,
    n_episodios_vida = uniqueN(ep_id),
    aih_cobre_obito_mesmo_cnes = any(cobre_obito & mesmo_cnes & !eh_obito_par),
    desf_aih_cobre_obito = { k <- which(cobre_obito & mesmo_cnes & !eh_obito_par); if (length(k)) desfecho[k[length(k)]] else NA_character_ },
    aih_cobre_obito_outro_cnes = any(cobre_obito & !mesmo_cnes & !eh_obito_par),
    n_internacoes_previas = uniqueN(ep_id[dt_saida < dt_obito - 1L & (is.na(ep_obito) | ep_id != ep_obito)]),
    dias_ultima_alta = { k <- which(dt_saida < dt_obito - 1L & (is.na(ep_obito) | ep_id != ep_obito)); if (length(k)) as.integer(dt_obito[1L] - max(dt_saida[k])) else NA_integer_ },
    cid_ultima_internacao = { k <- which(dt_saida < dt_obito - 1L & (is.na(ep_obito) | ep_id != ep_obito)); if (length(k)) cid_ent[k[length(k)]] else NA_character_ },
    reinternacao_30d = { k <- which(dt_saida < dt_obito - 1L & (is.na(ep_obito) | ep_id != ep_obito)); length(k) > 0 && as.integer(dt_obito[1L] - max(dt_saida[k])) <= 30 }
  ), by = id_sim]
  nasc <- r[as.integer(dt_inter - dt_nasc) <= 1L, .(aih_nascimento = TRUE,
            aih_nasc_mesmo_cnes = any(mesmo_cnes)), by = id_sim]
  ret <- merge(ret, nasc, by = "id_sim", all.x = TRUE)
  ret[is.na(aih_nascimento), `:=`(aih_nascimento = FALSE, aih_nasc_mesmo_cnes = FALSE)]
  sim <- merge(sim, ret, by = "id_sim", all.x = TRUE)
  sim <- merge(sim, u[, .(id_sim, n_sim_chave)], by = "id_sim", all.x = TRUE)
  sim[universo == TRUE & is.na(n_aih_vida), `:=`(n_aih_vida = 0L, n_episodios_vida = 0L,
      aih_cobre_obito_mesmo_cnes = FALSE, aih_cobre_obito_outro_cnes = FALSE,
      n_internacoes_previas = 0L, reinternacao_30d = FALSE, aih_nascimento = FALSE, aih_nasc_mesmo_cnes = FALSE)]
  sim[, chave_incompleta := universo & (is.na(sexo) | is.na(munic_res))]
  saveRDS(sim, tmp("sim_universo.rds"))
  say("  retroacao feita: ", fmt(nrow(ret)), " obitos do universo com >=1 AIH da crianca")
  rm(aih, r, ret, u); gc()
}

# ============================================================================
# PARTE 3 · SINASC — harmoniza por ANO DE NASCIMENTO (arquivo por UF de
#           ocorrência → um arquivo nacional por ano)
# ============================================================================
esc5 <- function(escmae, esc2010) {           # 1 nenhuma ... 5 12+ anos
  a <- num(escmae); a[!a %in% 1:5] <- NA
  b <- num(esc2010); b <- c(1L, 2L, 3L, 4L, 5L, 5L)[b + 1L]
  fcoalesce(a, b)
}
if (3 %in% RODAR) {
  say("\nPARTE 3 · SINASC")
  for (ano in CFG$anos_nasc) {
    fo <- tmp(sprintf("sinasc_h_%d.rds", ano))
    fbr <- file.path(CFG$dir_sinasc, sprintf("sinasc_BR_%d.rds", ano))   # OpenDataSUS (Brasil)
    if (file.exists(fbr)) { fs <- fbr; rot <- "BR" } else {
      fs <- file.path(CFG$dir_sinasc, sprintf("sinasc_%s_%d.rds", CFG$ufs, ano)); rot <- CFG$ufs }
    ok <- file.exists(fs) & file.size(fs) > 1000
    if (file.exists(fo) && all(ok)) next
    if (!any(ok)) { say("  ", ano, ": nenhum arquivo"); next }
    if (!all(ok)) say("  ", ano, ": faltam ", paste(rot[!ok], collapse = " "))
    l <- lapply(which(ok), function(i) {
      d <- as.data.table(readRDS(fs[i]))
      data.table(
        id_nasc = sprintf("NV%d%s%07d", ano, rot[i], seq_len(nrow(d))),
        numerodn = as.character(col(d, "NUMERODN")),
        dt_nasc = data_dm(col(d, "DTNASC")), sexo = sexo_12(col(d, "SEXO")),
        raca_nv = raca_12(as.character(col(d, "RACACOR"))),
        munic_res = mun6(col(d, "CODMUNRES")), munic_nasc = mun6(col(d, "CODMUNNASC")),
        cnes_nasc = cnes7(col(d, "CODESTAB")), locnasc = as.character(col(d, "LOCNASC")),
        peso = num(col(d, "PESO")), semagestac = num(col(d, "SEMAGESTAC")),
        gestacao = as.character(col(d, "GESTACAO")), parto = as.character(col(d, "PARTO")),
        gravidez = as.character(col(d, "GRAVIDEZ")), idademae = num(col(d, "IDADEMAE")),
        escmae = esc5(col(d, "ESCMAE"), col(d, "ESCMAE2010")),
        apgar5 = num(col(d, "APGAR5")), consultas = as.character(col(d, "CONSULTAS")),
        idanomal = as.character(col(d, "IDANOMAL")))
    })
    h <- rbindlist(l); rm(l)
    h <- h[!is.na(dt_nasc)]
    h[peso %in% c(0L, 9999L), peso := NA]; h[semagestac %in% c(0L, 99L) | semagestac > 45, semagestac := NA]
    h[idademae %in% c(0L, 99L), idademae := NA]
    saveRDS(h, fo)
    say("  ", ano, ": ", fmt(nrow(h)), " nascidos vivos")
  }
}

# ============================================================================
# PARTE 4 · Linkage SIM × SINASC (por ano de nascimento)
#   Blocagem A: data de nascimento + sexo + UF de residência
#   Blocagem B (só para quem não pareou em A): data + sexo + CNES do óbito =
#              CNES do nascimento (cobre mudança de residência na DO)
#   Escore (somente variáveis presentes nos DOIS registros contam):
#     município de residência igual +3 · CNES nasc = CNES óbito +2 ·
#     município nasc = município ocorrência do óbito +1 ·
#     peso |Δ|<=20 g +4, <=100 +2, <=250 0, >250 −3 ·
#     semanas iguais +2, |Δ|=1 +1, |Δ|>3 −2 (senão GESTACAO igual +1 / dif −1) ·
#     idade da mãe igual +3, |Δ|=1 +1, |Δ|>2 −3 · tipo de parto igual +1 / dif −1 ·
#     tipo de gravidez igual +1 / dif −2 · escolaridade da mãe igual +1 ·
#     raça/cor igual +1
#   Aceite: escore >= 10 ("alta") ou >= 7 ("média"), par 1:1 guloso.
#   Sem variável da mãe/nascimento na DO (comum em 1–6 anos): aceita só se a
#   chave nasc+sexo+município for única nos dois lados ("chave única").
# ============================================================================
pontua <- function(c) {
  c[, `:=`(
    p_mun  = fifelse(!is.na(mres) & !is.na(munic_res) & mres == munic_res, 3L, 0L),
    p_cnes = fifelse(!is.na(codestab) & !is.na(cnes_nasc) & codestab == cnes_nasc, 2L, 0L),
    p_mnas = fifelse(!is.na(munic_ocor) & !is.na(munic_nasc) & munic_ocor == munic_nasc, 1L, 0L),
    p_peso = fifelse(is.na(peso_o) | is.na(peso), 0L, { dd <- abs(peso_o - peso)
      fifelse(dd <= 20, 4L, fifelse(dd <= 100, 2L, fifelse(dd <= 250, 0L, -3L))) }),
    p_sem  = fifelse(!is.na(sem_o) & !is.na(semagestac), { dd <- abs(sem_o - semagestac)
      fifelse(dd == 0, 2L, fifelse(dd == 1, 1L, fifelse(dd > 3, -2L, 0L))) },
      fifelse(!is.na(gest_o) & !gest_o %chin% c("9", "") & !is.na(gestacao) & !gestacao %chin% c("9", ""),
              fifelse(gest_o == gestacao, 1L, -1L), 0L)),
    p_imae = fifelse(is.na(imae_o) | is.na(idademae), 0L, { dd <- abs(imae_o - idademae)
      fifelse(dd == 0, 3L, fifelse(dd == 1, 1L, fifelse(dd > 2, -3L, 0L))) }),
    p_part = fifelse(parto_o %chin% c("1", "2") & parto %chin% c("1", "2"), fifelse(parto_o == parto, 1L, -1L), 0L),
    p_grav = fifelse(grav_o %chin% c("1", "2", "3") & gravidez %chin% c("1", "2", "3"), fifelse(grav_o == gravidez, 1L, -2L), 0L),
    p_esc  = fifelse(!is.na(esc_o) & !is.na(escmae) & esc_o == escmae, 1L, 0L),
    p_raca = fifelse(!is.na(raca_sim) & !is.na(raca_nv) & raca_sim == raca_nv, 1L, 0L))]
  c[, escore := p_mun + p_cnes + p_mnas + p_peso + p_sem + p_imae + p_part + p_grav + p_esc + p_raca]
  c[, n_vars_mae := (!is.na(peso_o)) + (!is.na(sem_o)) + (!is.na(imae_o)) + (parto_o %chin% c("1","2")) + (grav_o %chin% c("1","2","3"))]
  c
}
escolhe <- function(c) {
  if (!nrow(c)) return(c)
  setorder(c, id_sim, -escore)
  c[, segundo := shift(escore, -1L), by = id_sim]
  c[, margem := escore - fcoalesce(segundo[1L], -99L), by = id_sim]
  c <- c[escore >= CFG$escore_sinasc_min & n_vars_mae >= 1]
  setorder(c, id_nasc, -escore); c <- c[, .SD[1L], by = id_nasc]
  setorder(c, id_sim, -escore); c <- c[, .SD[1L], by = id_sim]
  c[, classe := fifelse(escore >= CFG$escore_sinasc_alta & margem >= 2, "Alta", "Media")]
  c
}
if (4 %in% RODAR) {
  say("\nPARTE 4 · SIM x SINASC")
  sim <- readRDS(tmp("sim_universo.rds"))
  so <- sim[, .(id_sim, dt_nasc, sexo, uf_res, mres = munic_res, munic_ocor, codestab, raca_sim,
                peso_o = fifelse(peso %in% c(0L, 9999L), NA_integer_, peso),
                sem_o = fifelse(semagestac %in% c(0L, 99L) | semagestac > 45, NA_integer_, semagestac),
                gest_o = gestacao, imae_o = fifelse(idademae %in% c(0L, 99L), NA_integer_, idademae),
                parto_o = parto, grav_o = gravidez, esc_o = esc5(escmae, NA))]
  so[, ano_n := year(dt_nasc)]
  res <- list(); chave <- list()
  for (ano in CFG$anos_nasc) {
    fo <- tmp(sprintf("sinasc_h_%d.rds", ano)); if (!file.exists(fo)) next
    nv <- readRDS(fo); nv[, uf_res := unname(UF_COD[substr(munic_res, 1, 2)])]
    o <- so[ano_n == ano & !is.na(sexo)]
    if (!nrow(o)) next
    # multiplicidade de chave (nasc+sexo+município) no SINASC — usada adiante
    kn <- nv[, .N, by = .(dt_nasc, sexo, munic_res)]
    ko <- merge(o[, .(id_sim, dt_nasc, sexo, munic_res = mres)], kn, by = c("dt_nasc", "sexo", "munic_res"), all.x = TRUE)
    chave[[length(chave) + 1]] <- ko[, .(id_sim, n_nv_chave = fcoalesce(N, 0L))]
    nvA <- nv[, .(id_nasc, dt_nasc, sexo, uf_res, munic_res, munic_nasc, cnes_nasc, raca_nv,
                  peso, semagestac, gestacao, idademae, parto, gravidez, escmae)]
    # em blocos de 15 dias de nascimento para caber na memória
    o[, lote := (yday(dt_nasc) - 1L) %/% 15L]; nvA[, lote := (yday(dt_nasc) - 1L) %/% 15L]
    pA <- rbindlist(lapply(sort(unique(o$lote)), function(L) {
      cc <- merge(o[lote == L, !"lote"], nvA[lote == L, !"lote"], by = c("dt_nasc", "sexo", "uf_res"), allow.cartesian = TRUE)
      if (!nrow(cc)) return(NULL)
      cc <- pontua(cc)
      # pré-escolha: só os 3 melhores por óbito seguem para o 1:1 global
      setorder(cc, id_sim, -escore); cc <- cc[, head(.SD, 3L), by = id_sim]
      cc[, .(id_sim, id_nasc, escore, n_vars_mae)]
    }), fill = TRUE)
    nvA[, lote := NULL]; o[, lote := NULL]
    pA <- if (nrow(pA)) escolhe(pA) else pA
    if (nrow(pA)) pA[, passo := "A"]
    rest <- o[!id_sim %in% pA$id_sim & !is.na(codestab)]
    nvB <- nvA[!id_nasc %in% pA$id_nasc & !is.na(cnes_nasc)]
    cB <- merge(rest, nvB[, !"uf_res"], by.x = c("dt_nasc", "sexo", "codestab"),
                by.y = c("dt_nasc", "sexo", "cnes_nasc"), allow.cartesian = TRUE)
    if (nrow(cB)) { cB[, cnes_nasc := codestab]; pB <- escolhe(pontua(cB)); if (nrow(pB)) pB[, passo := "B"] } else pB <- NULL
    # chave única (sem variável da mãe na DO)
    usados_s <- c(pA$id_sim, pB$id_sim); usados_n <- c(pA$id_nasc, pB$id_nasc)
    oc <- o[!id_sim %in% usados_s & is.na(peso_o) & is.na(sem_o) & is.na(imae_o) & !is.na(mres)]
    oc[, n_o := .N, by = .(dt_nasc, sexo, mres)]
    nc <- nvA[!id_nasc %in% usados_n, .(id_nasc, dt_nasc, sexo, munic_res, cnes_nasc, peso, semagestac, idademae)]
    nc[, n_n := .N, by = .(dt_nasc, sexo, munic_res)]
    pC <- merge(oc[n_o == 1], nc[n_n == 1], by.x = c("dt_nasc", "sexo", "mres"), by.y = c("dt_nasc", "sexo", "munic_res"))
    if (nrow(pC)) pC[, `:=`(escore = NA_integer_, classe = "Chave unica", passo = "C", margem = NA_integer_)]
    keep <- c("id_sim", "id_nasc", "escore", "margem", "classe", "passo")
    pega <- function(x) if (!is.null(x) && nrow(x) && "passo" %in% names(x)) x[, ..keep] else NULL
    res[[length(res) + 1]] <- rbindlist(list(pega(pA), pega(pB), pega(pC)), fill = TRUE)
    say("  nasc ", ano, ": obitos ", fmt(nrow(o)), " | pareados A ", fmt(nrow(pA)), " B ", fmt(NROW(pB)), " C ", fmt(nrow(pC)))
    rm(nv, nvA, cA, cB, nc, oc); gc()
  }
  lk <- rbindlist(res, fill = TRUE); kk <- rbindlist(chave)
  saveRDS(lk, tmp("link_sim_sinasc.rds")); saveRDS(kk, tmp("chave_sim_sinasc.rds"))
  say("  total SIM x SINASC: ", fmt(nrow(lk)), " obitos pareados")
}

# ============================================================================
# PARTE 5 · SINASC × SIH — nascidos vivos com possíveis internações
#   Chave: data de nascimento + sexo + município de residência (a AIH não tem
#   variável da mãe). Vínculo aceito quando a chave é única no SINASC; se não
#   for, desempata pelo CNES do nascimento = CNES da AIH com internação nos 2
#   primeiros dias de vida (AIH do nascimento) — se ainda empatar, fica
#   "ambígua" e não entra.
# ============================================================================
if (5 %in% RODAR) {
  say("\nPARTE 5 · SINASC x SIH")
  # AIHs separadas por ano de nascimento (uma vez), para não manter 12,5 mi linhas na memória
  if (!file.exists(tmp("aih_nasc_2024.rds"))) {
    aih <- readRDS(tmp("aih.rds"))
    aih[, c("uf_hosp", "raca_sih", "cid_sec", "proc", "ord", "pac") := NULL]; gc()
    aih[, ano_n := year(dt_nasc)]
    for (y in CFG$anos_nasc) saveRDS(aih[ano_n == y], tmp(sprintf("aih_nasc_%d.rds", y)))
    rm(aih); gc()
  }
  lk_sim <- if (file.exists(tmp("link_sim_sinasc.rds"))) readRDS(tmp("link_sim_sinasc.rds")) else NULL
  sim <- readRDS(tmp("sim_universo.rds"))[, .(id_sim, dt_obito, causabas, universo, pareado)]
  resumo <- list()
  dir.create(arq("nascidos_vivos_internacoes"), showWarnings = FALSE)
  for (ano in CFG$anos_nasc) {
    fo <- tmp(sprintf("sinasc_h_%d.rds", ano)); if (!file.exists(fo)) next
    fr <- tmp(sprintf("resumo_nv_%d.rds", ano))
    if (file.exists(fr) && file.exists(arq("nascidos_vivos_internacoes", sprintf("nv_internacoes_%d.csv.gz", ano)))) {
      resumo[[length(resumo) + 1]] <- readRDS(fr); next }
    nv <- readRDS(fo)
    a <- readRDS(tmp(sprintf("aih_nasc_%d.rds", ano)))[!is.na(sexo) & !is.na(munic_res)]
    nv[, n_chave := .N, by = .(dt_nasc, sexo, munic_res)]
    # (i) chave única no SINASC
    k1 <- nv[n_chave == 1L, .(id_nasc, dt_nasc, sexo, munic_res)]
    um <- merge(a[, .(id_aih, dt_nasc, sexo, munic_res)], k1, by = c("dt_nasc", "sexo", "munic_res"))[, .(id_aih, id_nasc, metodo = "Chave unica")]
    # (ii) chave repetida: desempate pela AIH do nascimento no CNES do nascimento
    an <- a[!id_aih %in% um$id_aih & as.integer(dt_inter - dt_nasc) <= 1L & !is.na(cnes), .(id_aih, dt_nasc, sexo, munic_res, cnes)]
    kn <- nv[n_chave > 1L & !is.na(cnes_nasc), .(id_nasc, dt_nasc, sexo, munic_res, cnes = cnes_nasc)]
    de <- merge(an, kn, by = c("dt_nasc", "sexo", "munic_res", "cnes"))
    de[, n := .N, by = id_aih]
    de <- de[n == 1L, .(id_aih, id_nasc, metodo = "Desempate CNES nascimento")]
    pares <- rbind(um, de)
    pares <- merge(pares, a[, .(id_aih, ep_id)], by = "id_aih")
    # AIHs do mesmo episódio de uma AIH vinculada por desempate
    ext <- merge(a[!id_aih %in% pares$id_aih, .(id_aih, ep_id)],
                 unique(pares[metodo != "Chave unica", .(ep_id, id_nasc)]), by = "ep_id")
    if (nrow(ext)) pares <- rbind(pares, ext[, .(id_aih, id_nasc, metodo = "Mesmo episodio", ep_id)])
    pares <- merge(pares, a[, .(id_aih, dt_inter, dt_saida, desfecho, cid_ent, cnes, idade_dias, uti)], by = "id_aih")
    pares <- merge(pares, nv[, .(id_nasc, dt_nasc, cnes_nasc)], by = "id_nasc")
    setorder(pares, id_nasc, dt_inter)
    res_nv <- pares[, .(
      n_aih = .N, n_episodios = uniqueN(ep_id),
      aih_nascimento = any(as.integer(dt_inter - dt_nasc) <= 1L),
      aih_nasc_mesmo_cnes = any(as.integer(dt_inter - dt_nasc) <= 1L & !is.na(cnes) & cnes == cnes_nasc),
      primeira_internacao = min(dt_inter), idade_dias_1a_aih = min(as.integer(dt_inter - dt_nasc)),
      n_reinternacoes = pmax(0L, uniqueN(ep_id) - 1L),
      teve_uti = any(!is.na(uti) & uti > 0), obito_sih = any(desfecho == "Obito"),
      cid_1a_aih = cid_ent[1L], metodo_vinculo = paste(sort(unique(metodo)), collapse = "+")), by = id_nasc]
    nvs <- merge(nv[, !"n_chave"], res_nv, by = "id_nasc")
    if (!is.null(lk_sim)) {
      ob <- merge(lk_sim[, .(id_sim, id_nasc, classe_sinasc = classe)],
                  sim[, .(id_sim, dt_obito, causabas, universo, pareado_sih = pareado)], by = "id_sim")
      nvs <- merge(nvs, ob, by = "id_nasc", all.x = TRUE)
    }
    nvs[, ano_nasc := ano]
    fwrite(nvs, arq("nascidos_vivos_internacoes", sprintf("nv_internacoes_%d.csv.gz", ano)))
    nv[, uf_res := unname(UF_COD[substr(munic_res, 1, 2)])]
    nv[, com_aih := id_nasc %chin% res_nv$id_nasc]; nv[, ano_nasc := ano]
    rr <- nv[, .(nascidos_vivos = .N, com_aih_vinculada = sum(com_aih),
                 chave_unica = sum(n_chave == 1L)), by = .(ano_nasc, uf_res)]
    saveRDS(rr, fr); resumo[[length(resumo) + 1]] <- rr
    say("  nasc ", ano, ": NV ", fmt(nrow(nv)), " | AIHs da coorte ", fmt(nrow(a)),
        " | AIHs vinculadas ", fmt(uniqueN(pares$id_aih)), " (", pct(uniqueN(pares$id_aih), nrow(a)), "%)",
        " | NV com AIH ", fmt(nrow(res_nv)))
    rm(nv, a, pares, res_nv, nvs, um, de, an, kn, k1); gc()
  }
  rs <- rbindlist(resumo); fwrite(rs, arq("resumo_nascidos_vivos_aih.csv"))
}

# ============================================================================
# PARTE 6 · Tabelas (universo SUS), figura e bases finais
# ============================================================================
if (6 %in% RODAR) {
  say("\nPARTE 6 · Tabelas e bases finais")
  suppressPackageStartupMessages({ library(openxlsx); library(ggplot2) })
  sim <- readRDS(tmp("sim_universo.rds"))
  lk  <- if (file.exists(tmp("link_sim_sinasc.rds"))) readRDS(tmp("link_sim_sinasc.rds")) else NULL
  kk  <- if (file.exists(tmp("chave_sim_sinasc.rds"))) readRDS(tmp("chave_sim_sinasc.rds")) else NULL
  tem_sinasc <- !is.null(lk) && nrow(lk) > 0
  if (tem_sinasc) {
    nvcols <- c("id_nasc", "cnes_nasc", "munic_nasc", "peso", "semagestac", "idademae",
                "escmae", "parto", "gravidez", "apgar5", "consultas", "raca_nv")
    nvl <- rbindlist(lapply(CFG$anos_nasc, function(a) { f <- tmp(sprintf("sinasc_h_%d.rds", a))
      if (!file.exists(f)) return(NULL); x <- readRDS(f)[id_nasc %chin% lk$id_nasc, ..nvcols]; x }))
    setnames(nvl, setdiff(nvcols, "id_nasc"), paste0("nv_", setdiff(nvcols, "id_nasc")))
    lk2 <- merge(lk, nvl, by = "id_nasc", all.x = TRUE)
    sim <- merge(sim, lk2[, c("id_sim", "id_nasc", "escore", "classe", "passo", grep("^nv_", names(lk2), value = TRUE)), with = FALSE],
                 by = "id_sim", all.x = TRUE)
    setnames(sim, c("escore", "classe", "passo"), c("escore_sinasc", "classe_sinasc", "passo_sinasc"))
    if (!is.null(kk)) sim <- merge(sim, kk, by = "id_sim", all.x = TRUE)
    sim[, link_sinasc := !is.na(id_nasc)]
    sim[, nasceu_mesmo_hosp := link_sinasc & !is.na(nv_cnes_nasc) & !is.na(codestab) & nv_cnes_nasc == codestab]
  }
  U <- sim[universo == TRUE]
  U[, grupo := fifelse(pareado, "Pareados", "Sem AIH")]
  N_U <- nrow(U); N_S <- sum(!U$pareado)

  # ---- explicação hierárquica dos sem AIH (Tabela 3 refeita) --------------
  U[, explic := fcase(
    pareado, NA_character_,
    chave_incompleta, "Sexo ou municipio ignorado na DO (nao pareavel pela chave)",
    aih_cobre_obito_mesmo_cnes & desf_aih_cobre_obito %chin% "Obito", "AIH de obito no mesmo hospital nao pareada (gemeos/duplicidade ou chave divergente)",
    aih_cobre_obito_mesmo_cnes, "AIH no mesmo hospital na data do obito, com outro motivo de saida (alta/permanencia)",
    aih_cobre_obito_outro_cnes, "AIH em outro hospital na data do obito (transferencia sem AIH de obito)",
    n_internacoes_previas > 0, "Internacao anterior no SIH, mas sem AIH no episodio do obito",
    dia0 & (if (tem_sinasc) nasceu_mesmo_hosp else TRUE), "Obito no dia do nascimento, no hospital onde nasceu (provavel AIH da mae)",
    dia0, "Obito no dia do nascimento, demais",
    aih_nascimento, "Somente AIH do nascimento, obito em outra internacao sem AIH",
    default = "Nenhuma AIH da crianca encontrada no SIH")]
  t_univ <- sim[, .N, by = comp_universo][order(comp_universo)]
  t_univ[, pct := round(100 * N / sum(N), 1)]
  t_univ <- rbind(t_univ, data.table(comp_universo = "Total SIM 0-6 anos", N = nrow(sim), pct = 100))
  t_univ2 <- data.table(etapa = c("Obitos 0-6 anos no SIM, 2015-2024", "Universo SUS (hospital com AIH 0-6 no periodo)",
                                  "Pareados a uma AIH de obito", "Sem AIH pareada"),
                        n = c(nrow(sim), N_U, N_U - N_S, N_S),
                        pct = c(100, round(100 * N_U / nrow(sim), 1), round(100 * (N_U - N_S) / N_U, 1), round(100 * N_S / N_U, 1)))
  t3a <- U[pareado == FALSE, .N, by = comp_sem_aih][, pct := round(100 * N / sum(N), 1)][order(-N)]
  t3b <- U[pareado == FALSE, .N, by = explic][, pct := round(100 * N / sum(N), 1)][order(-N)]

  # ---- perfil pareados × sem AIH ------------------------------------------
  cat_tab <- function(D, v, rot, lab = NULL) {
    D <- copy(D); D[, cat := as.character(get(v))]; D[is.na(cat) | cat == "", cat := "Ignorado"]
    if (!is.null(lab)) D[, cat := fifelse(cat %chin% names(lab), lab[cat], cat)]
    t <- dcast(D[, .N, by = .(cat, grupo)], cat ~ grupo, value.var = "N", fill = 0)
    for (g in c("Pareados", "Sem AIH")) if (!g %in% names(t)) t[, (g) := 0L]
    t[, `:=`(pct_par = round(100 * Pareados / sum(Pareados), 1), pct_sem = round(100 * `Sem AIH` / sum(`Sem AIH`), 1),
             pct_sem_cat = round(100 * `Sem AIH` / (Pareados + `Sem AIH`), 1))]
    cbind(variavel = rot, t)
  }
  U[, mesmo_mun := fifelse(!is.na(munic_ocor) & munic_ocor == munic_res, "Mesmo municipio de residencia", "Outro municipio")]
  U[, periodo := fifelse(ano <= 2019, "2015-2019", "2020-2024")]
  U[, uti := fifelse(is.na(leitos_uti_neo), "CNES sem cadastro", fifelse((leitos_uti_neo + fcoalesce(leitos_uti_ped, 0L)) > 0, "Hospital com UTI neo/ped", "Hospital sem UTI neo/ped"))]
  lab_sim <- c("1" = "Sim", "2" = "Nao", "9" = "Ignorado")
  t4 <- rbindlist(list(
    cat_tab(U, "sexo", "Sexo", c(M = "Masculino", F = "Feminino")),
    cat_tab(U, "raca_sim", "Raca/cor"), cat_tab(U, "faixa", "Faixa etaria"),
    cat_tab(U, "mesmo_mun", "Municipio de ocorrencia"), cat_tab(U, "uti", "Tipo de estabelecimento"),
    cat_tab(U, "assistmed", "Assistencia medica", lab_sim), cat_tab(U, "necropsia", "Necropsia", lab_sim),
    cat_tab(U, "periodo", "Periodo")), fill = TRUE)
  med <- U[, .(mediana_idade_dias = as.numeric(median(idade_dias)), p25 = as.numeric(quantile(idade_dias, .25)),
               p75 = as.numeric(quantile(idade_dias, .75))), by = grupo]
  # mãe/nascimento (<1 ano)
  M <- U[idade_dias < 365]
  M[, `:=`(imae = fcase(is.na(idademae) | idademae %in% c(0L, 99L), "Ignorado", idademae < 20, "<20 anos", idademae < 35, "20-34 anos", default = "35+ anos"),
           esc = fcase(escmaeagr1 %chin% c("00"), "Sem escolaridade", escmaeagr1 %chin% c("01","02","03","04"), "Fundamental",
                       escmaeagr1 %chin% c("05","06"), "Medio", escmaeagr1 %chin% c("07","08"), "Superior", escmaeagr1 %chin% c("09"), "Ignorado",
                       escmae == "1", "Sem escolaridade", escmae %chin% c("2","3"), "Fundamental", escmae == "4", "Medio", escmae == "5", "Superior", default = "Ignorado"),
           ig = fcase(is.na(semagestac) | semagestac %in% c(0L, 99L), fcase(gestacao %chin% c("1","2"), "<28 sem", gestacao == "3", "28-31 sem", gestacao == "4", "32-36 sem", gestacao %chin% c("5","6"), "37+ sem", default = "Ignorado"),
                      semagestac < 28, "<28 sem", semagestac < 32, "28-31 sem", semagestac < 37, "32-36 sem", default = "37+ sem"),
           pesoc = fcase(is.na(peso) | peso %in% c(0L, 9999L), "Ignorado", peso < 1500, "<1.500 g", peso < 2500, "1.500-2.499 g", default = "2.500 g+"))]
  t5 <- rbindlist(list(cat_tab(M, "imae", "Idade da mae"), cat_tab(M, "esc", "Escolaridade da mae"),
                       cat_tab(M, "ig", "Idade gestacional"), cat_tab(M, "pesoc", "Peso ao nascer"),
                       cat_tab(M, "gravidez", "Tipo de gravidez", c("1" = "Unica", "2" = "Dupla", "3" = "Tripla+", "9" = "Ignorado")),
                       cat_tab(M, "parto", "Tipo de parto", c("1" = "Vaginal", "2" = "Cesareo", "9" = "Ignorado"))), fill = TRUE)
  # regiões / UF
  t6 <- U[, .(obitos_universo = .N, sem_aih = sum(!pareado), pct_sem_aih = round(100 * mean(!pareado), 1)), by = regiao][order(regiao)]
  t6uf <- U[, .(obitos_universo = .N, sem_aih = sum(!pareado), pct_sem_aih = round(100 * mean(!pareado), 1)), by = .(regiao, uf_res)][order(-pct_sem_aih)]
  # causas
  U[, grupo_causa := fcase(substr(causabas, 1, 1) == "P", "Perinatais (P)", substr(causabas, 1, 1) == "Q", "Malformacoes (Q)",
    substr(causabas, 1, 1) == "J", "Respiratorias (J)", substr(causabas, 1, 1) %chin% c("A", "B"), "Infecciosas (A-B)",
    substr(causabas, 1, 1) %chin% c("V", "W", "X", "Y"), "Causas externas (V-Y)", substr(causabas, 1, 1) == "G", "Sistema nervoso (G)",
    substr(causabas, 1, 1) == "C" | (substr(causabas, 1, 1) == "D" & num(substr(causabas, 2, 3)) <= 48), "Neoplasias (C-D48)",
    substr(causabas, 1, 1) == "R", "Mal definidas (R)", default = "Demais")]
  t7 <- cat_tab(U, "grupo_causa", "Grupo de causa basica")
  t7top <- U[pareado == FALSE, .N, by = causabas][order(-N)][1:15]
  t7top <- merge(t7top, U[, .(pct_sem_cod = round(100 * mean(!pareado), 1)), by = causabas], by = "causabas")[order(-N)]
  t7top[, pct_dos_sem := round(100 * N / N_S, 1)]
  # trajetória (retroação) — pareados × sem AIH
  t8 <- U[chave_incompleta == FALSE, .(obitos = .N,
          pct_alguma_aih = round(100 * mean(n_aih_vida > 0), 1),
          pct_aih_nascimento = round(100 * mean(aih_nascimento), 1),
          pct_internacao_previa = round(100 * mean(n_internacoes_previas > 0), 1),
          pct_reinternacao_30d = round(100 * mean(reinternacao_30d %in% TRUE), 1),
          mediana_episodios = as.numeric(median(n_episodios_vida)),
          pct_aih_aberta_obito_mesmo_hosp = round(100 * mean(aih_cobre_obito_mesmo_cnes), 1),
          pct_chave_ambigua_sim = round(100 * mean(n_sim_chave > 1, na.rm = TRUE), 1)), by = grupo]
  t8desf <- U[pareado == FALSE & aih_cobre_obito_mesmo_cnes == TRUE, .N, by = desf_aih_cobre_obito][, pct := round(100 * N / sum(N), 1)][order(-N)]
  # SINASC
  if (tem_sinasc) {
    t9 <- rbindlist(list(
      sim[, .(recorte = "Todos os obitos 0-6 (SIM)", obitos = .N, linkados = sum(link_sinasc)), ],
      sim[universo == TRUE, .(recorte = "Universo SUS", obitos = .N, linkados = sum(link_sinasc))],
      sim[universo == TRUE & idade_dias < 365, .(recorte = "Universo SUS, <1 ano", obitos = .N, linkados = sum(link_sinasc))],
      sim[universo == TRUE & idade_dias == 0, .(recorte = "Universo SUS, dia 0", obitos = .N, linkados = sum(link_sinasc))],
      sim[universo == TRUE & pareado == FALSE, .(recorte = "Universo SUS, sem AIH", obitos = .N, linkados = sum(link_sinasc))],
      sim[universo == TRUE & pareado == TRUE, .(recorte = "Universo SUS, pareados", obitos = .N, linkados = sum(link_sinasc))]))
    t9[, pct := round(100 * linkados / obitos, 1)]
    t9f <- sim[universo == TRUE, .(obitos = .N, linkados = sum(link_sinasc), pct = round(100 * mean(link_sinasc), 1)), by = faixa]
    t9c <- sim[link_sinasc == TRUE, .N, by = .(classe_sinasc, passo_sinasc)][, pct := round(100 * N / sum(N), 1)]
    t9v <- sim[link_sinasc == TRUE & universo == TRUE & idade_dias == 0,
               .(obitos_dia0_linkados = .N, pct_nasceu_no_hospital_do_obito = round(100 * mean(nasceu_mesmo_hosp), 1),
                 pct_peso_concorda_20g = round(100 * mean(abs(peso - nv_peso) <= 20, na.rm = TRUE), 1),
                 pct_idade_mae_igual = round(100 * mean(idademae == nv_idademae, na.rm = TRUE), 1))]
  }
  rs <- if (file.exists(arq("resumo_nascidos_vivos_aih.csv"))) fread(arq("resumo_nascidos_vivos_aih.csv")) else NULL
  t10 <- if (!is.null(rs)) rs[, .(nascidos_vivos = sum(nascidos_vivos), com_aih_vinculada = sum(com_aih_vinculada),
                                   pct_com_aih = round(100 * sum(com_aih_vinculada) / sum(nascidos_vivos), 1),
                                   pct_chave_unica = round(100 * sum(chave_unica) / sum(nascidos_vivos), 1)), by = ano_nasc][order(ano_nasc)] else NULL

  wb <- createWorkbook()
  add <- function(nome, x, titulo) { addWorksheet(wb, nome); writeData(wb, nome, titulo, startRow = 1)
    writeData(wb, nome, x, startRow = 3, headerStyle = createStyle(textDecoration = "bold")); setColWidths(wb, nome, 1:ncol(x), "auto") }
  add("T1_universo", t_univ2, "Tabela 1. Universo: obitos 0-6 anos em hospital que atende SUS, 2015-2024")
  add("T1b_exclusoes", t_univ, "Tabela 1b. Composicao do SIM e o que saiu do universo")
  add("T2_sem_AIH_dia0", t3a, "Tabela 2. Obitos sem AIH no universo SUS, por dia do obito")
  add("T3_sem_AIH_explicacao", t3b, "Tabela 3. De onde vem os obitos sem AIH (classificacao hierarquica, SIH + SINASC)")
  add("T4_perfil", t4, "Tabela 4. Perfil geral - pareados x sem AIH (universo SUS)")
  add("T4b_idade_mediana", med, "Idade ao obito em dias - mediana (IIQ)")
  add("T5_mae_nascimento", t5, "Tabela 5. Mae e nascimento (<1 ano) - variaveis da DO")
  add("T6_regiao", t6, "Tabela 6. Regioes"); add("T6b_UF", t6uf, "Tabela 6b. UF de residencia")
  add("T7_causas", t7, "Tabela 7. Grupos de causa basica"); add("T7b_top15", t7top, "Tabela 7b. 15 causas basicas mais frequentes entre os sem AIH")
  add("T8_trajetoria", t8, "Tabela 8. Retroacao no SIH - internacoes da crianca ate o obito")
  add("T8b_desfecho_AIH_aberta", t8desf, "Tabela 8b. Motivo de saida da AIH aberta no hospital do obito (sem AIH de obito)")
  if (tem_sinasc) { add("T9_SINASC", t9, "Tabela 9. Linkage SIM x SINASC"); add("T9b_faixa", t9f, "Tabela 9b. SIM x SINASC por faixa (universo)")
    add("T9c_classe", t9c, "Tabela 9c. Classe do par SIM x SINASC"); add("T9d_validacao_dia0", t9v, "Tabela 9d. Validacao - obitos no dia 0") }
  if (!is.null(t10)) add("T10_NV_internacoes", t10, "Tabela 10. Nascidos vivos com internacao vinculada no SIH")
  saveWorkbook(wb, arq("Tabelas_universo_SUS.xlsx"), overwrite = TRUE)

  # ---- figura: % sem AIH por UF (universo SUS) ----------------------------
  g <- ggplot(t6uf, aes(x = reorder(uf_res, pct_sem_aih), y = pct_sem_aih, fill = regiao)) +
    geom_col(width = .75) + coord_flip() +
    geom_hline(yintercept = 100 * N_S / N_U, linetype = 2, colour = "grey40") +
    geom_text(aes(label = sprintf("%.0f", pct_sem_aih)), hjust = -.15, size = 3) +
    scale_y_continuous(expand = expansion(mult = c(0, .08))) +
    scale_fill_manual(values = c(Norte = "#2a9d8f", Nordeste = "#e9c46a", Sudeste = "#e76f51", Sul = "#264653", `Centro-Oeste` = "#8e7dbe")) +
    labs(x = NULL, y = "% dos obitos sem AIH pareada", fill = NULL,
         title = "Obitos hospitalares 0-6 anos sem AIH - universo SUS",
         subtitle = sprintf("Hospitais com AIH 0-6 no periodo, 2015-2024. Linha: Brasil (%.1f%%)", 100 * N_S / N_U)) +
    theme_minimal(base_size = 10) + theme(legend.position = "bottom", panel.grid.major.y = element_blank())
  ggsave(arq("Fig01_pct_sem_AIH_UF_universo_SUS.png"), g, width = 7, height = 7.5, dpi = 200, bg = "white")
  f_uf <- file.path(raiz, "Dash/uf_sf_simplified.rds")
  if (file.exists(f_uf) && requireNamespace("sf", quietly = TRUE)) {
    mp <- merge(readRDS(f_uf), as.data.frame(t6uf), by.x = "abbrev_state", by.y = "uf_res")
    gm <- ggplot(mp) + geom_sf(aes(fill = pct_sem_aih), colour = "white", linewidth = .2) +
      geom_sf_text(aes(label = sprintf("%s\n%.0f", abbrev_state, pct_sem_aih)), size = 2.3, colour = "grey15") +
      scale_fill_gradient(low = "#fde9d9", high = "#b2182b", name = "% sem AIH") +
      labs(title = "Obitos 0-6 anos sem AIH pareada, por UF de residencia",
           subtitle = "Universo SUS: obito em hospital com AIH 0-6 no periodo (sem UPA/PS e sem saude suplementar), 2015-2024") +
      theme_void(base_size = 10) + theme(legend.position = "right")
    ggsave(arq("Fig02_mapa_pct_sem_AIH_UF_universo_SUS.png"), gm, width = 7.5, height = 7, dpi = 200, bg = "white")
  }

  # ---- bases finais ---------------------------------------------------------
  dir.create(arq("bases_finais"), showWarnings = FALSE)
  cols_ob <- intersect(c("id_sim", "contador", "ano", "dt_nasc", "dt_obito", "idade_dias", "faixa", "sexo", "raca_sim",
    "uf_res", "regiao", "munic_res", "munic_ocor", "lococor", "codestab", "nome", "tipo_unidade", "leitos_sus",
    "comp_universo", "universo", "causabas", "causabas4", "pareado", "metodo", "escore_sih", "comp_sem_aih",
    "n_aih_vida", "n_episodios_vida", "aih_nascimento", "aih_nasc_mesmo_cnes", "aih_cobre_obito_mesmo_cnes",
    "desf_aih_cobre_obito", "aih_cobre_obito_outro_cnes", "n_internacoes_previas", "dias_ultima_alta",
    "cid_ultima_internacao", "reinternacao_30d", "n_sim_chave", "chave_incompleta",
    "link_sinasc", "id_nasc", "escore_sinasc", "classe_sinasc", "passo_sinasc", "n_nv_chave", "nasceu_mesmo_hosp",
    grep("^nv_", names(sim), value = TRUE), "idademae", "escmae", "semagestac", "gestacao", "peso", "parto", "gravidez",
    "assistmed", "necropsia"), names(sim))
  sim <- merge(sim, U[, .(id_sim, explic_sem_aih = explic)], by = "id_sim", all.x = TRUE)
  cols_ob <- c(cols_ob, "explic_sem_aih")
  # um arquivo por ano do óbito (cabe no limite de upload do GitHub); filtros:
  #   universo == TRUE -> universo SUS · link_sinasc == TRUE -> pareados ao SINASC
  dir.create(arq("bases_finais", "obitos_linkados"), showWarnings = FALSE)
  for (a in CFG$anos) fwrite(sim[ano == a, ..cols_ob], arq("bases_finais", "obitos_linkados", sprintf("obitos_0_6_linkados_%d.csv.gz", a)))
  saveRDS(sim[, ..cols_ob], arq("bases_finais", "obitos_0_6_linkados_2015_2024.rds"))
  saveRDS(list(t_univ2 = t_univ2, t_univ = t_univ, t3a = t3a, t3b = t3b, t4 = t4, med = med, t5 = t5, t6 = t6,
               t6uf = t6uf, t7 = t7, t7top = t7top, t8 = t8, t8desf = t8desf,
               t9 = if (tem_sinasc) t9, t9f = if (tem_sinasc) t9f, t9c = if (tem_sinasc) t9c, t9v = if (tem_sinasc) t9v,
               t10 = t10, N_U = N_U, N_S = N_S, N_SIM = nrow(sim)), arq("tabelas_universo_sus.rds"))

  # ---- checagens -------------------------------------------------------------
  stopifnot(sum(t_univ$N[t_univ$comp_universo != "Total SIM 0-6 anos"]) == nrow(sim))
  stopifnot(sum(t3b$N) == N_S, sum(t3a$N) == N_S)
  if (tem_sinasc) stopifnot(!anyDuplicated(lk$id_nasc), !anyDuplicated(lk$id_sim))
  say("  universo SUS: ", fmt(N_U), " | sem AIH: ", fmt(N_S), " (", pct(N_S, N_U), "%)")
  say("  arquivos em ", CFG$dir_out)
}
say("\nFim: ", format(Sys.time(), "%H:%M"))
