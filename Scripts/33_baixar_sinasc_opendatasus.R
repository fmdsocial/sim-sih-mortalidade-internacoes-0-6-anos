# 33_baixar_sinasc_opendatasus.R — SINASC 2009–2024 pelo OpenDataSUS (S3, HTTPS)
# O FTP do DATASUS (porta 21) não conecta desta rede; o S3 do OpenDataSUS sim.
# Saída: SINASC/Dados/sinasc_BR_{ano}.rds (Brasil, colunas do linkage, códigos brutos)
raiz <- c("G:/My Drive/INSPER/Trabalho", "G:/Meu Drive/INSPER/Trabalho"); raiz <- raiz[dir.exists(raiz)][1]
dir_out <- file.path(raiz, "SINASC", "Dados"); dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
dir_zip <- file.path(Sys.getenv("TEMP", tempdir()), "sinasc_zip"); dir.create(dir_zip, showWarnings = FALSE)
lg <- file.path(dir_out, "_log_opendatasus.txt")
w <- function(...) { m <- paste0(format(Sys.time(), "%H:%M:%S "), ...); cat(m, "\n"); cat(m, "\n", file = lg, append = TRUE); flush.console() }
for (p in c("curl", "data.table")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org")
library(data.table)
VARS <- c("CODESTAB","CODMUNNASC","LOCNASC","CODMUNRES","DTNASC","HORANASC","SEXO","RACACOR","PESO",
          "GESTACAO","SEMAGESTAC","PARTO","GRAVIDEZ","IDADEMAE","ESCMAE","ESCMAE2010","ESCMAEAGR1",
          "APGAR1","APGAR5","CONSULTAS","IDANOMAL","CODANOMAL","QTDFILVIVO","QTDFILMORT","NUMERODN","CONTADOR")
ANOS <- 2024:2009
w("Inicio OpenDataSUS. Destino: ", dir_out)
for (ano in ANOS) {
  fo <- file.path(dir_out, sprintf("sinasc_BR_%d.rds", ano))
  if (file.exists(fo)) { w(ano, " ja existe"); next }
  url <- sprintf("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/csv/SINASC_%d_csv.zip", ano)
  z <- file.path(dir_zip, basename(url))
  ok <- FALSE
  for (t in 1:3) {
    r <- tryCatch({ curl::curl_download(url, z, quiet = TRUE, handle = curl::new_handle(connecttimeout = 60, low_speed_time = 120, low_speed_limit = 1000)); TRUE },
                  error = function(e) { w(ano, " download tentativa ", t, ": ", conditionMessage(e)); FALSE })
    if (r && file.size(z) > 1e6) { ok <- TRUE; break }
    Sys.sleep(5)
  }
  if (!ok) { w(ano, " FALHOU o download"); next }
  w(ano, " zip ", round(file.size(z) / 1e6), " MB")
  lst <- utils::unzip(z, list = TRUE); csv <- lst$Name[grepl("\\.csv$", lst$Name, ignore.case = TRUE)][1]
  ex <- tryCatch(utils::unzip(z, files = csv, exdir = dir_zip), error = function(e) { w(ano, " unzip erro: ", conditionMessage(e)); NULL })
  if (is.null(ex) || !length(ex)) { w(ano, " FALHOU unzip"); next }
  hd <- names(fread(ex, nrows = 0, encoding = "Latin-1"))
  sel <- hd[toupper(gsub('"', "", hd)) %in% VARS]
  d <- fread(ex, select = sel, colClasses = "character", encoding = "Latin-1", showProgress = FALSE)
  setnames(d, toupper(gsub('"', "", names(d))))
  saveRDS(d, paste0(fo, ".tmp"), compress = TRUE); file.rename(paste0(fo, ".tmp"), fo)
  w(ano, " OK ", format(nrow(d), big.mark = "."), " linhas, ", ncol(d), " colunas -> ", round(file.size(fo) / 1e6), " MB")
  unlink(c(ex, z)); rm(d); gc()
}
w("FIM")
writeLines(format(Sys.time()), file.path(dir_out, "_FIM_opendatasus.txt"))
