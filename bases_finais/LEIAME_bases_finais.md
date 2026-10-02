# Bases finais — universo SUS e linkage SIM × SIH × SINASC (02/10/2026)

Geradas por `Scripts/20_universo_sus_sinasc.R` (partes 1–6). Fontes: SIM e SIH/DATASUS (2015–2024) e SINASC/OpenDataSUS (2009–2024). Unidade e chave de cada arquivo abaixo.

## `obitos_linkados/obitos_0_6_linkados_AAAA.csv.gz` — um óbito do SIM por linha (ano do óbito)
Todos os 407.546 óbitos de 0–6 anos. Filtros principais:
- `universo == TRUE` → universo SUS (óbito em hospital, LOCOCOR 1, cujo CNES tem ≥1 AIH 0–6 no período; sem UPA/PS e sem hospital sem AIH). `comp_universo` diz por que cada óbito ficou fora.
- `pareado` → tem AIH de óbito pareada (regras v3.3: exato + probabilístico, ±3 dias, escore ≥3); `metodo`, `escore_sih`.
- `comp_sem_aih`, `explic_sem_aih` → classificação dos sem AIH (dia 0 / após; explicação hierárquica SIH + SINASC).
- Retroação no SIH (mesma criança: nascimento + sexo + município de residência): `n_aih_vida`, `n_episodios_vida`, `aih_nascimento`, `aih_cobre_obito_mesmo_cnes`, `desf_aih_cobre_obito`, `aih_cobre_obito_outro_cnes`, `n_internacoes_previas`, `dias_ultima_alta`, `cid_ultima_internacao`, `reinternacao_30d`, `n_sim_chave` (>1 = chave repetida no SIM).
- SINASC: `link_sinasc`, `id_nasc` (liga aos arquivos de nascidos vivos), `classe_sinasc` (Alta / Media / Chave unica), `passo_sinasc` (A: nasc+sexo+UF; B: nasc+sexo+CNES; C: chave única), `nasceu_mesmo_hosp`, variáveis `nv_*` do nascimento.

## `nascidos_vivos_internacoes/nv_internacoes_AAAA.csv.gz` — um nascido vivo por linha (ano de nascimento)
Só nascidos vivos com ≥1 AIH vinculada. Vínculo por nascimento + sexo + município de residência quando a chave é única no SINASC, ou pela AIH dos 2 primeiros dias de vida no CNES do nascimento (e demais AIHs do mesmo episódio). Colunas: `n_aih`, `n_episodios`, `aih_nascimento`, `aih_nasc_mesmo_cnes`, `primeira_internacao`, `idade_dias_1a_aih`, `n_reinternacoes`, `teve_uti`, `obito_sih`, `cid_1a_aih`, `metodo_vinculo`; se o nascido morreu e pareou ao SIM: `id_sim`, `dt_obito`, `causabas`, `universo`, `pareado_sih`.
Atenção: o SIH cobre 2015–2024 — nascidos antes de 2015 só têm internações a partir de 2015. Vínculo conservador (≈23% dos nascidos têm chave única): é a base de internações *possíveis*, não o total.

## `resumo_nascidos_vivos_aih.csv`
Todos os nascidos vivos por ano de nascimento e UF de residência: total, com AIH vinculada e com chave única.
