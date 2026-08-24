<div align="center">
  <h1>📊 SIM + SIH: Mortalidade e Internações (0 a 6 anos) no Brasil</h1>
  <p><strong>Projeto de análise reprodutível de dados de saúde pública (2015–2024)</strong></p>
  <p>Observatório de Saúde Infantil · INSPER · Hospital Pequeno Príncipe</p>

  <p>
    <img src="https://img.shields.io/badge/R-Shiny-004B87?logo=r&logoColor=white" alt="R Shiny">
    <img src="https://img.shields.io/badge/Dados-DATASUS%20(SIM%20·%20SIH%20·%20SINASC)-00A3A1" alt="DATASUS">
    <img src="https://img.shields.io/badge/Coorte-0–6%20anos-F4A261" alt="Coorte">
    <img src="https://img.shields.io/badge/Per%C3%ADodo-2015–2024-6c757d" alt="Período">
    <img src="https://img.shields.io/badge/Linkage-SIM%20↔%20SIH%20(v3.2)-8E44AD" alt="Linkage">
    <img src="https://img.shields.io/badge/Licen%C3%A7a-MIT-informational" alt="Licença MIT">
  </p>
</div>

---

## 📌 Resumo Executivo

Este projeto apresenta um mapeamento detalhado da **mortalidade e das internações hospitalares de crianças na primeira infância (0 a 6 anos)** no Brasil, com base em dados públicos oficiais do DataSUS.

O objetivo é fornecer subsídios epidemiológicos e assistenciais para a formulação de políticas públicas, identificação de desigualdades regionais, análise de causas prioritárias e compreensão dos fluxos de atendimento infantil no Sistema Único de Saúde (SUS).

O projeto integra três módulos analíticos:

* **SIM/SINASC** — mortalidade de crianças de 0 a 6 anos;
* **SIH/SINASC** — internações hospitalares de crianças de 0 a 6 anos;
* **Linkage SIM ↔ SIH** — pareamento entre o óbito hospitalar registrado no SIM e a AIH correspondente no SIH, permitindo reconstruir a trajetória hospitalar da criança e a transição do CID de entrada para o CID de óbito.

**Recortes etários analisados:**

* Neonatal precoce: 0–6 dias;
* Neonatal tardia: 7–27 dias;
* Pós-neonatal: 28 dias a <1 ano;
* Crianças de 1 a 6 anos.

---

## 🖥️ Dashboard Interativo (Shiny)

O projeto inclui o **Observatório de Saúde Infantil** (`app.R`), construído em R/Shiny com `bslib`, que reúne os três módulos na mesma navegação.

| Aba | Conteúdo |
|---|---|
| **Panorama** | Indicadores consolidados, mapa coroplético por UF, série histórica óbitos × internações e ranking de macrorregiões. |
| **Por Faixa Etária** | Três dashboards em um: ① Neonatal (0–27 d), ② Pós-neonatal (28 d a <1 ano) e ③ 1 a 6 anos — com KPIs, séries, priorização de causas e agrupamento de patologias semelhantes em cada faixa. |
| **Sistemas & Patologias** | Classificação de todo código CID-10 pelo capítulo e pelo sistema do organismo acometido, com consolidação de CIDs que representam a mesma condição clínica (cardiopatias congênitas, sepse, afogamento, prematuridade). |
| **Linkage & Trajetória** | Oito painéis do módulo de pareamento: qualidade do linkage, descritiva dos linkados, guia de CIDs, trajetória hospitalar (T₀→T₂), transição de CID (entrada → óbito), Δ entrada/óbito por região, causas secundárias e internações sem óbito (Etapa 5). |
| **Metas ODS/IPEA** | Acompanhamento das taxas frente às metas de referência. |
| **CID & Capítulos** | Detalhamento por CID, grandes capítulos, concentração em 2 dígitos e revisão de nomenclatura. |
| **Mortalidade** | Perfil temporal e causal, mapas e desigualdades, fluxos e polos de ocorrência do óbito. |
| **Internações** | Perfil temporal e causal, mapas e desigualdades, fluxos e polos de atendimento hospitalar. |
| **Metodologia** | Nota metodológica, fontes, cobertura do linkage carregado e lacunas de dados. |
| **Dados** | Tabelas executivas para auditoria e exportação (CSV/Excel). |

### Arquivos que o app carrega

O `app.R` procura os insumos **no próprio diretório de execução** (`readRDS("uf_sf_simplified.rds")`, `read_excel("Tabelas_Executivas_...")`), sem caminho absoluto e sem acesso à internet.

**Obrigatórios:**

```text
Tabelas_Executivas_Mortalidade_v2.xlsx
Tabelas_Executivas_Internacoes_0_6_Anos.xlsx
uf_sf_simplified.rds
```

**Opcionais** — quando ausentes, os painéis correspondentes exibem um banner explicando o que falta, em vez de quebrar ou de mostrar dado substituto:

```text
populacao_uf_faixa.csv       # saída de 01_baixar_populacao.R (IBGE) — libera o denominador de 1 a 6 anos
sih_sim_linkado.rds          # saída de 00_link_sih_sim_v3.R — libera trajetória e transição de CID
linkage_qualidade.rds        # métricas de qualidade do linkage
sih_nao_obito_agregado.rds   # Etapa 5 agregada
```

**Para executar localmente:**

```r
# na raiz do repositório, com os insumos ao lado do app.R
install.packages(c("shiny","bslib","bsicons","readxl","dplyr","tidyr",
                   "plotly","DT","sf","ggplot2","scales","htmltools"))
shiny::runApp("app.R")
```

> O `app.R` faz pré-checagem de dependências e falha com a lista de pacotes ausentes, em vez de erro opaco no meio do carregamento — o que também protege o deploy no shinyapps.io.

---

## 🔗 Linkage SIM ↔ SIH (fluxo em 5 etapas)

O script [`Scripts/00_link_sih_sim_v3.R`](Scripts/00_link_sih_sim_v3.R) (versão de lógica **v3.2**) pareia óbitos e internações sem download, lendo as bases já processadas localmente. A direção do pareamento parte do SIM, e não do SIH como na v2, porque o alvo epidemiológico é o óbito hospitalar e não a internação.

| Etapa | O que faz |
|---|---|
| **1 · Alvo** | Identifica no SIM os óbitos hospitalares pelo `LOCOCOR` (1 = hospital, 2 = outros estabelecimentos de saúde). Como não há informação sobre a fonte pagadora da internação, todo óbito hospitalar é candidato a constar na AIH. |
| **2 · Pareamento** | Linkage do alvo com as AIHs de desfecho óbito. Chaves: data de nascimento, sexo, data da alta/óbito, CNES (`CODESTAB` no SIM) e município de residência; raça/cor entra como verificação. Dois níveis: **exato** (todas as chaves batem, data idêntica) e **probabilístico** (bloco nascimento + sexo, escore com buffer de até ±3 dias na data e concordância parcial de CNES/município/raça, escore mínimo 3). |
| **3 · Retroação** | Para o subgrupo linkado, busca internações anteriores com desfecho alta ou transferência em janela de 30 dias antes da internação-índice, com 45 e 60 dias como análise de sensibilidade. |
| **4 · Base analítica** | Monta a base de trajetória e transição de CID consumida pelo app, agora com o histórico de internações prévias. |
| **5 · Baixa mortalidade** | As AIHs que não são óbito nem internação anterior de óbito formam o grupo agregado por ano, UF, faixa etária, sexo e sistema do CID, sem microdado. |

**Correções da v3.2 (08/08).** O desfecho do episódio deixa de vir da última AIH por data e passa a ter o óbito como prioridade sobre a ordem, corrigindo 48.602 AIHs de óbito que caíam no grupo "sem óbito" da Etapa 5 e ficavam fora do denominador do linkage, seja porque o motivo de cobrança estava fora das faixas mapeadas, seja porque duas AIHs empatavam na data. A AIH de óbito passa a ser a referência do episódio, fornecendo CNES e data de saída para o pareamento. Motivos de cobrança não mapeados deixam de virar `NA` silencioso e são contabilizados em `COB_NAO_MAPEADA`, gravado no arquivo de qualidade para inspeção, e a consistência dos totais é checada antes do script terminar.

**Cache versionado.** O resultado de cada UF é gravado em `dados/parciais_v3`, de modo que o script pode ser interrompido e retomado, sendo que a versão da lógica fica gravada junto com o cache: se a versão não bater, os arquivos parciais são descartados sozinhos, o que evita que uma correção deixe de chegar ao resultado final por reaproveitamento de cache antigo.

**Entradas e saídas:**

```text
ENTRADAS
  SIH · Temporarios_UF_Ano/sih_{UF}_{ano}.rds        (270 lotes: 27 UF × 10 anos)
  SIM · sim_brasil_0_a_6_anos_todas_vars_2015_2024.rds

SAÍDAS (gravadas na pasta do dashboard)
  sih_sim_linkado.rds          — base analítica
  linkage_qualidade.rds        — funil, cobertura por ano e por UF, exato × probabilístico, sensibilidade
  sih_nao_obito_agregado.rds   — Etapa 5 agregada
  relatorio_linkage_v3.txt     — relatório em texto
```

```bash
Rscript Scripts/00_link_sih_sim_v3.R   # sem internet; ~20–40 min para o Brasil inteiro
```

> ⚠️ **Ajuste os caminhos antes de rodar.** O bloco `CONFIG`, no início do script, aponta para os diretórios locais de quem gerou as bases (`dir_sih`, `arq_sim`, `dir_dash`). Nenhum microdado é distribuído neste repositório, então os três caminhos precisam ser reapontados para a máquina de quem for reproduzir.

---

## 🔬 Nota Metodológica

Para garantir maior precisão epidemiológica, este estudo utiliza o número de **nascidos vivos do SINASC** como denominador das taxas de mortalidade e de internação **até 1 ano de idade**. Para a faixa de **1 a 6 anos**, o denominador correto é a **população de crianças da faixa** (POPSVS/DATASUS-IBGE), somada ano a ano no período filtrado, e não os nascidos vivos — regra revista na reunião de 27/07 e aplicada em todos os painéis do app.

As taxas são expressas por **1.000 nascidos vivos** (até 1 ano) e por **1.000 crianças da faixa** (1 a 6 anos), permitindo comparações padronizadas entre anos, Unidades Federativas e macrorregiões.

**Fontes de dados oficiais:**

* **SIM** — Sistema de Informações sobre Mortalidade;
* **SIH/SUS** — Sistema de Informações Hospitalares do SUS;
* **SINASC** — Sistema de Informações sobre Nascidos Vivos;
* **POPSVS/DATASUS (IBGE)** — população residente por UF e faixa etária;
* **Extração e processamento:** DataSUS, TabNet e rotinas em R.

**Duas unidades de contagem.** O SIM conta óbitos e o SIH conta episódios de internação, com AIHs do mesmo paciente encadeadas, sendo que nenhuma das duas bases é subconjunto da outra. Por isso a aba de qualidade do linkage apresenta as duas trilhas lado a lado, com o ponto de encontro destacado, em vez de empilhar as contagens num funil descendente único, o que sugeriria um filtro que não existe.

**Classificação de CID.** Todo código é mapeado de forma determinística para capítulo, sistema do organismo e grupo de patologia, com marcação explícita de códigos **genéricos, mal definidos ou administrativos** (capítulo XVIII inteiro, códigos administrativos do capítulo XXI e intenção indeterminada, seguindo o critério de *garbage code* da literatura GBD/RIPSA). A matriz de transição entrada → óbito abre os quatro casos de interesse: genérico → específico, específico → genérico, genérico → genérico (causa nunca esclarecida) e troca de sistema.

**Polos de referência.** Os diagnósticos são segmentados em grupos de alta complexidade (oncologia, cardiopatias congênitas, malformações, doenças do sistema nervoso e metabólicas/genéticas). As **afecções perinatais** são analisadas em separado, pois parte do evento perinatal recebido nos polos reflete o **local de parto** (gestação de risco referenciada à maternidade da capital) e não o deslocamento da criança em busca de tratamento. Define-se assim o conceito de **referência terapêutica** (alta complexidade *sem* perinatal), que é o fluxo "limpo" para identificar centros de referência e vazios assistenciais.

> ℹ️ **Nota técnica sobre mapas.** As malhas geográficas (UF e centroides municipais) são obtidas de fontes abertas oficiais (GeoJSON com códigos IBGE e CSV de coordenadas municipais), baixadas uma única vez e cacheadas localmente (`uf_sf_simplified.rds`). Essa abordagem substitui o pacote `geobr` para garantir reprodutibilidade e execução offline após o primeiro download.

---

## 📁 Scripts do Projeto

Os scripts de extração, tratamento, análise, linkage e geração das visualizações estão na pasta `Scripts/`.

### Pipeline — preparação das bases

* [`Scripts/Script Insper_SIM.R`](Scripts/Script%20Insper_SIM.R)
  Extração, organização e preparação das bases do SIM/SINASC.
* [`Scripts/Script Insper_SIH.R`](Scripts/Script%20Insper_SIH.R)
  Extração, organização e preparação das bases do SIH/SINASC.

### Pipeline — análises

* [`Scripts/Script Insper_SIM_analises.R`](Scripts/Script%20Insper_SIM_analises.R)
  Análises epidemiológicas, tabelas executivas e figuras do módulo de mortalidade.
* [`Scripts/Script Insper_SIH_analises.R`](Scripts/Script%20Insper_SIH_analises.R)
  Análises epidemiológicas, tabelas executivas e figuras do módulo de internações.

### Pipeline — insumos do dashboard

* [`Scripts/00_link_sih_sim_v3.R`](Scripts/00_link_sih_sim_v3.R)
  Linkage SIM ↔ SIH em 5 etapas (v3.2). Gera `sih_sim_linkado.rds`, `linkage_qualidade.rds`, `sih_nao_obito_agregado.rds` e `relatorio_linkage_v3.txt`.
* [`Scripts/01_baixar_populacao.R`](Scripts/01_baixar_populacao.R)
  Download e organização da população residente por UF e faixa etária (IBGE). Gera `populacao_uf_faixa.csv`, denominador da faixa de 1 a 6 anos.

---

# ⚰️ SIM/SINASC — Mortalidade de Crianças de 0 a 6 Anos

Esta seção apresenta a análise dos óbitos de crianças de 0 a 6 anos no Brasil, com base no Sistema de Informações sobre Mortalidade (SIM), no período de 2015 a 2024.

As análises incluem evolução temporal, causas de mortalidade, desigualdades regionais, distribuição espacial e fluxos de ocorrência do óbito em relação ao município de residência.

---

## 1. Perfil Temporal e Causal da Mortalidade

### Evolução da Mortalidade por Faixa Etária — Óbitos Absolutos

<div align="center">
  <img src="outputs/SIM/Fig01_Evolucao_Faixa_Etaria_Absoluto.png" width="850">
</div>

### Evolução da Mortalidade — Visão Geral, Neonatal e Crianças

Recortes de evolução solicitados na apresentação, apresentando lado a lado a **taxa por 1.000 nascidos vivos** e o **número absoluto de óbitos** (2015–2024).

**Visão geral — menores de 1 ano, 1 a < 5 anos, 5 e 6 anos**

| Taxa por 1.000 N.V. | Óbitos absolutos |
|:---:|:---:|
| <img src="outputs/SIM/Fig01a_Evolucao_VisaoGeral_Taxa.png" width="420"> | <img src="outputs/SIM/Fig01a_Evolucao_VisaoGeral_Absoluto.png" width="420"> |

**Neonatal — precoce (0–6 d), tardia (7–27 d) e pós-neonatal (28 d a < 1 ano)**

| Taxa por 1.000 N.V. | Óbitos absolutos |
|:---:|:---:|
| <img src="outputs/SIM/Fig01b_Evolucao_Neonatal_Taxa.png" width="420"> | <img src="outputs/SIM/Fig01b_Evolucao_Neonatal_Absoluto.png" width="420"> |

**Crianças — 1 a < 2 anos, 2 a < 5 anos e 5 e 6 anos**

| Taxa por 1.000 N.V. | Óbitos absolutos |
|:---:|:---:|
| <img src="outputs/SIM/Fig01c_Evolucao_Criancas_Taxa.png" width="420"> | <img src="outputs/SIM/Fig01c_Evolucao_Criancas_Absoluto.png" width="420"> |

### Evolução das Causas de Mortalidade — Taxa por 1.000 Nascidos Vivos

<div align="center">
  <img src="outputs/SIM/Fig02_Evolucao_Causas_Taxa.png" width="850">
</div>

### Causas Prioritárias de Mortalidade por Faixa Etária

<div align="center">
  <img src="outputs/SIM/Fig03_Causas_Prioritarias_por_Faixa.png" width="850">
</div>

---

## 2. Análise Espacial e Desigualdades Regionais da Mortalidade

### Heterogeneidade Regional da Taxa de Mortalidade

<div align="center">
  <img src="outputs/SIM/Fig04_Heterogeneidade_Macro_Taxa.png" width="850">
</div>

### Distribuição Espacial da Taxa de Mortalidade por Estado

<div align="center">
  <img src="outputs/SIM/Fig05_Mapa_Taxa_Estado.png" width="700">
</div>

### Distribuição Espacial da Taxa de Mortalidade por Macrorregião

<div align="center">
  <img src="outputs/SIM/Fig06_Mapa_Taxa_Macrorregiao.png" width="700">
</div>

---

## 3. Fluxos de Mortalidade e Polos de Ocorrência

A análise de fluxo cruza o **município de residência** da criança com o **município de ocorrência do óbito**, permitindo identificar deslocamentos assistenciais, dependência regional e concentração de óbitos em polos de saúde infantil.

### Proporção de Óbitos Fora do Município de Residência

<div align="center">
  <img src="outputs/SIM/Fig07_Fluxo_Obitos_Fora_Municipio.png" width="850">
</div>

### Top 30 Municípios Polo de Saúde Infantil

<div align="center">
  <img src="outputs/SIM/Fig08_Polos_Saude_Infantil_Top30.png" width="850">
</div>

### Fluxo de Mortalidade Infantil entre UFs

<div align="center">
  <img src="outputs/SIM/Fig09_Heatmap_Fluxo_InterUF.png" width="850">
</div>

### Principais Fluxos para os Top 10 Municípios Receptores de Óbitos

<div align="center">
  <img src="outputs/SIM/Fig10_Fluxos_Top10_Municipios_Receptores.png" width="850">
</div>

---

## 3B. Segmentação Diagnóstica e Polos de Referência (Mortalidade)

Refinamento da análise de fluxo. As causas são segmentadas em grupos diagnósticos de **alta complexidade** (marcadores de centros de referência) e cruzadas com as faixas etárias e com a origem dos pacientes em nível de **CEP/município**. As afecções perinatais aparecem destacadas dos demais grupos, e o fluxo de **referência terapêutica** (alta complexidade sem perinatal) é usado para revelar a vocação de cada polo e os deslocamentos efetivos por tratamento.

### Segmentação Diagnóstica nos Polos, por Faixa Etária

<div align="center">
  <img src="outputs/SIM/Fig11_CID_Segmentado_por_Faixa_Polos.png" width="850">
</div>

### Polos por Especialidade — Fluxo de Referência Terapêutica

<div align="center">
  <img src="outputs/SIM/Fig13_Polos_por_Especialidade.png" width="850">
</div>

### Fluxo Origem → Polo por CEP/Município (Alta Complexidade)

<div align="center">
  <img src="outputs/SIM/Fig12_Fluxo_CEP_Origem_Destino_Polos.png" width="850">
</div>

---

## 📂 Tabelas Executivas — Mortalidade

Os arquivos executivos com os resultados sumarizados da mortalidade estão disponíveis para download:

* [Tabelas Executivas de Mortalidade](outputs/SIM/Tabelas_Executivas_Mortalidade_v2.xlsx)
* [Tabelas dos Top 10 Municípios Receptores — SIM](outputs/SIM/Tabelas_Top10_Municipios_Receptores.xlsx)
* [Segmentação de CID e Fluxo por CEP — SIM](outputs/SIM/Tabelas_CID_Segmentado_e_Fluxo_CEP.xlsx)

---

# 🏥 SIH/SUS — Internações de Crianças de 0 a 6 Anos

Esta seção apresenta a análise das internações hospitalares de crianças de 0 a 6 anos no Brasil, com base no Sistema de Informações Hospitalares do SUS (SIH/SUS), no período de 2015 a 2024.

As análises complementam o módulo de mortalidade do SIM, permitindo avaliar padrões de utilização hospitalar, causas prioritárias de internação, desigualdades regionais, fluxos assistenciais e óbitos hospitalares entre crianças internadas.

---

## 4. Perfil Temporal e Causal das Internações

### Evolução das Internações por Faixa Etária — Internações Absolutas

<div align="center">
  <img src="outputs/SIH/Fig01_Evolucao_Faixa_Etaria_Absoluto.png" width="850">
</div>

### Evolução das Internações — Visão Geral, Neonatal e Crianças

Recortes de evolução solicitados na apresentação, apresentando lado a lado a **taxa por 1.000 nascidos vivos** e o **número absoluto de internações** (2015–2024).

**Visão geral — menores de 1 ano, 1 a < 5 anos, 5 e 6 anos**

| Taxa por 1.000 N.V. | Internações absolutas |
|:---:|:---:|
| <img src="outputs/SIH/Fig01a_Evolucao_VisaoGeral_Taxa.png" width="420"> | <img src="outputs/SIH/Fig01a_Evolucao_VisaoGeral_Absoluto.png" width="420"> |

**Neonatal — precoce (0–6 d), tardia (7–27 d) e pós-neonatal (28 d a < 1 ano)**

| Taxa por 1.000 N.V. | Internações absolutas |
|:---:|:---:|
| <img src="outputs/SIH/Fig01b_Evolucao_Neonatal_Taxa.png" width="420"> | <img src="outputs/SIH/Fig01b_Evolucao_Neonatal_Absoluto.png" width="420"> |

**Crianças — 1 a < 2 anos, 2 a < 5 anos e 5 e 6 anos**

| Taxa por 1.000 N.V. | Internações absolutas |
|:---:|:---:|
| <img src="outputs/SIH/Fig01c_Evolucao_Criancas_Taxa.png" width="420"> | <img src="outputs/SIH/Fig01c_Evolucao_Criancas_Absoluto.png" width="420"> |

### Evolução das Causas de Internação — Taxa por 1.000 Nascidos Vivos

<div align="center">
  <img src="outputs/SIH/Fig02_Evolucao_Causas_Taxa.png" width="850">
</div>

### Causas Prioritárias de Internação por Faixa Etária

<div align="center">
  <img src="outputs/SIH/Fig03_Causas_Prioritarias_por_Faixa.png" width="850">
</div>

---

## 5. Análise Espacial e Desigualdades Regionais nas Internações

### Heterogeneidade Regional da Taxa de Internação

<div align="center">
  <img src="outputs/SIH/Fig04_Heterogeneidade_Macro_Taxa.png" width="850">
</div>

### Distribuição Espacial da Taxa de Internação por Estado

<div align="center">
  <img src="outputs/SIH/Fig05_Mapa_Taxa_Estado.png" width="700">
</div>

### Distribuição Espacial da Taxa de Internação por Macrorregião

<div align="center">
  <img src="outputs/SIH/Fig06_Mapa_Taxa_Macrorregiao.png" width="700">
</div>

---

## 6. Fluxos Assistenciais e Polos de Atendimento Infantil

A análise de fluxo das internações cruza o **município de residência** da criança com o **município de internação**, permitindo identificar deslocamentos assistenciais, concentração de atendimentos em polos regionais e dependência de municípios receptores para o cuidado hospitalar infantil.

### Proporção de Internações Fora do Município de Residência

<div align="center">
  <img src="outputs/SIH/Fig07_Fluxo_Internacoes_Fora_Municipio.png" width="850">
</div>

### Top 30 Municípios Polo de Atendimento Infantil

<div align="center">
  <img src="outputs/SIH/Fig08_Polos_Saude_Infantil_Top30.png" width="850">
</div>

### Fluxo de Internações Infantis entre UFs

<div align="center">
  <img src="outputs/SIH/Fig09_Heatmap_Fluxo_InterUF.png" width="850">
</div>

### Principais Fluxos para os Top 10 Municípios Receptores de Internações

<div align="center">
  <img src="outputs/SIH/Fig10_Fluxos_Top10_Municipios_Receptores_SIH.png" width="850">
</div>

---

## 7. Óbitos Hospitalares entre Internações

Esta análise descreve os óbitos hospitalares registrados entre internações de crianças de 0 a 6 anos, permitindo avaliar a magnitude absoluta dos óbitos em ambiente hospitalar e a letalidade hospitalar ao longo do período.

### Óbitos Hospitalares e Letalidade Hospitalar

<div align="center">
  <img src="outputs/SIH/Fig11_Obitos_Hospitalares_SIH.png" width="850">
</div>

---

## 7B. Segmentação Diagnóstica e Polos de Referência (Internações)

Mesmo refinamento aplicado ao SIM, agora sobre as internações. Como o **CEP de residência** consta da AIH, o fluxo de origem é detalhado em nível de **área CEP** (5 dígitos), e não apenas por município. As afecções perinatais são analisadas em separado, e o fluxo de **referência terapêutica** (alta complexidade sem perinatal) revela a vocação de cada polo hospitalar.

### Segmentação Diagnóstica nos Polos, por Faixa Etária

<div align="center">
  <img src="outputs/SIH/Fig12_CID_Segmentado_por_Faixa_Polos_SIH.png" width="850">
</div>

### Polos por Especialidade — Fluxo de Referência Terapêutica

<div align="center">
  <img src="outputs/SIH/Fig13_Polos_por_Especialidade_SIH.png" width="850">
</div>

### Fluxo Origem → Polo por CEP de Residência (Alta Complexidade)

<div align="center">
  <img src="outputs/SIH/Fig14_Fluxo_CEP_Origem_Destino_Polos_SIH.png" width="850">
</div>

---

## 📂 Tabelas Executivas — Internações

Os arquivos executivos com os resultados sumarizados das internações estão disponíveis para download:

* [Tabelas Executivas de Internações — 0 a 6 anos](outputs/SIH/Tabelas_Executivas_Internacoes_0_6_Anos.xlsx)
* [Tabelas dos Top 10 Municípios Receptores — SIH](outputs/SIH/Tabelas_Top10_Municipios_Receptores_SIH.xlsx)
* [Segmentação de CID e Fluxo por CEP — SIH](outputs/SIH/Tabelas_CID_Segmentado_e_Fluxo_CEP_SIH.xlsx)

---

# 📂 Estrutura do Repositório

```text
sim-sih-mortalidade-internacoes-0-6-anos/
│
├── app.R                          # Dashboard Shiny (Observatório de Saúde Infantil)
│
├── Tabelas_Executivas_Mortalidade_v2.xlsx        # insumos obrigatórios do app,
├── Tabelas_Executivas_Internacoes_0_6_Anos.xlsx  #   lidos do diretório de execução
├── uf_sf_simplified.rds                          #   (mesma pasta do app.R)
│
├── Scripts/
│   ├── 00_link_sih_sim_v3.R       # Linkage SIM ↔ SIH em 5 etapas (v3.2)
│   ├── 01_baixar_populacao.R      # População IBGE → populacao_uf_faixa.csv
│   ├── Script Insper_SIM.R
│   ├── Script Insper_SIM_analises.R
│   ├── Script Insper_SIH.R
│   └── Script Insper_SIH_analises.R
│
├── outputs/
│   │
│   ├── SIM/
│   │   ├── Fig01_Evolucao_Faixa_Etaria_Absoluto.png
│   │   ├── Fig01a_Evolucao_VisaoGeral_Taxa.png
│   │   ├── Fig01a_Evolucao_VisaoGeral_Absoluto.png
│   │   ├── Fig01b_Evolucao_Neonatal_Taxa.png
│   │   ├── Fig01b_Evolucao_Neonatal_Absoluto.png
│   │   ├── Fig01c_Evolucao_Criancas_Taxa.png
│   │   ├── Fig01c_Evolucao_Criancas_Absoluto.png
│   │   ├── Fig02_Evolucao_Causas_Taxa.png
│   │   ├── Fig03_Causas_Prioritarias_por_Faixa.png
│   │   ├── Fig04_Heterogeneidade_Macro_Taxa.png
│   │   ├── Fig05_Mapa_Taxa_Estado.png
│   │   ├── Fig06_Mapa_Taxa_Macrorregiao.png
│   │   ├── Fig07_Fluxo_Obitos_Fora_Municipio.png
│   │   ├── Fig08_Polos_Saude_Infantil_Top30.png
│   │   ├── Fig09_Heatmap_Fluxo_InterUF.png
│   │   ├── Fig10_Fluxos_Top10_Municipios_Receptores.png
│   │   ├── Fig11_CID_Segmentado_por_Faixa_Polos.png
│   │   ├── Fig12_Fluxo_CEP_Origem_Destino_Polos.png
│   │   ├── Fig13_Polos_por_Especialidade.png
│   │   ├── Tabelas_Executivas_Mortalidade_v2.xlsx
│   │   ├── Tabelas_Top10_Municipios_Receptores.xlsx
│   │   └── Tabelas_CID_Segmentado_e_Fluxo_CEP.xlsx
│   │
│   └── SIH/
│       ├── Fig01_Evolucao_Faixa_Etaria_Absoluto.png
│       ├── Fig01a_Evolucao_VisaoGeral_Taxa.png
│       ├── Fig01a_Evolucao_VisaoGeral_Absoluto.png
│       ├── Fig01b_Evolucao_Neonatal_Taxa.png
│       ├── Fig01b_Evolucao_Neonatal_Absoluto.png
│       ├── Fig01c_Evolucao_Criancas_Taxa.png
│       ├── Fig01c_Evolucao_Criancas_Absoluto.png
│       ├── Fig02_Evolucao_Causas_Taxa.png
│       ├── Fig03_Causas_Prioritarias_por_Faixa.png
│       ├── Fig04_Heterogeneidade_Macro_Taxa.png
│       ├── Fig05_Mapa_Taxa_Estado.png
│       ├── Fig06_Mapa_Taxa_Macrorregiao.png
│       ├── Fig07_Fluxo_Internacoes_Fora_Municipio.png
│       ├── Fig08_Polos_Saude_Infantil_Top30.png
│       ├── Fig09_Heatmap_Fluxo_InterUF.png
│       ├── Fig10_Fluxos_Top10_Municipios_Receptores_SIH.png
│       ├── Fig11_Obitos_Hospitalares_SIH.png
│       ├── Fig12_CID_Segmentado_por_Faixa_Polos_SIH.png
│       ├── Fig13_Polos_por_Especialidade_SIH.png
│       ├── Fig14_Fluxo_CEP_Origem_Destino_Polos_SIH.png
│       ├── Tabelas_Executivas_Internacoes_0_6_Anos.xlsx
│       ├── Tabelas_Top10_Municipios_Receptores_SIH.xlsx
│       └── Tabelas_CID_Segmentado_e_Fluxo_CEP_SIH.xlsx
│
├── .gitignore
├── LICENSE
└── README.md
```

> **Nota.** Os arquivos `sih_sim_linkado.rds`, `linkage_qualidade.rds`, `sih_nao_obito_agregado.rds` e `populacao_uf_faixa.csv` são gerados localmente pelos scripts `00_` e `01_` e não são versionados, seja pelo volume, seja por conterem microdado individualizado no caso do linkage. Sem eles o app abre normalmente, com os painéis dependentes sinalizando o insumo ausente.

---

# 💻 Reprodutibilidade

A ordem de execução importa: as bases do SIM e do SIH precisam existir antes do linkage, e o linkage precisa rodar antes de os painéis de trajetória ficarem disponíveis no dashboard.

```r
# 1. Preparação das bases
source("Scripts/Script Insper_SIM.R")
source("Scripts/Script Insper_SIH.R")

# 2. Análises descritivas, tabelas executivas e figuras
source("Scripts/Script Insper_SIM_analises.R")
source("Scripts/Script Insper_SIH_analises.R")

# 3. Insumos do dashboard
source("Scripts/01_baixar_populacao.R")     # denominador de 1 a 6 anos (IBGE)
source("Scripts/00_link_sih_sim_v3.R")      # linkage SIM ↔ SIH (~20–40 min)

# 4. Dashboard interativo
shiny::runApp("app.R")
```

---

# 📌 Observações sobre os Dados

Os microdados brutos não são armazenados no repositório, em razão do volume dos arquivos e das boas práticas de organização, versionamento e reprodutibilidade.

As rotinas em R permitem reconstruir as bases analíticas a partir das fontes oficiais do DataSUS, respeitando a estrutura dos sistemas nacionais de informação em saúde.

O linkage SIM ↔ SIH é executado inteiramente em ambiente local, sobre bases já baixadas, sem qualquer transmissão de dado individualizado, e os produtos versionados neste repositório são apenas agregados.

---

# 📄 Licença

Este projeto está disponibilizado sob a **Licença MIT**, para fins de pesquisa, ensino, auditoria técnica, transparência pública e apoio à formulação de políticas de saúde. Consulte o arquivo [`LICENSE`](LICENSE).

<div align="center">
  <sub>Repositório: <strong>fmdsocial/sim-sih-mortalidade-internacoes-0-6-anos</strong> · Felipe Delpino · INSPER</sub>
</div>
