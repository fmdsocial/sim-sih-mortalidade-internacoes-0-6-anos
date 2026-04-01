# SIM + SIH: Mortalidade e Internações (0 a 6 anos) no Brasil

Projeto público em R para análise reprodutível da mortalidade (SIM) e internações hospitalares (SIH) em crianças de 0 a 6 anos no Brasil.

---

## População de interesse

Este projeto analisa exclusivamente crianças de **0 a 6 anos de idade**, segmentadas nos seguintes grupos etários:

- Neonatal precoce (0–6 dias)  
- Neonatal tardia (7–27 dias)  
- Pós-neonatal (28 dias a <1 ano)  
- 1 a 6 anos  

Todos os dados, análises e visualizações são restritos a essa faixa etária.

---

## Objetivos

- Descrever a evolução temporal da mortalidade em crianças de 0 a 6 anos no Brasil.
- Caracterizar padrões de internação hospitalar (SIH) na mesma faixa etária.
- Avaliar desigualdades e heterogeneidade regional nas taxas de mortalidade.
- Identificar causas prioritárias de morte com potencial de intervenção médica e de saúde pública.
- Analisar o fluxo de ocorrência de óbitos (município de residência vs. município de ocorrência) para identificar polos de atração e gargalos de atendimento infantil.

---

## 📌 Nota Metodológica

Buscando o maior rigor epidemiológico possível, o cálculo das Taxas de Mortalidade Infantil (TMI) para menores de 1 ano utiliza como denominador o número de **Nascidos Vivos (SINASC)** do mesmo período e localidade, substituindo estimativas populacionais intercensitárias do IBGE. As taxas são apresentadas por **1.000 nascidos vivos**.

---

## Recorte temporal

Período analisado: **2015 a 2024**

---

## Fontes de Dados

- **SIM** (Sistema de Informações sobre Mortalidade)  
- **SINASC** (Sistema de Informações sobre Nascidos Vivos) - *Utilizado como denominador*
- **SIH** (Sistema de Informações Hospitalares)  
- **DataSUS** (Extração via pacote `microdatasus` em R e arquivos TabNet)

---

## Principais Resultados (Visualizações)

### 1. Perfil Temporal e Causal
**Evolução da Mortalidade por Faixa Etária (Absoluto)**
![Figura 1](Fig01_Evolucao_Faixa_Etaria_Absoluto.png)

**Evolução das Causas de Mortalidade (Taxa por 1.000 N.V.)**
![Figura 2](Fig02_Evolucao_Causas_Taxa.png)

**Causas Prioritárias de Intervenção por Faixa Etária**
![Figura 3](Fig03_Causas_Prioritarias_por_Faixa.png)

### 2. Análise Espacial e Desigualdades
**Heterogeneidade Regional (Taxa Média)**
![Figura 4](Fig04_Heterogeneidade_Macro_Taxa.png)

**Mapa da Distribuição Espacial - Taxa por Estado**
![Figura 5](Fig05_Mapa_Taxa_Estado.png)

**Mapa da Distribuição Espacial - Taxa por Macrorregião**
![Figura 6](Fig06_Mapa_Taxa_Macrorregiao.png)

### 3. Análise de Fluxo e Rede de Atendimento
**Proporção de Óbitos Ocorridos Fora do Município de Residência**
![Figura 7](Fig07_Fluxo_Obitos_Fora_Municipio.png)

**Top 30 Municípios Polo de Saúde Infantil (Atendimento a Não-Residentes)**
![Figura 8](Fig08_Polos_Saude_Infantil_Top30.png)

**Heatmap de Fluxo Inter-Estadual de Mortalidade**
![Figura 9](Fig09_Heatmap_Fluxo_InterUF.png)

---

## Estrutura do projeto

- `Scripts/`: Códigos em R para download, limpeza, merge e geração dos gráficos.  
- `Dados/`: Arquivos brutos (SIM, SINASC) ignorados no versionamento (`.gitignore`) por questões de tamanho e segurança.
- Os gráficos `.png` gerados e a tabela executiva (`Tabelas_Executivas_Mortalidade_v2.xlsx`) estão disponíveis na raiz deste repositório para rápida visualização.
- `Docs/`: Documentação complementar e dicionários de variáveis.  

---

## Reprodutibilidade

Para reproduzir as análises em sua máquina local, clone este repositório, adicione os arquivos brutos na pasta `Dados/` conforme especificado nos scripts, e execute:

```r
# Exemplo de fluxo de execução
source("Scripts/01_download_preparo_sim_0a6.R")
source("Scripts/02_analises_sim_0a6.R")
