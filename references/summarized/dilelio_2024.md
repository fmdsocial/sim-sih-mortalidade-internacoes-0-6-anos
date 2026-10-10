# Dilélio et al. (2024) — Structure and process in primary health care for children and spatial distribution of infant mortality

**Autores:** Alitéia Santiago Dilélio, Márcio Natividade, Luiz Augusto Facchini, Marcos Pereira, Elaine Tomasi

**Publicação:** Revista de Saúde Pública, 58:21, 2024. DOI: 10.11606/s1518-8787.2024058005527

---

## Objetivo

Identificar padrões espaciais da qualidade da estrutura dos serviços de atenção primária à saúde (APS) e do processo de trabalho das equipes, e seus efeitos sobre a mortalidade infantil no Brasil.

## Métodos

Estudo ecológico, transversal, com agregados espaciais usando os 5.570 municípios do Brasil como unidades de análise. Ano de referência: **2018**.

**Fontes de dados:**
- PMAQ-AB (3º ciclo, 2018) — 37.350 equipes em 28.939 UBS — para estrutura das UBS e processo de trabalho das equipes.
- SIM (mortalidade infantil); SINASC (nascidos vivos); IBGE (IDH-M, porte populacional); e-Gestor (cobertura ESF); SI-PNI (coberturas vacinais).

**Exposições principais:**
- *Proporção de UBS com estrutura adequada:* sala de vacinação, equipamentos (esfigmomanômetros, balanças, sonar/Pinard, refrigeradores para vacinas, etc.), caderneta da criança, testes rápidos, depressores de língua.
- *Proporção de equipes com processo de trabalho adequado:* planejamento familiar, registro de gestantes de alto risco, consulta puerperal em 1 semana, penicilina na UBS, acompanhamento integral da criança (vacinas, crescimento, PKU, violência, acidentes), busca ativa de crianças em atraso.

**Análise:** suavização bayesiana empírica das taxas brutas de TMI; Índice Global de Moran (GMI) para autocorrelação espacial; regressão linear espacial (SAR – Lag Model) bivariada e multivariada (ajustada por cobertura ESF, IDH-M, proporção de NV com ≥7 consultas de pré-natal, proporção de baixo peso, cobertura VIP, pentavalente e tríplice viral). Software: QGIS 2.18 e GeoDa 1.14.

## Resultados

### TMI nacional e regional (2018)
- TMI Brasil: **12,4/1.000 NV**.
- Variação regional: Sul = 10,6/1.000; Sudeste = 11,2/1.000; Nordeste = 14,1/1.000; Norte = 14,5/1.000.
- Autocorrelação espacial da TMI: GMI = 0,511 (p<0,05) — municípios vizinhos apresentam TMI semelhantes; clusters de alto risco concentrados no Norte.
- Clusters LISA: 690 municípios alto-alto (maioria no Norte/Nordeste); 942 baixo-baixo (Sul/Sudeste).

### Autocorrelação das covariáveis
| Indicador | GMI |
|---|---|
| IDH-M | 0,793 |
| Proporção de NV com ≥7 consultas pré-natal | 0,617 |
| Cobertura ESF | 0,403 |
| Proporção de UBS com estrutura adequada | 0,159 |
| Proporção de equipes com processo adequado | 0,161 |

### Regressão espacial multivariada
| Variável | β ajustado | p |
|---|---|---|
| Proporção de equipes com processo adequado | **−3,13** | <0,05 |
| Proporção de UBS com estrutura adequada | −0,34 | 0,03 |
| IDH-M | −0,51 | <0,05 |
| Cobertura ESF | −1,12 | <0,05 |
| Proporção de NV com ≥7 consultas pré-natal | −0,47 | <0,05 |
| Proporção de baixo peso ao nascer | +0,14 | 0,05 |
| Cobertura VIP | −0,15 | 0,04 |

### Principais achados
- Processo de trabalho adequado das equipes é a variável com maior efeito independente sobre redução da TMI (β=−3,13), apesar de na bivariada não ter sido significativo (β=−7,41, p=0,83), tornando-se significativo após ajustamento.
- Estrutura adequada das UBS também associada independentemente à redução da TMI.
- Baixo peso ao nascer: única variável com associação direta (positiva) à TMI no modelo ajustado.
- Clusters de baixa estrutura e baixo processo de trabalho concentrados no Norte e Nordeste — as regiões com maior TMI.

## Conclusão

A qualidade da estrutura dos serviços de APS e o processo de trabalho das equipes têm impacto sobre a mortalidade infantil, independentemente de fatores individuais como pré-natal, peso ao nascer e vacinação. Investimento na qualificação da APS — especialmente no processo de trabalho das equipes — pode contribuir para a redução da TMI e melhoria da saúde infantil.

## Relevância para o projeto

Referência metodológica para uso de análise espacial (Moran, LISA, SAR) na investigação de determinantes da TMI em nível municipal. Demonstra que estrutura e processo da APS são associados independentes da TMI, mesmo controlando para IDH-M, ESF e pré-natal. Dados do PMAQ-AB 2018 são específicos para estrutura de UBS voltadas à saúde infantil — relevante para contextualizar diferenças na qualidade da atenção básica entre regiões.
