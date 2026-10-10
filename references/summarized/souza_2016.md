# Sousa et al. (2016) — Estimação e análise dos fatores determinantes da redução da taxa de mortalidade infantil no Brasil

**Autores:** Janaildo Soares de Sousa (PRODEMA/UFC), Robério Telmo Campos (MAER/UFC), Andréa Ferreira da Silva (UFPB), Filomena Nádia Rodrigues Bezerra (MAER/UFC), Jaqueline Saraiva de Lira (MAER/UFC).

**Publicação:** Revista Brasileira de Estudos Regionais e Urbanos (RBERU), 10(2):140-155, 2016. Recebido: 22/11/2015; aceito: 21/07/2016.

---

## Objetivo

Mensurar e analisar os fatores determinantes da redução da Taxa de Mortalidade Infantil (TMI) nos estados brasileiros (26 estados + DF), no período de 2001 a 2011.

## Métodos

Estudo econométrico com **dados em painel** (combinação de corte transversal e série temporal), seguindo abordagem de Sousa e Leite Filho (2008) e detalhamento de Mendonça e Motta (2007)/Greene (2008). Período escolhido por disponibilidade de dados e por ser posterior à Declaração dos Objetivos do Milênio (ODM).

**Fontes de dados:** DATASUS/SIM/Ministério da Saúde (TMI), IPEADATA (cobertura do PSF) e PNAD (Índice de Gini, renda per capita familiar, percentual de domicílios com fossa séptica como proxy de saneamento básico).

**Variáveis do modelo** (todas logaritmizadas, interpretação em elasticidades):
- **Dependente:** TMI (óbitos <1 ano por 1.000 NV)
- **PSF** (cobertura do Programa Saúde da Família) — sinal esperado negativo
- **GINI** (desigualdade de renda) — sinal esperado positivo
- **RENPER** (renda familiar per capita) — sinal esperado negativo
- **DOMSAN** (% domicílios com saneamento) — sinal esperado negativo

**Modelo:** LogTMI_it = α_i + β₀LogPSF_it + β₁LogGini_it + β₂LogRenper_it + β₃LogSansan_it + u_it, estimado por **Efeitos Fixos (EF)** e **Efeitos Aleatórios (EA)**, com escolha do modelo via **Teste de Hausman**.

## Resultados

### Estatísticas descritivas (2001-2011, médias estaduais)
| Variável | Média | Mínimo | Máximo |
|---|---|---|---|
| TMI | 24,67 | 13,13 | 36,72 |
| PSF (% cobertura) | 65,62 | 10,70 | 95,72 |
| GINI | 0,58 | 0,52 | 0,63 |
| RENPER (R$ 2011) | 402,93 | 263,69 | 601,85 |
| DOMSAN | 0,52 | 0,13 | 1,12 |

### Rankings estaduais (médias 2001-2011)
- **Maior TMI:** Amapá (27,18), Pará (26,30), Tocantins (26,22), Piauí (26,07), Bahia (26,04).
- **Menor TMI:** Santa Catarina (12,77), Distrito Federal (13,01), Rio Grande do Sul (13,50), São Paulo (13,68), Paraná (14,41).
- **Maior cobertura PSF:** Sergipe (83,76%), Piauí (80,89%), Paraíba (75,94%). **Menor cobertura:** Distrito Federal (8,77%) — atribuída à baixa TMI já existente, reduzindo a necessidade do programa.
- **Maior desigualdade (Gini):** Distrito Federal (0,62), Acre (0,60), Piauí (0,59). **Menor:** Santa Catarina (0,47), Rio Grande do Sul/São Paulo (0,52).
- **Maior renda per capita:** Distrito Federal (R$1.358,81), São Paulo, Rio de Janeiro. **Menor:** Maranhão (R$329,96), Alagoas, Piauí — todos do Nordeste.
- **Melhor saneamento:** Distrito Federal (0,99), São Paulo (0,96), Rio de Janeiro (0,92). **Pior:** Mato Grosso do Sul (0,23), Tocantins (0,26), Alagoas (0,30).

### Estimação do modelo (Tabela 7)
Teste de Hausman (Chi² = 1,96 < 5%) indicou o **modelo de Efeitos Aleatórios** como mais adequado. Todos os coeficientes significativos a 1%, com sinais conforme esperado:

| Variável | Coeficiente (EA) | Interpretação |
|---|---|---|
| Constante | 7,68* | — |
| PSF | **−0,09** | ↑1% cobertura PSF → ↓0,09% TMI |
| GINI | **+0,73** (maior magnitude) | ↑1% desigualdade → ↑0,73% TMI |
| RENPER | **−0,63** | ↑1% renda per capita → ↓0,63% TMI |
| DOMSAN | **−0,15** | ↑1% saneamento → ↓0,15% TMI |

R² = 0,7665; N = 297 observações (27 unidades × 11 anos). A desigualdade de renda (Gini) foi o estimador de maior impacto entre todos.

## Discussão

Os resultados confirmam a literatura prévia (Aquino, Oliveira e Barreto 2009; Almeida e Szwarcwald 2012; Lourenço et al. 2014) quanto ao papel do PSF na redução da TMI, atribuído à atuação de equipes multiprofissionais que promovem aleitamento materno, pré-natal, cuidado neonatal e prevenção de doenças prevalentes na infância.

A desigualdade de renda (Gini) emergiu como o fator mais determinante — uma redução na desigualdade tem impacto direto na queda da TMI, independente do nível de renda médio. O aumento da renda per capita também reduz a TMI, mas a análise capta apenas pobreza unidimensional (renda); os autores apontam que a pobreza multidimensional provavelmente tem impacto adicional não capturado pelo modelo. O acesso a saneamento básico mostrou efeito redutor mais modesto, mas significativo.

Os achados são consistentes com estudos internacionais (Lisa, Flore e Sandrine 2013, 100 países em desenvolvimento; Galiani et al. 2005, sobre privatização do abastecimento de água) e nacionais (Sousa e Leite Filho 2008, Nordeste 1991-2000; Garcia e Santana 2011, Brasil 1993-2008, usando Índice de Concentração).

## Considerações finais

A TMI média nos estados brasileiros (2001-2011) ainda é elevada, mais alta nas regiões Norte e Nordeste — confirmando o padrão regional já amplamente documentado na literatura, embora os autores não tenham testado essa correlação diretamente neste modelo. A desigualdade de renda e de saneamento entre estados permanece muito alta, apesar da redução geral da pobreza no período. Recomenda-se ação conjunta de estados/municípios via mecanismos de gestão de saúde (secretarias, conselhos, fundos), além de políticas de redistribuição de renda priorizadas para estados com desigualdades socioeconômicas persistentes. Sugestões para trabalhos futuros: replicar a análise em nível municipal, incorporar o IDH, e investigar o impacto da pobreza multidimensional sobre a TMI.

## Relevância para o projeto

Único estudo desta coleção com abordagem **econométrica de dados em painel** (efeitos fixos/aleatórios com teste de Hausman) quantificando elasticidades entre determinantes socioeconômicos e a TMI estadual — complementa os estudos descritivos/ecológicos predominantes na coleção (ex. Pasklan 2020, Min. Saúde 2021) com estimativas de magnitude de efeito. Contribuições específicas: (1) quantifica que a desigualdade de renda (Gini) é o determinante de maior impacto elástico sobre a TMI (+0,73), reforçando quantitativamente a tese qualitativa de concentração de mortalidade infantil em populações de baixa renda discutida em Santos 2010 e Pasklan 2020; (2) confirma estatisticamente o efeito redutor do PSF/ESF sobre a TMI, dado citado de forma qualitativa em Pasklan 2020 e Santos 2010; (3) os rankings estaduais de TMI, PSF, Gini, renda e saneamento (2001-2011) servem como referência cruzada complementar à série histórica de TMI por UF do Min. Saúde 2021 (1990-2019); (4) limitação relevante: o estudo não testa diretamente a interação entre região geográfica e os determinantes, apenas observa que Norte/Nordeste concentram as piores taxas — esse refinamento espacial é mais bem tratado por Pasklan 2020 (GWR municipal) e Salim 2020 (macrorregiões).
