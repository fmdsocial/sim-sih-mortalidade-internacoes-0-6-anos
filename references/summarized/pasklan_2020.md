# Pasklan et al. (2021) — Análise espacial da qualidade dos serviços de APS na redução da mortalidade infantil

**Autores:** Amanda Namíbia Pereira Pasklan, Rejane Christine de Sousa Queiroz, Thiago Augusto Hernandes Rocha, Núbia Cristina da Silva, Aline Sampieri Tonello, João Ricardo Nickening Vissoci, Elaine Tomasi, Elaine Thumé, Catherine Staton, Erika Bárbara Abreu Fonseca Thomaz.

**Instituições:** UFMA, UFMG, Duke University, UFPel.

**Publicação:** Ciência & Saúde Coletiva, 26(12):6247–6258, 2021. DOI: 10.1590/1413-812320212612.24732020. (Arquivo: pasklan_2020.pdf)

---

## Objetivo

Analisar a correlação da qualidade dos serviços de Atenção Primária à Saúde (APS) com a redução da mortalidade infantil (TMI) no Brasil, utilizando geoprocessamento para identificar padrões espaciais e determinantes localmente variáveis.

## Métodos

Estudo ecológico, transversal. **Período de análise:** 2000–2015. **Unidade:** 5.565 municípios brasileiros (5.011 para análise espacial).

**Fontes de dados:**
- **SIM/DATASUS:** TMI por município, 2000–2015
- **PMAQ-AB (2º ciclo, 2014):** 29.778 equipes de APS e 24.055 UBS em 5.011 municípios
- **IBGE:** variáveis socioeconômicas e demográficas
- **Rocha et al.:** índice de acessibilidade a serviços de alta complexidade
- **Calvo et al.:** estratificação de municípios por desempenho em saúde

**Variáveis independentes (PMAQ-AB):**
- Escore **infraestrutura** (equipe mínima, equipamentos, insumos, imunobiológicos, testes diagnósticos)
- Escore **disponibilidade** (horário mínimo, dois turnos, visita domiciliar, educação permanente, coordenação do cuidado)
- Variáveis individuais: referência para o parto, consulta pré-natal, consulta de puericultura, promoção da saúde, planejamento familiar

**Variáveis socioeconômicas/demográficas:** proporção de <5 anos, taxa de nascidos vivos, analfabetismo, renda per capita, taxa de desemprego, proporção de parto vaginal e hospitalar.

**Métodos analíticos (três etapas):**
1. **Análise de cluster espacial diferencial (I de Moran local)** — GEODA — identifica clusters High-High, Low-Low e outliers Low-High e High-Low de TMI entre 2000–2015.
2. **Regressão espacial OLS (Mínimos Quadrados Ordinários)** — exploratória, múltiplas combinações; seleção por AIC e R². Koenker (BP) significante → fenômeno não-estacionário.
3. **Regressão Geograficamente Ponderada (GWR)** — ARCGIS 10.5 — equações customizadas por município, confirmando não-estacionariedade.

## Resultados

### TMI nacional e regional (2000–2015)
| Região | 2000 | 2015 | Redução |
|---|---|---|---|
| Nordeste | 30,88 | 14,27 | **−53,79%** (maior) |
| Norte | ~24 | ~15 | — |
| Sudeste | ~22 | ~12 | — |
| Sul | ~18 | ~11 | — |
| Centro-Oeste | 17,93 | 13,89 | **−22,53%** (menor) |
| **Brasil** | **24,14** | **13,26** | **−45,07%** |

### Extremos estaduais
| Período | Maiores TMI | Menores TMI |
|---|---|---|
| 2000 | Paraíba (41,65), Acre (41,61), Pernambuco (37,66) | DF (14,40), RS (15,27), SC (17,16) |
| 2015 | **Roraima (23,28)**, Acre (19,89), Amazonas (17,82) | SC (9,54), RS (9,60), DF (10,58) |

Destaque negativo: **Roraima** apresentou virtually nenhuma melhora em 15 anos. Quatro estados do Norte (AM, PA, RR, RO), quatro do Nordeste (AL, PI, BA, MA) e três do Centro-Oeste (MT, GO, DF) apresentaram piora relativa no ranking de TMI entre 2000 e 2015.

### Análise de cluster (749 municípios significantes)
| Tipo de cluster | N municípios | Interpretação |
|---|---|---|
| High-High (persistência alta TMI) | **153** | Concentrados no Norte/Nordeste; áreas próximas a Boa Vista e Macapá sem melhora |
| Low-Low (baixa TMI persistente) | 294 | Sul/Sudeste |
| Low-High (baixa TMI próximo a alta) | 211 | Outliers positivos |
| High-Low (alta TMI próximo a baixa) | 91 | Outliers negativos |

Manaus e áreas adjacentes apresentaram melhora mesmo dentro da região Norte.

### GWR — Associações com a TMI (nível nacional)
| Variável | Associação com TMI |
|---|---|
| Acessibilidade a serviços de alta complexidade | **Inversa** (maior acessibilidade → menor TMI) |
| Estrato da gestão em saúde e porte populacional | **Inversa** |
| Referência para o parto | **Inversa** (maior referenciamento → menor TMI) |
| Taxa de nascidos vivos | **Inversa** |
| Renda per capita | **Inversa** (todas as regiões) |
| Taxa de desemprego | **Inversa** (provável mediação pelo Bolsa Família) |
| Infraestrutura das UBS | **Direta** (paradoxal — confundimento: melhores UBS atraem casos mais graves) |

### Especificidades regionais
- **Acessibilidade a alta complexidade:** inversa em NE, N e S; direta em SE e CO.
- **Referência para o parto:** direta em todas as regiões exceto Norte (paradoxo de demanda: mais referenciamento indica maior necessidade).
- **Infraestrutura:** associação direta em todas as regiões — confirmando o confundimento por gravidade dos casos.

## Discussão

A redução da TMI foi heterogênea: estados do Norte e Nordeste distantes das capitais tiveram menor melhora; a região Norte foi a que apresentou maior desafio mesmo próximo às capitais. A acessibilidade à alta complexidade é determinante — regiões que a possuem conseguem reduzir TMI mesmo com outros fatores adversos. O referenciamento ao parto é essencial para continuidade do cuidado e redução da mortalidade perinatal; a cobertura pré-natal melhorou, mas ainda está mal integrada com o sistema de referência ao parto.

Fatores identificados como investimentos prioritários: Bolsa Família, educação feminina, saneamento, ESF, EACS, e acesso à alta complexidade. O paradoxo da infraestrutura das UBS (associação direta) reflete confundimento por gravidade: melhores estruturas concentram casos mais complexos, não causam maior mortalidade.

**Limitação metodológica importante:** Sub-registro do SIM foi mantido sem correção a nível municipal (modelos de correção têm muitas limitações nessa granularidade). A análise transversal com dados PMAQ (2014) não captura causalidade temporal.

## Conclusão

Houve redução crescente da TMI entre 2000–2015, mas com desigualdades regionais persistentes. A TMI é inversamente associada ao acesso a serviços de alta complexidade, à referência ao parto, à renda per capita e ao estrato de gestão em saúde, sendo esses os principais alvos para investimento. Políticas focadas nesses determinantes são essenciais para o alcance das metas ODS até 2030.

## Relevância para o projeto

Único estudo desta coleção com análise GWR municipal cobrindo todos os 5.565 municípios do Brasil — permite identificar heterogeneidade espacial nas associações entre estrutura da APS e TMI que estudos globais ou regionais não capturam. Documenta: (1) persistência de clusters High-High de TMI em Norte/Nordeste (153 municípios), informando prioridades geográficas; (2) Roraima como caso extremo de não-redução em 15 anos — consistente com sua posição de pior U5MR em 2018–2022 em Lopes 2025; (3) acessibilidade a serviços de alta complexidade como determinante independente mais robusto — argumento estrutural complementar ao da ESF; (4) tabela completa de TMI por UF e ano 2000–2015 (Tabela 1), útil como série de referência.
