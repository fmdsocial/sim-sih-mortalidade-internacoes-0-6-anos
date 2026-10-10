# Critérios de classificação da CID-10 — Observatório de Saúde Infantil 0–6 anos

**Projeto:** SIM + SIH-SUS, Brasil, 2015–2024 · INSPER / Hospital Pequeno Príncipe **Origem:** reunião de 27/07 — itens 1, 2 e 3 do plano de trabalho da Maria **Implementação:** bloco `1f` de `app.R` (função `cid_classify`), reaproveitado pelo ETL `00_link_sih_sim.R` para que painel e microdados usem exatamente a mesma regra. **Anexo:** `dicionario_cid10_revisado.csv` — 132 códigos, pronto para edição.

------------------------------------------------------------------------

## 1. Arquitetura em duas camadas

A classificação é feita em duas camadas independentes, e essa separação é deliberada.

**Camada 1 — determinística, por faixa de código.** Todo código CID-10 é mapeado para o capítulo correspondente pela faixa em que cai (A00–B99 → capítulo I, e assim por diante), e cada capítulo tem um sistema do organismo associado. A vantagem é que códigos que ainda não apareceram nas bases — ou que apareçam em uma exportação futura — são classificados sem edição manual. Nenhuma linha de análise trava por falta de dicionário.

**Camada 2 — curada, por código.** Sobre a camada 1, um dicionário revisado corrige nomenclaturas, consolida patologias semelhantes e marca códigos genéricos. É aqui que entra o julgamento clínico, e é este o arquivo que a Maria edita.

Quando um código está no dicionário, a camada 2 prevalece. Quando não está, a camada 1 responde sozinha, e uma heurística textual mais uma lista de faixas de *garbage code* decide se o código é genérico.

------------------------------------------------------------------------

## 2. Sistemas do organismo

Vinte e dois capítulos da CID-10 foram agrupados nos sistemas abaixo. A correspondência é um-para-um com o capítulo, o que mantém a classificação auditável contra a estrutura oficial.

| Capítulo | Faixa   | Sistema do organismo                      |
|----------|---------|-------------------------------------------|
| I        | A00–B99 | Doenças infecciosas e parasitárias        |
| II       | C00–D48 | Neoplasias (oncologia)                    |
| III      | D50–D89 | Sangue, órgãos hematopoéticos e imunidade |
| IV       | E00–E90 | Endócrino, nutricional e metabólico       |
| V        | F00–F99 | Saúde mental e comportamento              |
| VI       | G00–G99 | Sistema nervoso                           |
| VII      | H00–H59 | Olho e anexos                             |
| VIII     | H60–H95 | Ouvido                                    |
| IX       | I00–I99 | Sistema cardiovascular                    |
| X        | J00–J99 | Sistema respiratório                      |
| XI       | K00–K93 | Sistema digestório                        |
| XII      | L00–L99 | Pele e tecido subcutâneo                  |
| XIII     | M00–M99 | Sistema osteomuscular                     |
| XIV      | N00–N99 | Sistema geniturinário                     |
| XV       | O00–O99 | Gestação, parto e puerpério               |
| XVI      | P00–P96 | Afecções perinatais                       |
| XVII     | Q00–Q99 | Malformações congênitas e cromossômicas   |
| XVIII    | R00–R99 | Causas mal definidas                      |
| XIX      | S00–T98 | Lesões e traumatismos                     |
| XX       | V01–Y98 | Causas externas                           |
| XXI      | Z00–Z99 | Registro administrativo (não-diagnóstico) |
| XXII     | U00–U99 | Códigos especiais (COVID-19 e afins)      |

Duas observações que valem para a interpretação dos gráficos:

O capítulo XIX (lesões) e o XX (causas externas) descrevem o mesmo evento por ângulos diferentes — a lesão sofrida e o mecanismo que a causou. **O SIM codifica a causa básica pelo mecanismo (XX); o SIH codifica o diagnóstico principal pela lesão (XIX).** Por isso "causas externas" praticamente desaparece na aba de internações e reaparece como traumatismo. Não é erro de dados: é a natureza dos dois sistemas, e o painel registra isso na nota de cada aba.

O capítulo XXI não é diagnóstico. Z37, Z38, Z03 e Z76 marcam eventos administrativos (resultado do parto, nascido vivo, observação por suspeita). No SIH neonatal eles somam **17,9% do bloco "Outras Causas / Difícil Prevenção"** — Z03 com 8,4%, Z37 com 4,4%, Z38 com 2,8% e Z76 com 2,3%. Isso inflava artificialmente aquele grupo. O painel agora tem um filtro global que os exclui, ligado por padrão.

------------------------------------------------------------------------

## 3. Agrupamento de patologias semelhantes

Trinta e nove grupos consolidam códigos que representam a mesma condição clínica. Os principais:

**Malformações congênitas** — Cardiopatias congênitas (Q20–Q25); Malformações do sistema nervoso central (Q00, Q02–Q05); Malformações da parede abdominal e do diafragma (Q79); Malformações do trato digestivo (Q38–Q42); Fendas orofaciais (Q35–Q37); Malformações respiratórias (Q33); Malformações do trato urinário (Q60); Malformações genitais (Q53, Q54); Malformações osteoarticulares e craniofaciais (Q18, Q66, Q69, Q78); Síndromes genéticas e cromossômicas (Q87, Q90, Q91); Malformações múltiplas e inespecíficas (Q89).

**Infecções** — Sepse e infecções sistêmicas (A31, A41, A48, A49, B34, B99, P36, P39); Infecções congênitas, sífilis e TORCH (A50, A53, P35, P37); Meningites e infecções do SNC (A39, G00); Infecções respiratórias baixas (J12, J15, J18, J21, J69, P23); Infecções respiratórias altas (J06, J35); Diarreia e gastroenterite (A04, A08, A09, K52); Infecções de pele e partes moles (L01–L03, L98); Infecções e transtornos do trato urinário (N39).

**Período perinatal** — Prematuridade e baixo peso (P05, P07); Asfixia e hipóxia perinatal (P20, P21, P91); Complicações maternas e placentárias (P00–P02); Aspiração neonatal (P24); Enterocolite necrotizante (P77); Icterícia e distúrbios hematológicos neonatais (P57–P59); Distúrbios metabólicos neonatais (P70, P74); Transtornos cardiovasculares perinatais (P29); Outras afecções perinatais (P83, P92, P94, P96).

**Causas externas** — Afogamento (W67, W69, W74); Sufocação, aspiração e engasgo (W75, W78, W79, W84); Traumatismos (S06, S42, S52).

**Alta complexidade** — Leucemias (C91, C92); Tumores do sistema nervoso central (C71); Tumores sólidos extracranianos (C74); Hemoglobinopatias (D57); Epilepsia e encefalopatias (G40, G80, G91, G93); Cardiomiopatias (I42).

**Demais** — Insuficiência e outros transtornos respiratórios (J81, J96, J98, P22, P27, P28); Asma e sibilância (J45); Afecções cirúrgicas do abdome (K35, K40, K42, K56, K63, K92); Desnutrição (E43, E46); Desidratação e distúrbios hidroeletrolíticos (E86, E87); Morte súbita do lactente (R95); Causas mal definidas (R50, R69, R98, R99, Y09, Y34); Doenças tropicais negligenciadas (B55); Procedimentos urológicos eletivos (N47); Registros administrativos (Z03, Z37, Z38, Z76).

------------------------------------------------------------------------

## 4. Códigos genéricos e mal definidos

Esta é a marcação que sustenta a pergunta levantada na reunião — *entra com um CID genérico e sai com um genérico de mortalidade, sem que a causa exata seja identificada*. Sem ela, não há como medir o fenômeno.

Um código recebe `generico = 1` quando se enquadra em um destes critérios:

1.  **Capítulo XVIII inteiro** (R00–R99) — sintomas, sinais e achados anormais.
2.  **Capítulo XXI inteiro** (Z00–Z99) — registro administrativo.
3.  **Faixas de *garbage code*** reconhecidas na literatura (GBD, RIPSA): Y10–Y34 (intenção indeterminada), Y86–Y89 (sequelas sem causa), X59 (exposição a fator não especificado), V99 (transporte não especificado), W84 (obstrução não especificada da respiração), Y09 (agressão por meios não especificados).
4.  **Códigos "não especificado", "outras" e NCOP** dentro de qualquer capítulo, identificados por análise da descrição oficial.

Resultado: **42 dos 132 códigos** presentes nas bases são genéricos. Nas tabelas de óbito com CID nomeado isso corresponde a uma fatia relevante do total, exibida como KPI no Panorama e no painel de Sistemas.

Os códigos genéricos de maior volume merecem leitura à parte:

| CID | Descrição | Por que é genérico |
|------------------------|------------------------|------------------------|
| J18 | Pneumonia por microrganismo não especificado | Pneumonia sem agente identificado — alto volume, baixo poder informativo |
| A41 | Outras septicemias, agente não especificado | Sepse sem agente |
| R99 | Outras causas mal definidas e não especificadas de mortalidade | Código-fim de linha da CID |
| Q24 | Outras malformações congênitas do coração | Maior volume do bloco cardíaco e, ainda assim, é o "outras" |
| Q89 | Outras malformações congênitas NCOP | Idem, para malformações em geral |
| P96 | Outras afecções originadas no período perinatal | Idem, perinatal |
| W84 | Obstrução não especificada da respiração | Registra o desfecho sem o mecanismo |
| G93 | Outros transtornos do encéfalo | Neurologia sem especificação |

------------------------------------------------------------------------

## 5. Revisões de nomenclatura aplicadas

Catorze códigos receberam correção ou nota. Os quatro que mudam a leitura dos resultados:

**W84 — corrigido.** O rótulo anterior era "obstrução respiratória não especificada", o que invertia o sentido do código. A descrição oficial é **"obstrução não especificada da respiração"**: registra-se que houve obstrução, sem dizer o mecanismo (engasgo? sufocação? aspiração?). É um *garbage code* de causa externa, e não uma doença respiratória. Foi o caso levantado na reunião e está agora marcado como genérico. No SIM pós-neonatal, W84 responde por 4,9% do bloco "Outras Causas / Difícil Prevenção", e W78 e W79 — que descrevem o mesmo tipo de evento *com* mecanismo — somam mais 10,7%. Juntos, esses três códigos indicam um problema de qualificação de asfixia mecânica em lactentes que vale investigar por região.

**Q79 — reclassificado.** A descrição oficial ("malformações congênitas do sistema osteomuscular NCOP") levava o código para o sistema osteomuscular. Na faixa de 0 a 6 anos, porém, os óbitos por Q79 são majoritariamente hérnia diafragmática (Q79.0), onfalocele (Q79.2) e gastrosquise (Q79.3) — condições cirúrgicas neonatais de alta complexidade, não doença osteomuscular. Foi movido para o grupo **"Malformações da parede abdominal e do diafragma"**. Com 5.774 óbitos no período, essa realocação muda materialmente o perfil de malformações do painel neonatal.

**C74 — nomenclatura ajustada.** O rótulo dizia "câncer da suprarrenal (neuroblastoma)". C74 é a topografia — neoplasia maligna da glândula suprarrenal —, e neuroblastoma é a morfologia, codificada em C47/M9500. O rótulo foi corrigido para a topografia.

**A39 e D57 — nomenclatura ajustada** para as descrições oficiais ("infecção meningocócica" e "transtornos falciformes").

As demais notas — A41, B34, J18, Q24, Y09, Z03, Z37, Z38, Z76 — registram a marcação como genérico ou administrativo e estão na aba **CID & Capítulos → Revisão de nomenclatura** do painel, com exportação para CSV e Excel.

------------------------------------------------------------------------

## 6. Como editar

O arquivo `dicionario_cid10_revisado.csv` tem uma linha por código e as colunas `cid`, `descricao_revisada`, `capitulo`, `capitulo_nome`, `sistema_organismo`, `grupo_patologia`, `generico_mal_definido` e `nota_revisao`.

Para incorporar uma revisão ao painel, edite o bloco `cid_dic` em `app.R` — que usa o mesmo separador `|` e a mesma ordem de colunas — ou substitua o bloco pela leitura do CSV. Acrescentar um código novo exige apenas uma linha: a camada determinística já resolve capítulo e sistema, e o dicionário só precisa informar grupo, marcação de genérico e nota.

------------------------------------------------------------------------

## 7. O que ainda depende de decisão clínica

Três pontos ficaram deliberadamente em aberto, porque exigem julgamento que não cabe a uma regra automática:

**Nível de agregação dos grupos.** Cardiopatias congênitas estão em um único grupo. Se a discussão do HPP exigir separar cardiopatias cianóticas de acianóticas, ou obstrutivas de septais, isso é uma edição de `grupo_patologia` — sem impacto no restante do código.

**Q24 dentro de cardiopatias.** O código é ao mesmo tempo o de maior volume do bloco cardíaco e um código "outras". Mantivemos no grupo com marcação de genérico, mas cabe decidir se ele deve aparecer separado nos gráficos.

**Doenças raras.** Não existe faixa CID-10 que as delimite; hoje elas se distribuem entre síndromes genéticas e cromossômicas, erros inatos do metabolismo e malformações múltiplas. Se o HPP quiser um recorte próprio de "doenças raras", o caminho é uma lista de códigos definida pela equipe clínica, que entra como uma coluna adicional no dicionário.