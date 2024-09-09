#### IMPORTANDO BASE DO Sistema de Informações sobre Mortalidade (SIM)####
library("basedosdados")
# Defina o seu projeto no Google Cloud
set_billing_id("rapid-pact-400813")
# Para carregar o dado direto no R
query <- bdplyr("basedosdados.br_ms_sim.municipio_causa")
sim <- bd_collect(query)

#### Analisando violencia em 2019 #####
# Vetor de prefixos para as causas violentas
prefixos <- c("W32", "W33", "W34", "X85", "X86", "X87", "X88", "X89", "X90", 
              "X91", "X92", "X93", "X94", "X95", "X96", "X97", "X98", "X99", 
              "Y00", "Y01", "Y02", "Y03", "Y04", "Y05", "Y06", "Y07", "Y08", 
              "Y09", "Y10", "Y11", "Y17", "Y18", "Y19", "Y20", "Y21", "Y22", 
              "Y23", "Y24", "Y25", "Y26", "Y27", "Y28", "Y29", "Y30", "Y31", 
              "Y32", "Y33", "Y34", "Y35", "Y87", "Y89", "Y12", "Y13", "Y14", 
              "Y15", "Y16")


library(tidyverse)
# Filtrando a tabela
sim <- sim %>%
  filter(str_detect(causa_basica, paste0("^", prefixos, collapse = "|"))) |> 
  filter(ano == 2019) |> 
  count(id_municipio)

#### IMPORTANDO BASE DO Anuário Brasileiro de Segurança Pública ####
# Para carregar o dado direto no R
query <- bdplyr("basedosdados.br_fbsp_absp.municipio")
absp <- bd_collect(query)

#### POPULAÇÃO ####
# Calcular em função da população
query <- bdplyr("br_ibge_populacao.municipio")
populacao <- bd_collect(query)



