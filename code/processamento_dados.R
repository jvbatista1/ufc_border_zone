dropbox <- "c:/Users/victor/dropbox/DISSERTACAO"

library(tidyverse)
library(readxl)

# Base do Sistema de Informações sobre Mortalidade (SIM)####
library(basedosdados)
query <- bdplyr("basedosdados.br_ms_sim.municipio_causa")
sim <- bd_collect(query)

#### Analisando mortes violentas em 2019 #####
# Vetor de prefixos para as causas violentas
prefixos <- c("W32", "W33", "W34", "X85", "X86", "X87", "X88", "X89", "X90", 
              "X91", "X92", "X93", "X94", "X95", "X96", "X97", "X98", "X99", 
              "Y00", "Y01", "Y02", "Y03", "Y04", "Y05", "Y06", "Y07", "Y08", 
              "Y09", "Y10", "Y11", "Y17", "Y18", "Y19", "Y20", "Y21", "Y22", 
              "Y23", "Y24", "Y25", "Y26", "Y27", "Y28", "Y29", "Y30", "Y31", 
              "Y32", "Y33", "Y34", "Y35", "Y87", "Y89", "Y12", "Y13", "Y14", 
              "Y15", "Y16")


# Filtrando a tabela
sim <- sim %>%
  filter(str_detect(causa_basica, paste0("^", prefixos, collapse = "|"))) |> 
  filter(ano == 2019) |> 
  count(id_municipio, name = "mortes_violentas")

#Link base ministério segurança publica
#https://www.gov.br/mj/pt-br/assuntos/sua-seguranca/seguranca-publica/estatistica/download/dnsp-base-de-dados/bancovde-2019.xlsx/@@download/file
banco <- read_xlsx(file.path(dropbox, "BancoVDE 2019.xlsx"), col_types = c("text", "text", "text", "date", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "numeric"))

banco <- banco |> 
  rename("nome" = "municipio")

#### CRIAR BASES SEPARADAS PARA CADA EVENTO ####
# Carregar os pacotes necessários
library(dplyr)
library(purrr)

# Vamos criar uma lista com as diferentes categorias de eventos
categorias_evento <- banco %>% 
  distinct(evento) %>% 
  pull()

# Função para salvar cada categoria em um CSV
salvar_csv <- function(evento) {
  banco %>% 
    filter(evento == !!evento) %>% 
    write.csv(file = paste0("evento_", evento, ".csv"), row.names = FALSE)
}

# Aplicar a função em cada categoria
walk(categorias_evento, salvar_csv)
rm(banco)

#VARIÁVEIS COM abrangencia estadual
cocaina_table <- read_csv(file.path(dropbox, "evento_Apreensão de Cocaína.csv")) |> 
  count(uf, nome, wt=total_peso, name = "cocaina")
maconha_table <- read_csv(file.path(dropbox, "evento_Apreensão de Maconha.csv")) |> 
  count(uf, nome, wt=total_peso, name = "maconha")
apreensao_arma <- read_csv(file.path(dropbox, "evento_Arma de Fogo Apreendida.csv")) |> 
  count(uf, nome, wt=total, name = "armas") 
furto_veiculo <- read_csv(file.path(dropbox, "evento_Furto de veículo.csv")) |> 
  count(uf, nome, wt=total, name = "furto_vei")
roubo_banco_table <- read_csv(file.path(dropbox, "evento_Roubo a instituição financeira.csv")) |> 
  count(uf, nome, wt=total, name = "rou_banco")
roubo_carga_table <- read_csv(file.path(dropbox, "evento_Roubo de carga.csv")) |> 
  count(uf, nome, wt=total, name = "rou_carga")
roubo_veículo_table <- read_csv(file.path(dropbox, "evento_Roubo de veículo.csv")) |> 
  count(uf, nome, wt=total, name = "rou_vei")
trafico_table <- read_csv(file.path(dropbox, "evento_Tráfico de drogas.csv")) |> 
  count(uf, nome, wt=total, name = "trafico")

#VARIÁVEIS COM abrangencia municipal
feminicidio_table <- read_csv(file.path(dropbox, "evento_Feminicídio.csv")) |> 
  count(uf, nome, wt=total_vitimas, name = "feminicidio")
hom_doloso_table <- read_csv(file.path(dropbox, "evento_Homicídio doloso.csv")) |> 
  count(uf, nome, wt=total_vitimas, name = "hom_doloso")
lesao_table <- read_csv(file.path(dropbox, "evento_Lesão corporal seguida de morte.csv")) |> 
  count(uf, nome, wt=total_vitimas, name = "lesao")
mandado_table <- read_csv(file.path(dropbox, "evento_Mandado de prisão cumprido.csv")) |> 
  count(uf, nome, wt=total, name = "mandado")
transito_table <- read_csv(file.path(dropbox, "evento_Morte no trânsito ou em decorrência dele (exceto homicídio doloso).csv")) |> 
  count(uf, nome, wt=total_vitimas, name = "transito")
esclarecer_table <- read_csv(file.path(dropbox, "evento_Mortes a esclarecer (sem indício de crime).csv")) |> 
  count(uf, nome, wt=total_vitimas, name = "esclarecer")
latrocinio_table <- read_csv(file.path(dropbox, "evento_Roubo seguido de morte (latrocínio).csv")) |> 
  count(uf, nome, wt=total_vitimas, name = "latrocinio")
tentativa_homicidio_table <- read_csv(file.path(dropbox, "evento_Tentativa de homicídio.csv")) |> 
  count(uf, nome, wt=total_vitimas, name = "tentativa_hom")

#tabela final
municipal_table <- feminicidio_table |> 
  left_join(hom_doloso_table, join_by(uf, nome)) |> 
  left_join(lesao_table, join_by(uf, nome)) |> 
  left_join(mandado_table, join_by(uf, nome)) |> 
  left_join(transito_table, join_by(uf, nome)) |> 
  left_join(esclarecer_table, join_by(uf, nome)) |> 
  left_join(latrocinio_table, join_by(uf, nome)) |> 
  left_join(tentativa_homicidio_table, join_by(uf, nome))

estadual_table <- cocaina_table |> 
  left_join(maconha_table, join_by(uf, nome)) |> 
  left_join(apreensao_arma, join_by(uf, nome)) |> 
  left_join(furto_veiculo, join_by(uf, nome)) |> 
  left_join(roubo_banco_table, join_by(uf, nome)) |> 
  left_join(roubo_carga_table, join_by(uf, nome)) |> 
  left_join(roubo_veículo_table, join_by(uf, nome)) |> 
  left_join(trafico_table, join_by(uf, nome))

#### IMPORTANDO BASE PARA INSERIR OS CÓDIGOS IBGE ####
library("basedosdados")
# Defina o seu projeto no Google Cloud
set_billing_id("rapid-pact-400813")
# Para carregar o dado direto no R
query <- bdplyr("br_bd_diretorios_brasil.municipio")
codigos <- bd_collect(query)

# Transformar os nomes dos municípios na tabela 'codigos' para maiúsculas
codigos <- codigos %>%
  mutate(nome_upper = toupper(nome))

# Unir as tabelas com base no nome do município
municipal_table <- municipal_table %>%
  left_join(codigos, by = c("nome" = "nome_upper", "uf"="sigla_uf"))

# Remover a coluna temporária 'nome_upper' se desejar
municipal_table <- municipal_table %>%
  select(!id_municipio_tse:centroide)

municipal_table <- municipal_table |> 
  left_join(sim, join_by(id_municipio))

#### POPULACAO ####
# Calcular em função da população
query <- bdplyr("br_ibge_populacao.municipio")
populacao <- bd_collect(query)

municipal_table <- municipal_table |> 
  left_join(populacao |>
              filter(ano == 2019) |> 
              select(id_municipio:populacao), join_by(id_municipio)) |> 
  mutate(
    mortes_violentas_pc = (mortes_violentas/populacao)*100000,
    feminicidio_pc = (feminicidio/populacao)*100000,
    hom_doloso_pc = (hom_doloso/populacao)*100000,
    lesao_pc = (lesao/populacao)*100000,
    mandado_pc = (mandado/populacao)*100000,
    transito_pc = (transito/populacao)*100000,
    esclarecer_pc = (esclarecer/populacao)*100000, 
    latrocinio_pc = (latrocinio/populacao)*100000,
    tentativa_hom_pc = (tentativa_hom/populacao)*100000
  ) |> 
  select(!feminicidio:tentativa_hom) |> 
  select(!mortes_violentas)

#salvar
write_csv(municipal_table, file.path(dropbox, "municipal_MSP.csv"))
write_csv(estadual_table, file.path(dropbox,"estadual_MSP.csv"))
