dropbox <- "c:/Users/victo/dropbox/DISSERTACAO"

library(tidyverse)
library(arrow)
library(readxl)

homicidios_af <- read_csv2(file.path(dropbox, "taxa_de_homicidios_por_armas_de_fogo.csv"))
homicidios_af_jovens <- read_csv2(file.path(dropbox, "taxa_de_homicidio_de_jovens_por_armas_de_fogo.csv"))
obitos_acidentes <- read_csv2(file.path(dropbox, "taxa_de_obitos_em_acidentes_de_transporte.csv"))
mortes_violentas<- read_csv2(file.path(dropbox, "mortes_violentas.csv"))

#Link base ministério segurança publica
#https://www.gov.br/mj/pt-br/assuntos/sua-seguranca/seguranca-publica/estatistica/download/dnsp-base-de-dados/bancovde-2019.xlsx/@@download/file
banco <- read_xlsx(file.path(dropbox, "BancoVDE 2019.xlsx"))

banco |> 
  filter(municipio == "ASSIS BRASIL") |> 
  print(n= 108)

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
  count(uf, municipio, wt=total_peso, name = "cocaina")
maconha_table <- read_csv(file.path(dropbox, "evento_Apreensão de Maconha.csv")) |> 
  count(uf, municipio, wt=total_peso, name = "maconha")
apreensao_arma <- read_csv(file.path(dropbox, "evento_Arma de Fogo Apreendida.csv")) |> 
  count(uf, municipio, wt=total, name = "armas") 
furto_veiculo <- read_csv(file.path(dropbox, "evento_Furto de veículo.csv")) |> 
  count(uf, municipio, wt=total, name = "furto_vei")
roubo_banco_table <- read_csv(file.path(dropbox, "evento_Roubo a instituição financeira.csv")) |> 
  count(uf, municipio, wt=total, name = "rou_banco")
roubo_carga_table <- read_csv(file.path(dropbox, "evento_Roubo de carga.csv")) |> 
  count(uf, municipio, wt=total, name = "rou_carga")
roubo_veículo_table <- read_csv(file.path(dropbox, "evento_Roubo de veículo.csv")) |> 
  count(uf, municipio, wt=total, name = "rou_vei")
trafico_table <- read_csv(file.path(dropbox, "evento_Tráfico de drogas.csv")) |> 
  count(uf, municipio, wt=total, name = "trafico")

#VARIÁVEIS COM abrangencia municipal
feminicidio_table <- read_csv(file.path(dropbox, "evento_Feminicídio.csv")) |> 
  count(uf, municipio, wt=total, name = "feminicidio")
hom_doloso_table <- read_csv(file.path(dropbox, "evento_Homicídio doloso.csv")) |> 
  count(uf, municipio, wt=total, name = "hom_doloso")
lesao_table <- read_csv(file.path(dropbox, "evento_Lesão corporal seguida de morte.csv")) |> 
  count(uf, municipio, wt=total, name = "lesao")
mandado_table <- read_csv(file.path(dropbox, "evento_Mandado de prisão cumprido.csv")) |> 
  count(uf, municipio, wt=total, name = "mandado")
transito_table <- read_csv(file.path(dropbox, "evento_Morte no trânsito ou em decorrência dele (exceto homicídio doloso).csv")) |> 
  count(uf, municipio, wt=total, name = "transito")
esclarecer_table <- read_csv(file.path(dropbox, "evento_Mortes a esclarecer (sem indício de crime).csv")) |> 
  count(uf, municipio, wt=total, name = "esclarecer")
latrocinio_table <- read_csv(file.path(dropbox, "evento_Roubo seguido de morte (latrocínio).csv")) |> 
  count(uf, municipio, wt=total, name = "latrocinio")
tentativa_homicidio_table <- read_csv(file.path(dropbox, "evento_Tentativa de homicídio.csv")) |> 
  count(uf, municipio, wt=total, name = "tentativa_hom")

#tabela final
municipal_table <- feminicidio_table |> 
  left_join(hom_doloso_table, join_by(uf, municipio)) |> 
  left_join(lesao_table, join_by(uf, municipio)) |> 
  left_join(mandado_table, join_by(uf, municipio)) |> 
  left_join(transito_table, join_by(uf, municipio)) |> 
  left_join(esclarecer_table, join_by(uf, municipio)) |> 
  left_join(latrocinio_table, join_by(uf, municipio)) |> 
  left_join(tentativa_homicidio_table, join_by(uf, municipio))

estadual_table <- cocaina_table |> 
  left_join(maconha_table, join_by(uf, municipio)) |> 
  left_join(apreensao_arma, join_by(uf, municipio)) |> 
  left_join(furto_veiculo, join_by(uf, municipio)) |> 
  left_join(roubo_banco_table, join_by(uf, municipio)) |> 
  left_join(roubo_carga_table, join_by(uf, municipio)) |> 
  left_join(roubo_veículo_table, join_by(uf, municipio)) |> 
  left_join(trafico_table, join_by(uf, municipio))

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
  left_join(codigos, by = c("municipio" = "nome_upper", "uf"="sigla_uf"))

# Remover a coluna temporária 'nome_upper' se desejar
municipal_table <- municipal_table %>%
  select(-id_municipio, !id_municipio_tse:centroide)

#salvar
write_csv(municipal_table, file.path(dropbox, "municipal_MSP.csv"))
write_csv(estadual_table, file.path(dropbox,"estadual_MSP.csv"))
