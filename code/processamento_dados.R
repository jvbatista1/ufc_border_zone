dropbox <- "c:/Users/victor/dropbox/DISSERTACAO"

library(sf)
fronteira <- st_read(file.path(dropbox,"Fronteira/Faixa_de_Fronteira_por_UF_2022.shp")) %>%
  st_transform("WGS84")

library(dplyr)
#Uniformiza a faixa de fronteira como uma única região
linha_fronteira <- fronteira %>%
  mutate(pais = "BR") %>% 
  group_by(pais) %>% 
  summarise()

library(geobr)
#Carrega sf dos municípios brasileiros
municipios <- read_municipality(year=2020, showProgress = T) %>%
  st_transform("WGS84")

#Carrega o sf dos países da América do Sul
america <- st_read(file.path(dropbox,"America/South_America.shp")) %>% 
  st_transform("WGS84")

#Carrega o sf dos municípios da faixa de fronteira
municipios_fronteira <- st_read(file.path(dropbox,"Municipios_Fronteira/Municipios_Faixa_Fronteira_2022.shp")) %>%
  st_transform("WGS84")

# Estabelece a nova proposta de faixa de fronteira
linha_fronteira_300km <- st_buffer(linha_fronteira, dist = 150000)

# Verifica municípios que passam a pertencer à região
# Adiciona variável de intercessão
municipios$inter <- st_intersects(municipios, linha_fronteira_300km, sparse = F)

# Adiciona variável de pertencimento à fronteira original
municipios <- municipios %>%
  mutate(fronteira = ifelse(code_muni %in% municipios_fronteira$CD_MUN, 1, 0))

# Cria tratamento e controle
df <- municipios |>
  #filtra os municípios na nova faixa
  filter(inter == T) |> 
  # cria o grupo de tratamento e controle
  mutate(treated = ifelse(code_muni %in% municipios_fronteira$CD_MUN, 1, 0),
         groups = ifelse(treated == 1, "treatment", "control"),
         # cria os arcos
         arcos = case_when(abbrev_state %in% c("AP", "PA", "AM", "AC", "RR") ~ "Arco Norte",
                           abbrev_state %in% c("RO", "MS", "MT") ~ "Arco Central",
                           abbrev_state %in% c("PR", "SC", "RS") ~ "Arco Sul",
                           abbrev_state %in% c("SP") ~ "Arco Sudeste")) |> 
  # exclui a variável classificatória. as recém criadas a substituem
  dplyr::select(-inter)

# prepara a tabela da fronteira para mergir com a df principal (municipios)
t <- municipios_fronteira |> 
  # remove colunas indesejadas
  select(-c("NM_REGIAO", "CD_UF", "NM_UF", "SIGLA_UF", "NM_MUN", "geometry")) |> 
  # padroniza o nome code_muni
  rename("code_muni" = "CD_MUN") |> 
  # altera o tipo das colunas para numeric e logic
  mutate(code_muni = as.numeric(code_muni),
         CID_GEMEA = ifelse(is.na(CID_GEMEA) == F, 1, 0))

# remove o componente gráfico
st_geometry(t) <- NULL

# realiza o join
df <- dplyr::left_join(df, t, by = "code_muni")
rm(t)

# prepara a tabela da sede dos mun da faixa da fronteira para mergir com a df principal (municipios)
sede_municipios <- st_read(file.path(dropbox,"Sedes_Municipios_Faixa_de_Fronteira_Cidades_Gemeas_2022_shp/Sedes_Municipios_Faixa_de_Fronteira_Cidades_Gemeas_2022.shp")) %>%
  st_transform("WGS84")

t <- sede_municipios %>%
  # seleciona colunas desejadas
  select(c("CD_MUN", "FAIXA_SEDE")) %>%
  # harmoniza os nomes de variáveis
  rename("code_muni" = "CD_MUN") %>%
  # modifica a classe da variável
  mutate(code_muni = as.numeric(code_muni),
         FAIXA_SEDE = ifelse(FAIXA_SEDE == "sim", 1, 0))

# remove a geometria da tabela para realizar o join
st_geometry(t) <- NULL

# realiza o join
df <- dplyr::left_join(df, t, by = "code_muni")
rm(t)

# prepara para juntar demais países da américa do sul na base de municípios
# Remove regiões sem fronteira com o br
america2 <- america %>%
  filter(!(COUNTRY %in% c("Brazil", "Falkland Islands (UK)",
                          "South Georgia and the South Sandwich Is (UK)", "Chile", "Ecuador")))

# verifica interseções
a <- st_intersects(df, america2, sparse = FALSE)

# renomeia colunas e cria variáveis dummy
a <- as.data.frame(a) %>%
  rename("Argentina" = "V1",
         "Bolivia" = "V2",
         "Colombia" = "V3",
         "French_Guiana" = "V4",
         "Guyana" = "V5",
         "Suriname" = "V6",
         "Paraguay" = "V7",
         "Peru" = "V8",
         "Uruguay" = "V9",
         "Venezuela" = "V10") %>%
  mutate(Argentina = ifelse(Argentina == T, 1, 0),
         Bolivia = ifelse(Bolivia == T, 1, 0),
         Colombia = ifelse(Colombia == T, 1, 0),
         French_Guiana = ifelse(French_Guiana == T, 1, 0),
         Guyana = ifelse(Guyana == T, 1, 0),
         Suriname = ifelse(Suriname == T, 1, 0),
         Paraguay = ifelse(Paraguay == T, 1, 0),
         Peru = ifelse(Peru == T, 1, 0),
         Uruguay = ifelse(Uruguay == T, 1, 0),
         Venezuela = ifelse(Venezuela == T, 1, 0))

df <- cbind(df, a)
rm(a)

t <- sede_municipio |> 
  select(code_muni, distancia_fronteira_terrestre, distancias_fronteira_interior)
st_geometry(t) <- NULL

df <- df |> 
  left_join(t, by = "code_muni") |> 
  mutate(distancias_fronteira_interior = ifelse(groups == "treatment", 
                                                -distancias_fronteira_interior, 
                                                distancias_fronteira_interior))


rm(t)



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
