dropbox <- "c:/Users/victor/dropbox/DISSERTACAO"

#### Meu exemplo
library(sf)
library(dplyr)
fronteira <- read_sf(file.path(dropbox,"Fronteira/Faixa_de_Fronteira_por_UF_2022.shp")) %>%
  st_transform("WGS84") |> 
  rename("id_uf" = "CD_UF",
         "nome_uf" = "NM_UF",
         'sigla_uf' = "SIGLA_UF",
         "nome_regiao" = "NM_REGIAO",
         "area_uf" = "AREA_KM2",
         "area_integrada" = "AREA_INT",
         "porcentagem_integrada" = "PORC_INT")
fronteira

#Uniformiza a faixa de fronteira como uma única região
linha_fronteira <- fronteira %>%
  mutate(pais = "BR") %>% 
  group_by(pais) %>% 
  summarise()

#Carrega o sf dos municípios da faixa de fronteira
municipios_fronteira <- read_sf(file.path(dropbox,"Municipios_Fronteira/Municipios_Faixa_Fronteira_2022.shp")) %>%
  st_transform("WGS84") |> 
  rename("id_municipio" = "CD_MUN",
         "nome" = "NM_MUN",
         "id_uf" = "CD_UF",
         "nome_uf" = "NM_UF",
         'sigla_uf' = "SIGLA_UF",
         "nome_regiao" = "NM_REGIAO",
         "area_municipio" = "AREA_TOT",
         "area_integrada" = "AREA_INT",
         "porcentagem_integrada" = "PORC_INT") |> 
  mutate(gemea = !(is.na(CID_GEMEA)), .keep = "unused", .after = "porcentagem_integrada")
municipios_fronteira

#Carrega sf dos municípios brasileiros
library(geobr)
municipios <- read_municipality(year=2020, showProgress = T, simplified = T) %>%
  st_transform("WGS84")

municipios

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

#Carrega o sf dos países da América do Sul
america <- st_read(file.path(dropbox,"America/South_America.shp")) %>% 
  st_transform("WGS84")

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

library(units)

brasil <- america %>%
  filter(COUNTRY == "Brazil")

sede_municipios <- read_municipal_seat(year=2010, showProgress = T) %>%
  st_transform("WGS84")

sede_municipios <- sede_municipios |> 
  filter(name_region == "Norte") |> 
  filter(abbrev_state!="TO")

# Filtrar os países que fazem fronteira terrestre com o Brasil
paises_fronteira <- america %>%
  filter(COUNTRY %in% c("French Guiana (France)", "Suriname", "Guyana", "Venezuela", 
                        "Colombia", "Peru", "Bolivia", "Paraguay", "Argentina", "Uruguay"))

# Criar um buffer de 50 metros para dentro do polígono do Brasil na fronteira com esses países
buffer_fronteira <- st_buffer(paises_fronteira, dist = 50)

# Definir a fronteira terrestre do Brasil com base nesse buffer
fronteira_terrestre <- st_intersection(brasil, buffer_fronteira)

# Converter a fronteira terrestre em uma linha (apenas a borda do Brasil que faz fronteira terrestre)
fronteira_linha <- st_boundary(fronteira_terrestre) |> 
  group_by(COUNTRY) |> 
  summarise()

# Calcular a distância entre as sedes municipais e a linha da fronteira terrestre
distancias_terrestres <- st_distance(sede_municipios, fronteira_linha)

# Adicionar a distância calculada ao dataframe de sede_municipio
sede_municipios$distancia_fronteira_terrestre <- as.numeric(distancias_terrestres)

#### limite da faixa de fronteira
faixa <- st_read(file.path(dropbox,"Municipios_Fronteira/Municipios_Faixa_Fronteira_2022.shp")) %>%
  st_transform("WGS84") |> 
  mutate(pais = "BR") |>  
  group_by(pais) |> 
  summarise()

faixa_linha <- st_boundary(faixa)

faixa_linha

# Criar um buffer de 50 metros para dentro do polígono do Brasil na fronteira com esses países
buffer_fronteira <- st_buffer(fronteira_linha, dist = 5000) |> 
  st_union()

# Definir a faixa de fronteira interior do Brasil com base nesse buffer
faixa_interior <- st_difference(faixa_linha, buffer_fronteira)

plot(st_geometry(faixa_interior))

# Calcular a distância entre as sedes municipais e a linha da fronteira terrestre
distancias_interior <- st_distance(sede_municipios, faixa_interior)

# Adicionar a distância calculada ao dataframe de sede_municipio
sede_municipios$distancias_fronteira_interior <- as.numeric(distancias_interior)

t <- sede_municipios |> 
  select(code_muni, distancia_fronteira_terrestre, distancias_fronteira_interior)
st_geometry(t) <- NULL

df <- df |> 
  left_join(t, by = "code_muni") |> 
  mutate(distancias_fronteira_interior = ifelse(groups == "treatment", 
                                                -distancias_fronteira_interior, 
                                                distancias_fronteira_interior))


rm(t)
