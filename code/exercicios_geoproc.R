library(sf)
library(terra)         # classes and functions for raster data
library(spData)        # load geographic data
library(spDataLarge)   # load larger geographic data
library(tidyverse)

dropbox <- "c:/Users/victor/dropbox/DISSERTACAO"

plot(world)

summary(world["lifeExp"])

world_mini = world[1:2, 1:3]
world_mini

world_dfr = st_read(system.file("shapes/world.shp", package = "spData"))
world_tbl = read_sf(system.file("shapes/world.shp", package = "spData"))
class(world_dfr)

class(world_tbl)

plot(world[3:6])
plot(world["pop"])

world_asia = world[world$continent == "Asia", ]
asia = st_union(world_asia)

plot(world["pop"], reset = FALSE)
plot(asia, add = TRUE, col = "red")

plot(world["continent"], reset = FALSE)
cex = sqrt(world$pop) / 10000
world_cents = st_centroid(world, of_largest = TRUE)
plot(st_geometry(world_cents), add = TRUE, cex = cex)

nigeria = world[world$name_long == "Nigeria", ]
plot(st_geometry(nigeria), expandBB = c(0, 0.2, 0.1, 1), col = "blue", lwd = 5)
plot(st_geometry(world), add = TRUE)
text(world)

raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
my_rast = rast(raster_filepath)
class(my_rast)

my_rast
tmap::tmap(my_rast)

single_raster_file = system.file("raster/srtm.tif", package = "spDataLarge")
single_rast = rast(raster_filepath)


nz
canterbury = nz |> filter(Name == "Canterbury")

st_union(canterbury)

canterbury_height = nz_height[canterbury, ]

plot(st_geometry(nz))
plot(st_geometry(canterbury), expandBB = c(0, 0, 0, 0), col = "gray", lwd = 3, add = T)
plot(st_geometry(nz_height), pch = 2, add = T, , col = "red")
plot(st_geometry(canterbury_height), pch = 2, add = T, , col = "red")

polygon_matrix = cbind(
  x = c(0, 0, 1, 1,   0),
  y = c(0, 1, 1, 0.5, 0)
)
polygon_sfc = st_sfc(st_polygon(list(polygon_matrix)))

point_df = data.frame(
  x = c(0.2, 0.7, 0.4),
  y = c(0.1, 0.2, 0.8)
)
point_sf = st_as_sf(point_df, coords = c("x", "y"))

st_intersects(point_sf, polygon_sfc)

nz_highest = nz_height |> slice_max(n = 1, order_by = elevation)
canterbury_centroid = st_centroid(canterbury)
st_distance(nz_highest, canterbury_centroid)

co = filter(nz, grepl("Canter|Otag", Name))
st_distance(nz_height[1:3, ], co)

plot(st_geometry(co))
plot(st_geometry(nz_height[1:3, ]), add=T)

set.seed(2018) # set seed for reproducibility
(bb = st_bbox(world)) # the world's bounds
random_df = data.frame(
  x = runif(n = 10, min = bb[1], max = bb[3]),
  y = runif(n = 10, min = bb[2], max = bb[4])
)
random_points = random_df |> 
  st_as_sf(coords = c("x", "y"), crs = "EPSG:4326") # set coordinates and CRS


#### canterbury_height

#### Meu exemplo
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
plot(fronteira[fronteira$nome_regiao == "Norte", "area_integrada"])
  
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


plot(municipios_fronteira |> select(area_integrada, porcentagem_integrada, gemea))

municipios_norte <- municipios_fronteira |> 
  filter(sigla_uf %in% c("AC", "AM", "AP", "PA", "RR"))

plot(municipios_norte[,9:10])

library(units)

# Carregar o shapefile da América do Sul (fronteiras) e dos municípios
# Substitua "path_to_shapefile" pelo caminho correto dos seus arquivos

america <- st_read(file.path(dropbox,"America/South_America.shp")) %>% 
  st_transform("WGS84")

brasil <- america %>%
  filter(COUNTRY == "Brazil")

sede_municipio <- read_municipal_seat(year=2010, showProgress = T) %>%
  st_transform("WGS84")

sede_municipio <- sede_municipio |> 
  filter(name_region == "Norte") |> 
  filter(abbrev_state!="TO")

library(sf)
library(dplyr)

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
distancias_terrestres <- st_distance(sede_municipio, fronteira_linha)

# Adicionar a distância calculada ao dataframe de sede_municipio
sede_municipio$distancia_fronteira_terrestre <- as.numeric(distancias_terrestres)

# Visualizar os resultados
head(sede_municipio)

# Plotar a linha da fronteira terrestre e as sedes municipais
plot(st_geometry(fronteira_linha), col = "blue")
plot(st_geometry(sede_municipio), col = "red", add = TRUE)

#### limite da faixa de fronteira
faixa <- st_read(file.path(dropbox,"Municipios_Fronteira/Municipios_Faixa_Fronteira_2022.shp")) %>%
  st_transform("WGS84") |> 
  mutate(pais = "BR") |>  
  group_by(pais) |> 
  summarise()

faixa_linha <- st_boundary(faixa)

faixa_linha

# Criar um buffer de 50 metros para dentro do polígono do Brasil na fronteira com esses países
buffer_fronteira <- st_buffer(paises_fronteira, dist = 100000) |> 
  st_union()

# Definir a faixa de fronteira interior do Brasil com base nesse buffer
faixa_interior <- st_union(buffer_fronteira, faixa_linha)

plot(st_geometry(faixa_interior))

plot(st_geometry(faixa_linha))
plot(st_geometry(buffer_fronteira), col="pink", add=T)

plot(st_geometry(faixa_linha), col="blue")
plot(st_geometry(buffer_fronteira), add=T, col="red")

# Verificar o CRS de cada objeto
st_crs(brasil) # Checar o CRS do Brasil
st_crs(sede_municipio) # Checar o CRS dos municípios
st_crs(faixa_linha) # Checar o CRS da faixa de fronteira
st_crs(buffer_fronteira) # Checar o CRS da faixa de fronteira

# Plotar o mapa do Brasil, os municípios e a faixa de fronteira juntos
plot(st_geometry(brasil), col = "gray", main = "Mapa Comparativo")
plot(st_geometry(faixa), col = "blue", add = TRUE)
plot(st_geometry(sede_municipio), col = "red", add = TRUE)

