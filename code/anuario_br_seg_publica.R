#### IMPORTANDO BASE DO Anuário Brasileiro de Segurança Pública ####
library("basedosdados")
# Defina o seu projeto no Google Cloud
set_billing_id("rapid-pact-400813")
# Para carregar o dado direto no R
query <- bdplyr("basedosdados.br_fbsp_absp.municipio")
absp <- bd_collect(query)

# Essa base não útil, uma vez que só posusi registros de crimes nas capitais dos estados