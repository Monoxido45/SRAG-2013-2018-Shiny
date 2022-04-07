# importando e ajeitando os dados por aqui e salvando em rds
library(tidyverse)

# urls para baixar
urls <- map_chr(13:18, 
                function(x){paste0("https://d26692udehoye.cloudfront.net/SRAG/2013-2018/INFLUD", 
                                   x,
                                   ".csv")})

# percorrendo e baixando dados do vetor de URLS
dados_SRAG <- urls |>
  map(read_delim, delim = ";", escape_double = FALSE, trim_ws = TRUE) |>
  plyr::ldply() |>
  # jogando fora algumas variaveis
  # tirando variaveis com mts NA's
  select(-c(SRAG2012FINAL,
            SRAG2013FINAL, 
            SRAG2014FINAL, 
            SRAG2015FINAL, 
            SRAG2017FINAL, 
            SRAG2018FINAL,
            DT_UT_DOSE),
         -ID_OCUPA_N,
         -SG_UF,
         -ID_MN_RESI,
         -DT_DIGITA,
         -OBES_IMC, 
         -OUT_ANTIV) |>
  janitor::remove_empty(which = "cols") |>
  purrr::discard(~sum(is.na(.x))/length(.x)* 100 >= 80)

# cruzando com as bases do IBGE para obter o nome dos municipios e plotar tudo no leaflet
# base do IBGE
ibge_muni <- "data-raw/DTB_2021/RELATORIO_DTB_BRASIL_MUNICIPIO.ods" |>
  readODS::read_ods()

# substituindo NA por 9 e outras coisas
troca_NA <- function(doenca){
  ifelse(is.na(doenca), 9, doenca)
}

dados_SRAG <- dados_SRAG |>
  mutate(across(c(CS_GESTANT:OUTRO_SIN, CARDIOPATI:OUT_MORBI), troca_NA),
         across(c(CS_GESTANT:OUTRO_SIN, CARDIOPATI:OUT_MORBI), as.character))


### Para rodar o app_teste1.R
dados_SRAG <- dados_SRAG |>
  mutate(across(c(CLASSI_FIN,CRITERIO,EVOLUCAO), troca_NA),
         across(c(CLASSI_FIN,CRITERIO,EVOLUCAO), as.character))


# dando join para extrair nome de municipios, UF e mesorregiao
dados_SRAG <- dados_SRAG |> left_join(ibge_muni |> select(Nome_UF,"Mesorregião Geográfica",
                                                        "Nome_Mesorregião",
                                                        "Região Geográfica Imediata",
                                                        "Nome Região Geográfica Imediata",
                                                        "Código Município Completo",
                                                        "Nome_Município") |>
                                      rename("ID_MUNICIP" = "Código Município Completo") |>
                                      mutate(ID_MUNICIP = ID_MUNICIP |> as.character() |> 
                                               substring(1, 6) |> as.numeric()),
                                      by = "ID_MUNICIP")


dados_SRAG |> write_rds("analise/dados_SRAG.rds")


