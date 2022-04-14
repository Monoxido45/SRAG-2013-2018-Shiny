# importando e ajeitando os dados por aqui e salvando em rds
library(tidyverse)
library(chron)

#remotes::install_github("abjur/abjData")

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



dados_SRAG <- dados_SRAG |>
  mutate(CS_SEXO = recode_factor(CS_SEXO,
                                 "F"=" Feminino",
                                 "M"=" Masculino",
                                 "I"="9"),
         CS_GESTANT=recode_factor(CS_GESTANT,
                                  "1"=" 1° Trimestre",
                                  "2"=" 2° Trimestre",
                                  "3"=" 3° Trimestre",
                                  "4"="9",
                                  "5"="9",
                                  "6"="9"),
         
         CS_RACA=recode_factor(CS_RACA,
                                  "1"=" Branca",
                                  "2"=" Preta",
                                  "3"=" Amarela",
                                  "4"=" Parda",
                                  "5"=" Indígena",
                                  "9"="9"),
         
         CS_ESCOL_N=recode_factor(CS_ESCOL_N,
                               "0"=" Analfabeto",
                               "1"=" Fundamental",
                               "2"=" Média",
                               "3"=" Superior",
                               "9"="9",
                               "10"="9"),
         
         VACINA=recode_factor(VACINA,
                              "1"=" Vacinado",
                              "2"=" Não Vacinado"),
         
         FEBRE=recode_factor(FEBRE,
                             "1"=" Com Febre",
                             "2"=" Sem Febre"),
         
         TOSSE=recode_factor(TOSSE,
                             "1"=" Com Febre",
                             "2"=" Sem Febre"),
         
         DISPNEIA=recode_factor(DISPNEIA,
                             "1"=" Apresentou Dispineia",
                             "2"=" Não Apresentou Dispineia"),
         
         GARGANTA=recode_factor(GARGANTA,
                                "1"=" Dor de Garganta",
                                "2"=" Sem dor de Gargante"),
         
         MIALGIA=recode_factor(MIALGIA,
                               "1"=" Dor Muscular",
                               "2"=" Sem Dor Muscular"),
         
         OUTRO_SIN=recode_factor(OUTRO_SIN,
                                 "1"=" Com Outros Sintomas",
                                 "2"=" Sem Outros Sintomas"),
         
         CARDIOPATI=recode_factor(CARDIOPATI,
                                  "1"=" Com Cardiopatia Crônica",
                                  "2"=" Sem Cardiopatia Crônica"),
         
         PNEUMOPATI=recode_factor(CARDIOPATI,
                                  "1"=" Com Pneumopatia Crônica",
                                  "2"=" Sem Pneumopatia Crônica"),
         
         RENAL=recode_factor(RENAL,
                             "1"=" Com Doença Renal",
                             "2"=" Sem Doença Renal"),
         
         IMUNODEPRE=recode_factor(IMUNODEPRE,
                             "1"=" Supressão do sistema imunológico",
                             "2"=" Sistema imunológico normal"),
         
         IMUNODEPRE=recode_factor(IMUNODEPRE,
                                  "1"=" Supressão do sistema imunológico",
                                  "2"=" Sistema imunológico normal"),
         
         METABOLICA=recode_factor(METABOLICA,
                                  "1"=" Com Doença metabólica",
                                  "2"=" Sem Doença Metabólica"),
         
         OUT_MORBI=recode_factor(OUT_MORBI,
                                  "1"=" Possui morbidades",
                                  "2"=" Sem morbidades"))



### Para rodar o app_teste1.R
# dados_SRAG <- dados_SRAG |>
#   mutate(across(c(CLASSI_FIN,CRITERIO,EVOLUCAO), troca_NA),
#          across(c(CLASSI_FIN,CRITERIO,EVOLUCAO), as.character))

dados_SRAG <- dados_SRAG |>
  mutate(Tempo_Surv = chron(dates=DT_OBITO,
                             format = c(dates="d/m/y")) -
  chron(dates=DT_SIN_PRI,
        format = c(dates="d/m/y")), 
  Tempo_Surv = as.numeric(ifelse(Tempo_Surv<1 ,NA,
                                 Tempo_Surv)))


# dando join para extrair nome de municipios, UF e mesorregiao
dados_SRAG <- dados_SRAG |> left_join(ibge_muni |> select(Nome_UF,
                                                        "Região Geográfica Intermediária",
                                                        "Código Município Completo",
                                                        "Nome_Município") |>
                                      rename("ID_MUNICIP" = "Código Município Completo") |>
                                      mutate(ID_MUNICIP = ID_MUNICIP |> as.character() |> 
                                               substring(1, 6) |> as.numeric()),
                                      by = "ID_MUNICIP") |>
  left_join(abjData::pnud_min |> filter(ano == "2010") |>
              rename(ID_MUNICIP = muni_id) |>
              mutate(ID_MUNICIP = ID_MUNICIP |> as.character() |> 
                       substring(1, 6) |> as.numeric()) |>
              select(ID_MUNICIP, regiao_nm, pop, lat, lon),
            by = "ID_MUNICIP") |>
  mutate(regiao_nm = case_when(regiao_nm == "Norte" ~ 1,
                   regiao_nm == "Nordeste" ~ 2,
                   regiao_nm == "Sudeste" ~ 3,
                   regiao_nm == "Sul" ~ 4,
                   regiao_nm == "Centro-Oeste" ~ 5)) |>
  rename(MesoRegion = "Região Geográfica Intermediária",
         State = SG_UF_NOT,
         Region = regiao_nm)


dados_SRAG |> write_rds("analise/dados_SRAG.rds")

# obtendo estatisticas associadas as pop em 2010
estat_muni_comp <- abjData::pnud_min |> filter(ano == "2010") |>
  select(uf_sigla, regiao_nm, pop, muni_id) |>
  mutate(muni_id = muni_id |> as.numeric(),
        regiao_nm = case_when(regiao_nm == "Norte" ~ 1,
                               regiao_nm == "Nordeste" ~ 2,
                               regiao_nm == "Sudeste" ~ 3,
                               regiao_nm == "Sul" ~ 4,
                               regiao_nm == "Centro-Oeste" ~ 5)) |>
left_join(ibge_muni |>  select(UF,
                               "Região Geográfica Intermediária",
                               "Código Município Completo",
                               "Nome_Município") |>
            rename("muni_id" = "Código Município Completo"),
          by = "muni_id") |>
  rename(MesoRegion = "Região Geográfica Intermediária",
         State = UF,
         Region = regiao_nm)

# calculando pops
pop_reg <- estat_muni_comp |> 
  group_by(Region) |>
  summarise(pop = sum(pop))

# salvando
pop_reg |> write_rds("analise/pop_Region.rds")

# calculando para os estados
pop_uf <- estat_muni_comp |> 
  group_by(State) |>
  summarise(pop = sum(pop))
pop_uf |> write_rds("analise/pop_State.rds")

# calculando para as macorregios
pop_meso <- estat_muni_comp |> 
  group_by(MesoRegion) |>
  summarise(pop = sum(pop))
pop_meso |> write_rds("analise/pop_MesoRegion.rds")


