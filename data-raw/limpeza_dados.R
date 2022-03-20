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
  plyr::ldply() 


dados_SRAG <- dados_SRAG |>
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

dados_SRAG |> write_rds("analise/dados_SRAG.rds")