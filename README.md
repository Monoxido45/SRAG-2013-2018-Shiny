
O aplicativo pode ser encontrado por meio do link: https://vinicius-hideki.shinyapps.io/SRAG_2013_2018/



## Arquivos

### analise (pasta)

  - app.R
    - Contém códigos da ui e server para executar o aplicativo shiny 

### data-raw (pasta)

  - limpeza_dados.R 
    - Código em R usado para baixar e limpar a base de dados SRAG 2013 a 2018.
    - Criar .rds para facilitar a rápida execução do aplicativo shiny
    - Localmente, deve-se executar esses arquivo primeiro antes de executar o app.R 
  - DTB_2021
    - Pasta com informações (bases de dados em .ods) do IBGE 
    - Cruzamento com informações do SRAG para facilitar a criação dos mapas

### Projeto.Rproj

  - Usado para abrir o projeto no RStudio localmente.

### .gitignore 

  - Aponta os arquivos criados pelo código na pasta data-raw

