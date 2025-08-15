# DESCRIPTION 

Aqui serão colocadas as bases de dados organizadas das orquídeas.

"db_caat.rda" contempla as famílias das orquídeas ocorrentes na Caatinga, com a ocorrencia de cada espécies por localização, portanto, as espécies podem se repetir mais de uma vez. Desta base de dados foram retiradas as espécies que apresentavam genero e specie iguais, retiramos os NAs, aquelas com species = 'sp' e nomes inexistentes.

"db_caat_principal.rda" seria a base de dados da flora e funga do brasil filtrada com a base de dados de "db_caat.rda" sem dados duplicados, ou seja, dados únicos das espécies ocorrentes da Caatinga.

"db_completa.rda" é a base de dados com as espécies de Vivi sem repetição filtradas da base de dados total do Reflora Brasil e com todas as informações.

"db_completa_com_ID_synm.rda" é a base de dados completa, mas com todos os sinonimos e IDs por espécie  ("db_total.xlsx").

"db_webscrp_id_synm.rda" é a base de dados obtida por webscraping cruzada com a base de dados original ("wdb.xlsx").
