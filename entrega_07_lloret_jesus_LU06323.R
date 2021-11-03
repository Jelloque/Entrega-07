#- ENTREGA nº 07
#---- Pon tus datos de identificación ----------------------------
#- Apellidos: Lloret Quero
#- Nombre: Jesus
#- e-mail: jelloque@alumni.uv.es
#- NPA: LU06323
#-------------------------------------------------------------------------------
#- Para entregar tus tareas, graba esta plantilla en un script (archivo .R)
#- Evidentemente, incorpora (en los huecos que hay) las soluciones a las tareas
#- No te olvides de rellenar los datos de identificación
#-------------------------------------------------------------------------------
#- Solo has de subir al uploader el fichero .R (solo el fichero .R, sin comprimir)
#- Repito, no hay que subir el Rproject, hay que subir solo el fichero .R (sin comprimir)
#- El fichero .R  se ha de llamar entrega_05_perez_pedro_GG445566.R
#-------------------------------------------------------------------------------


#- Objetivo: tener información sobre como lleváis el curso y "examinaros" (un poco)

#- Ya arreglé los datos --------------------------------------------------------
#- datos de poblacion. Los retoco un poco ------------
# url_pob <- "https://github.com/perezp44/pjpv.datos.01/raw/master/data/pob_muni_1996_2020.rda"
# df_pob <- rio::import(url_pob)    #- datos de población municipal
# rm(url_pob)
# 
# df_pob <- df_pob %>% select(year, ine_muni, ine_muni.n, ine_prov, pob_total, pob_hombres, pob_mujeres, ine_prov.n, ine_ccaa, ine_ccaa.n, capital_prov, capital_ccaa)  
# df_pob <- df_pob %>% 
#   pivot_longer(cols = pob_total:pob_mujeres, names_to = "poblacion", values_to = "pob_values") %>% 
#   relocate(c(poblacion, pob_values), .after= ine_prov)
# rio::export(df_pob, here::here("datos", "df_pob_mun.rds"))

#- datos de bebes. Los retoco un poco ------------
#url_2 <- "https://github.com/perezp44/archivos_download/raw/master/df_partos_17_CV.rds"
# df_bebes <- readr::read_rds(here::here("datos", "bebes_ok_2016y2019.rds"))
# df_bebes <- df_bebes %>% 
#   select(year_parto, muni_inscrip, muni_inscrip.n, prov_inscrip, prov_inscrip.n, lugar_parto.f,
#          sexo_bebe, peso_bebe, parto_nn_semanas, 
#          parto_normal.f, parto_cesarea.f, parto_asistido.f,  
#          nn_nacidos_parto_con_o_sin_vida, nn_hijos_vivos_totales,
#          nacionalidad_esp_madre, pais_nacionalidad_madre.n, estudios_madre.f, ocupacion_madre.f, 
#          nacionalidad_esp_padre, pais_nacionalidad_padre.n, estudios_padre.f, ocupacion_padre.f, 
#          edad_madre, edad_madre.1, edad_padre, edad_padre.1, 
#          fecha_parto, fecha_parto_anterior, intervalo_intergenesico, intervalo_parto_anterior.1, 
#          nn_hijos_tot, nn_hijos_tot, 
#          id_parto, id_bebe_parto)
# str(df_bebes)
# rio::export(df_bebes, here::here("datos", "df_bebes_16y19.rds"))


#- cargamos los datos ----------------------------------------------------------
library(tidyverse)
df_pob <- rio::import(here::here("datos", "df_pob_mun.rds"))
df_bebes <- rio::import(here::here("datos", "df_bebes_16y19.rds"))


#- Pregunta 1: 
#- ¿Que 10 municipios tuvieron una mayor tasa de natalidad en 2019?
#- definimos la tasa de natalidad como: (nº de bebes nacidos/numero de mujeres en el municipio)*1000
#- Pistas: con df_bebes puedes calcular el nº de bebes en cada municipio y en df_pob está el número de mujeres en cada municipio, solo tienes que hacerlo y juntar las tablas y ....

#- nº de mujeres en cada municipio en 2019
df_pob_M <- df_pob %>%
  filter(year == "2019") %>%
  filter(poblacion == "pob_mujeres") %>%
  select(ine_muni.n, poblacion, pob_values) %>%
  tidyr::drop_na()
 

#- bebes totales nacidos en 2019 en cada municipio
df_bebe_T <- df_bebes %>% 
  filter(year_parto == "2019") %>%
  select(muni_inscrip.n) %>%
  group_by(muni_inscrip.n) %>%
  summarise(numero_nacimientos_pueblo = n()) %>%
  tidyr::drop_na()



#- salen NAs xq el INE no da los datos de los municipios de menos de 10.000 habitantes
#- quito los NAs
df_bebe_T <- df_bebe_T %>% ...   


#- hemos de juntar las 2 tablas

df <- left_join(x= df_bebe_T, y = df_pob_M, by=c("muni_inscrip.n" = "ine_muni.n") ) 
  

#- ahora ya calculamos la tasa de natalidad como: (nº de bebes nacidos/numero de mujeres en el municipio)*1000
dff <- df %>% 
  mutate(nn= (numero_nacimientos_pueblo/pob_values)*100 ) %>%
  arrange(desc(nn))

df <- df %>% 
  mutate(tasa_natalidad = (numero_nacimientos_pueblo / pob_values)*1000, .after = pob_values) %>% 
  arrange(desc(tasa_natalidad))





#SOLUCION


names(df_pob)
#- nº de mujeres en cada municipio en 2019
df_pob_M <- df_pob %>% 
  filter(year == 2019) %>% 
  filter(poblacion == "pob_mujeres") %>% 
  select(pob_values, ine_muni, ine_muni.n, ine_prov.n, capital_prov) 



#- bebes totales nacidos en 2019 en cada municipio
df_bebe_T <- df_bebes %>% 
  filter(year_parto == 2019) %>% 
  group_by(muni_inscrip, muni_inscrip.n, prov_inscrip.n) %>% 
  summarise(nn_bebes = n())  %>%     #- tb se puede con count()
  ungroup()



#- salen NAs xq el INE no da los datos de los municipios de menos de 10.000 habitantes
#- quito los NAs
df_bebe_T <- df_bebe_T %>% 
  filter(!is.na(muni_inscrip))


#- hemos de juntar las 2 tablas
df <- left_join(df_bebe_T, df_pob_M, by = c("muni_inscrip" = "ine_muni")) %>% 
  select(- muni_inscrip.n, - prov_inscrip.n)


#- ahora ya calculamos la tasa de natalidad como: (nº de bebes nacidos/numero de mujeres en el municipio)*1000
df <- df %>% 
  mutate(tasa_natalidad = (nn_bebes / pob_values)*1000, .after = pob_values) %>% 
  arrange(desc(tasa_natalidad))



































#- Pregunta 2: 
#- ¿Que 10 provincias tuvieron una mayor tasa de natalidad en 2019?
#- definimos la tasa de natalidad como: (nº de bebes nacidos/numero de mujeres en el municipio)*1000


#- nº de mujeres en cada PROVINCIA en 2019


#- bebes totales nacidos en 2019 en cada PROVINCIA



#- Ya no salen NAs, xq calculamos a nivel provincial, no municipal


#- hemos de juntar las 2 tablas


#- ahora ya calculamos la tasa de natalidad como: (nº de bebes nacidos/numero de mujeres en el municipio)*1000