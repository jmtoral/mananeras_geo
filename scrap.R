library(tidyverse)
library(rvest)

url <- paste0("https://www.gob.mx/presidencia/es/archivo/articulos?filter_id=5169&filter_origin=archive&idiom=es&page=", 1:151)

archivo <- lapply(url, function(i){
  i %>% 
    read_html() %>% 
    html_nodes("a") %>% 
    html_attr('href') %>% 
    str_subset("archivo", negate =T) %>% 
    str_remove_all("\\\"") %>% 
    str_remove_all("\\\\") %>% 
    str_c("https://www.gob.mx",.)
}) %>% 
  unlist()


lugar <- lapply(archivo[2:1357], function(i){
  i %>% 
    read_html() %>% 
    html_nodes(".pull-left h2") %>% 
    html_text()
})%>% 
  unlist()


fecha <- lapply(archivo[2:1357], function(i){
  i %>% 
    read_html() %>% 
    html_nodes("dd~ dd") %>% 
    html_text()
})%>% 
  unlist()


geo <- tibble(fecha, lugar)

geo %>% 
  write.csv("output/mananeras_geo.csv", row.names = F)



geo %>% 
  rowid_to_column("id") %>% 
  filter(!str_detect(lugar, "Conferencia encabezada por")) %>% 
  filter(!str_detect(lugar, "Olga|Adán|COVID|Zaragoza")) %>% 
  separate(lugar, c("tirar", "lugar"), 
           sep = "desde| en |, en$|,en$|en su visita a|realizado en")->x

rev <- x %>% filter(is.na(lugar))

archivo[2:1357] %>% 
  as.data.frame() %>% 
  rowid_to_column("id") %>% 
  filter(id %in% rev$id)

faltantes <- tibble(
  id = rev$id,
  lugar_faltantes = c("Ciudad de México", "Nueva York, Estados Unidos",
                      "Nueva York, Estados Unidos", "Vícam Pueblo Territorio Yaqui, Sonora",
                      "Ciudad de México", "Ciudad de México","Virtual","Virtual","Virtual",
                      "Ciudad de México","Ciudad de México","Ciudad de México","Ciudad de México",
                      "Ciudad de México","Oaxaca","Ciudad de México","Benemérito de las Américas, Chiapas",
                      "Ciudad de México","Tlacolula, Oaxaca", "Miahuatlán de Porfirio Díaz, Oaxaca",
                      "San Ildefonso Villa Alta, Oaxaca","Huautla de Jiménez, Oaxaca", "Ciudad de México",
                      "Santa Lucía del Camino, Oaxaca","Concepción del Oro, Zacatecas", "Villanueva, Zacatecas",
                      "Río Grande, Zacatecas","Durango, Durango","Guadalupe Victoria, Durango",
                      "Rodeo, Durango","Rural de Tuxpan, Michoacán","Huetamo de Núñez, Miachoacán",
                      "Metepec, Hidalgo",
                      "Xicotepec de Juárez, Puebla","Zongolica, Veracruz","Veracruz","Jaltipan de Morelos, Veracruz",
                      "Matías Romero Avendaño, Oaxaca","Ocozocoautla de Espinosa, Chiapas",
                      "Bochil, Chiapas", "Chicontepec, Veracruz","Huejutla de Reyes, Hidalgo",
                      "Tamazunchale, San Luis Potosí", "Tamuín, San Luis Potosí","Cerritos, San Luis Potosí",
                      "Sabinas, Coahuila","Minatitlán, Veracruz", "Ciudad de México", "Ciudad de México", "Ciudad de México"
  )
)

final <-  x %>% 
  left_join(faltantes, by = "id") %>% 
  mutate(lugar = case_when(
    is.na(lugar) ~ lugar_faltantes,
    TRUE ~ lugar
  )) %>% 
  select(-lugar_faltantes) %>% 
  mutate(lugar = str_squish(lugar),
         lugar = str_remove(lugar, "^el |^la "))



nodf <- final %>% 
  filter(!str_detect(lugar, "Antropología|CDMX|Palacio Nacional|Bellas Artes|Virtual|Estados Unidos|Washington|evento|Adminis"),
         !str_detect(lugar, "Heroico Colegio Militar|Ayuntamiento|Monumento a la Revolución|Zócalo|Técnica"),
         !str_detect(lugar, "Enfermería|Chapultepec|Teatro")) %>%
  mutate(entidad = case_when(
    str_detect(lugar, ",") ~ str_remove(lugar, "[\\s\\S]*[,] "),
    TRUE ~ lugar
  )) %>% 
  mutate(entidad = case_when(
    str_detect(lugar, "Campeche") ~ "Campeche",
    str_detect(lugar, "Tehuantepec") ~ "Oaxaca",
    str_detect(lugar, "Michoacán|Miachoacán") ~ "Michoacán",
    str_detect(lugar, "Mexe|Tenango de Doria") ~ "Hidalgo",
    str_detect(lugar, "Tianguis") ~ "Guerrero",
    str_detect(lugar, "Tlapa de Comonfort") ~ "Guerrero",
    str_detect(lugar, "tramos") ~ "Nayarit",
    str_detect(lugar, "Nuxiño|Oaxac") ~ "Oaxaca",
    str_detect(lugar, "Monte") ~ "Tlaxcala",
    str_detect(lugar, "Roo") ~ "Quintana Roo",
    str_detect(lugar, "Pahuatlán") ~ "Puebla",
    str_detect(lugar, "Edomex") ~ "Estado de México",
    TRUE ~ entidad
  )) %>% 
  mutate(entidad = str_remove(entidad, "\\.")) %>% 
  filter(entidad != "Ciudad de México") %>% 
  mutate(control = nchar(entidad)) %>% 
  arrange(-control)



nodf %>% count(entidad)->y
