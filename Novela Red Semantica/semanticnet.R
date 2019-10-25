#install.packages("tidytext")
#install.packages("tidyverse")
#install.packages("tm")


library(tidytext)
library(tidyverse)
library(tm)
library(igraph)
library(ggraph)

#download.file(url = "https://raw.githubusercontent.com/LexRuso/ujcm/master/55563-0.txt", destfile = "55563-0.txt")

read_lines("novela.txt") %>%
  head(15)

file.show("novela.txt")

novela <-
  read_lines("novela.txt", n_max = 10612) %>%
  map(trimws) %>%
  ifelse(. == "", "_salto_", .) %>%
  paste0(., collapse = " ") %>%
  strsplit(split = "_salto_") %>%
  map(trimws) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  tbl_df() %>%
  {
    names(.) <- "texto"
    .
  }

leer_texto <- function(archivo, inicio, final) {
  read_lines(archivo, skip = inicio, n_max = (final - inicio)) %>%
    map_chr(trimws)
}

crear_parrafos <- function(texto) {
  texto %>%
    map(trimws) %>%
    ifelse(. == "", "_salto_", .) %>%
    paste0(., collapse = " ") %>%
    strsplit(split = "_salto_") %>%
    map(trimws) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    tbl_df() %>%
    {
      names(.) <- "texto"
      .
    }
}

novela <-
  novela %>%
  filter(!texto %in% c(" ", "")) %>%
  mutate_all(trimws)

borrar_vacios <- function(libro_vacios) {
  libro_vacios %>%
    filter(!texto %in% c(" ", "")) %>%
    mutate_all(trimws)
}

novela %>%
  filter(grepl("^[[:upper:]]+$", texto))

novela <-
  novela %>%
  mutate(capitulo = ifelse(grepl("^[[:upper:]]+$", texto), texto, NA)) %>%
  fill(capitulo) %>%
  filter(texto != capitulo)

encontrar_capitulos <- function(libro) {
  libro %>%
    mutate(capitulo = ifelse(grepl("^[[:upper:]]+$", texto), texto, NA)) %>%
    fill(capitulo) %>%
    filter(texto != capitulo)
}

novela_bigrama <-
  novela %>%
  unnest_tokens(input = "texto", output = "bigrama", token = "ngrams", n = 2)

novela_bigrama

novela_bigrama %>%
  count(bigrama, sort = T)

stopwords(kind = "es") %>% head(15)

novela_bigrama <-
  novela_bigrama %>%
  separate(bigrama, into = c("uno", "dos"), sep = " ") %>%
  filter(!uno %in% stopwords(kind = "es")) %>%
  filter(!dos %in% stopwords(kind = "es")) %>%
  count(uno, dos)

generar_bigramas <- function(libro_parrafo) {
  libro_parrafo %>%
    unnest_tokens(input = "texto", output = "bigrama", token = "ngrams", n = 2) %>%
    separate(bigrama, into = c("uno", "dos"), sep = " ") %>%
    filter(!uno %in% stopwords("es")) %>%
    filter(!dos %in% stopwords("es")) %>%
    count(uno, dos)
}

set.seed(175)
novela_bigrama %>%
  filter(n >= 5) %>%
  graph_from_data_frame() %>%
  ggraph() +
  geom_edge_link(arrow = arrow(type = "closed", length = unit(.075, "inches"))) +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

novela_bigrama <-
  novela_bigrama %>%
  filter(!uno %in% c("á", "ó")) %>%
  filter(!dos %in% c("á", "ó"))

set.seed(175)
novela_bigrama %>%
  filter(n >= 5) %>%
  graph_from_data_frame() %>%
  ggraph() +
  geom_edge_link(arrow = arrow(type = "closed", length = unit(.075, "inches"))) +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

crear_red <- function(libro_bigrama, umbral = 5) {
  libro_bigrama %>%
    filter(n > umbral) %>%
    graph_from_data_frame() %>%
    ggraph() +
    geom_edge_link(aes(edge_alpha = n),
                   arrow = arrow(type = "closed", length = unit(.1, "inches"))) +
    geom_node_point(size = 2, color = "#9966dd") +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

generar_bigramas <- function(libro_parrafo) {
  libro_parrafo %>%
    unnest_tokens(input = "texto", output = "bigrama", token = "ngrams", n = 2) %>%
    separate(bigrama, into = c("uno", "dos"), sep = " ") %>%
    filter(!uno %in% c(stopwords("es"), "á", "ó")) %>%
    filter(!dos %in% c(stopwords("es"), "á", "ó")) %>%
    count(uno, dos)
}

red_texto <- function(archivo, inicio, final, umbral = 5) {
  leer_texto(archivo, inicio = inicio, final = final)  %>%
    crear_parrafos() %>%
    encontrar_capitulos() %>%
    borrar_vacios() %>%
    generar_bigramas() %>%
    crear_red(umbral = umbral)
}

set.seed(175)
red_texto(archivo = "novela.txt", inicio = 153, final = 10612, umbral = 5)