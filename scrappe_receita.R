library(RSelenium)
library(tidyverse)
library(scales)
library(rvest)
library(httr)
library(lubridate)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Initial variables ----

base_url <- "http://normas.receita.fazenda.gov.br/sijut2consulta/consulta.action?facetsExistentes=&orgaosSelecionados=&tiposAtosSelecionados=&lblTiposAtosSelecionados=&ordemColuna=&ordemDirecao=&tipoConsulta=formulario&tipoAtoFacet=&siglaOrgaoFacet=&anoAtoFacet=&termoBusca=&numero_ato=&tipoData=2&dt_inicio=&dt_fim=&ano_ato=1999&optOrdem=Publicacao_DESC"

years <- 1980:2020

remDr <- remoteDriver(port = 4445L, browser = "chrome")

css <- list(
  tipo = ".linhaResultados td:nth-child(1)",
  num = ".linhaResultados td:nth-child(2)",
  orgao = ".linhaResultados td:nth-child(3)",
  data = ".linhaResultados td:nth-child(4)",
  ementa = ".linhaResultados td:nth-child(5)",
  regs = ".total-regs-encontrados",
  proxima_pag = "#tabelaAtos #btnProximaPagina2",
  titulo = ".tituloAto",
  pub = ".tituloPublicacao",
  texto = "#divTexto",
  assinatura = ".fecho",
  href_conteudo = "#divConteudo a",
  espaco_modificados = ".novos",
  espaco_modificaram = ".antigos",
  espaco_este_ato = ".atual",
  ato_nunca_alterado = ".ana",
  ato_nao_vigente = ".anv",
  ato_ja_alterado = ".aja"
)

# Functions ----

fun <- list.files('/Users/lucasthevenard/Documents/prog/Rfunctions', '\\.rds$', full.names = TRUE) %>%
  lapply(readRDS)
names(fun) <- list.files('/Users/lucasthevenard/Documents/prog/Rfunctions', '\\.rds$') %>%
  str_remove("\\.rds$")

extract_using_css <- function(page, css) {
  vector <- page %>%
    html_nodes(css) %>%
    html_text() %>%
    str_squish()
  return(vector)
}

test_css <- function(page, css) {
  test <- page %>%
    html_nodes(css) %>%
    html_text() %>%
    is_empty()
  return(test)
}

extract_links <- function(page, css) {
  vector <- page %>%
    html_nodes(css) %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    str_squish() %>%
    fun$invert_paste("http://normas.receita.fazenda.gov.br/sijut2consulta/")
  return(vector)
}

change_year <- function(year, url=base_url) {
  new_url <- base_url %>% 
    str_replace("ano_ato=\\d\\d\\d\\d", paste0("ano_ato=", year))
  return(new_url)
}

calc_page_results <- function(page) {
  regs <- extract_using_css(page, css$regs)
  items <- str_extract(regs, "\\d+") %>%
    as.numeric()
  pages <- ceiling(items / 100)
  results <- c(items, pages)
  return(results)
}

iterate_results <- function(year, tryagain = FALSE) {
  page <- fun$get_html(remDr)
  num_results <- calc_page_results(page)
  
  page_list <- list()
  
  page_list[[1]] <- list(
    year = year,
    results = num_results[[1]]
  )
  page_list[[2]] <- vector('list', num_results[[2]])
  page_list[[2]][[1]] <- page
  if (num_results[[2]] > 1) {
    for (j in 2:num_results[[2]]) {
      error <- try(fun$find_by_css_and_click(selector = css$proxima_pag))
      if (tryagain & typeof(error) == "character") {
        break()
      }
      fun$szz(8, 10, paste0("Downloading sub_page ", j))
      page_list[[2]][[j]] <- fun$get_html(remDr)
    }
  }
  return(page_list)
}

fix2019 <- function(page_list) {
  pages <- page_list[[40]][[2]]
  url <- "http://normas.receita.fazenda.gov.br/sijut2consulta/consulta.action?facetsExistentes=&orgaosSelecionados=&tiposAtosSelecionados=&lblTiposAtosSelecionados=&ordemColuna=&ordemDirecao=&tipoConsulta=formulario&tipoAtoFacet=&siglaOrgaoFacet=&anoAtoFacet=&termoBusca=&numero_ato=&tipoData=2&dt_inicio=&dt_fim=&ano_ato=2019&p=56&optOrdem=Publicacao_DESC&p=56"
  for (i in seq_along(pages)) {
    tries <- 0
    time_1 <- 8
    time_2 <- 10
    while (is.null(pages[[i]]) & tries < 10) {
      new_url <- url %>%
        str_replace_all("p=\\d+", paste0("p=", i))
      error <- try(remDr$navigate(new_url))
      if (is.null(error)) {
        fun$szz(time_1, time_2, paste0("Downloading subpage", i))
        pages[[i]] <- fun$get_html(remDr)
      }
      time_1 <- time_1 + 2
      time_2 <- time_2 + 2
      tries <- tries + 1
    }
    len_items <- pages[[i]] %>%
      extract_using_css(css$tipo) %>%
      length()
    tries <- 0
    time_1 <- 8
    time_2 <- 10
    while (len_items == 0 & tries < 10) {
      new_url <- url %>%
        str_replace_all("p=\\d+", paste0("p=", i))
      error <- try(remDr$navigate(new_url))
      if (is.null(error)) {
        fun$szz(time_1, time_2, paste0("Downloading subpage", i))
        pages[[i]] <- fun$get_html(remDr)
        len_items <- pages[[i]] %>%
          extract_using_css(css$tipo) %>%
          length()
      }
      time_1 <- time_1 + 2
      time_2 <- time_2 + 2
      tries <- tries + 1
    }
  }
  page_list[[40]][[2]] <- pages
  return(page_list)
}

scrape_receita <- function(base_url, years) {
  page_list <- vector('list', length(years))
  for (i in seq_along(years)) {
    url <- change_year(years[[i]])
    remDr$navigate(url)
    fun$szz(8, 10, paste0("Accessing data from master page ", i, ", year: ", years[[i]], "\nDownloading sub_page 1"))
    page_list[[i]] <- iterate_results(years[[i]])
  }
  return(page_list)
}

extract_data <- function() {
  
  character_vector <- vector("character", num_pages)
  variables <- list(
    tipo = character_vector,
    num = character_vector,
    orgao = character_vector,
    data = character_vector,
    ementa = character_vector
  )
}

extract_table_data <- function(page_list) {
  norms <- list(
    year = vector('character', length(years)),
    total = vector('numeric', length(years)),
    year_table = vector('list', length(years))
  )
  for (i in seq_along(page_list)) {
    year_list <- page_list[[i]]
    norms$year[[i]] <- year_list[[1]]$year
    results <- year_list[[1]]$results
    norms$total[[i]] <- results
    pages <- year_list[[2]]
    year_table <- tibble()
    for (j in seq_along(pages)) {
      page <- pages[[j]]
      tipo <- page %>%
        extract_using_css(css$tipo)
      num <- page %>%
        extract_using_css(css$num)
      orgao <- page %>%
        extract_using_css(css$orgao)
      data <- page %>%
        extract_using_css(css$data)
      ementa <- page %>%
        extract_using_css(css$ementa)
      link <- page %>%
        extract_links(css$tipo)
      add_lines <- tibble(
        tipo = tipo,
        num = num,
        orgao = orgao,
        data = data,
        ementa = ementa,
        link = link
      )
      year_table <- bind_rows(
        year_table, add_lines
      )
    }
    norms$year_table[[i]] <- year_table
  }
  return(norms)
}

generate_relational_link <- function(link) {
  base_url <- "http://normas.receita.fazenda.gov.br/sijut2consulta/link.action?naoPublicado=&idAto=39251&visao=relacional"
  id_ato <- link %>%
    str_extract("Ato=\\d+")
  url <- base_url %>%
    str_replace("Ato=\\d+", id_ato)
  return(url)
}

empty_relational <- function(page) {
  page %>%
    html_nodes(".atual") %>%
    is_empty()
}

count_acts_by_css <- function(page, section_css) {
  page %>%
    html_nodes(section_css) %>%
    html_nodes("div") %>%
    html_attr("class") %>%
    .[!is.na(.)] %>%
    .[str_detect(., "^ato ")] %>%
    length()
}

classify_this_act <- function(page, act_css=css$espaco_este_ato) {
  page %>%
    html_nodes(act_css) %>%
    html_nodes("div") %>%
    html_attr("class") %>%
    .[!is.na(.)] %>%
    .[str_detect(., "^ato ")] %>%
    str_remove("^ato") %>%
    trimws() %>%
    paste(collapse="; ")
}


# Execution ----
## Setting up ----

remDr$open(silent=TRUE)

page_list <- vector('list', length(years))

missing_years <- ""

for (i in seq_along(years)) {
  year <- years[[i]]
  if (!(year %in%  missing_years)) {
    next()
  }
  url <- change_year(year)
  remDr$navigate(url)
  fun$szz(10, 12, paste0("Accessing data from master page ", i, ", year: ", year, "\nDownloading sub_page 1"))
  page_list[[i]] <- try(iterate_results(year, tryagain = TRUE))
}

# Fixing downloads for general pages ----

# for (i in seq_along(years)) {
#   year <- years[[i]]
#   counter$year[[i]] <- year
#   if (typeof(page_list[[i]]) == "character") {
#     counter$n[[i]] <- NA
#     next()
#   } else {
#     counter$n[[i]] <- page_list[[i]][[1]]$results
#   }
#   base_dir <- paste0("./pages/", year)
#   if (!dir.exists(base_dir)) {
#     dir.create(base_dir)
#   } else {
#     next()
#   }
#   pages <- page_list[[i]][[2]]
#   for (j in seq_along(pages)) {
#     full_path <- paste0(base_dir, "/", j, ".html")
#     write_html(pages[[j]], full_path)
#   }
# }
# 
# missing_years <- as_tibble(counter) %>%
#   filter(is.na(n)) %>%
#   .$year
# 
# page_list <- fix2019(page_list)

## Consolidating general data ----

table_data <- extract_table_data(page_list)

totais <- tibble(
  year = table_data$year,
  total = table_data$total
)

write_csv(totais, "totais.csv")

normas <- bind_rows(table_data$year_table)

normas <- normas %>%
  mutate(id = row_number()) %>%
  relocate(id, .before = tipo)

write_csv(normas, "normas_receita.csv")
saveRDS(normas, "normas_receita.rds")

for (i in seq_along(page_list)) {
  year <- page_list[[i]][[1]]$year
  pages <- page_list[[i]][[2]]
  for (j in seq_along(pages)) {
    write_html(
      pages[[j]],
      paste0("./norm_pages/", i, "_", year, "_", j, ".html")
    )
  }
}

# Downloading internal data for selection ----

normas_select <- normas %>%
  filter(str_detect(str_to_lower(tipo), "portaria|\\bnorma|resolução"))

scope <- nrow(normas_select)

internal_pages <- vector('list', scope)
errors <- vector('numeric', scope)

for (i in 1:scope) {
  url <- normas_select$link[[i]]
  error <- try(remDr$navigate(url))
  fun$szz(5, 6, paste0("Downloading page: ", i , " / ", scope))
  if (is.null(error)) {
    page <- fun$get_html(remDr)
    internal_pages[[i]] <- page
    id <- normas_select$id[[i]]
    write_html(page, paste0("./internal_pages/", id, ".html"))
  } else {
    errors[[i]] <- 1
  }
}

empty_pages <- NULL

for (i in seq_along(internal_pages)) {
  if (is.null(internal_pages[[i]])) {
    print(i)
    empty_pages <- c(empty_pages, i)
  }
}

# Extracting data from internal pages ----

normas_select <- normas_select %>%
  mutate(titulo = vector('character', scope),
         dou = vector('character', scope),
         assinatura = vector('character', scope),
         texto = vector('character', scope))

for (i in seq_along(internal_pages)) {
  print(i)
  page <- internal_pages[[i]]
  if (test_css(page, css$titulo)) {
    normas_select$titulo[[i]] <- NA
  } else {
    normas_select$titulo[[i]] <- page %>%
      extract_using_css(css$titulo) %>%
      paste(collapse = "\n")
  }
  if (test_css(page, css$pub)) {
    normas_select$dou[[i]] <- NA
  } else {
    normas_select$dou[[i]] <- page %>%
      extract_using_css(css$pub) %>%
      paste(collapse = "\n")
  }
  if (test_css(page, css$assinatura)) {
    normas_select$assinatura[[i]] <- NA
  } else {
    normas_select$assinatura[[i]] <- page %>%
      extract_using_css(css$assinatura) %>%
      paste(collapse = "\n")
  }
  if (test_css(page, css$texto)) {
    normas_select$texto[[i]] <- NA
  } else {
    normas_select$texto[[i]] <- page %>%
      extract_using_css(css$texto)%>%
      paste(collapse = "\n")
  }
}

saveRDS(normas_select, "select_database.rds")

# Fixing downloads for internal pages ----

for (i in seq_along(internal_pages)) {
  if (is.na(normas_select$texto[[i]])) {
    url <- normas_select$link[[i]]
    remDr$navigate(url)
    fun$szz(8, 10, i)
    page <- fun$get_html(remDr)
    internal_pages[[i]] <- page
    id <- normas_select$id[[i]]
    write_html(page, paste0("./internal_pages/", id, ".html"))
  }
}

for (i in seq_along(internal_pages)) {
  print(i)
  page <- internal_pages[[i]]
  if (is.na(normas_select$texto[[i]])) {
    if (test_css(page, css$titulo)) {
      normas_select$titulo[[i]] <- NA
    } else {
      normas_select$titulo[[i]] <- page %>%
        extract_using_css(css$titulo) %>%
        paste(collapse = "\n")
    }
    if (test_css(page, css$pub)) {
      normas_select$dou[[i]] <- NA
    } else {
      normas_select$dou[[i]] <- page %>%
        extract_using_css(css$pub) %>%
        paste(collapse = "\n")
    }
    if (test_css(page, css$assinatura)) {
      normas_select$assinatura[[i]] <- NA
    } else {
      normas_select$assinatura[[i]] <- page %>%
        extract_using_css(css$assinatura) %>%
        paste(collapse = "\n")
    }
    if (test_css(page, css$texto)) {
      normas_select$texto[[i]] <- NA
    } else {
      normas_select$texto[[i]] <- page %>%
        extract_using_css(css$texto)%>%
        paste(collapse = "\n")
    }
  }
}

saveRDS(normas_select, "select_database.rds")

# Identifying which internal pages have relational data ----

tem_relacional <- vector('logical', length(internal_pages))

for (i in seq_along(internal_pages)) {
  page <- internal_pages[[i]]
  tem_relacional[[i]] <- page %>%
    html_nodes(css$href_conteudo) %>%
    html_text() %>%
    paste(collapse="\n") %>%
    str_squish() %>%
    str_detect("[Rr]elacional")
}

relacional_data <- tibble(
  id = normas_select$id,
  tem_relacional = tem_relacional
)

write_csv(relacional_data, "relacional_data.csv")

# Downloading relational pages ----

scope <- sum(tem_relacional)
relacional_pages <- vector('list', scope)
idx <- 9205

for (i in seq_along(tem_relacional)) {
  if (tem_relacional[[i]] & normas_select$id[[i]] >= 30537) {
    link <- normas_select$link[[i]] %>%
      generate_relational_link()
    relacional_pages[[idx]] <- list(
      id = normas_select$id[[i]]
    )
    error <- try(remDr$navigate(link))
    if (is.null(error)) {
      fun$szz(4, 5, paste("Downloading page:", idx, "of", scope))
      relacional_pages[[idx]][["page"]] <- fun$get_html(remDr)
    } else {
      fun$szz(1, 2, paste("Error while attempting to download page:", idx))
    }
    idx <- idx + 1
  }
}


redownload <- NULL

for (i in seq_along(relacional_pages)) {
  if (empty_relational(relacional_pages[[i]][[2]])) {
    print(i)
    redownload <- c(redownload, i)
  }
}

for (i in seq_along(redownload)) {
  idx <- redownload[[i]]
  re_id <- relacional_pages[[idx]][["id"]]
  rel_link <- normas_select %>%
    filter(id == re_id) %>%
    .$link %>%
    generate_relational_link()
  broken_page <- TRUE
  attempts <- 0
  while (broken_page & attempts < 10) {
    attempts <- attempts + 1
    error <- try(remDr$navigate(rel_link))
    if (is.null(error)) {
      fun$szz(8, 10, paste("Downloading page", idx, "on attempt", attempts))
      page <- fun$get_html(remDr)
      if (!empty_relational(page)) {
        relacional_pages[[idx]][["page"]] <- page
        broken_page <- FALSE
      }
    }
  }
}

for (i in seq_along(relacional_pages)) {
  id <- relacional_pages[[i]][["id"]]
  page <- relacional_pages[[i]][["page"]]
  write_html(page, paste0("./relacional_pages/relacional_", id, ".html"))
}

# Extracting relational_data ----

relacional_list <- vector('list', length(relacional_pages))

for (i in seq_along(relacional_pages)) {
  id <- relacional_pages[[i]][["id"]]
  page <- relacional_pages[[i]][["page"]]
  modificaram <- count_acts_by_css(page, css$espaco_modificaram)
  modificados <- count_acts_by_css(page, css$espaco_modificados)
  classificacao <- classify_this_act(page)
  relacional_list[[i]] <- tibble(
    id = id,
    modificaram = modificaram,
    modificados = modificados,
    classificacao = classificacao
  )
}

relacional_df <- bind_rows(relacional_list)

relacional_df_clean <- relacional_df %>%
  filter(!duplicated(id))

alter_date_tibble <- vector('list', length(relacional_pages))

for (i in seq_along(relacional_pages)) {
  id <- relacional_pages[[i]][["id"]]
  page <- relacional_pages[[i]][["page"]]
  alter_date <- page %>%
    html_nodes(css$espaco_modificaram) %>%
    html_text() %>%
    str_squish() %>%
    paste(collapse = " ") %>%
    str_extract("\\d{2}/\\d{2}/\\d{4}")
  alter_date_tibble[[i]] <- tibble(
    id = id,
    alter_date = alter_date
  )
}

alter_date_tibble <- bind_rows(alter_date_tibble)

alter_date_tibble <- alter_date_tibble %>%
  filter(!duplicated(id))

relacional_df_clean <- relacional_df_clean %>%
  left_join(alter_date_tibble, by="id")

saveRDS(relacional_df_clean, "relacional_df.rds")
