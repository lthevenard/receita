library(tidyverse)
library(scales)
library(DescTools)
library(lubridate)


normas_receita <- readRDS("normas_receita.rds")

theme_set(theme_bw() + theme(legend.position = "bottom"))

normas_select <- readRDS("select_database.rds")

write_csv(normas_select, "select_databse.csv")

# Functions ----

save_plot <- function(filename, path = "./plots", height= 6, width= 10, dpi = 300, ...) {
  ggsave(filename=filename, height= height, width= width, path = path, dpi = dpi,...)
}

match_regex_cases <- function(texts, regex, name, lower_case = TRUE) {
  results <- NULL
  is_match <- vector('numeric', length(texts))
  for (i in seq_along(texts)) {
    if (is.na(texts[[i]])) {
      next()
    }
    if (lower_case) {
      text <- str_to_lower(texts[[i]])
    } else {
      text <- texts[[i]]
    }
    if (str_detect(text, regex)) {
      is_match[[i]] <- 1
    }
    results <- c(results, str_extract_all(text, regex) %>% unlist())
  }
  num_results <- sum(is_match)
  results <- results[!is.na(results)]
  unique_results <- unique(results)
  incidence <- vector('numeric', length(unique_results))
  for (i in seq_along(unique_results)) {
    incidence[[i]] <- sum(results == unique_results[[i]])
  }
  df <- tibble(
    imposto = name,
     `Expressão regular` = regex,
     `Documentos com ao menos 1 ocorrência` = num_results,
     `Casos encontrados` = unique_results,
     `Número total de ocorrências` = incidence
  ) %>% arrange(desc(`Número total de ocorrências`))
  return(df)
}

generate_incidence_table <- function(texts, patterns) {
  incidence_tables <- vector('list', length(patterns))
  for (i in seq_along(patterns)) {
    regex <- patterns[[i]]
    name <- names(patterns)[[i]]
    incidence_tables[[i]] <- match_regex_cases(texts, regex, name)
  }
  return(bind_rows(incidence_tables))
}

generate_annual_match_prop <- function(normas_select, name) {
  anos <- normas_select$data %>% str_extract("\\d{4}") %>% unique() %>% sort()
  ns <- vector('numeric', length(anos)) 
  props <- vector('numeric', length(anos)) 
  for (i in seq_along(anos)) {
    ano <- anos[[i]]
    ano_select <- normas_select %>% 
      filter(str_extract(data, "\\d{4}") == ano)
    ns[[i]] <- ano_select[[name]] %>% sum()
    props[[i]] <- ns[[i]] / nrow(ano_select)
  }
  df <- tibble(ano = anos,
               n = ns,
               prop = ifelse(is.na(props), 0, props))
  return(df)
}

generate_tax_count <- function(normas_select, column_range) {
  taxes <- vector('character', length(column_range))
  counts <- vector('numeric', length(column_range))
  idx = 1
  for (i in column_range) {
    taxes[[idx]] <- str_to_upper(names(normas_select)[[i]]) %>% 
      str_replace("_", "/")
    counts[[idx]] <- normas_select[[i]] %>% sum(na.rm=TRUE)
    idx <- idx + 1
  }
  df <- tibble(tax = taxes, n = counts) %>% 
    filter(tax != "PIS" & tax != "PASEP")
  return(df)
}

generate_word_mean_per_year <- function(normas_select) {
  anos <- normas_select$data %>% 
    str_extract("\\d{4}") %>% 
    unique() %>% 
    sort()
  sum_palavras <- vector('numeric', length(anos))
  std_palavras <- vector('numeric', length(anos))
  mean_palavras <- vector('numeric', length(anos))
  median_palavras <- vector('numeric', length(anos))
  for (i in seq_along(anos)) {
    ano_palavras <- normas_select %>% 
      filter(str_extract(data, "\\d{4}") == anos[[i]]) %>% 
      .$palavras
    sum_palavras[[i]] <- sum(ano_palavras, na.rm=TRUE)
    std_palavras[[i]] <- sd(ano_palavras, na.rm=TRUE)
    mean_palavras[[i]] <- mean(ano_palavras, na.rm=TRUE)
    median_palavras[[i]] <- median(ano_palavras, na.rm=TRUE)
  }
  df <- tibble(
    ano = anos,
    sum = sum_palavras,
    std = std_palavras,
    mean = mean_palavras,
    median = median_palavras
  )
  return(df)
}

generate_word_complex_per_year <- function(normas_select) {
  anos <- normas_select$data %>% 
    str_extract("\\d{4}") %>% 
    unique() %>% 
    sort()
  word_complex_perc <- vector('numeric', length(anos))
  word_complex_abs <- vector('numeric', length(anos))
  for (i in seq_along(anos)) {
    ano_palavras <- normas_select %>% 
      filter(!is.na(palavras)) %>% 
      filter(str_extract(data, "\\d{4}") == anos[[i]]) %>% 
      .$palavras
    word_complex_abs[[i]] <- sum(ano_palavras > 1000)
    word_complex_perc[[i]] <- word_complex_abs[[i]] / length(ano_palavras)
  }
  df <- tibble(
    ano = anos,
    abs = word_complex_abs,
    perc = word_complex_perc
  )
  return(df)
}

generate_content_support_table <- function(df, pattern, name) {
  list_output <- df$texto %>% 
    str_extract_all(pattern)
  tibble_output <- vector('list', length(list_output))
  for (i in seq_along(list_output)) {
    if (!is_empty(list_output[[i]])) {
      tibble_output[[i]] <- tibble(
        id = df$id[[i]],
        type = name,
        extraction = list_output[[i]]
      )
    }
  }
  return(bind_rows(tibble_output))
}

iterate_content_analysis_tables <- function(df, pattern_list) {
  tibble_list <- vector('list', length(pattern_list))
  for (i in seq_along(pattern_list)) {
    category <- names(pattern_list)[[i]]
    pattern <- pattern_list[[i]]
    tibble_list[[i]] <- generate_content_support_table(
      df, pattern, category
    )
  }
  return(bind_rows(tibble_list))
}

plot_content_analysis <- function(content_table, chosen_type, title) {
  content_table %>% 
    filter(type == chosen_type) %>% 
    count(id) %>% 
    right_join(normas_select_impostos) %>% 
    mutate(ano = str_extract(data, "\\d{4}")) %>% 
    filter(ano != "2021") %>% 
    mutate(n = ifelse(is.na(n), 0, n)) %>% 
    group_by(ano) %>% 
    summarise(`Incidência total` = sum(n, na.rm = TRUE), `Médias anuais` = mean(n, na.rm = TRUE)) %>% 
    mutate(ano = as.integer(ano)) %>% 
    pivot_longer(cols = c(`Incidência total`, `Médias anuais`)) %>% 
    ggplot(aes(x= ano, y = value, group = "")) +
    geom_point() +
    labs(title = title,
         subtitle = subtitle,
         x = "", y = "") +
    geom_smooth(method = "lm", color = brewer_pal()(7)[5], size = 0.5, linetype = "dashed") +
    facet_wrap(~name, scales = "free_y")
}

generate_content_analysis_table <- function(content_table, df) {
  counts <- content_table %>% count(id, type)
  counts <- bind_cols(counts, ano = "")
  for (i in 1:nrow(counts)) {
    counts$ano[[i]] <- df %>% 
      filter(id == counts$id[[i]]) %>% 
      .$data %>% 
      str_extract("\\d{4}")
  }
  return(
    counts %>% 
      group_by(ano, type) %>% 
      summarise(Documents = n(), 
                N = sum(n, na.rm=TRUE), 
                mean = mean(n, na.rm=TRUE), 
                median = median(n, na.rm=TRUE),
                std = sd(n, na.rm=TRUE))
  )
}


# Data transformations ----

impostos <- list(
  ii = "impostos?(\\s+\\S+){0,3}\\s+importa[cç]([aã]o|[õo]es)",
  irpf = "impostos?(\\s+\\S+){0,3}\\s+renda(\\s+\\S+){0,3}\\s+pessoas?\\s+f[íi]sicas?|\\birpf\\b",
  irpj = "impostos?(\\s+\\S+){0,3}\\s+renda(\\s+\\S+){0,3}\\s+pessoas?\\s+jur[íi]dicas?|\\birpj\\b",
  iof = "impostos?(\\s+\\S+){0,3}\\s+opera[cç]([aã]o|[õo]es)(\\s+\\S+){0,3}\\s+cr[ée]dito|\\biof\\b",
  ipi = "impostos?(\\s+\\S+){0,3}\\s+produtos?(\\s+\\S+){0,3}\\s+industrializados?|\\bipi\\b",
  cofins = "contribui[çc]([ãa]o|[õo]es)(\\s+\\S+){0,3}\\s+socia(l|is)(\\s+\\S+){0,3}\\s+financiamentos?(\\s+\\S+){0,3}\\s+seguridades?(\\s+\\S+){0,3}\\s+socia(l|is)|\\bcofins\\b",
  pis = "programas?(\\s+\\S+){0,3}\\s+integra[çc]([ãa]o|[õo]es)(\\s+\\S+){0,3}\\s+socia(l|is)|\\bpis\\b",
  pasep = "programas?(\\s+\\S+){0,3}\\s+forma[çc]([ãa]o|[õo]es)(\\s+\\S+){0,3}\\s+patrim[ôo]nios?(\\s+\\S+){0,3}\\s+servidor(es)?(\\s+\\S+){0,3}\\s+p[úu]blicos?|\\bpasep\\b",
  csll = "contribui[çc]([ãa]o|[õo]es)(\\s+\\S+){0,3}\\s+socia(l|is)(\\s+\\S+){0,3}\\s+lucros?(\\s+\\S+){0,3}\\s+l[íi]quidos?|\\bcsll\\b",
  inss = "contribui[çc]([ãa]o|[õo]es)(\\s+\\S+){0,3}(?<!excetuadas\\sas)\\s+previd[êe]nci[áa]rias?|\\binss\\b"
)

content_analysis <- list(
  dever = "(\\S+\\s+){0,6}(\\bdeve|\\bobriga|\\brespons|arcar|[ôo]nus)\\S*(\\s+\\S+){0,6}",
  pena = "(\\S+\\s+){0,6}(\\bpena|\\bmulta|\\bsanç|\\binfraç|\\bautua[çd])\\S*(\\s+\\S+){0,6}",
  prazo = "(\\S+\\s+){0,6}(\\bprazo|\\bdia(..)?\\s*([úu]teis|[úu]til|corridos?)?|\\bprorroga|d[ea]\\s+ci[êe]ncia)\\S*(\\s+\\S+){0,6}"
)

normas_select_impostos <- normas_select %>% 
  filter(dmy(data) >= dmy("05/10/1988")) %>% 
  mutate(ii = str_detect(str_to_lower(texto), impostos$ii),
         irpf = str_detect(str_to_lower(texto), impostos$irpf),
         irpj = str_detect(str_to_lower(texto), impostos$irpj),
         iof = str_detect(str_to_lower(texto), impostos$iof),
         ipi = str_detect(str_to_lower(texto), impostos$ipi),
         cofins = str_detect(str_to_lower(texto), impostos$cofins),
         pis = str_detect(str_to_lower(texto), impostos$pis),
         pasep = str_detect(str_to_lower(texto), impostos$pasep),
         pis_pasep = pis | pasep,
         csll = str_detect(str_to_lower(texto), impostos$csll),
         inss = str_detect(str_to_lower(texto), impostos$inss))

write_csv(normas_select_impostos, "normas_selecao_final.csv")

normas_select <- normas_select %>% 
  mutate(palavras = str_count(texto, "\\w+"))

incidence_table <- generate_incidence_table(normas_select_impostos$texto, impostos)

write_csv(incidence_table, "incidencia_impostos.csv")

generate_annual_match_prop(normas_select, "ipi") %>% 
  ggplot(aes(x = ano, y = prop, group = "")) +
  geom_line() +
  scale_y_continuous(labels = label_percent()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))

subtitle <- c("FONTE: Portal da Receita Federal (1988-2020)", 
              "FONTE: Portal da Receita Federal (1988-2021)",
              "FONTE: Portal da Receita Federal (1995-2020)")


relacional_data <- read_csv("relacional_data.csv")

normas_select_impostos_sport <- normas_select_impostos %>% 
  filter(!str_detect(str_to_lower(tipo), "portaria"))

content_table <- iterate_content_analysis_tables(
  normas_select_impostos, content_analysis
)

content_table_sport <- iterate_content_analysis_tables(
  normas_select_impostos_sport, content_analysis
)

relacional_df <- readRDS("relacional_df.rds")
write_csv(relacional_df, "relacional_df.csv")

# Analysis ----

## Tipos ----

normas_receita %>% 
  filter(dmy(data) >= dmy("05/10/1988")) %>% 
  filter(!(tipo %in% unique(normas_select$tipo))) %>% 
  count(tipo) %>% 
  arrange(desc(n)) %>%
  View()

normas_select %>% 
  filter(dmy(data) >= dmy("05/10/1988")) %>% 
  count(tipo) %>% 
  arrange(desc(n)) %>%
  View()

normas_select %>% 
  ggplot(aes(x = fct_rev(fct_infreq(tipo)))) +
  geom_bar(fill = brewer_pal()(7)[5]) +
  labs(title = "Tipologia das Normas da Receita Federal",
       subtitle = subtitle[1],
       x = "", y = "Número de Normas") +
  coord_flip()

save_plot("tipologia.png")

## Evolução ----

normas_select %>% 
  filter(dmy(data) >= dmy("05/10/1988")) %>% 
  mutate(ano = str_extract(data, "\\d{4}")) %>% 
  filter(ano != "2021") %>% 
  ggplot(aes(x = ano)) +
  geom_bar(fill = brewer_pal()(7)[5]) +
  labs(title = "Evolução da produção normativa da Receita Federal",
       subtitle = subtitle[1],
       x = "", y = "Número de Atos Normativos") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))

save_plot("evolucao.png")

## Tributos ----

normas_select_impostos %>% 
  generate_tax_count(12:22) %>% 
  mutate(tax = fct_reorder(tax, n)) %>% 
  ggplot(aes(x = tax, y = n)) +
  geom_col(fill=brewer_pal()(7)[5]) +
  labs(x = "", y = "Número de Atos Normativos que Mencionam o Tributo",
       title = "Tributos Federais nos Atos Normativos da Receita",
       subtitle = subtitle[2]) +
  coord_flip()

save_plot("tributos.png")

## Palavras por ano ----

word_mean_per_year <- normas_select %>%
  filter(dmy(data) >= dmy("05/10/1988")) %>% 
  generate_word_mean_per_year()

word_mean_per_year %>% 
  ggplot(aes(x=ano, y=mean, group="")) +
  geom_point(color=brewer_pal()(7)[5]) +
  geom_line(color=brewer_pal()(7)[5]) +
  geom_smooth(method = "lm", color = "black", size = 0.5, linetype = "dashed") +
  labs(x = "", y = "Número Médio de Palavras por Ato Normativo",
       title = "Extensão média dos atos normativos da Receita Federal ao longo do tempo",
       subtitle = subtitle[2]) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))

save_plot("tamanho_evolucao.png")

cor(as.integer(word_mean_per_year$ano), word_mean_per_year$mean)

normas_select %>% 
  filter(dmy(data) >= dmy("05/10/1988")) %>% 
  mutate(ano = str_extract(data, "\\d{4}")) %>% 
  filter(ano != "2021") %>% 
  count(ano) %>% 
  left_join(word_mean_per_year, by = "ano") %>% View()
  mutate(std = round(std, digits = 2),
         mean = round(mean, digits = 2)) %>% 
  relocate(std, .after = median) %>% 
  write_csv2("evolucao.csv")
  

## Muitas Palavras ----

# normas_select %>% 
  
normas_select %>% 
  filter(dmy(data) >= dmy("05/10/1988")) %>% 
  ggplot(aes(x = Winsorize(palavras, na.rm=TRUE))) +
  geom_histogram()

palavras_na <- normas_select %>% 
  filter(dmy(data) >= dmy("05/10/1988")) %>% 
  filter(!is.na(palavras)) %>% .$palavras

sum(palavras_na <= 1000) / length(palavras_na)

normas_select %>% 
  filter(dmy(data) >= dmy("05/10/1988")) %>% 
  filter(!is.na(palavras)) %>% 
  mutate(complex = ifelse(palavras <= 1000, "Até 1000 palavras", "Mais de 1000 palavras"),
         ano = str_extract(data, "\\d{4}")) %>% 
  ggplot(aes(x = ano, fill = complex)) +
  geom_bar(position = "fill") +
  scale_fill_brewer() +
  scale_y_continuous(labels=label_percent()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))


## Tipologia ----

normas_select %>% 
  filter(dmy(data) >= dmy("05/10/1988")) %>% 
  ggplot(aes(x = fct_rev(fct_infreq(tipo)))) +
  geom_bar(fill = brewer_pal()(7)[5]) +
  labs(x = "", y = "Número de Atos Normativos",
       title = "Tipologia dos atos normativos publicados pela Receita Federal",
       subtitle = subtitle) +
  coord_flip()

save_plot("tipologia.png")

## Análise de conteúdo ----

ca_table <- generate_content_analysis_table(content_table, normas_select_impostos)

ca_table %>% 
  mutate(class = ifelse(
    type == "dever", "Deveres / Obrigações", ifelse(
      type == "pena", "Multas / Penalidades", "Prazos"
    ))) %>% 
  write_csv2("content_analysis.csv")

ca_table %>% 
  mutate(class = ifelse(
    type == "dever", "Deveres e Obrigações", ifelse(
      type == "pena", "Multas e Penalidades", "Prazos Legais"
    )),
    ano = as.integer(ano)
  ) %>% 
  ggplot(aes(x = ano, y = N)) +
  geom_point() +
  geom_smooth(method = "lm", color = brewer_pal()(7)[5], size = 0.5, linetype = "dashed") +
  labs(title = "Incidência de termos que indicam estabelecimento de obrigações e penalidades ou alteração de prazos",
       y = "Incidência Total", x = "", subtitle = subtitle) +
  facet_wrap(~class)
  
save_plot("content_general.png", height = 5)

ca_table %>% 
  mutate(class = ifelse(
    type == "dever", "Deveres / Obrigações", ifelse(
      type == "pena", "Multas / Penalidades", "Prazos"
    ))) %>%
  write_csv2("content_analysis.csv")

ca_table_sport <- generate_content_analysis_table(content_table_sport, normas_select_impostos_sport)

ca_table_sport %>% 
  mutate(class = ifelse(
    type == "dever", "Deveres e Obrigações", ifelse(
      type == "pena", "Multas e Penalidades", "Prazos Legais"
    )),
    ano = as.integer(ano)
  ) %>% 
  pivot_longer(c("N", "mean")) %>% 
  mutate(name = ifelse(name == "N", "Incidência Total", "Médias Anuais"),
         culminate = paste(class, name, sep = " - ")) %>% 
  ggplot(aes(x = ano, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", color = brewer_pal()(7)[5], size = 0.5, linetype = "dashed") +
  labs(title = "Incidência de termos que indicam estabelecimento de obrigações e penalidades ou alteração de prazos,\nsem considerar as portarias",
       y = "", x = "", subtitle = subtitle) +
  facet_wrap(~culminate, nrow = 3, scales = "free_y")

save_plot("content_general_sport.png", height = 7)

   content_plots_info <- list(
  dever = c("dever", 
            "Incidência de termos que indicam o estabelecimento de deveres / obrigações"),
  pena = c("pena", 
           "Incidência de termos que indicam a imposição de multas e penalidades"),
  prazo = c("prazo", 
            "Incidência de termos indicam o estabelecimento ou alteração de prazos")
)

plot_content_analysis(content_table, 
                      content_plots_info[[1]][1],
                      content_plots_info[[1]][2])

save_plot("dever.png", height = 5)

set.seed(100)

content_table %>% 
  filter(type == "dever") %>% 
  sample_n(50) %>% 
  mutate(extraction = paste0(" ... ", extraction, " ... ")) %>% 
  left_join(normas_select, by = "id") %>% 
  select(titulo, data, extraction) %>% 
  rename(`Ato Normativo` = "titulo",
         `Data de publicação` = "data",
         `Trecho identificado` = "extraction") %>% 
  write_csv("content.csv")



plot_content_analysis(content_table, 
                      content_plots_info[[2]][1],
                      content_plots_info[[2]][2])

save_plot("pena.png", height = 5)


set.seed(100)

content_table %>% 
  filter(type == "pena") %>% 
  sample_n(50) %>% 
  mutate(extraction = paste0(" ... ", extraction, " ... ")) %>% 
  left_join(normas_select, by = "id") %>% 
  select(titulo, data, extraction) %>% 
  rename(`Ato Normativo` = "titulo",
         `Data de publicação` = "data",
         `Trecho identificado` = "extraction") %>% 
  write_csv("content.csv")

plot_content_analysis(content_table, 
                      content_plots_info[[3]][1],
                      content_plots_info[[3]][2])

save_plot("prazo.png", height = 5)

set.seed(100)

content_table %>% 
  filter(type == "prazo") %>% 
  sample_n(50) %>% 
  mutate(extraction = paste0(" ... ", extraction, " ... ")) %>% 
  left_join(normas_select, by = "id") %>% 
  select(titulo, data, extraction) %>% 
  rename(`Ato Normativo` = "titulo",
         `Data de publicação` = "data",
         `Trecho identificado` = "extraction") %>% 
  write_csv("content.csv")

## Disponibilidade Relacional ----

normas_select %>% 
  left_join(relacional_data, by="id") %>% 
  filter(dmy(data) >= dmy("05/10/1988")) %>% 
  filter(tem_relacional)

normas_select %>% 
  filter(dmy(data) >= dmy("05/10/1988")) %>% 
  left_join(relacional_data, by = "id") %>% 
  mutate(relacional = ifelse(tem_relacional, "Sim", "Não"),
         ano = str_extract(data, "\\d{4}")) %>%
  filter(ano != "2021") %>% 
  ggplot(aes(x = ano, fill = relacional)) +
  geom_bar(position =  "fill") +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_brewer() +
  labs(y = "Percentual dos Atos Normativos do Ano",
       x = "", fill = "Possui informações relacionais? ",
       title = "Disponibilidade de dados relacionais sobre os atos normativos",
       subtitle = subtitle) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))

save_plot("disponibilidade_relacional.png")

## Dados Relacionais ----

normas_select %>% 
  left_join(relacional_df, by="id") %>% 
  filter(!is.na(modificaram)) %>% 
  mutate(ano = as.integer(str_extract(data, "\\d{4}"))) %>%
  filter(ano >= 1995 & ano != 2021) %>% 
  group_by(ano) %>% 
  summarise(mod_ativa = log10(sum(modificaram)),
            mod_passiva = log10(sum(modificados)),
            interacoes_total = log10(sum(modificaram + modificados)),
            media_mod_ativa = mean(modificaram),
            media_mod_passiva = mean(modificados))  %>% 
  pivot_longer(cols = c(mod_ativa, mod_passiva, interacoes_total)) %>% 
  mutate(name = str_replace(name,
                            "interacoes_total",
                            "Score de Interações") %>% 
           str_replace("mod_ativa", "Score de Modificações Ativas") %>% 
           str_replace("mod_passiva", "Score de Modificações Passivas")) %>% 
  ggplot(aes(x = ano, y = value, group = name, color = name)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = brewer_pal(direction=-1)(7)[c(1, 3, 6)]) +
  labs(color = "Dados Relacionais ",
       y = "Score de Interação (log10)",
       x = "", title = "Evolução das interações entre as normas da Receita Federal",
       subtitle = subtitle[3])
  
save_plot("interacao.png")


