library(tidyverse)
library(janitor)

setwd("C:/Users/t31099033/Documents/Andre/d3e")

## Government expenditure per student, tertiary (% of GDP per capita)

df <- data.frame(categ = c("Brasil \n Privadas (SFL)", 
                           "Brasil \n Privadas", 
                           "Brasil \n IFs",
                           "Brasil \n Estaduais", 
                           "Brasil \n Federais"),
                 exp_per_cap = c(12625, 14837, 27844, 32199, 40893))

df <- df %>% 
      mutate(exp_pib = exp_per_cap/30407,
             exp_pib = exp_pib*100,
             ano = 2016) %>% 
      rename(pais = categ,
             valor = exp_pib) %>% 
      select(pais, ano, valor)

world <- read_csv("API_SE.XPD.TERT.PC.ZS_DS2_en_csv_v2_10477055.csv", skip = 4)

world %>% 
      clean_names %>% 
      select(-c(country_code, indicator_name, indicator_code)) %>% 
      melt(id.vars = c("country_name")) %>% 
      mutate(variable = str_replace_all(variable, "x", "")) %>% 
      rename(year = variable) %>% 
      filter(!is.na(value)) %>%
      arrange(country_name) %>% 
      filter(country_name %in% c("Australia", "Mexico", "United Kingdom", "Portugal", "South Africa",
                                 "OECD members", "United States")) %>% 
      arrange(country_name, desc(year)) %>% 
      group_by(country_name) %>% 
      slice(1) %>% 
      rename(pais = country_name, ano = year, valor = value) %>% 
      ungroup %>% 
      mutate(pais = c("Austrália", "México", "OCDE", "Portugal", "África do Sul", 
                      "Reino Unido", "EUA")) %>% 
      rbind(df) %>% 
      mutate(valor = as.numeric(valor),
             pais = reorder(pais, -valor)) %>% 
      ggplot(aes(pais, valor)) +
      geom_bar(stat = "identity", fill = "dodgerblue4", width = .7) +
      ggthemes::theme_gdocs() +
      labs(y = "", x = "") +
      theme(panel.grid.major.x = element_blank(),
            axis.text.y = element_text(face = c(rep("bold", 4), "plain",
                                                "bold", rep("plain", 6)), size = 12),
            axis.text.x = element_text(size = 12)
            ) +
      # labs(y = "%") +
      # theme(axis.title.x = element_blank(),
      #       axis.title.y = element_text(angle = 0, face = NULL, size = 10)) +
      scale_y_continuous(breaks = seq(0, 140, by = 10)) +
      coord_flip()
      

## Expenditure on tertiary education (% of government expenditure on education)

world2 <- read_csv("API_SE.XPD.TERT.ZS_DS2_en_csv_v2_10480181.csv", skip = 4)

world2 %>% 
      clean_names %>% 
      select(-c(country_code, indicator_name, indicator_code)) %>% 
      melt(id.vars = c("country_name")) %>% 
      mutate(variable = str_replace_all(variable, "x", "")) %>% 
      rename(year = variable) %>% 
      filter(!is.na(value)) %>%
      arrange(country_name) %>% 
      filter(country_name %in% c("Australia", "Mexico", "United Kingdom", "Portugal", "South Africa",
                                 "OECD members", "United States", "Brazil")) %>% 
      arrange(country_name, desc(year)) %>% 
      group_by(country_name) %>% 
      slice(1) %>% 
      rename(pais = country_name, ano = year, valor = value) %>% 
      ungroup %>% 
      mutate(pais = c("Austrália", "Brasil", "México", "OCDE", "Portugal", "África do Sul", 
                      "Reino Unido", "EUA")) %>% 
      dplyr::mutate(valor = as.numeric(valor),
             pais = reorder(pais, -valor)) %>% 
      ggplot(aes(pais, valor)) +
      geom_bar(stat = "identity", fill = "dodgerblue4", width = .7) +
      ggthemes::theme_gdocs() +
      labs(y = "", x = "") +
      theme(panel.grid.major.x = element_blank(),
            axis.text.x = element_text(face = c(rep("plain", 5), "bold",
                                                rep("plain", 2)), size = 12),
            axis.text.y = element_text(size = 12)) +
      # labs(y = "%") +
      # theme(axis.title.x = element_blank(),
      #       axis.title.y = element_text(angle = 0, face = NULL, size = 10)) +
      scale_y_continuous(breaks = seq(0, 30, by = 5), 
                         limits = c(0, 30))



# Referencias

#' https://www.valor.com.br/brasil/4890354/pib-capita-cai-44-em-2016-e-tem-3-ano-consecutivo-de-queda
#' http://documents.worldbank.org/curated/en/884871511196609355/pdf/121480-REVISED-PORTUGUESE-Brazil-Public-Expenditure-Review-Overview-Portuguese-Final-revised.pdf



