library(tidyverse)
library(jsonlite)
fromJSON("./data/raw/bitelamina/bitelamina.json") -> data_raw

create_df <- function(str){

    Wiederholung <- rep(str, 9) %>% as.factor()

    data_raw[[str]]  %>% as.tibble() %>% names() %>% as.factor() -> Nummer

    seq(0.5,8,0.5) %>% as.character() -> new_col_names
    data_raw[[str]]  %>% as.tibble() %>% t() %>% as.tibble(.name_repair = function(x) new_col_names) -> values

    cbind(Wiederholung, Nummer, values) %>% as.tibble() -> df

    return(df)
}

data <- do.call(rbind, lapply(c("N", "M", "S"), create_df)) %>%
    pivot_longer(-c(Wiederholung, Nummer), names_to = "Tiefe", values_to = "Wert") %>%
    mutate(Tiefe = Tiefe %>% as.factor() %>% fct_rev())

data %>% write_csv("./data/raw/bitelamina/bitelamina.csv")