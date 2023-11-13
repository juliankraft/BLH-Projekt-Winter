library(tidyverse)
library(readxl)

import_sheet <- function(sheet) {
colnames <- c("Loch", paste0("L", as.character(1:10)))

read_excel("./data/raw/bitelamina/Resultate_Koederstreifen.xlsx", sheet = sheet, skip = 4, col_names = colnames)%>% 
    filter(Loch %in% as.character(1:16)) %>% 
    select(! "L10") %>%
    mutate(
        Gruppe = paste0("G",sheet-1) %>% as.factor(),
        Wiederholung = c(rep("W1", 16), rep("W2", 16), rep("W3", 16)) %>% as.factor(),
        Tiefe = as.numeric(Loch) / 2
        ) %>%
    mutate_at(vars(L1:L9), as.numeric) %>%
    select(Gruppe, Wiederholung, Tiefe, L1:L9) -> data

    return(data)
}

do.call(rbind, lapply(2:7, import_sheet)) %>%
    pivot_longer(
        -c(Gruppe, Wiederholung, Tiefe), 
        names_to = "Streifen", 
        values_to = "Wert"
    ) %>% 
    write_csv("./data/raw/bitelamina/bitelamina.csv")