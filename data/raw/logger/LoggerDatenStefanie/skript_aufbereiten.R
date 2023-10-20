####################################################################################################
# setting up the environment
####################################################################################################

library(tidyverse)

# defining a function to convert the time format from 12h to 24h
change_to_24 = Vectorize(function(x) {
    gsub("\\s+", "", x) -> x
    if (grepl("AM", x)) {
        substr(x, 1, nchar(x) - 3) -> new
        strsplit(new, ":")[[1]] -> temp
        if (temp[1] == "12") {
            temp[1] <- "00"
        }
        if (nchar(temp[1]) == 1) {
            temp[1] <- paste0("0", temp[1])
        }
        paste(temp, collapse = ":") %>% as.character() -> out
    }
    else
    {
        substr(x, 1, nchar(x) - 3) -> new
        strsplit(new, ":")[[1]] -> temp
        if (temp[1] != "12") {
            temp[1] <- as.numeric(temp[1]) + 12
        }
        if (temp[1] == "12") {
            temp[1] <- "12"
        }
        paste(temp, collapse = ":") %>% as.character() -> out
    }

    return(out)
}
)

# define function to ad leading zeros to the date
add_zeros = function(x) {
    if (nchar(x) == 1) {
        paste0("0", x) -> out
    }
    else
    {
        out <- x
    }
    return(out)
}


# defining a function to convert the date format
date_conv = Vectorize(function(x) {
    strsplit(x, "/")[[1]] -> temp
    add_zeros(temp[1]) -> mm
    add_zeros(temp[2]) -> dd
    paste0("20", temp[3]) -> yyyy
    paste(yyyy, mm, dd, sep = "-") -> out
    return(out)
}
)

# defining a function to import and convert the data
import_conv = function(source, name) {
    read.csv(source, skip = 14) %>% 
    as_tibble() %>%
    mutate(
        time = paste(date_conv(Date), change_to_24(Time)) %>% as_datetime()
    ) %>% select(time, !!sym(name) := Value) -> out
    return(out)
}

# defining a function to import and merge all the files
import_merge = function(source, name) {
    full_join(import_conv(source[1], name[1]), import_conv(source[2], name[2]), by = "time") %>%
                 full_join(import_conv(source[3], name[3]), by = "time") -> out
    return(out)
}

####################################################################################################
# execution
####################################################################################################

# scanning the directory for the files
list.files(pattern = "\\.csv$") -> file_names
sapply(strsplit(file_names, "_"), "[", 1) -> name

# fixing the curiosity on line 15 of the logger files
for (file in file_names) {
    lines <- readLines(file)
    lines[15] <- gsub("/", ",", lines[15])
    writeLines(lines, file)
}

# importing and merging the data
import_merge(file_names, name) %>% write.csv("logger_summarised.csv", row.names = FALSE)