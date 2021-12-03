library(tidyverse)
Mode <- function(x) {  # credit: https://stackoverflow.com/a/8189441
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}
bin_char_vec_to_dec <- function(x)
    strtoi(paste(x, collapse = ""), base = 2)

d <- read_table("inputs/day03.txt", "x")
m <- str_split(d$x, pattern = "", simplify = TRUE)

# Part 1
gamma <- apply(m, 2, Mode)
epsilon <- as.character(1 - as.integer(gamma))
bin_char_vec_to_dec(gamma) * bin_char_vec_to_dec(epsilon)

# Part 2
# Oxygen
for (i in seq_len(ncol(m))) {
    column <- m[, i]
    tab <- table(column)
    mode_bit <- if (tab[1] == tab[2]) "1" else names(tab)[which.max(tab)]
    m <- m[column == mode_bit, ]
    if (is.vector(m)) {
        message("Finished on column ", i)
        break()
    }
}
O2 <- bin_char_vec_to_dec(m)

# CO2
m <- str_split(d$x, pattern = "", simplify = TRUE)
for (i in seq_len(ncol(m))) {
    column <- m[, i]
    tab <- table(column)
    mode_bit <- if (tab[1] == tab[2]) "0" else names(tab)[which.min(tab)]
    m <- m[column == mode_bit, ]
    if (is.vector(m)) {
        message("Finished on column ", i)
        break()
    }
}
CO2 <- bin_char_vec_to_dec(m)

O2 * CO2
