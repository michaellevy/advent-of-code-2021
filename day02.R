library(tidyverse)
d <- read_table2("inputs/day02.txt", col_names = c("dir", "len"))

# Part 1
moves <- count(d, dir, wt = len)
vert <- with(moves, n[dir == "down"] - n[dir == "up"])
horiz <- moves$n[moves$dir == "forward"]
vert * horiz

# Part 2
d <-
    d %>%
    mutate(
        aim_change = case_when(
            dir == "down" ~ len,
            dir == "up" ~ -len,
            TRUE ~ 0
        ),
        aim = cumsum(aim_change),
        horiz = ifelse(dir == "forward", len, 0),
        vert = ifelse(dir == "forward", len * aim, 0)
    )
sum(d$horiz) * sum(d$vert)
