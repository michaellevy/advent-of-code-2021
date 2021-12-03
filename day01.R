library(tidyverse)
d <- read_table("inputs/day01.txt", col_names = "x")

# Part 1
sum(d$x > lag(d$x), na.rm = TRUE)


# Part 2
slides <- d$x + lead(d$x, 1) + lead(d$x, 2)
sum(slides > lag(slides), na.rm = TRUE)
