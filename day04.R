library(tidyverse)
calls <- read_lines("inputs/day04.txt", n_max = 1)
calls <- as.integer(str_split(calls, ",")[[1]])

boards <-
    map(1:100, function(b) {
        l <- read_lines("inputs/day04.txt", skip = 2 + (b - 1) * 6, n_max = 5)
        m <-
            map(l, ~ {
                .x %>%
                    # Trim leading whitespace
                    str_remove("^\\s+") %>%
                    # Split into five numbers
                    str_split_fixed(pattern = "\\s+", n = 5)
            }) %>%
            do.call(rbind, .)
        class(m) <- "integer"
        return(m)
    })

# Part 1
logical_boards <-
    replicate(length(boards), matrix(0L, nrow = 5, ncol = 5), simplify = FALSE)

solution <- 0L
st <- Sys.time()
for (number in calls) {
    for (b in seq_along(boards)) {
        # update logical board with current hit
        logical_boards[[b]] <-
            as.integer(boards[[b]] == number) + logical_boards[[b]]
        if (
            any(rowSums(logical_boards[[b]]) == 5) ||
            any(colSums(logical_boards[[b]]) == 5)
        ) {
            message("Found a solution on board ", b, " with call ", number)
            solution <- sum(boards[[b]][!logical_boards[[b]]]) * number
            break()
        }
    }
    if (solution)
        break()
}
(elap <- Sys.time() - st)  # .1 sec
solution

# Part 2
logical_boards <-
    replicate(length(boards), matrix(0L, nrow = 5, ncol = 5), simplify = FALSE)
winners <- rep(0L, length(boards))
st <- Sys.time()
for (number in calls) {
    for (b in seq_along(boards)) {
        # update logical board with current hit
        logical_boards[[b]] <-
            as.integer(boards[[b]] == number) + logical_boards[[b]]
        if (
            any(rowSums(logical_boards[[b]]) == 5) ||
            any(colSums(logical_boards[[b]]) == 5)
        ) {
            winners[b] <- 1L
        }
    }
    if (sum(winners) == 99)
        loser <- which(!winners)
    if (sum(winners) == 100) {
        solution2 <- sum(boards[[loser]][!logical_boards[[loser]]]) * number
        break()
    }
}
(elap <- Sys.time() - st)  # .3 sec
solution2
