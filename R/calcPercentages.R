#' Calculates age proportion distributions
#'
#' \code{calcPercentages} calculates the proportions of individuals aged 0-100,
#' as well as the proportion aged less, and greater than each age.
#'
#' @param data is a data frame of population counts per age, with the counts
#' given in fields which must be named \code{Female}, \code{Male} and
#' \code{Persons}. Each row corresponds to each year of age from 0-100.

calcPercentages <- function(data){
    ## First, we need the total population in this year
    popTotals <- colSums(data %>% select(Female, Male, Persons))
    ## Now we need to loop over each row and perform the calculations
    ## Lets set up a local function that does that.
    localPerc <- function(i, pops, tot){
        if(i == 1){
            inAge <- pops[i] / tot
            belowAge <- 0
            aboveAge <- sum(pops[-i]) / tot
        } else if(i == length(pops)){
            inAge <- pops[i] / tot
            belowAge <- sum(pops[-i]) / tot
            aboveAge <- 0
        } else {
            inAge <- pops[i] / tot
            belowAge <- sum(pops[c(1:(i - 1))]) / tot
            aboveAge <- sum(pops[c((i + 1):length(pops))]) / tot
        }
        c(inAge, belowAge, aboveAge)
    }
    females <- sapply(seq_len(nrow(data)), localPerc,
                      pops = data$Female, tot = popTotals["Female"])
    males <- sapply(seq_len(nrow(data)), localPerc,
                    pops = data$Male, tot = popTotals["Male"])
    persons <- sapply(seq_len(nrow(data)), localPerc,
                      pops = data$Persons, tot = popTotals["Persons"])
    data <- data %>%
        mutate(femaleIn = females[1,],
               femaleBelow = females[2,],
               femaleAbove = females[3,],
               maleIn = males[1,],
               maleBelow = males[2,],
               maleAbove = males[3,],
               personsIn = persons[1,],
               personsBelow = persons[2,],
               personsAbove = persons[3,]
               )
    data
}
