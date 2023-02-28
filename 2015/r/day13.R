#--- Day 13: Knights of the Dinner Table ---

input <- read.table('2015/data/input13.txt')

invitees <- (\(input) {
  x <- data.frame(
    p1 = input$V1,
    p2 = gsub('.$', '', input$V11),
    net_gain = ifelse(input$V3 == 'gain', 1, -1) * input$V4
  )
  x$net_gain <- x$net_gain + x$net_gain[order(x$p2, x$p1)]
  x
})(input)

find_optimum <- function(invitees) {
  pairings <- setNames(invitees$net_gain, paste0(invitees$p1, invitees$p2))

  seat_next <- function(seated) {
    if (all(invitees$p1 %in% seated)) {
      return(sum(pairings[paste0(seated, c(tail(seated, -1), seated[1]))]))
    } else {
      
      sapply(unique(invitees$p1[!invitees$p1 %in% seated]),
             \(p) seat_next(c(seated, p))) |>
        max()
    }
  }
  
  seat_next(invitees$p1[1])
}

# Part 1
find_optimum(invitees)

# Part 2
rbind(invitees,
      data.frame(p1 = unique(invitees$p1), p2 = 'Me', net_gain = 0),
      data.frame(p1 = 'Me', p2 = unique(invitees$p1), net_gain = 0)
      ) |>
  find_optimum()
