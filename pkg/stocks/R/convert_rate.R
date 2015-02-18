convert.rate <- function(rate, days.in = 1, days.out = 1) {
  out.rate <- ((rate + 1)^(days.out/days.in)) - 1
  return(out.rate)
}