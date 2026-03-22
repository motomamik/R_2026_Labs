# Factorial_loop – wykorzystująca pętlę (np. for),
# Factorial_reduce – wykorzystująca funkcję reduce() z pakietu purrr lub funkcję Reduce() z pakietu podstawowego,
# Factorial_recursive – wersja rekurencyjna,
# Factorial_mem – wersja korzystająca z memoizacji (cache'owanie wyników pośrednich).

library(purrr)
library(memoise)
library(microbenchmark)

factorial_loop <- function(x) {
  if (x == 0) return(1)
  result <- 1
  for (i in 1:x) {
    result <- result * i
  }
  result
}
factorial_loop(6)


factorial_reduce <- function(x) {
  if (x == 0) return(1)
  1:x |> reduce(`*`)
}
factorial_reduce(6)


factorial_recursive <- function(x) {
  if (x == 0) return(1)
  x * factorial_recursive(x - 1)
}
factorial_recursive(6)


factorial_mem <- memoise(function(x) {
  if (x == 0) return(1)
  x * factorial_recursive(x - 1)
})
factorial_mem(6)



run_benchmark <- function(n_val) {
  results <- microbenchmark(
    Loop = factorial_loop(n_val),
    Reduce = factorial_reduce(n_val),
    Recursive = factorial_recursive(n_val),
    Memoized = factorial_mem(n_val),
    times = 100
  )
  
  sink("benchmark.txt", append = TRUE)
  cat("\nWyniki testu dla n =", n_val, "\n")
  print(results)
  sink()
  
  return(results)
}


# test 
run_benchmark(10)
run_benchmark(50)
run_benchmark(100)
run_benchmark(200)