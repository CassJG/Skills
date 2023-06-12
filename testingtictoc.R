library(microbenchmark)
library(magrittr)
library(tidyverse)
test <- microbenchmark(
  slr_gd(mtcars, mpg, hp),
  mlr_gd(mtcars, mpg),
  mlr_qr(mtcars, mpg)
)

test

tictoc::tic()
slr_gd(mtcars, mpg, cyl)
tictoc::toc()

tictoc::tic()
mlr_gd(mtcars, mpg)
tictoc::toc()

tictoc::tic()
mlr_qr(mtcars, mpg)
tictoc::toc()
