test <- function(a,b){
  paste(a,b)
}

mapply(test, c("hi","you"), "there")

list_test <- lapply(c("hi","there", "you"), test, b="thing")

l1 <- c("pf_mu"=expression(mu), "pf_sigma"=expression(sigma), "fpf_static"="non-labile fraction", "permafrost_c0"="Initial permafrost carbon")
