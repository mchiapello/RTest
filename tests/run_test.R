all_files <- list.files("testthat",full.names=T)

sapply(all_files,source)