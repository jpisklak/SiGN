library(digitize)

rat <- "K30"
img <- digitize(paste0("images/", rat, ".png"))

img$x <- c(10, 20, 30, 40, 50)
img$subject <- rat
write.csv(img, paste0("digi_data/", rat, ".csv"), row.names = FALSE)
