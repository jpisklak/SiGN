b <- 1
r_a <- 0.053
r_b <- 0.045
delta_a <- 11.545
delta_b <- 0.571
k_r <- 1
k_d <- 0.5


b <- 1
r_a <- 0.053
r_b <- 0.045
delta_a <- 11.545
delta_b <- 0.571
k_r <- 1
k_d <- 1


   (b * (r_a^k_r * delta_a^k_d)) /
  ((b * (r_a^k_r * delta_a^k_d)) +
        (r_b^k_r * delta_b^k_d))


   (b * (r_a^k_r * exp(delta_a * k_d))) /
  ((b * (r_a^k_r * exp(delta_a * k_d))) +
        (r_b^k_r * exp(delta_b * k_d)))



