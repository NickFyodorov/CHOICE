require(Rssa)

CHOICE <- function(f1, f2, k, L = (k+1) %/% 2, r_ssa, r_mssa) {
  n <- length(f1)
  
  f1_base <- f1[1:k]
  f2_base <- f2[1:k]
  
  f1_test <- f1[(k+1):n]
  
  ssa_f1 <- ssa(f1_base, L = L)
  mssa_f1_f2 <- ssa(cbind(f1_base, f2_base), L = L, kind = "mssa")
  
  ssa_test <- rforecast(ssa_f1, groups = list(1:r_ssa), len = n - k, only.new = TRUE)
  mssa_test <- rforecast(mssa_f1_f2, groups = list(1:r_mssa), len = n - k, only.new = TRUE)[,1]
  
  gamma_ssa <- mean((f1_test - ssa_test)^2)
  gamma_mssa <- mean((f1_test - mssa_test)^2)
  
  tau_gamma <- as.numeric(gamma_mssa < gamma_ssa)
  tau_gamma
}

# EXAMPLE
# 
# set.seed(42)
# 
# s1 <- 20 * cos(2 * pi * 1:100 / 12)
# s2 <- 30 * cos(2 * pi * 1:100 / 12)
# 
# f1 <- s1 + 2 * rnorm(100)
# f2 <- s2 + 0.5 * rnorm(100)
# 
# ssa_rec <- reconstruct(ssa(f1), groups = list(1:2))[[1]]
# mssa_rec <- reconstruct(ssa(cbind(f1, f2)), groups = list(1:2))[[1]][,1]
# 
# mse_ssa <- mean((s1 - ssa_rec)^2)
# mse_mssa <- mean((s1 - mssa_rec)^2)
# 
# tau_mse <- as.numeric(mse_mssa < mse_ssa)
# tau_gamma <- CHOICE(f1, f2, k = 75, r_ssa = 2, r_mssa = 2)
# 
# print(sprintf("tau_mse = %g, tau_gamma = %g", tau_mse, tau_gamma))