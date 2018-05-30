require(Rssa)

CHOICE <- function(f1, f2, k, L = (k+1) %/% 2, r_ssa, r_mssa) {
  f1_base <- f1[1:k]
  f2_base <- f2[1:k]
  
  f1_test <- f1[(k+1):n]
  
  ssa_f1 <- ssa(f1, L = L)
  mssa_f1_f2 <- ssa(cbind(f1, f2), L = L, kind = "mssa")
  
  ssa_test <- rforecast(ssa_f1, groups = list(1:r_ssa), len = n - k, only.new = TRUE)
  mssa.test <- rforecast(mssa_f1_f2, groups = list(1:r_mssa), len = n - k, only.new = TRUE)[,1]
  
  gamma_ssa <- mean((f1.test - ssa.test)^2)
  gamma_mssa <- mean((f1.test - mssa.test)^2)
  
  tau_gamma <- as.numeric(gamma.mssa < gamma.ssa)
  tau_gamma
}