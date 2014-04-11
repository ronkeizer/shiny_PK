
### Pirana-generated deSolve code
library (deSolve)
library (MASS)
library (lattice)

### Dose and time Settings
A_init     <- rep(0, 2)  # Initial state of ODE system
n_doses    <- 3
dose_cmt   <- 1
ii         <- 24
dose_times <- seq (from = 0, by=ii, to=n_doses*ii)
dose_amts  <- c(rep (100, n_doses), 0)
times      <- seq(from=0, to=ii*n_doses, by=.5)  # Integration window and stepsize
obs_c      <- c(1:2)  # Observation compartments
n_ind      <- 20
n_par      <- 10

### Parameters
theta <- c(0.1, 0.1, 0.1, 0.1, 1)
mat <- c((0.1),
         0, (0.1),
         0, 0, (0.1))
trans.lower <- function(x,y) { ifelse(y<x, x*(x-1)/2 + y, y*(y-1)/2 + x) }
full.mat <- function(p) { outer(1:p,1:p, trans.lower) }
omega <- matrix (mat[full.mat(3)], nrow=3, byrow=T)
etas   <- mvrnorm(n = n_ind, mu=rep(0, 3), Sigma=omega )

draw_params <- function (eta) {
  p <- list()  # Parameter list 
  p$CL <- theta[1]*exp(eta[1]) 
  p$V  <- theta[2]*exp(eta[2]) 
  p$KA <- theta[3]*exp(eta[3]) 
  p$S1 <- p$V 
  return(p)
}

### ODE system
des <- function (t, A, p) {  # ODE system 
  p$K20 <- p$CL/p$V
  dAdt_1 <- -p$KA*A[1] 
  dAdt_2 <- -p$K20*A[2]  +p$KA*A[1]
  return ( list ( c (  dAdt_1, dAdt_2 ) ) )
}

### Perform numerical integration, collect data
num_int_wrapper<- function (i, times, A_init, des, p_ind) {
  des_out <- lsoda(A_init, times, des, p_ind)
  dat_ind <- c()
  for (j in 1:length(obs_c)) {
    dat_ind <- rbind (dat_ind, cbind(i, t=des_out[,1], comp=obs_c[j], ipred=des_out[,(obs_c[j]+1)]))
  }
  return(data.frame(dat_ind))
}

comb_dat <- c()
for (i in 1:n_ind) {
  p_ind   <- draw_params (eta = etas[i,])
  for (k in 1:(length(dose_times)-1)) {
    if (k > 1) {
      A_upd <- dat_ind[dat_ind$t==tail(time_window,1),]$ipred
    } else {
      A_upd <- A_init
    }
    A_upd[dose_cmt] <- A_upd[dose_cmt] + dose_amts[k]
    time_window <- times[(times > dose_times[k]) & (times <= dose_times[k+1])]
    dat_ind <- num_int_wrapper (i, time_window, A_upd, des, p_ind)
    comb_dat <- rbind (comb_dat, dat_ind)
  }
}

### Plot
xyplot ( ipred~t|as.factor(comp), group=i, data=data.frame(comb_dat),
         type='l', col='lightblue' )
