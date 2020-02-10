# code for lab 3

# you are an alpaca shephard!
browseURL("https://www.jwilber.me/permutationtest")

# Treatment assignment vector
d <- c(0,1,1,1,0,1,0,0,1)

# Observed outcomes
Y <- c(2,1,3,4,2,3,0,4,6)

# compute the sample ATE
obs.sate <- mean(Y[which(d==1)]) - mean(Y[which(d==0)])


# selecting only the rows where the number of treated is equal to 5
all_permute_treatment <- all_permute_treatment[rowSums(all_permute_treatment) == 5, ]

#For each permutation, estimate the ATE.
permute.sate <- c(length(all_permute_treatment))

for(i in 1:nrow(all_permute_treatment)){
  D_star <- unlist(all_permute_treatment[i,])
  permute.sate[i] <- mean(Y[which(D_star==1)]) - mean(Y[which(D_star==0)])
}

# this is our sampling distribution!
library(ggplot2)
qplot(permute.sate) +
  labs(x = "Simulated Effect Size", y = "Count") +
  geom_vline(xintercept = obs.sate, lty =2, col = "red", lwd = 2) +
  theme_bw()


# This is a two sided test
t_star <- length(permute.sate[which(abs(permute.sate) >= abs(obs.sate))])
t_star/nrow(all_permute_treatment)

# do the same with ri2 package
library(ri2)
# using the same toy example...
dat_table <- data.frame(Z = d, Y = Y)

# "declare" your randomization procedure: we have 9 observations, 5 of which are treated
declaration <- declare_ra(N = 9, m = 5)

# Conduct Randomization Inference
ri <- conduct_ri(
  formula = Y ~ Z,
  declaration = declaration,
  sharp_hypothesis = 0,
  data = dat_table
  # sims = 100
)

plot(ri)


# Simulating Clustered Data
# sample size
N <- 1000
# number of clusters
M <- 50

# Begin by creating a cluster indicator
J <- rep(c(1:M), each = 20)

# Draw group-level means for each cluster
mu_j0 <- runif(M, 0, 10)
mu_j1 <- runif(M, 6, 16)

# Calculate new values (potential outcomes under control and treatment) with clustered data
Y0 <- rnorm(N, mean = rep(mu_j0, each = 20), sd = 1)
Y1 <- rnorm(N, mean = rep(mu_j1, each = 20), sd = 1)

# randomly pick 25 of the 50 clusters to be assigned to treatment
D <- sample(unique(J), length(unique(J))/2)
D_j <- rep(0, length(unique(J)))

# find the randomly chosen clusters and assign them to treatment (D = 1)
D_j[which(unique(J) %in% D)] <- 1
D_ij <- rep(D_j, each = length(J)/length(unique(J)))


# calculate the realized outcome Y
Y_ij <- Y1*D_ij + Y0*(1-D_ij) 

clustered_data <- data.frame(outcome = Y_ij, treated = D_ij, clusterid = J)

# difference in means
mean(clustered_data$outcome[clustered_data$treated == 1]) - mean(clustered_data$outcome[clustered_data$treated == 0])














