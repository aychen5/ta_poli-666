#path <- "~/Dropbox/psets/PS7"
child_data <- read.csv(file.path(path, "child_soldiering.csv"))

# our linear model
mod <- lm(educ ~ abd + age + fthr.ed + mthr.ed + C.ach, data = child_data)

# What is half the magnitude of the treatment coefficient?
half_beta <- mod$coefficients[2]/2

# values of delta
hyp_delta <- seq(0.001, 1, by = 0.01)

# Solve for gamma
hyp_gamma <- half_beta/hyp_delta

## plotting the hypothetical degree of confounding needed to reduce treatment
## to reduce treatment effect by half
ggplot(data.frame(delta = hyp_delta, 
                  gamma = hyp_gamma), 
       aes(x = delta, y = gamma)) +
  geom_path() +
  xlab(expression(delta)) + ylab(expression(gamma)) +
  ylim(-5, 1) +
  theme_bw()


# estimate a propensity score:
# delta is relationship between covariates and treatment
ps_mod <- glm(abd ~ age + fthr.ed + mthr.ed + C.ach, data = child_data, 
             family = binomial(link = "logit"))

######### ACH ###########
# Create two dfs for predction: setting residency in Ach to 1 and to 0
mod_ach1 <- data.frame(model.matrix(ps_mod)[, 1:4], C.ach = 1)
mod_ach0 <- data.frame(model.matrix(ps_mod)[, 1:4], C.ach = 0)

# Now, predict setting C.ach at 1 or 0, conditioned by other covariates
ps_ach1 <- predict(ps_mod, newdata = mod_ach1)
ps_ach0 <- predict(ps_mod, newdata = mod_ach0)

                        # Recall the definition of delta...P(U=1|D=1) - P(U=1|D=0) 
ach_point <- data.frame(delta = mean(ps_ach1) - mean(ps_ach0), 
                        # gamma is just relationship between covariates and outcome
                        gamma = mod$coefficients[6])

######### AGE ###########
# same idea with age, but now we set age to the first and third quartiles

# Get predicted probabilities
summary(child_data$age)
pscore_age17 <- predict(ps_mod, 
                        newdata = data.frame(model.matrix(ps_mod)[, c(1, 3:5)], 
                                             age = 17))
pscore_age25 <- predict(ps_mod, 
                        newdata = data.frame(model.matrix(ps_mod)[, c(1, 3:5)], 
                                             age = 25))

age_point <- data.frame(delta = mean(pscore_age25) - mean(pscore_age17), 
# "assuming have a negative relationship (but the same magnitude) with our outcome"
                        gamma = - mod$coefficients[3])



######### TRY DOING FATHER'S EDUCATION YOURSELF? ;-) ###########

# plot the points against the curve from earlier!
ggplot(data.frame(delta = hyp_delta, 
                  gamma = hyp_gamma), 
       aes(x = delta, y = gamma)) +
  geom_path() + 
  geom_point(aes(ach_point$delta, ach_point$gamma)) +
  geom_point(aes(age_point$delta, age_point$gamma)) +
  xlab(expression(delta)) + ylab(expression(gamma)) +
  ylim(-5, 1) +
  theme_bw()
