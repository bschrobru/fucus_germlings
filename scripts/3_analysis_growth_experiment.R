library(brms)
library(rstan)
library(lmerTest)
library(bayesplot)
library(dplyr)
library(ggpubr)


growth_data = readRDS("data/clean_growth_data.rds")

glimpse(growth_data)

#Treatment and Jars as factor
growth_data$Treatment <- as.factor(growth_data$Treatment)
growth_data$Jar <- as.factor(growth_data$Jar)

growth_data$length_relative = scale(growth_data$Length_diff/growth_data$Length_1_cm)
growth_data$Length_1_cm = scale(growth_data$Length_1_cm)
growth_data$Length_diff = scale(growth_data$Length_diff)

growth_data$area_relative = scale(log(growth_data$Area_2_cm_2)-log(growth_data$Area_1_cm_2))
#growth_data$Area_1_cm_2 = scale(log(growth_data$Area_1_cm_2))
#growth_data$Area_diff = scale(log(growth_data$Area_diff))


#####Length#####

ggscatter(growth_data, x = "Length_1_cm", y= "Length_diff", color = "Treatment", add = "reg.line")

# Fit the model
model <- brm(Length_diff ~ Length_1_cm * Treatment + (1|Jar), 
             data = growth_data, 
             family = gaussian(), 
         #    prior = priors,
             control = list(adapt_delta = 0.9),)

summary(model)


# Posterior predictive checks
#pp_check(model)
library(bayesplot)
posterior_samples <- posterior_samples(model)
mcmc_dens(posterior_samples, pars = c("b_Intercept", "b_Length_1_cm", "b_TreatmentSnail", "b_Length_1_cm:TreatmentSnail")) + xlim(c(-1.5,1.5))

# Fit the model
model <- lmer(Length_diff ~ Length_1_cm * Treatment + (1|Jar), 
             data = growth_data)
summary(model)

#####Length relative#####

ggscatter(growth_data, x = "Length_1_cm", y= "length_relative", color = "Treatment", add = "reg.line")

# Fit the model
model <- brm(length_relative ~ Length_1_cm * Treatment + (1|Jar), 
             data = growth_data, 
             family = gaussian(), 
             #    prior = priors,
             control = list(adapt_delta = 0.9),)

summary(model)


# Posterior predictive checks
#pp_check(model)
library(bayesplot)
posterior_samples <- posterior_samples(model)
mcmc_dens(posterior_samples, pars = c("b_Intercept", "b_Length_1_cm", "b_TreatmentSnail", "b_Length_1_cm:TreatmentSnail")) + xlim(c(-1.5,1.5))

# Fit the model
model <- lmer(length_relative ~ Length_1_cm * Treatment + (1|Jar), 
              data = growth_data)
summary(model)

#####Area#####

ggscatter(growth_data, x = "Area_1_cm_2", y= "Area_diff", color = "Treatment", add = "reg.line")

# Fit the model
model <- brm(Area_diff ~ Area_1_cm_2 * Treatment + (1|Jar), 
             data = growth_data, 
             family = gaussian(), 
             #    prior = priors,
             control = list(adapt_delta = 0.9),)

summary(model)


# Posterior predictive checks
#pp_check(model)
library(bayesplot)
posterior_samples <- posterior_samples(model)
mcmc_dens(posterior_samples, pars = c("b_Intercept", "b_Area_1_cm_2", "b_TreatmentSnail", "b_Area_1_cm_2:TreatmentSnail")) + xlim(c(-1.5,1.5))

# Fit the model
model <- lmer(Area_diff ~ Area_1_cm_2 * Treatment + (1|Jar), 
              data = growth_data)
summary(model)

#####Area relative#####
#growth_data$Area_1_cm_2 = log(growth_data$Area_1_cm_2)
ggscatter(growth_data, x = "Area_1_cm_2", y= "area_relative", color = "Treatment", add = "reg.line")

# Fit the model
model <- brm(area_relative ~ Area_1_cm_2 * Treatment + (1|Jar), 
             data = growth_data, 
             family = gaussian(), 
             #    prior = priors,
             control = list(adapt_delta = 0.9),)

summary(model)


# Posterior predictive checks
#pp_check(model)
library(bayesplot)
posterior_samples <- posterior_samples(model)
mcmc_dens(posterior_samples, pars = c("b_Intercept", "b_Area_1_cm_2", "b_TreatmentSnail", "b_Area_1_cm_2:TreatmentSnail")) + xlim(c(-1.5,1.5))

# Fit the model
model <- lmer(area_relative ~ Area_1_cm_2 * Treatment + (1|Jar), 
              data = growth_data)
summary(model)





















# 1. Generate new data for predictions
newdata <- expand.grid(Length_1_cm = seq(min(stats_data$Length_1_cm), max(stats_data$Length_1_cm), length.out = 100),
                       Treatment = unique(stats_data$Treatment))

# 2. Predict using this new data
predictions <- predict(model, newdata = newdata, re_formula = NA, allow_new_levels = TRUE)
predictions = as.data.frame(predictions)

# 3. Merge the predictions with the new data for plotting
newdata$Estimate = predictions$Estimate
newdata$Q2.5 = predictions$Q2.5
newdata$Q97.5 = predictions$Q97.5

# 4. Plot
library(ggplot2)

ggplot(stats_data, aes(x = Length_1_cm, y = Length_diff, color = Treatment)) +
  geom_point() +
  geom_line(data = newdata, aes(y = Estimate)) +
  geom_ribbon(data = newdata, aes(ymin = Q2.5, ymax = Q97.5, x = Length_1_cm), alpha = 0.2, inherit.aes = FALSE) +
  theme_minimal() +
  labs(title = "Scatter plot with Bayesian regression lines and intervals", y = "Length Difference", x = "Initial Length")

