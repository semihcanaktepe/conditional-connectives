# Conditional Connectives (218K184 Study 1)

# Load the dataset
cond_connectives <- read.csv("~/cond_connectives.csv")

# Check types
str(cond_connectives)

# Set the types if they are not as the following.
cond_connectives$subject <- as.factor(cond_connectives$subject)
cond_connectives$item <- as.factor(cond_connectives$item)
cond_connectives$list <- as.factor(cond_connectives$list)
cond_connectives$adult <- as.factor(cond_connectives$adult)
cond_connectives$group <- as.factor(cond_connectives$group)
cond_connectives$connective <- as.factor(cond_connectives$connective)
cond_connectives$ratio <- as.factor(cond_connectives$ratio)

# It should be numeric for visualization, We will convert it back to factor before glmm.
cond_connectives$ratio <- as.numeric(cond_connectives$ratio)

# VISUALISATION

# Here we draw the line graphs for each quantifier as in Nadathur & Lassiter (2015)
cond_connectives$ss <- 1
d1 <- aggregate(response ~ group + connective + ratio, data = cond_connectives, FUN = mean)
d2 <- aggregate(response ~ group + connective + ratio, data = cond_connectives, FUN = sd)
d3 <- aggregate(ss ~ group + connective + ratio, data = cond_connectives, FUN = sum)

d <- cbind(d1, sd = d2$response, ss = d3$ss)
alpha = 0.05
d$ci <- qt(p=alpha/2, df=d$ss,lower.tail=F)*(d$sd/sqrt(d$ss))
d$Low <- d$response - d$ci
d$High <- d$response + d$ci

ch <- subset(d, group == "child")
ch1 <- subset(ch, connective == "if")
ch2 <- subset(ch, connective == "ifnot")
ch3 <- subset(ch, connective == "unless")

ad <- subset(d, group == "adult")
ad1 <- subset(ad, connective == "if")
ad2 <- subset(ad, connective == "ifnot")
ad3 <- subset(ad, connective == "unless")

# NEW PLOTS with ADULTS
par(mfrow=c(1,3))
#IF
plot(ad1$ratio-0.025, ad1$response, type = "b", pch = 19, col= "red", xaxt='n', lwd=2, xlim = c(1,3), ylim = c(0,1), xlab = "Ratio", ylab = "Acceptance Rate", main = "If")
arrows(ad1$ratio-0.025, ad1$Low, ad1$ratio-0.025, ad1$High, length=0.05, angle=90, code=3, col = "red", lwd=2)
lines(ch1$ratio+0.025, ch1$response, type = "b", pch = 24, col = "blue", lwd=2)
arrows(ch1$ratio+0.025, ch1$Low, ch1$ratio+0.025, ch1$High, length=0.05, angle=90, code=3, col = "blue", lwd=2)
axis(1, at=c(1,2,3), labels=c("0.0", "0.4", "1.0"))
legend(2.45, 0.09, legend=c("Adults", "Children"), col=c("red", "blue"), lty = 1, pch = c(19,24), lwd=2)

#IF NOT
plot(ad2$ratio-0.025, ad2$response, type = "b", pch = 19, col= "red", xaxt='n', lwd=2, xlim = c(1,3), ylim = c(0,1), xlab = "Ratio", ylab = "Acceptance Rate", main = "If Not")
arrows(ad2$ratio-0.025, ad2$Low, ad2$ratio-0.025, ad2$High, length=0.05, angle=90, code=3, col = "red", lwd=2)
lines(ch2$ratio+0.025, ch2$response, type = "b", pch = 24, col = "blue", lwd=2)
arrows(ch2$ratio+0.025, ch2$Low, ch2$ratio+0.025, ch2$High, length=0.05, angle=90, code=3, col = "blue", lwd=2)
axis(1, at=c(1,2,3), labels=c("0.0", "0.4", "1.0"))
legend(2.45, 0.09, legend=c("Adults", "Children"), col=c("red", "blue"), lty = 1, pch = c(19,24), lwd=2)

#UNLESS
plot(ad3$ratio-0.025, ad3$response, type = "b", pch = 19, col= "red", xaxt='n', lwd=2, xlim = c(1,3), ylim = c(0,1), xlab = "Ratio", ylab = "Acceptance Rate", main = "Unless")
arrows(ad3$ratio-0.025, ad3$Low, ad3$ratio-0.025, ad3$High, length=0.05, angle=90, code=3, col = "red", lwd=2)
lines(ch3$ratio+0.025, ch3$response, type = "b", pch = 24, col = "blue", lwd=2)
arrows(ch3$ratio+0.025, ch3$Low, ch3$ratio+0.025, ch3$High, length=0.05, angle=90, code=3, col = "blue", lwd=2)
axis(1, at=c(1,2,3), labels=c("0.0", "0.4", "1.0"))
legend(2.45, 0.09, legend=c("Adults", "Children"), col=c("red", "blue"), lty = 1, pch = c(19,24), lwd=2)

mtext("Mean Acceptance Rates for Each Connective", side = 3, line = -1.25, outer = TRUE)

# To clean the plots and reset gridding, use,
dev.off()

## STATISTICAL ANALYSIS

# Switch ratio back to factor after visualizations.
cond_connectives$ratio <- as.character(cond_connectives$ratio)

# Rename the levels
cond_connectives$ratio[cond_connectives$ratio == "1"] <- "0.0"
cond_connectives$ratio[cond_connectives$ratio == "2"] <- "0.4"
cond_connectives$ratio[cond_connectives$ratio == "3"] <- "1.0"

# Here we convert it to factor.
cond_connectives$ratio <- as.factor(cond_connectives$ratio)
# Change the order of the levels.
cond_connectives$group <- factor(cond_connectives$group, levels = c("child", "adult"))

#Load the necessary package
install.packages("lme4")
library(lme4)

# This is the maximal random slope model. This model has singular error.
m1 <- glmer(response ~ group*connective*ratio + (1 + group | subject) + (1 + connective | item),
            data = cond_connectives, family = binomial, nAGQ = 0)
summary(m1)

# We remove the random intercept-slope correlation for item, but again the same problem.
m2 <- glmer(response ~ group*connective*ratio + (1 + group | subject) + (1 + connective || item),
            data = cond_connectives, family = binomial, nAGQ = 0)
summary(m2)

# We remove the random intercept-slope correlation for both subject and item, but again the same problem.
m3 <- glmer(response ~ group*connective*ratio + (1 + group || subject) + (1 + connective || item),
            data = cond_connectives, family = binomial, nAGQ = 0)
summary(m3)

# Reducing to random intercept only for item, yet again the same problem.
m4 <- glmer(response ~ group*connective*ratio + (1 + group | subject) + (1 | item),
            data = cond_connectives, family = binomial, nAGQ = 0)
summary(m4)

# Reducing to random intercept only for item and no intercept-slope correlation for subject,
# but the same problem again.
m5 <- glmer(response ~ group*connective*ratio + (1 + group || subject) + (1 | item),
            data = cond_connectives, family = binomial, nAGQ = 0)
summary(m5)

# We remove random slope of subject, same singular error.
m6 <- glmer(response ~ group*connective*ratio + (1 | subject) + (1 | item),
            data = cond_connectives, family = binomial, nAGQ = 0)
summary(m6)

# We remove random effect of item totally and add intercept-slope correlation back for subject.
# And it fits without the singular error.
m7 <- glmer(response ~ group*connective*ratio + (1 + group | subject),
            data = cond_connectives, family = binomial, nAGQ = 0)
summary(m7)

## Compare the random effect structures of maximal and final models.
### There is not significant difference between the models, but AIC and BIC scores
### of final model (m7) is better than the maximal model (m1). 
anova(m1,m7)

### Now we fit the NULL MODEL
m0 <- glmer(response ~ 1 + (1 + group | subject),
            data = cond_connectives, family = binomial, nAGQ = 0)

## Comparing maximal model that fits and the null model.
anova(m0,m7)

## Check the residuals

### Binned residual plot
install.packages("arm")
library(arm)

par(mfrow=c(1,2))

binnedplot(fitted(m7),
           residuals(m1, type = "response"), 
           nclass = NULL,
           ylim = c(-0.2,0.2),
           xlab = "Expected Values", 
           ylab = "Average Residual", 
           main = "Binned Residual Plot (Full Data)", 
           col.int = "gray")

### Classical Plots
plot(resid(m7), main = "Distributions of Residuals", xlab = "Residuals")

plot(fitted(m7), resid(m7), main = "Residuals vs. Fitted Values",
     xlab = "Fitted", ylab = "Residuals")

### Fitted values are on probability scale, so we can convert them into continuous scale
### using logit function.
logit <- function(p){log(p/(1-p))}
inv_logit <- function(x){exp(x)/(exp(x)+1)}

fitted <- logit(fitted(m7))

plot(fitted, resid(m7), main = "Residuals vs. Fitted Values",
     xlab = "Fitted (logit)", ylab = "Residuals")

## QQ plot of residuals
qqnorm(resid(m7), main="Normal Q-Q Plot")
qqline(resid(m7), col = "red")

## Histogram of residuals
x <- resid(m7) #Extract residuals
h <- hist(x, col = "white", xlab = "Residuals",
          main="Histogram of Residuals")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="red", lwd=3)

# Checking the influential participants and items.
subject_agg <- aggregate(response ~ subject, FUN = mean, data = cond_connectives)
item_agg <- aggregate(response ~ item, FUN = mean, data = cond_connectives)

# Remove participant 6, 7, 14, 18, 19, 20
cond_connectives2 <- cond_connectives

for (i in c("6", "7", "14", "18", "19", "20")){
  cond_connectives2 <- subset(cond_connectives2, subject != i)
  cond_connectives2
}

## Re-run the model
m7.2 <- glmer(response ~ group*connective*ratio + (1 + group | subject),
              data = cond_connectives2, family = binomial, nAGQ = 0)
summary(m7.2)

### Check for normality
par(mfrow=c(1,2))

binnedplot(fitted(m7.2), 
           residuals(m1, type = "response"), 
           nclass = NULL,
           ylim = c(-0.2,0.2),
           xlab = "Expected Values", 
           ylab = "Average Residual", 
           main = "Binned Residual Plot (Participants Excluded)", 
           col.int = "gray")

### Previous model's binned plot
binnedplot(fitted(m7),
           residuals(m1, type = "response"), 
           nclass = NULL,
           ylim = c(-0.2,0.2),
           xlab = "Expected Values", 
           ylab = "Average Residual", 
           main = "Binned Residual Plot (Full Data)", 
           col.int = "gray")

# Running a pairwise comparison among groups, connectives and ratios

## Load the necessary package
install.packages("emmeans")
install.packages("ggplot2")
library(emmeans)
library(ggplot2)

## Pairwise comparisons
marginal <- emmeans(m7, ~ group + connective + ratio)
pairs(marginal, adjust="tukey")

# Visualize the comparisons for better readability
p <- plot(marginal, ylab = "Variables", xlab = "Estimates with 95% CI", colors = "black", "black")
p + theme_bw() + labs(title = "Estimated Marginal Means")

# adult age
age <- c(33,23,34,35,22,23,35,36,21,37,22,22,22,22,22)


# Effect size calculation (Cohen's d)

## Variance of Residuals
var_resid <- var(resid(m7))

## Variance of random intercept of subject is 3.273, random slope of subject by group is 1.427.
var_sum <- 3.273 + 1.427 + var_resid

### Effect size of group (Very large)
d_group <- 3.64887/sqrt(var_sum)

### Effect sizes of connectives (Very small effect size)
d_con_ifnot <- 0.32564/sqrt(var_sum)
d_con_unless <- 0.43507/sqrt(var_sum)

### Effect size of ratios (Very small for 0.4 and very large for 1.0)
d_ratio_04 <- 0.10845/sqrt(var_sum)
d_ratio_10 <- 2.87826/sqrt(var_sum)

# Power Calculations

## Load necessary packages
install.packages("devtools")
library(devtools)
devtools::install_github("DejanDraschkow/mixedpower") 
library(mixedpower)

### Convert subject column from factor to numeric as "id".
cond_connectives$id <- as.numeric(cond_connectives$subject)

### Run the simulation with 30, 60, 90 and 120 participants.
power_of_m7_subject <- mixedpower(model = m7, data = cond_connectives,
                                  fixed_effects = c("group", "connective", "ratio"),
                                  simvar = "id", steps = c(30, 60, 90, 120),
                                  critical_value = 2)

### Convert item column from factor to numeric as "trial".
cond_connectives$trial <- as.numeric(cond_connectives$item)

### Run the simulation with 36, 48, 72 and 96 participants.
power_of_m7_item <- mixedpower(model = m7, data = cond_connectives,
                               fixed_effects = c("group", "connective", "ratio"),
                               simvar = "trial", steps = c(36, 48, 72, 96),
                               critical_value = 2)


## Alternative Analysis (based on m1, which has both subject and item random intercept and slope)
### Power calculations were made at https://jakewestfall.shinyapps.io/two_factor_power/

### Calculate VPC values.
VPC_interpart <- 3.273/var_sum
VPC_slopepart <- 1.1427/var_sum
VPC_interitem <- 0.000000000002616/var_sum
VPC_slopeitem <- 0.000000000003149/var_sum
VPC_resid <- var_resid/var_sum

# 0.995 for detecting group difference (high power)
# 0.168 for detecting connector difference (low power)
# 0.947 for detecting ratio difference (high power)

# Simulations for power analysis
### Create simulation data

N1 <- 30 # Number of participant
N2 <- 72 # Number of items
subject <- rep(1:N1, each=N2) # subject index
item <- rep(1:N2, times=N1) # item index
group <- rep(c("child","adult"), each=N1*N2/2)  # group
connective <- rep(rep(c("if", "ifnot", "unless"), each=3), times=N1*N2/9) # connective
ratio <- rep(c("0.0", "0.4", "1.0"), times=N1*N2/3) # ratio
simdat <- data.frame(subject=as.numeric(subject), group=as.factor(group),
                item=as.numeric(item), connective=as.factor(connective),
                ratio=as.factor(ratio)) # generate the dataset


b <- c(0.1, 0.5, 0.5) # fixed intercept and slope
V <- matrix(c(0.5,0.05,0.05,
              0.05,0.5,0.05,
              0.05,0.05,0.5), 3) # random intercept and slope variance-covariance matrix

model1 <- makeGlmer(y ~ connective + (1 + connective | item), family="binomial", fixef=b, VarCorr=V, data=simdat)
print(model1)


power_of_sim <- mixedpower(model = model1, data = simdat,
                          fixed_effects = c("connective"),
                          simvar = "subject", steps = c(30, 60),
                          critical_value = 2, SESOI = b)

SessionInfo()
