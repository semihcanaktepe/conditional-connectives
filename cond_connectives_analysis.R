# Conditional Connectives (218K184 Study 1)
# Semih Can Aktepe

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

library(lme4)
# This is the maximal random slope model. Does not converge. So we reduce it to a random intercept only model.
m0 <- glmer(response ~ group*connective*ratio + (1 + group | subject) + (1 + connective | item),
            data = cond_connectives, family = binomial, nAGQ = 0)

summary(m0)

# Running a pairwise comparison among groups, connectives and ratios

## Load the necessary package
library(emmeans)

## Pairwise comparisons
marginal <- emmeans(m0, ~ group + connective + ratio)
pairs(marginal, adjust="tukey")

# Visualize the comparisons for better readability
p <- plot(marginal, ylab = "Variables", xlab = "Estimates with 95% CI", colors = "black", "black")
p + theme_bw() + labs(title = "Estimated Marginal Means")




