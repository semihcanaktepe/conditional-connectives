# Conditional Connectives (218K184 Study 1)
# Semih Can Aktepe

# Load the dataset
load("cond_connectives.rdata")

# Check types
str(cond_connectives)

# Set the types if they are not as the following.
cond_connectives$subject <- as.factor(cond_connectives$subject)
cond_connectives$item <- as.factor(cond_connectives$item)
cond_connectives$list <- as.factor(cond_connectives$list)
cond_connectives$connective <- as.factor(cond_connectives$connective)
cond_connectives$ratio <- as.factor(cond_connectives$ratio)

# It should be numeric for visualization, We will convert it back to factor before glmm.
cond_connectives$ratio <- as.numeric(cond_connectives$ratio)

# VISUALISATION

# Here we draw the line graphs for each quantifier as in Nadathur & Lassiter (2015)
cond_connectives$ss <- 1
d1 <- aggregate(response ~ connective + ratio, data = cond_connectives, FUN = mean)
d2 <- aggregate(response ~ connective + ratio, data = cond_connectives, FUN = sd)
d3 <- aggregate(ss ~ connective + ratio, data = cond_connectives, FUN = sum)

d <- cbind(d1, sd = d2$response, ss = d3$ss)
alpha = 0.05
d$ci <- qt(p=alpha/2, df=d$ss,lower.tail=F)*(d$sd/sqrt(d$ss))
d$Low <- d$response - d$ci
d$High <- d$response + d$ci

ch1 <- subset(d, connective == "if")
ch2 <- subset(d, connective == "ifnot")
ch3 <- subset(d, connective == "unless")

# RAW PLOT
plot(ch1$ratio-0.05, ch1$response, type = "b", pch = 15, col= "red", xaxt='n',xlim = c(1,3), ylim = c(0,1), xlab = "Ratio", ylab = "Acceptance Rate", main = "Mean Acceptance Rates For Each Connective")
arrows(ch1$ratio-0.05, ch1$Low, ch1$ratio-0.05, ch1$High, length=0.05, angle=90, code=3, col = "red")
lines(ch2$ratio, ch2$response, type = "b", pch = 16, col = "blue")
arrows(ch2$ratio, ch2$Low, ch2$ratio, ch2$High, length=0.05, angle=90, code=3, col = "blue")
lines(ch3$ratio+0.05, ch3$response, type = "b", pch = 17, col = "green")
arrows(ch3$ratio+0.05, ch3$Low, ch3$ratio+0.05, ch3$High, length=0.05, angle=90, code=3, col = "green")
axis(1, at=c(1,2,3), labels=c("0.0", "0.4", "1.0"))
legend(2.5, 0.2, legend=c("If", "If Not", "Unless"), col=c("red", "blue", "green"), lty = 1, pch = c(15,16,17))


# To clean the plots and reset gridding, use,
dev.off()

## STATISTICAL ANALYSIS

# Switch ratio back to factor after visualizations.
cond_connectives$ratio <- as.character(cond_connectives$ratio)

# Rename the levels
cond_connectives$ratio[cond_connectives$ratio == "1"] <- "0.0"
cond_connectives$ratio[cond_connectives$ratio == "2"] <- "0.4"
cond_connectives$ratio[cond_connectives$ratio == "3"] <- "1.0"

#Here we convert it to factor.
cond_connectives$ratio <- as.factor(cond_connectives$ratio)

library(lme4)
# This is the maximal random slope model. Converges well.
m0 <- glmer(response ~ connective*ratio + (1 + connective | subject) + (1 + connective | item),
            data = cond_connectives, family = binomial, nAGQ = 1)

# As there is no interaction, we remove the interaction term from the model.
m1 <- glmer(response ~ connective + ratio + (1 + connective | subject) + (1 + connective | item),
            data = cond_connectives, family = binomial, nAGQ = 1)

# Finally, as there is no effect of connective, we remove the connective from the model.
m2 <- glmer(response ~ ratio + (1 + ratio | subject) + (1 + ratio | item),
            data = cond_connectives, family = binomial, nAGQ = 1)

# Reducing nAGQ parameter to 0 to fit the model at the cost of precision.
m2n <- glmer(response ~ ratio + (1 + ratio | subject) + (1 + ratio | item),
            data = cond_connectives, family = binomial, nAGQ = 0)

# m2 fails to converge, so we remove the random slope and continue with the random intercept model
m3 <- glmer(response ~ ratio + (1 | subject) + (1 | item),
            data = cond_connectives, family = binomial, nAGQ = 1)

# Model comparison
anova(m0, m1, m2, m3)

# Running a pairwise comparison among ratios
library(multcomp)
g1 <- glht(m0, linfct = mcp(ratio = "Tukey"))
summary(g1)







