### Norming Analysis
# Semih Can Aktepe

# Load the dataset
norming <- read.csv("~/Norming.csv")

# Change the data format from wide to long
install.packages("reshape2")
library(reshape2)

## Change column names
colnames(norming)[1:5] <- c("subject", "volunteer", "age", "sex", "education")

### Some demographic information
min_age <- min(norming$age)
max_age <- max(norming$age)
mean_age <- mean(norming$age)
sd_age <- sd(norming$age)

## Long format transformation
d <- melt(norming, id.vars=c("subject", "volunteer", "age", "sex", "education"))

## Add new column names
colnames(d)[6:7] <- c("sentence", "response")

### Add id variable
d$id <- as.factor(as.integer(as.factor(d$subject)))
d <- d[order(d$id),]

## Load variable assignment dataset
norming_list <- read.csv("~/norming_list.csv", sep=";")

## Extend it as long as the main dataset
long_norming_list <- do.call("rbind", replicate(nrow(d)/54, norming_list, simplify = FALSE))

## Bind the columns of the d and long_norming_list
d <- cbind(d, long_norming_list)

### Adjust column order
col_order <- c("id", "age", "sex", "education", "item",
               "condition", "connective", "response",
               "sentence", "volunteer", "subject")
d <- d[, col_order]

### Change data types
d$subject <- as.factor(d$subject)
d$sex <- as.factor(d$sex)
d$edu <- as.factor(d$edu)
d$response <- as.factor(d$response)
d$item <- as.factor(d$item)
d$condition <- as.factor(d$condition)
d$connective <- as.factor(d$connective)
d$frequency <- 1

### Save the data
write.csv(d, "~/d.csv", row.names=TRUE)

# VISUALIZATION

## Aggregate data for visualization
agg_data <- aggregate(d, frequency ~ condition + connective + response, FUN=sum)

### Group by condition
gram <- subset(agg_data, condition == "grammatical")
expr <- subset(agg_data, condition == "experimental")
ungr <- subset(agg_data, condition == "ungrammatical")

#### Group grammatical by connective
gram_if <- subset(gram, connective == "if")
gram_ifnot <- subset(gram, connective == "ifnot")
gram_unless <- subset(gram, connective == "unless")

#### Group experimental by connective
expr_if <- subset(expr, connective == "if")
expr_ifnot <- subset(expr, connective == "ifnot")
expr_unless <- subset(expr, connective == "unless")

#### Group ungrammatical by connective
ungr_if <- subset(ungr, connective == "if")
ungr_ifnot <- subset(ungr, connective == "ifnot")
ungr_unless <- subset(ungr, connective == "unless")

## VISUALIZATION

### First all responses
total <- aggregate(d, frequency ~ response, FUN = sum)

barplot(total$frequency, names.arg = 1:7,
        main = "Distribution of Responses", xlab = "Response", ylab = "Frequency",
        col = "dodgerblue3", space = 1)
abline(h=0)

### Now by condition and connective
par(mfrow = c(3, 3))

### Grammatical Control
barplot(gram_if$frequency, names.arg = 1:7,
        main = "Grammatical-If", xlab = "Response", ylab = "Frequency",
        col = "dodgerblue3", space = 1)
abline(h=0)

barplot(gram_ifnot$frequency, names.arg = 1:7,
        main = "Grammatical-If Not", xlab = "Response", ylab = "Frequency",
        col = "dodgerblue3", space = 1)
abline(h=0)

barplot(gram_unless$frequency, names.arg = 1:7,
        main = "Grammatical-Unless", xlab = "Response", ylab = "Frequency",
        col = "dodgerblue3", space = 1)
abline(h=0)

### Experimental Sentences
barplot(expr_if$frequency, names.arg = 1:7,
        main = "Experimental-If", xlab = "Response", ylab = "Frequency",
        col = "dodgerblue3", space = 1)
abline(h=0)

barplot(expr_ifnot$frequency, names.arg = 1:7,
        main = "Experimental-If Not", xlab = "Response", ylab = "Frequency",
        col = "dodgerblue3", space = 1)
abline(h=0)

barplot(expr_unless$frequency, names.arg = 1:7,
        main = "Experimental-Unless", xlab = "Response", ylab = "Frequency",
        col = "dodgerblue3", space = 1)
abline(h=0)

### Ungrammatical Control
barplot(ungr_if$frequency, names.arg = 1:7,
        main = "Ungrammatical-If", xlab = "Response", ylab = "Frequency",
        col = "dodgerblue3", space = 1)
abline(h=0)

barplot(ungr_ifnot$frequency, names.arg = 1:7,
        main = "Ungrammatical-If Not", xlab = "Response", ylab = "Frequency",
        col = "dodgerblue3", space = 1)
abline(h=0)

barplot(ungr_unless$frequency, names.arg = 1:7,
        main = "Ungrammatical-Unless", xlab = "Response", ylab = "Frequency",
        col = "dodgerblue3", space = 1)
abline(h=0)
###

# STATISTICAL INFERENCE

## Load the necessary package to fit linear mixed effects model with ordinal categorical distribution
## Cumulative Link Mixed Model
install.packages("ordinal")
library(ordinal)

## Fit the model
m <- clmm(response ~ condition*connective + (1 | id) + (1 | item), data = d, threshold = "flexible")
summary(m)

## Pairwise comparison

### Load the necessary packages for pairwise comparison
install.packages("emmeans")
library(emmeans)

## Post-hoc analysis
marginal <- emmeans(m, ~ condition + connective)
pairs(marginal, adjust="tukey")

# Visualize the comparisons for better readability
p <- plot(marginal, ylab = "Variables", xlab = "Estimates with 95% CI", colors = "black", "black")
p + theme_bw() + labs(title = "Estimated Marginal Means")

