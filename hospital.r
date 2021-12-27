install.packages("readxl")
library("readxl")
data <- read_excel("~/Desktop/hospital.xlsx")

dim(data)

str(data)

box_vals <- boxplot(data$`length of stay`, ylab = "length of stay")$out

outliers <- which(data$`length of stay` %in% box_vals)
data_new <- data[-outliers,]

qqnorm(data_new$`length of stay`, ylab = "length of stay", 
       pch = 1, frame = FALSE)
qqline(data_new$`length of stay`, col = "red", lwd = 2)

shapiro.test(data_new$`length of stay`)

t.test(data_new$`length of stay`, mu = 9)

data_rand <- data_new[sample(nrow(data_new), ceiling(0.8*nrow(data_new))),]

dim(data_rand)

str(data_rand)

data_rand$region <- factor(data_rand$region)
data_rand$`medical school affiliation`[data_rand$
          `medical school affiliation` %in% 2] <- 0

str(data_rand)

logit <- glm(`medical school affiliation` ~ `infection risk` + region + 
    `average daily census`, data = data_rand, family = binomial("logit"))

summary(logit)
confint(logit)


install.packages("aod")
library(aod) 
wald.test(b = coef(logit), Sigma = vcov(logit), Terms = 2:6)