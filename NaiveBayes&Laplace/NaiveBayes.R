install.packages("e1071")
library(e1071)

dat <- tribble(~Spam, ~Viagra, ~Money,
               T, F, T,
               T, T, F,
               T, T, T,
               F, F, T,
               F, T, T,
               T, T, F)

naiveBayes(Spam ~ ., data = dat, laplace = 0)
naiveBayes(Spam ~ ., data = dat, laplace = 1)

model <- naiveBayes(Spam ~ ., data = dat, laplace = 1)
predict(model, data.frame(Viagra = c(T,F), Money = c(T,F)), type = "raw")

words = c(100, 200, 300, 1000, 20, 10)
dat2 <- cbind(dat, words)
model2 <- naiveBayes(Spam ~ ., data = dat2, laplace = 0)
model2

predict(model, data.frame(Viagra = c(T,F), Money = c(T,F), words = c(999, 2)), type = "raw")
predict(model, data.frame(Viagra = c(T,F), Money = c(T,F), words = c(4, 200)), type = "raw")
