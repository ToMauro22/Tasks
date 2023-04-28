setwd('C:\\Users\\migma\\OneDrive\\Desktop\\Evolution\\Tasks\\Task_10')
x <- rnorm(n = 100, mean = 0, sd = 2)
y <- x * 5 + 2 + runif(100, min = 0, max = 0.1)
model <- lm(y ~ x)
summary(model)
slope <- vector("numeric", 100)
intercept <- vector("numeric", 100)
z <- vector("numeric", 100)
for (i in 1:100){
  x <- rnorm(n = 100, mean = 0, sd =2)
  z[i] <- runif(1, min = 0.5, max = 2)
  y < - x * 5 * z[i] + 2 + runif(100, min = 0, max = 0.1)
  model <- lm(y ~x)
  slope[i] <- coef(model)[2] * z[i]
  intercept[i] <- coef(model)[1] * z[i] + 2
}
plot(z, slope, xlab = "z", ylab = "Estimated Slope")
abline(lm(slope ~ z), col = "blue")
n <- 10000
prize <- sample(c("1", "2", "3"), size = n, replace = TRUE)
opened_door <- ifelse(prize=="1", sample(c("2","3"), size = n, replace = TRUE), ifelse(prize == "2", "3", "2"))
closed_door <- sum(prize == "1")/n  
same_door <- sum(prize == "1")/n
diff_door <- sum(prize == closed_door)/n
win_frequency <- c(same_door, diff_door)
barplot(win_frequency, names.arg = c("Same Door", "Different Door"), ylab = "Frequency of Wins (10,000 Runs)", ylim = c(0, 0.8), main = "Chance of Winning the Grand Prize", col = "lightpink")
u <- "C:/Users/migma/OneDrive/Pictures/Screenshots/Screenshot (280).png"
meme(u, "", "When the kiwi fruit has an egg", size = 1)

