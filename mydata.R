library(readr)
df <- read.csv("mydata/My_Data.csv")
head(df)
print(nrow(df))

summary(df)
df$Tasks <- as.numeric(df$Tasks)
df$AI_Impact_num <- as.numeric(gsub("%", "", df$AI.Impact))

pearson_result <- cor.test(df$Tasks, df$AI_Impact_num, method = "pearson")
pearson_result

spearman_result <- cor.test(df$Tasks, df$AI_Impact_num, method = "spearman")
spearman_result

plot(
  df$Tasks,
  df$AI_Impact_num,
  main = "Scatterplot of Tasks vs AI Impact (%)",
  xlab = "Number of Tasks",
  ylab = "AI Impact (%)",
  pch = 19,
  col = "blue"
)

model <- lm(AI_Impact_num ~ Tasks, data = df)
abline(model, col = "red", lwd = 2)

hist(
  df$AI_Impact_num,
  breaks = 20,
  probability = TRUE,
  col = "steelblue",
  border = "black",
  main = "Histogram of AI Impact (%) with Normal Curve",
  xlab = "AI Impact (%)",
  xlim = c(0, 100)
)


mean_value <- mean(df$AI_Impact_num, na.rm = TRUE)
sd_value <- sd(df$AI_Impact_num, na.rm = TRUE)

curve(
  dnorm(x, mean = mean_value, sd = sd_value),
  col = "red",
  lwd = 2,
  add = TRUE,
  xlim = c(0,100)
)