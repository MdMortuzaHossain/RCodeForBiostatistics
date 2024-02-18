Author: Md Mortuza Hossain<br>
#**One_way ANOVA** <br>
library(agricolae)<br>
group_A <- c(23, 20, 27, 24, 35, 28, 22, 25, 33, 21)<br>
group_B <- c(31, 22, 34, 27, 28, 25, 26, 30, 29, 23)<br>
group_C <- c(29, 30, 28, 32, 31, 27, 33, 26, 34, 35)<br>
data <- data.frame(<br>
  score = c(group_A, group_B, group_C),<br>
  group = factor(rep(c("A", "B", "C"), each = 10)))<br>
anova_result <- aov(score ~ group, data = data)<br>
summary(anova_result)<br><br>

#**T Test** <br>
group_A <- c(23, 20, 27, 24, 35, 28, 22, 25, 33, 21)<br>
group_B <- c(31, 22, 34, 27, 28, 25, 26, 30, 29, 23)<br>
t.test_A_B <- t.test(group_A, group_B, var.equal = TRUE) <br>
print(t.test_A_B)<br><br>


#**DMRT (Duncan's multiple range test**) <br>
library(agricolae)<br>
group_A <- c(23, 20, 27, 24, 35, 28, 22, 25, 33, 21)<br>
group_B <- c(31, 22, 34, 27, 28, 25, 26, 30, 29, 23)<br>
group_C <- c(29, 30, 28, 32, 31, 27, 33, 26, 34, 35)<br>
data <- c(group_A, group_B, group_C)<br>
groups <- factor(rep(c("A", "B", "C"), each=10))<br>
anova_result <- aov(data ~ groups)<br>
duncan_result <- duncan.test(anova_result, "groups", alpha=0.05)<br>
print(duncan_result)<br><br>


#**Linear effect** <br>
your_data_frame <- data.frame(<br>
  TRT = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4),<br>
  Totalmercaptan = c(23, 20, 27, 24, 35, 28, 22, 25, 33, 21, 31, 22, 34, 27, 28, 25)<br>
)<br>
model <- lm(Totalmercaptan ~ TRT + I(TRT^2), data = your_data_frame)<br>
summary(model)<br><br>


#**bar diagram (colorful)** <br>
library(ggplot2)<br>
group_A <- c(23, 20, 27, 24, 35, 28, 22, 25, 33, 21)<br>
group_B <- c(31, 22, 34, 27, 28, 25, 26, 30, 29, 23)<br>
group_C <- c(29, 30, 28, 32, 31, 27, 33, 26, 34, 35)<br>

data <- data.frame(<br>
  score = c(group_A, group_B, group_C),<br>
  group = factor(rep(c("A", "B", "C"), each = 10))<br>
)<br>
ggplot(data, aes(x = group, y = score)) + <br>
  geom_boxplot() +<br>
  labs(title = "Boxplot of Scores by Group",<br>
       x = "Group",<br>
       y = "Score") +<br>
  theme_minimal()<br><br>



#**bar diagram (black white)** <br>
library(ggplot2)<br>
group_A <- c(23, 20, 27, 24, 35, 28, 22, 25, 33, 21)<br>
group_B <- c(31, 22, 34, 27, 28, 25, 26, 30, 29, 23)<br>
group_C <- c(29, 30, 28, 32, 31, 27, 33, 26, 34, 35)<br>
<br>
data <- data.frame(<br>
  score = c(group_A, group_B, group_C),<br>
  group = factor(rep(c("A", "B", "C"), each = 10))<br>
)<br>
ggplot(data, aes(x = group, y = score, fill = group)) + <br>
  geom_boxplot() +<br>
  labs(title = "Boxplot of Scores by Group",<br>
       x = "Group",<br>
       y = "Score") +<br>
  theme_minimal() +<br>
  scale_fill_brewer(palette = "Pastel1")<br><br>

