

data <- read.csv("~/Downloads/blood+transfusion+service+center/transfusion.data")
colnames(data)
model1 <- glm(whether.he.she.donated.blood.in.March.2007 ~ Recency..months.,
              data = data,
              family = binomial)

summary(model1)


rec <- seq(min(data$Recency..months.), max(data$Recency..months.), length.out = 100)

pred_prob <- predict(model1,
                     newdata = data.frame(Recency..months. = rec),
                     type = "response")

plot(rec, pred_prob, type = "l",
     xlab = "Recency (months)",
     ylab = "Predicted Probability of Donating")

model_full <- glm(whether.he.she.donated.blood.in.March.2007 ~ Recency..months. + Frequency..times. + Monetary..c.c..blood. + Time..months.,
                  data = data,
                  family = binomial)

summary(model_full)

