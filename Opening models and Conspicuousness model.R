A2<-read.table("All.txt",header=TRUE)
as.factor(A2$ID)
as.integer(A2$Rank)
as.factor(A2$Age)
as.factor(A2$Sex)
as.factor(A2$Group)
A2

library(brms)

#Opening method
A2$Open <- factor(A2$Open)
A2$ID <- factor(A2$ID)

o <- brm(
  Open ~ 1 + (1 | ID),
  data = A2,
  family = categorical(),
  cores = 4
)
summary(o)


o2 <- brm(
  Open ~ Rank + Age + Sex + Group + (1 | ID),
  data = A2,
  family = categorical()
  )
summary(o2)



A2$Drink <- ordered(A2$Drink, levels = 0:4)
A2$ID <- factor(A2$ID)

A2
d_consp <- brm(
  Drink ~ Rank + (1 | ID),
  data = A2,
  family = cumulative("logit")
)
summary(d_consp)



rank_seq <- seq(
  min(A2$Rank),
  max(A2$Rank),
  length.out = 100
)
newdata <- data.frame(
  Rank = rank_seq,
  ID = NA   
)
pred <- fitted(
  d_consp,
  newdata = newdata,
  re_formula = NA,   
  scale = "response"
)
library(dplyr)
library(tidyr)



pred_df <- cbind(newdata, pred)

pred_long <- pred_df %>%
  pivot_longer(
    cols = -Rank,
    names_to = c(".value", "Category"),
    names_pattern = "(Estimate|Q2.5|Q97.5)\\.P\\(Y = (.+)\\)"
  ) %>%
  filter(!is.na(Category), !is.na(Q2.5), !is.na(Q97.5))

pred_long$Category <- factor(pred_long$Category, levels = c("1","2","3","4"))

ggplot(pred_long, aes(x = Rank, y = Estimate, 
                      colour = Category, 
                      linetype = Category,
                      group = Category)) +
  
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = Category),
              alpha = 0.2, colour = NA) +
  
  geom_line(linewidth = 1.2) +
  
  scale_x_continuous(breaks = seq(min(A2$Rank), max(A2$Rank), by = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  
  scale_colour_manual(values = c(
    "1" = "red",  
    "2" = "black",  
    "3" = "blue",  
    "4" = "yellow"
  ), name = "Conspicuousness") +
  
  scale_fill_manual(values = c(
    "1" = "red4",  
    "2" = "black",  
    "3" = "blue4",  
    "4" = "yellow4"
  ), name = "Conspicuousness") +
  
  scale_linetype_manual(values = c(
    "1" = "solid",
    "2" = "dashed",
    "3" = "dotdash",
    "4" = "twodash"
  ), name = "Conspicuousness") +
  
  labs(x = "Dominance Rank", y = "Predicted Probability") +
  theme_minimal()

