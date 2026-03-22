A<-read.table("All2.txt",header=TRUE)
as.factor(A$ID)
as.integer(A$Rank)
as.factor(A$Age)
as.factor(A$Sex)
as.factor(A$Group)
A

library(brms)


#Drinking method
A$Drink <- factor(A$Drink)
A$ID <- factor(A$ID)

d <- brm(
  Drink ~ 1 + (1 | ID),
  data = A,
  family = categorical(),
  cores = 4
)
summary(d)
d

# predictor of choice
d2 <- brm(
  Drink ~ Rank + Age + Sex + Group + (1 | ID),
  data = A,
  family = categorical()
)
summary(d2)
par(mfrow=c(2,1))

A_group <- subset(A, Group == "Toropo")
A_group
rank_df <- aggregate(cbind(Rank) ~ ID + Sex, data = A_group, FUN = mean)
rank_df$Sex <- factor(rank_df$Sex, levels = c("M","F"))
rank_df <- rank_df[order(rank_df$Sex, rank_df$Rank), ]
ordered_IDs <- rank_df$ID
tab <- table(A_group$ID, A_group$Drink)
tab <- tab[ordered_IDs, ]
prop_tab <- prop.table(tab, margin = 1)
n_per_ID <- rowSums(tab)
labels <- paste(ordered_IDs, sep=" ")
par(mar = c(4,3.5,5,8),mgp = c(2, 0.5, 0), xpd=TRUE)   
cols <- c("#4E79A7","#F28E2B","grey","#76B7B2","#59A14F","#EDC948")
colnames(prop_tab) <- c("Pour on floor",
                        "Bowl ground",
                        "Head on floor",
                        "Bowl elevated",
                        "Overhead nor",
                        "Overhead oth")
bar_positions <- barplot(t(prop_tab),
                         col = cols,
                         border = NA,
                         xaxt = "n",
                         yaxt = "n")



axis(1,
     at = bar_positions,
     labels = labels,
     line = -0.2,
     las = 2,
     cex.axis = 1)
axis(2, line = -1.2)
text(x = bar_positions,
     y = 1.02,
     labels = paste("n=", n_per_ID, sep=""),
     cex = 0.9)
legend("right",
       inset = c(-0.45,0),
       legend = colnames(prop_tab),
       fill = cols,
       bty = "n",
       xpd = TRUE)
title(xlab = "Individual",
      line = 2.5,
      ylab = "Proportion",
      main = "Toropo Drinking")


A

B_group <- subset(A, Group == "Sehlalah")
B_group
rank_df <- aggregate(cbind(Rank) ~ ID + Sex, data = B_group, FUN = mean)
rank_df$Sex <- factor(rank_df$Sex, levels = c("M","F"))
rank_df <- rank_df[order(rank_df$Sex, rank_df$Rank), ]
ordered_IDs <- rank_df$ID
tab <- table(B_group$ID, B_group$Drink)
tab <- tab[ordered_IDs, ]
prop_tab <- prop.table(tab, margin = 1)
n_per_ID <- rowSums(tab)
labels <- paste(ordered_IDs, sep=" ")
par(mar = c(4,3.5,5,8),mgp = c(2, 0.5, 0), xpd=TRUE) 
cols <- c("#4E79A7","#F28E2B","grey","#76B7B2","#59A14F","#EDC948")
colnames(prop_tab) <-  c("Pour on floor",
                         "Bowl ground",
                         "Head on floor",
                         "Bowl elevated",
                         "Overhead nor",
                         "Overhead oth")
bar_positions <- barplot(t(prop_tab),
                         col = cols,
                         border = NA,
                         xaxt = "n",
                         yaxt = "n")

axis(1,
     at = bar_positions,
     labels = labels,
     line = -0.2,
     las = 2,
     cex.axis = 1)
axis(2, line = -1.2)
text(x = bar_positions,
     y = 1.02,
     labels = paste("n=", n_per_ID, sep=""),
     cex = 0.9)
legend("right",
       inset = c(-0.40,0),
       legend = colnames(prop_tab),
       fill = cols,
       bty = "n",
       xpd = TRUE)
title(xlab = "Individual",
      ylab = "Proportion",
      main = "Sehlahla Drinking",
      line =2.5)



C_group <- subset(A, Group == "Toropo")
C_group
rank_df <- aggregate(cbind(Rank) ~ ID + Sex, data = C_group, FUN = mean)
rank_df$Sex <- factor(rank_df$Sex, levels = c("M","F"))
rank_df <- rank_df[order(rank_df$Sex, rank_df$Rank), ]
ordered_IDs <- rank_df$ID
tab <- table(C_group$ID, C_group$Open)
tab <- tab[ordered_IDs, ]
prop_tab <- prop.table(tab, margin = 1)
n_per_ID <- rowSums(tab)
labels <- paste(ordered_IDs, sep=" ")
par(mar = c(4,3.5,5,8),mgp = c(2, 0.5, 0), xpd=TRUE) 
cols <- c("#4E79A7","#F28E2B","grey","#76B7B2","#59A14F","#EDC948")
colnames(prop_tab) <- c("Already open",
                        "Make new hole",
                        "Pull up mouth",
                        "Rip lid teeth",
                        "Unscrew hand",
                        "Unscrew mouth")
                        
                        
bar_positions <- barplot(t(prop_tab),
                         col = cols,
                         border = NA,
                         xaxt = "n",
                         yaxt = "n")

axis(1,
     at = bar_positions,
     labels = labels,
     las = 2,
     cex.axis = 1)
axis(2, line = - 1.2)
text(x = bar_positions,
     y = 1.02,
     labels = paste("n=", n_per_ID, sep=""),
     cex = 0.9)
legend("right",
       inset = c(-0.43,0),
       legend = colnames(prop_tab),
       fill = cols,
       bty = "n",
       xpd = TRUE)
title(xlab = "Individual",
      ylab = "Proportion",
      main = "Toropo Opening",
      line = 2.5)





D_group <- subset(A, Group == "Sehlalah")
D_group
rank_df <- aggregate(cbind(Rank) ~ ID + Sex, data = D_group, FUN = mean)
rank_df$Sex <- factor(rank_df$Sex, levels = c("M","F"))
rank_df <- rank_df[order(rank_df$Sex, rank_df$Rank), ]
ordered_IDs <- rank_df$ID
tab <- table(D_group$ID, D_group$Open)
tab <- tab[ordered_IDs, ]
prop_tab <- prop.table(tab, margin = 1)
n_per_ID <- rowSums(tab)
labels <- paste(ordered_IDs, sep=" ")
par(mar = c(4,3.5,5,8),mgp = c(2, 0.5, 0), xpd=TRUE) 
cols <- c("#4E79A7","#F28E2B","purple3", "grey","#76B7B2","#59A14F","#EDC948","pink")
colnames(prop_tab) <- c("Already open",
                        "Make new hole",
                        "Pull up",
                        "Pull up mouth",
                        "Rip lid teeth",
                        "Unscrew hand",
                        "Unscrew mouth",
                        "Smash")


bar_positions <- barplot(t(prop_tab),
                         col = cols,
                         border = NA,
                         xaxt = "n",
                      yaxt = "n")

axis(1,
     at = bar_positions,
     labels = labels,
     las = 2,
     cex.axis = 1)
axis(2, line = - 1.2)
text(x = bar_positions,
     y = 1.02,
     labels = paste("n=", n_per_ID, sep=""),
     cex = 0.9)
legend("right",
       inset = c(-0.43,0),
       legend = colnames(prop_tab),
       fill = cols,
       bty = "n",
       xpd = TRUE)
title(xlab = "Individual",
      ylab = "Proportion",
      main = "Sehlahla Open",
      line =2.5)

