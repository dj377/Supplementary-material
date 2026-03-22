
W <- as.matrix(read.table("SFM.txt",header = TRUE, row.names =1 ))
W

n <- nrow(W)


P <- matrix(0, n, n)
for(i in 1:n){
  for(j in 1:n){
    if(i != j){
      total <- W[i,j] + W[j,i]
      if(total > 0){
        P[i,j] <- W[i,j] / total
      } else {
        P[i,j] <- 0
      }
    }
  }
}


W_i <- rowSums(P, na.rm = TRUE)
L_i <- colSums(P, na.rm = TRUE)


wW <- sapply(1:n, function(i) sum(P[i,] * W_i))
wL <- sapply(1:n, function(i) sum(P[,i] * L_i))


DS <- W_i + wW - L_i - wL
names(DS) <- rownames(W)


nDS <- (DS - min(DS)) / (max(DS) - min(DS))


DS_df <- data.frame(
  Individual = rownames(W),
  DS = DS,
  nDS = nDS
)

DS_df[order(-DS_df$DS), ]  


DS_sorted <- DS_df[order(-DS_df$DS), ]

DS_sorted <- DS_df[order(-DS_df$DS), ]




plot(
  x = 1:nrow(DS_sorted),
  y = DS_sorted$nDS,
  type = "b",        
  pch = 16,
  xaxt = "n",
  xlab = "",
  ylab = "Normalized David's Score (0–1)",
  main = "Sehlahla Females",
  cex.main=1.5,
  cex.lab=1.2,
  ylim = c(0, 1),
  mgp=c(2.5,1,0)
)
mtext(text = "Individual ID",
      side = 1,line = 3.5,cex=1.2)


axis(
  side = 1,
  at = 1:length(DS_sorted$Individual),
  labels = DS_sorted$Individual,
  las = 2,
  cex.axis = 1.1
)


segments(
  x0 = 1:length(DS_sorted$nDS),
  y0 = 0,
  x1 = 1:length(DS_sorted$nDS),
  y1 = DS_sorted$nDS,
  col = "grey70"
)


points(
  DS_sorted$nDS,
  pch = 16,
  cex = 1.2
)




