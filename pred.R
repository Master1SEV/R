com_list <- list()

for (j in 1:4) {
  nPred <- j # количество предикторов
  com <-
    combn(ncol(tab) - 1, nPred) # таблица комбинаций предикторов 5 из 30
  
  n <- ncol(com)
  r <- numeric(n)
  a <- numeric(n)
  b <- numeric(n)
  d <- numeric(n)
  
  for (i in 1:n) {
    m <- lm(tab$NPL ~ ., data = tab[, 2:ncol(tab)][com[, i]])
    a[i] <- AIC(m)
    b[i] <- BIC(m)
    r[i] <- summary(m)$adj.r.squared # adj.r.squared
    d[i] <- deviance(m)
  }
  if (j == 1) {
    com_list[[j]] <-
      data.frame(
        X1 = t(com),
        AIC = a,
        BIC = b,
        R2 = r,
        Disp = d
      ) %>% arrange(desc(R2))
  } else {
    com_list[[j]] <-
      data.frame(
        t(com),
        AIC = a,
        BIC = b,
        R2 = r,
        Disp = d
      ) %>% arrange(desc(R2))
  }
}
