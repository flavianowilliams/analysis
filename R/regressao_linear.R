rm(list=ls())

graphics.off()

library(tidyverse)
library(plotly)

# adicione os valores para x e y
x = c(10,20,20,40)
y = c(10,20,25,40)

df = data.frame(x=x, y=y)

tibble(df)

regressao = lm(y~x, data = df)
summary(regressao)

stats_df = summary(regressao)$coefficients

A = stats_df["x", "Estimate"]
B = stats_df["(Intercept)", "Estimate"]

cat("O coeficiente de atrito encontrado foi", A)

p = ggplot(data = df, aes(x, y))+
  geom_point(size = 2)+
  geom_abline(intercept = B, slope = A, colour = "red")+
  annotate("text", x=0.75*max(x), y = 0.4*max(y), label=sprintf("Coeficiente de atrito: %1.2f", A))

ggplotly(p)
