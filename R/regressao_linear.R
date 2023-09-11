rm(list=ls())

graphics.off()

library(tidyverse)
library(plotly)

# adicione os valores para x e y
x = c(10,20,30,40)
y = c(10,20,30,40)

df = data.frame(x=x, y=y)

tibble(df)

regressao = lm(y~x, data = df)

stats_df = summary(regressao)$coefficients

A = stats_df["x", "Estimate"]

cat("O coeficiente de atrito encontrado foi", A)

p = ggplot(data = df, aes(x, y))+
  geom_point(size = 3, aes(color="Dados"))+
  geom_smooth(method = "lm", se = FALSE, aes(color="y=Ax+B"))+
  annotate("text", x=30, y=15, label=sprintf("Coeficiente de atrito: %1.2f", A))+
  scale_color_manual(name="Cores", breaks = c("Dados", "y=Ax+B"), values = c("black", "red"))

ggplotly(p)
