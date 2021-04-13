#### Ejercicio 1

library("tidyverse")
install.packages("gapminder")
library("gapminder")
df <- gapminder
str(df)
summary(df$gdpPercap)
plot(df$year, df$gdpPercap/1000,
     main = "Gráfico 1: GDP per cápita",
     sub = "GPD per cápita para 172 países entre los año 1952 y 2007",
     xlab = "Año",
     ylab = "GPD per cápita (en miles)",
     yaxt = "n",
     pch = 23,
     bg = "lightgreen",
     col = "green",
     cex = 0.5,
     lwd = 0.5,
     col.main = "darkblue",
     col.sub = "darkblue",
     col.lab ="darkblue",
     col.axis= "darkred",
     fg="darkred"
     ) + axis(2, col="darkred",col.axis="darkred", las=1, at = seq(0, round(max(df$gdpPercap)), by = 20))

#### Ejercicio 2
library(ggplot2)
ggplot(df, aes(x = year, y = gdpPercap/1000)) +
  geom_point(alpha = 0.4, color="green") +
  ggtitle("Gráfico 1: GDP per cápita") +
  labs(x = "Año", 
       y = "GPD per cápita (en miles)",
       caption = "GPD per cápita para 172 países entre los año 1952 y 2007") +
  theme(plot.title = element_text(color="blue4",face="bold",hjust = 0.5),
        plot.caption = element_text(hjust = 0.5,color="blue4"),
        axis.title.x = element_text(color="blue4",face="bold"),
        axis.title.y = element_text(color="blue4",face="bold"),
        axis.text.x = element_text(color="darkred"),
        axis.text.y = element_text(color="darkred")) + scale_y_continuous(breaks=seq(0,120,20))

#### Ejercicio 3
df_eu=df[df$continent=="Europe",]

myfunc <- function(data) {
  boxplot(data,
          col = "orange",
          outpch = 23,
          outbg = "lightgreen",
          whiskcol = "darkgreen")
}

myfunc(df_eu$lifeExp)


#### Ejercicio 4

datos <- read_csv("Puntajes.csv")

ggplot(datos, aes(x=x)) +
  geom_histogram(aes(y=..density..), color="yellow", alpha=0.3) +
  geom_density(fill="green", alpha=0.3, kernel="epanechnikov") +
  geom_density(fill="lightblue", alpha=0.3, kernel="gaussian") + 
  stat_density(aes(x=x, colour="Kernel Gaussiano"), geom="line",position="identity", kernel="gaussian") +
  stat_density(aes(x=x, colour="Kernel Epanechnikov"), geom="line",position="identity", kernel="epanechnikov")

#### Ejercicio 5





#### Ejercicio 6

df_espec <- read_delim("espectaculos.csv", delim = ";", col_names=TRUE, local = locale(encoding = "latin1"))
sapply(df_espec, class)
view(df_espec)

df_espec$ESPECTACULOS <- as.factor(df_espec$ESPECTACULOS)
df_espec$asistentes <- (df_espec$Mes1AsistGratuitos+df_espec$Mes1AsistPagados+
                         df_espec$Mes2AsistGratuitos+df_espec$Mes2AsistPagados+
                         df_espec$Mes3AsistGratuitos+df_espec$Mes3AsistPagados+
                         df_espec$Mes4AsistGratuitos+df_espec$Mes4AsistPagados+
                         df_espec$Mes5AsistGratuitos+df_espec$Mes5AsistPagados+
                         df_espec$Mes6AsistGratuitos+df_espec$Mes6AsistPagados)

ggplot(df_espec, aes(x=ESPECTACULOS, y=asistentes, fill=1)) +
  geom_violin() +
  xlab("Tipo de Evento") +
  ylab("Cantidad de Asistentes") +
  ylim(0,2000) +
  theme(legend.position="none") +
  labs(title="Cantidad de Asistentes por tipo de Espectáculo", subtitle="Periodo 2014-2019")

df_espec$REGIONESTABLECIMIENTO <- as.factor(df_espec$REGIONESTABLECIMIENTO)
ggplot(df_espec, aes(x=REGIONESTABLECIMIENTO, y=asistentes, fill=1)) +
  geom_violin() +
  xlab("Región") +
  ylab("Cantidad de Asistentes") +
  ylim(0,1000) +
  theme(legend.position="none") +
  coord_flip() +
  labs(title="Cantidad de Asistentes por Región", subtitle="Periodo 2014-2019")


dim(df_espec)
