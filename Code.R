
library(corrplot)
library(car) 
library(FactoMineR)
library(factoextra)
library(MASS)


dati <- read.csv("Gruppo_F/dati_meteo_O3.csv")
dati <- na.omit(dati)
summary(dati)




dati_pca <- dati[,c(3:12)]


R <- cor(dati_pca)

corrplot(R)

library(psych)

pairs.panels(dati_pca,
             gap = 0,
             bg = c("red", "yellow", "blue")[dati$campag],
             pch=21)


mean_radizioni <- tapply(dati$radiazione,dati$campag , mean)
mean_O3 <- tapply(dati$O3,dati$campag , mean)
par(mfrow=c(1,2))
barplot(mean_radizioni, ylab = "Radiazione", xlab = "Campagna")
barplot(mean_O3, ylab = "O3", xlab = "Campagna")



dati_no_O3 <- dati[,c(3:11)]

par(mfrow=c(2,1))

pca <- PCA(X = dati_no_O3, scale.unit = T, graph = FALSE)

fviz_pca_biplot(pca)

fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
             )


pairs.panels(pca$ind$coord,
             gap=0,
             bg = c("red", "yellow", "blue")[dati$campag],
             pch=21)

pca$eig

fviz_eig(pca)

pca$var$coord

#pca$ind$coord

pca_coord <- as.data.frame(pca$ind$coord)
pca_coord[,6] <- dati[,12]
colnames(pca_coord) <- c("Dim.1", "Dim.2", "Dim.3", "Dim.4", "Dim.5", "O3")


mod_completo <- lm(formula = O3 ~ ., data = pca_coord)
summary(mod_completo)



mod_1 <- lm(formula = O3 ~ Dim.1 + Dim.2 + Dim.3 + Dim.4, data = pca_coord)


mod_2 <- stepAIC(mod_1, direction = "both")

summary(mod_1)

summary(mod_2)

plot(mod_2,which = 1)

plot(mod_2,which = 2)

plot(mod_2, which=3)

plot(mod_2, which=4)

plot(mod_2, which=5)

plot(mod_2, which=6)

olsrr::ols_plot_resid_lev(mod_2)

outlierTest(mod_2)


e_std <- studres(mod_2)

shapiro.test(e_std)

hist(e_std, prob = T,breaks = 20)
lines(x = density(x = e_std), col = "red",lwd=2)
curve(dnorm(x,mean=mean(e_std),sd=sd(e_std)), add=TRUE,col="blue",lwd=3)
legend("topright", c("e_std distribution", "normal distribution"), fill=c("red", "blue"))

e_norm <- rnorm(1000,mean=mean(e_std, na.rm=TRUE), sd=sd(e_std, na.rm=TRUE))

boxplot(e_std,e_norm,
main = "Boxplot of e_std -vs- normal", at=c(1,2),names = c('e_std','e_norm'),
col = c("light blue","forest green"), las=2,
border = "blue",
horizontal = TRUE,
notch = TRUE
)

crPlots(mod_2)

mod_11 <- lm(formula = log(O3) ~ Dim.1 + Dim.2 + Dim.3 + Dim.4, data = pca_coord)
mod_12 <- stepAIC(mod_11, direction = "both")

summary(mod_2)

summary(mod_12)

par(mfrow=c(1,2))
plot(mod_12,which = 1)
plot(mod_12,which = 5)

par(mfrow=c(1,2))
plot(mod_12,which = 4)
plot(mod_12,which = 6)


library(devtools)
library(ggbiplot)

ggbiplot(pca,ellipse=TRUE,  labels=rownames(pca), groups=dati$campag)+
  ggtitle("PCA of weather dataset")

ggbiplot(pca,ellipse=TRUE,choices = c(3,4),  labels=rownames(pca), groups=dati$campag)

fviz_pca_var(pca,axes = c(3,4),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
             )

fviz_pca_var(pca,axes = c(1,5),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
             )












