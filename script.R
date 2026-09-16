
# Visualizza le posizioni dei dati mancanti
missing_positions <- which(is.na(data_1), arr.ind = TRUE)
print(missing_positions)

#trasformiamo i valori in numeri
data_1 <- data_1[sapply(data_1, is.numeric)]


library(corrplot)

# Calcola la matrice di correlazione, assicurandoti di trattare solo dati numerici e di gestire i valori NA
data_numerica <- data_1[, sapply(data_1, is.numeric)]  # Seleziona solo colonne numeriche
matrice_correlazione <- cor(data_numerica, use = "pairwise.complete.obs")  # Gestisce i valori NA

# Assegna le prime due lettere di ogni nome di colonna come nomi delle variabili nella matrice di correlazione
colnames(matrice_correlazione) <- substr(names(data_numerica), 1, 3)
rownames(matrice_correlazione) <- substr(names(data_numerica), 1, 3)

# Crea il grafico della matrice di correlazione mantenendo l'ordine originale delle colonne
library(corrplot)
corrplot(matrice_correlazione, method = "number", type = "upper",
         tl.col = "black", tl.srt = 45, cl.pos = 'n', cl.cex = 0.7,
         number.cex = 0.8, number.digits = 2, order = "original")


#CREIAMO UN ALTRA VARIABILI IDENTICA A data_1 così possiamo fare modifiche senza intaccare il file originale
Data<-data_1

###################################################################################
#Effettuiamo una prima selezione delle variabili esplicative utilizzando il modello best-sbsets
install.packages("leaps")
library(leaps)
colnames(Data)<-substr(names(Data), 1, 4)
Data<-Data[,c(2, 4, 6, 9, 14, 16, 17, 18, 22, 23, 24, 25, 30)]
colnames(Data)[4]<- "GNI"
colnames(Data)[10]<- "NET"
colnames(Data)

attach(Data)

#abbiamo visto dai grafici che la maggior parte sono relazioni lineari tranne Popu e GNI. e che queste due sono relazioni logaritmiche
plot(Agri[-1], Life[-1], ylab= "Life Expectacy", xlab="Agriculture, forestry, and fishing, value added (% of GDP)")
abline(lm(Life[-1] ~  Agri[-1]), col="red")
colnames(data_1)
plot(Fert, Life)
plot(Fore, Life)
plot(log(GNI), Life,ylab= "Life Expectacy", xlab="Logarithm of GNI per capita, Atlas method")
abline(lm(Life ~log(GNI)), col="red")
plot(Immu,Life)

plot(Indu, Life,ylab = "Life Expectacy",xlab = "Industry, value added (% of GDP)")
abline(lm(Life ~Indu),col="red")

plot(Infl, Life)
plot(Mort, Life,ylab = "Life Expectacy",xlab ="Mortality rate, under-5 (per 1,000 live births)")
abline(lm(Life ~Mort),col="red")
plot(NET, Life)
plot(Pers, Life)
plot(log(Popu), Life,ylab = "Life Expectacy",xlab ="Logarithm of Population density")                
abline(lm(Life~log(Popu)),col="red")
plot(Time, Life )

#sapendo che il modello best subset effettua l'assunzione di relazioni lineari, adiamo a loglinearizzare GNI e Popu
modello_best_subset<-regsubsets(Life ~ Agri + Fert + log(GNI)  + Immu + Indu + Infl + Mort + NET + Pers + log(Popu) + Time, data=Data, method = "exhaustive")
subset<-summary(modello_best_subset)
subset$which
subset$rsq
subset$adjr2

assex<-c(1:8)
plot(assex,subset$adjr2, type= "b",xlab = "Number of variables",ylab = "Adjusted R^2")
points(assex[5],subset$adjr2[5], col = "red", pch = 19, cex = 1.5)

#ci diceva di prendere 5 variabili
MODELLONE<-lm(Life ~ Agri + log(GNI) + Indu + Mort + log(Popu))
summary(MODELLONE)

#il modellone ha però una variabile non significativa (Indu)
cor.test(Life, log(Popu))
cor.test(Life, Indu)
#facciamo il cor.test dule 2 meno significative
#sebbene il cor.test dica che popu=0 in un contesto multivariato importa dare valore al p-value del modello in quanto tiene  conto anche per delle altre variabili

#facciamo il MODELLONE senza Indu
MODELLONE<-lm(Life ~ Agri + log(GNI) + log(Popu) + Mort)
summary(MODELLONE)
formula(MODELLONE)

#andiamo a verificare se la prima assunzione di G.M. è verificata(errori a media nulla)
residui<-residuals(MODELLONE)
plot(fitted(MODELLONE),residui,ylab = "Residuals",xlab = "Estimated values",main = "E(u|X) = 0")
sum(residui)
abline(0,0,col="red")
t.test(residui)

#andiamo a verificare la seconda assunzione (omoschedasticità)
library(lmtest)
bptest(MODELLONE)
#Dato che il test non ha rivelato eteroscedasticità, possiamo assumere che il modello attuale sia adeguato 
#sotto l'aspetto della costanza della varianza dei residui. Questo significa che gli errori standard, i coefficienti 
#di regressione, e i test statistici basati sul modello di minimi quadrati ordinari (OLS) sono validi.

#adesso passiamo alla verifica della terza assunzione (multicollinearità)
library(car)
vif_valori<-vif(MODELLONE)
vif_valori
#il vif è compreso tra 1 e 5 quindi non c'è motivo di ritenere che vi sia multicollinearità eccessiva

#quarta assunzione: correlazione tra le X e i residui
cor.test(residui, log(GNI))
cor.test(residui, Mort)
cor.test(residui, log(Popu))
cor.test(residui, Agri)

#quinta assunzione: Normalità
#SHAPIRO
shapiro.test(residui)

#QQPLOT
qqnorm(scale(residui))
abline(0,1, col="red")

#KOLGOMOROV SMIRNOV
residui_std <- (residui - mean(residui)) / sd(residui)
# Eseguire il test di Kolmogorov-Smirnov
ks_result <- ks.test(residui_std, "pnorm")
ks_result
?pnorm


#adesso passiamo alla sesta assunzioni(correlazione dei residui)
dw_result <- dwtest(MODELLONE)
dw_result
#il dwtest ha rilevato che non vi è autocorrelazione tra i residui
Anova(MODELLONE)


#####################################################################################
#MODELLO LOGIT
attach(data_2)
#creiamo la variabile dummy tasso di fertilità
mean(Fer)
data_2$dummyfer<-ifelse(Fer>1.78088, 1, 0)
sum(data_2$dummyfer)
#regrediamo la dummy fertilità su agr, ub e gdp growth
names(data_2)[8]<- "GDP_growth"
names(data_2)[31]<- "urban_pop_growth"
mylogit<- glm(dummyfer ~  Agri + urban_pop_growth + GDP_growth, data = data_2, family = "binomial")
summary(mylogit)

#intervalli di confidenza
confint(mylogit)

#calcoliamo le probabilià per ogni paese che Y=1
post_logit <- predict(mylogit, type = "response")

#assegnamo i valori o o 1 a tutte le unità che superao la soglia del0.42
which(post_logit>0.42)
default_logit <- ifelse(post_logit>0.42, "PreV: Sì", "Prev: No")

#creiamo la matrice di confusione
table(default_logit,dummyfer)
hitrate<- 39/46

#calcoliamo la curva roc
library(pROC)
def_roc<-roc(response=dummyfer, predictor=post_logit)
plot(def_roc, main ="ROC curve")
auc_value<-def_roc$auc
text(x = 0, y = 0.2, labels = paste("AUC =", round(auc_value, 3)), cex = 1, col = "red")

#creiamo un grafico plottando gpdrocpite con urb e verficare quali hanno alta fertilità e quali no
plot(gdp,Lif)
cor(gdp, Lif)
# Supponiamo che tu abbia un dataframe chiamato df con le colonne x, y, e dummyfer

# all'aumentare del reddito l'aspettativa di vita aumenta e possiamo notare come i paesi che hanno un' alta aspettativa di vita
#e un alto gdp sono paesi poco fertili
plot(gdp, Lif, col=ifelse(dummyfer == 0, 'red', 'blue'), pch=16, xlab="GDP per capita", ylab="Life Expectacy", main="Comparison between Fertility Rate with \n Life Expectacy on GDP per capita")

# Aggiungere una legenda
legend("bottomright", legend=c("low fertility", "high fertility"), col=c("red", "blue"), pch=16)

