#install.packages("here")
# postavi WD na folder u kom je ova .R skripta
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(readxl)
df <- read_excel("iceland.xls")


#podela do 23 je za fitovanje a za testiranje su 23,24-linijska podela
godinaTest=2023#od ove godine ce sluziti za proveru
x_Train<- ts(df$import[df$year < godinaTest],frequency = 1, start = min(df$year))
#napravi trening test kao vremensku seriju tako da ucitas svaki podataka,pocevsi od minimalne godine-prva godina
x_Test<- ts(df$import[df$year >= godinaTest], frequency = 1, start = godinaTest)

#1.VIZUELNI PRIKAZ,OSNOVNE OSOBINE:STACIONARNOST/AUTOKORELISANOST
# a) grafički prikaz VS

library(ggplot2)

df_plot <- data.frame(
  year = as.numeric(time(x_Train)),
  value = as.numeric(x_Train)
)

ggplot(df_plot, aes(x = year, y = value)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
       x = "Year", y = "Imports of goods and services-% of GDP") +
  theme_minimal(base_size = 11)




## vremenska serija nema sezonalnost-na godisnjem nivou su podaci, 
#jasno uocljiv trend kroz ceo period,ali se u okviru pojedinacnih delova vide trendovi 
#1970–2010: trend (u dve faze)
#od 1970 do ranih 90-ih: dug pad nivoa, a onda rane 90-te–2010: dug rast nivoa
#2010–dalje: ciklične oscilacije oko višeg nivoa + šok 2020

#1.STRUKTURNE PROMENE
#install.packages("strucchange")
library(strucchange)

bp <- breakpoints(x_Train ~ 1)     # lomovi u proseku (level shifts)
summary(bp)                  # broj lomova, BIC, RSS itd.
plot(bp)                     # grafički prikaz
confint(bp)                  # intervali poverenja za lomove


#posto ne mozemo dekompoziciju(nema sezone) radimo pokretni prosek
x <- as.numeric(x_Train)
t <- as.numeric(time(x_Train))

roll_mean <- stats::filter(x, rep(1/3, 3), sides = 2)

plot(t, roll_mean, type="l", lwd=3, col="red",
     main="Rolling mean (3 years)",
     xlab="Year", ylab="Rolling mean")
grid()

#nije konstantan znaci da imamo nestacionarnost srednje vrednosti//trend uocljiv po fazama




# b) ACF i PACF posmatrane VS u nivou--
#acf je corr(xt,xt-k) gde je k od 0 do 20(lag.max) stubic je korelacija xt i xt-k
  #sporo opadanje acf-a znak trenda/nestacionarnosti

#pacf je corr(xt,xt-k|xt-1..xt-k) direktna veza posle uklanjanja posrednih lagova
pacf(x_Train, lag.max = 20)
#pacf za ar odnosno koliko prethodno stanje ima uticaja
#lag 1 visok -jaka zavisnost od prethodne godine cak i kada ostale kontrolises
#da bi za ar odredili p moramo diferencirati prvo!

# c)  LjungBox Q test #provera autokorelacije 
Box.test(x_Train,lag=20,type=c("Ljung-Box"))
#H1 prihvatamo>ima znacajne zavisnosti koje treba modelovati

# d) ADF-jedinicni koren #provera stacionarnosti
#install.packages("tseries") 
library(tseries) #za adf
adf.test(x_Train)#>0.05 H0 nestacionarna!

#2. DIFERENCIRANJE-osobine diferencirane VS
#2. grafik stacionarne uporediti sa gore u nivou koja nije
x_Traindiff1<-diff(x_Train, differences=1)

adf.test(x_Traindiff1)#<0.05 H1 stacionarna!
kpss.test(x_Traindiff1)


library(ggplot2)

df_plot <- data.frame(
  year = as.numeric(time(x_Traindiff1)),
  value = as.numeric(x_Traindiff1)
)

ggplot(df_plot, aes(x = year, y = value)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    x = "Year", y = "Imports of goods and services-% of GDP") +
  theme_minimal(base_size = 11)

#pokretni prosek oko 0
x1 <- as.numeric(x_Traindiff1)
t1 <- as.numeric(time(x_Traindiff1))

roll_mean1 <- stats::filter(x1, rep(1/3, 3), sides = 2)

plot(t1, roll_mean1, type="l", lwd=3, col="red",
     main="Rolling mean (3 years)",
     xlab="Year", ylab="Rolling mean")
grid()
#g)  LjungBox Q test-autokorelisanost kao gore sto receno
Box.test(x_Traindiff1,lag=4,type=c("Ljung-Box"))

#f) Nacrtati i komentarisati ACF i PACF posmatrane VS u odgovarajućoj diferenci u odnosu na ADF
acf(x_Traindiff1, lag.max = 20) 
pacf(x_Traindiff1, lag.max = 20) 



#3.ARIMA -pravljenje modela,formule, model po AIC
#Prikazati 2-3 rešenja modela sa odgovarajućim formulama i testirati značajnost koeficijenata



library(forecast)
library(lmtest)



m1 <- Arima(x_Train, order=c(0,1,0),include.drift = FALSE)#NEMA NIKAKVOG TESTA JER SU KOEFICIJENTI P,Q 0!

# 2) MA(1)
m2 <- Arima(x_Train, order=c(0,1,1),include.drift = FALSE)
coeftest(m2)
# 3) AR(1)
m3 <- Arima(x_Train, order=c(1,1,0),include.drift = FALSE)
coeftest(m3)


aicModel<-c(AIC(m1), AIC(m2) ,AIC(m3))

#i) Izabrati odgovarajuće rešenje u odnosu na AIC kriterijum i komentarisati rezultate

#4.
#Pomoću funkcije auto.arima() finalni model. Prikazati finalni model odgovarajućom formulom i testirati ga
#k) Grafički prikazati reziduale, grafikon, ACF, histogram
#l) Ispitati prisustvo autokorelacije u finalnom modelu

fit <- auto.arima(x_Train)   # automatski izabere p,d,q
checkresiduals(fit)
#sa prelomom

x_Train_2004 <- window(x_Train, start = 2004)

#plot(x_Train_2004)
#mali br tacaka acf,pacf nisu pokazali isto kao auto arima
#acf((x_Train_2004), lag.max = 20)
#pacf(diff(x_Train_2004), lag.max = 20)
library(forecast)
fit_2004 <- auto.arima(x_Train_2004)#auto arima predvidjanja
summary(fit_2004)
library(lmtest)
coeftest(fit_2004)
shapiro.test(residuals(fit_2004))   # Shapiro–Wilk (dobar za manje uzorke)
#h0:reziduali su iz normalne raspodele, h1:reziduali nisu iz normalne raspodele
checkresiduals(fit_2004)
#Reziduali su oko belog šuma, što je dobar pokazatelj modelovanja. 
#Model df označava da su je jedan procenjeni AR/MA parametar modela 1(u ovom slučaju je to MA),
#a total lags used označava da se gleda do 10 broj lagova. 



#m) Ispitati prisustvo heteroskedastičnosti u finalnom modelu 

#install.packages("FinTS")-za varijansu homoskedasticna je
library(FinTS)
e <- residuals(fit) #reziduali modela 1
ArchTest(e)
e1 <- residuals(fit_2004) #reziduali modela 2
ArchTest(e1)
#predvidjanja
#Tabela

fc <- forecast(fit, h = 11,level=c(95,99),,bootstrap=TRUE)#11 godina vise jer do 33 a do 22 imamo
fc
#provera nisu iz norm rasp reziduali da l ekstr vr uticale--
e <- residuals(fit)

summary(e)
max(abs(e))                 # najveće odstupanje
which.max(abs(e))           # koja pozicija #2022 korona sok najveci 
time(e)[which.max(abs(e))]  # koja godina 

# standardizovani reziduali
z <- e / sd(e, na.rm=TRUE)
sum(abs(z) > 2)             # koliko iznad 2 SD
sum(abs(z) > 3)             # koliko iznad 3 SD#moze i obicno nema znacajnih ekstr vr jer ovde 0 vrednost





fc_2004 <- forecast(fit_2004, h = 11,level=c(95,99))
#OVDE NE MORA DODATNA PROVERA ZATO STO JE OVO PODSKUP BIO OD GORE ,bootstrap=TRUE nema potrebe

fc_2004 
#predvidjanje grafici
autoplot(fc)
autoplot(fc_2004)
#tacnost
accuracy(fc, x_Test)
accuracy(fc_2004, x_Test)

AIC   = c(AIC(fit), AIC(fit_2004))

h_test <- length(x_Test)
fc_test <- forecast(fit, h = h_test,bootstrap=TRUE)
fc2004_test <- forecast(fit_2004, h = h_test,bootstrap=TRUE)
#poredjenje

fit_test <- forecast(fit, h = length(x_Test))
fit2004_test <- forecast(fit_2004, h = length(x_Test))
autoplot(x_Test, series="Stvarno") +
  autolayer(fit_test$mean, series="Prognoza fc") +
  autolayer(fit2004_test$mean, series="Prognoza fc_2004") +
  ggtitle("Samo test period (iste godine)") +
  xlab("Godina") + ylab("Vrednost")


library(forecast)

D <- ifelse(time(x_Train) >= 2004, 1, 0)#d je vrednosti 1 posle 2004-posle loma

fit_break <- auto.arima(x_Train, xreg = D)#xreg je spoljašnja prom koju ubacujemo u model, deo serije objasnjen je lomom D
summary(fit_break)
coef(fit_break)   # koeficijenti, uključujući dummy
coeftest(fit_break)#da stat znac proveri 
checkresiduals(fit_break)#reziduali da l sve obuhv-nema autokorelisanosti
t.test(residuals(fit_break))     # srednja vr blizu 0
library(FinTS)
ArchTest(residuals(fit_break), lags=5) #reziduali heteroskedasticni
#predvidjanje poredjenje s ostalim
fc_break<- forecast(fit_break, xreg = c(1,1),h = 11,level=c(95,99))#OVO PROGNOZA DO 33 ODNOSNO 11 GODINA
# YA 2 GOD PROGNOZA


h_test <- length(x_Test)#ILI 2 DA JE ODMAH, AL GORE TEST SET STAVLJENO PA ONDA STAVLJENO KOLIKA DUZINA TESTA
fc_test <- forecast(fit_break, xreg = rep(1, h_test), h = h_test)

acc_fc     <- accuracy(fc_test, x_Test)["Test set", c("RMSE","MAE","MAPE")]
acc_f   <- accuracy(fit_test, x_Test)["Test set", c("RMSE","MAE","MAPE")]
acc_2004   <- accuracy(fit2004_test, x_Test)["Test set", c("RMSE","MAE","MAPE")]

aicc_tab <- c(
  fc = fit$aicc,
  break_dummy = fit_break$aicc
)
aicc_tab

#provera train test malo veci od 2016, pa kod pocetnog postavi se samo nova godina pracenja i uradi poredjenje
#Performansi kod arima bez i sa prelomom
godinaNova <- 2016

train_vec <- df$import[df$year < godinaNova]
test_vec  <- df$import[df$year >= godinaNova]

x_TrainNovo <- ts(train_vec, frequency = 1, start = min(df$year[df$year < godinaNova]))
x_TestNovo  <- ts(test_vec,  frequency = 1, start = min(df$year[df$year >= godinaNova]))  # = godinaNova


library(forecast)

h_test <- length(x_TestNovo)

fit_rw <- Arima(x_TrainNovo, order = c(0,1,0))
fc_rw  <- forecast(fit_rw, h = h_test)

acc_rw <- accuracy(fc_rw, x_TestNovo)["Test set", c("RMSE","MAE","MAPE")]
acc_rw
D_train <- ifelse(time(x_TrainNovo) >= 2004, 1, 0)
D_test  <- ifelse(time(x_TestNovo)  >= 2004, 1, 0)

fit_x <- auto.arima(x_TrainNovo, xreg = D_train)
fc_x  <- forecast(fit_x, xreg = D_test, h = h_test)

acc_x <- accuracy(fc_x, x_TestNovo)["Test set", c("RMSE","MAE","MAPE")]
acc_x
rbind(ARIMA_010 = acc_rw, ARIMAX_dummy = acc_x)


