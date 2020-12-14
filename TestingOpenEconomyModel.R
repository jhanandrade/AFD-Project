#Load the package
library(Rcpp)

#Set the Working Directory
mywd = getwd()
setwd(paste0(mywd, "/source"))

#Load the algorithm
source("SourceCode.R")

#Put the equations into the algorithm
SOEM <- cppMakeSys(fileName = "../EQA.R",reportVars=3)
cppSOEM <- cppRK4(SOEM)
View(cppSOEM)
plot(cppSOEM$resrat)

#"In this case, we are just setting the switch to 1 because the value of the shock 
#is already defined in the parameter of the Equation file"
event <- list(triggerDate=5, switchpolicy="1")

#Put the equations again into the algorithm
Baseline <- cppMakeSys(fileName = "../EQA.R",reportVars=3, eventTime=list(event))
cppBaseline <- cppRK4(Baseline)
cppBaseline

#------------------------------------------------------------------------

#Gráficas modelo sin choque - Sección 1
C.GDP.w = cppSOEM$Con/cppSOEM$GDP #Consumo/GDP
I.GDP.w = (cppSOEM$Ir*cppSOEM$p)/cppSOEM$GDP #Inversión/GDP
Ex.GDP.w= (cppSOEM$EX)/cppSOEM$GDP #Exportaciones/GDP
IM.GDP.w= (cppSOEM$IM*cppSOEM$pw*cppSOEM$en)/cppSOEM$GDP #Exportaciones/GDP
TB.GDP.w= cppSOEM$TB  #Balance comercial
CA.GDP.w= cppSOEM$CA #Balance en cuenta corriente
Un.w= (cppSOEM$Pop-cppSOEM$L)/cppSOEM$Pop  #Tasa de desempleo
wsh.w= (cppSOEM$w*cppSOEM$L)/cppSOEM$GDP  #Participación salarial/GDP
prf.w= cppSOEM$gproff/cppSOEM$GDP  #Beneficios de las firmas/GDP
prb.W= cppSOEM$gprofb/cppSOEM$GDP  #Beneficios de los bancos/GDP

X11()
par(mfcol=c(2,5))
matplot(cppSOEM$time, C.GDP.w, main="Consumption (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppSOEM$time, I.GDP.w, main= " Investment (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppSOEM$time, Ex.GDP.w, main= "Exports (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppSOEM$time, IM.GDP.w, main= "Imports (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppSOEM$time, TB.GDP.w, main= "Trade Balance (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppSOEM$time, CA.GDP.w, main= "Current Account (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppSOEM$time, Un.w, main= "Unemployment Rate (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppSOEM$time, wsh.w, main= "Wage Share (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppSOEM$time, prf.w, main= "Firm profits (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppSOEM$time, prb.W, main= "Bank profits (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))

#Gráficas modelo sin choque - Sección 2

PD.GDP.w= cppSOEM$Bg/cppSOEM$GDP #Deuda pública/GDP
PDh.GDP.w= cppSOEM$Bgh/cppSOEM$GDP #Deuda pública hogares/GDP
PDb.GDP.w= cppSOEM$Bgb/cppSOEM$GDP #Deuda pública bancos/GDP
PDw.GDP.w= cppSOEM$Bgrow/cppSOEM$GDP #Deuda pública Resto del mundo/GDP
G.GDP.w= (cppSOEM$G+cppSOEM$Gs)/cppSOEM$GDP #Gasto Público/GDP
T.GDP.w= ((cppSOEM$T_Rev)/cppSOEM$GDP) #Ingresos fiscales/GDP
FD.GDP.w= (cppSOEM$BgDot/cppSOEM$GDP) #Déficit fiscal
FXR.w = cppSOEM$Rfx/(cppSOEM$IM*cppSOEM$pw)


X11()
par(mfcol=c(2,4))
matplot(cppSOEM$time, PD.GDP.w, main="Public Debt (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppSOEM$time, PDh.GDP.w, main="Households holdings of gov.Bonds (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppSOEM$time, PDb.GDP.w, main="Banks holdings of gov.Bonds (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppSOEM$time, PDw.GDP.w, main="Foreign holdings of gov.Bonds (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppSOEM$time, G.GDP.w, main="Public Spending (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppSOEM$time, T.GDP.w, main="Fiscal Revenue (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppSOEM$time, FD.GDP.w, main="Public Deficit (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppSOEM$time, FXR.w, main="FX reserves (%Imports)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))

#Gráficas del modelo sin choque-Sección 3

infl.w= cppSOEM$pDot/cppSOEM$p #Inflación
ner.w= cppSOEM$en #Tasa de cambio nominal
rer.w= cppSOEM$er #Tasa de cambio real
r.idep.w= cppSOEM$idep-(cppSOEM$pDot/cppSOEM$p) #Real Deposit Rate
r.ibg.w= cppSOEM$ibg-(cppSOEM$pDot/cppSOEM$p) #Real Public Bond Rate
idL.w= cppSOEM$idL #interest rate on domestic currency loans
idLfx.w= cppSOEM$ifxf #interest rate on FX loans
arb.f.w= cppSOEM$arbf #Firms arbitrage
arb.w.w= cppSOEM$arbrow #Arbitrage of the RoW
rsk.w= cppSOEM$rsk #Country risk

X11()
par(mfcol=c(2,5))
matplot(cppSOEM$time,infl.w, main="Inflation Rate" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20),ylim=c(0.04,0.06))
matplot(cppSOEM$time,ner.w, main="Nominal Exchange Rate" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppSOEM$time,rer.w, main="Real Exchange Rate" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20),ylim=c(0.8,1.1))
matplot(cppSOEM$time,r.idep.w, main="Real Deposit Rate" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20),ylim=c(0.015,0.03))
matplot(cppSOEM$time,r.ibg.w, main="Real Public Bonds Rate" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20),ylim=c(0.015,0.03))
matplot(cppSOEM$time,idL.w, main="Interest rate on Dom. Currency Loans" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppSOEM$time,idLfx.w, main="Interest rate on FX Loans" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20),ylim=c(0.05,0.12))
matplot(cppSOEM$time,arb.f.w, main="Firms FX arbitrage" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppSOEM$time,arb.w.w, main="RoW arbitrage" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppSOEM$time,rsk.w, main="Country Risk" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))

#-----------------------------------------------------------------------------------------------

#Gráficas modelo con choque - Sección 1

C.GDP.B = cppBaseline$Con/cppBaseline$GDP #Consumo/GDP
I.GDP.B = (cppBaseline$Ir*cppBaseline$p)/cppBaseline$GDP #Inversión/GDP
Ex.GDP.B= (cppBaseline$EX)/cppBaseline$GDP #Exportaciones/GDP
IM.GDP.B= (cppBaseline$IM*cppBaseline$pw*cppBaseline$en)/cppBaseline$GDP #Exportaciones/GDP
TB.GDP.B= cppBaseline$TB  #Balance comercial
CA.GDP.B= cppBaseline$CA #Balance en cuenta corriente
Un.B= (cppBaseline$Pop-cppBaseline$L)/cppBaseline$Pop  #Tasa de desempleo
Wsh.B= (cppBaseline$w*cppBaseline$L)/cppBaseline$GDP  #Participación salarial/GDP
prf.B= cppBaseline$gproff/cppBaseline$GDP  #Beneficios de las firmas/GDP
prb.B= cppBaseline$gprofb/cppBaseline$GDP  #Beneficios de los bancos/GDP

X11()
par(mfcol=c(2,5))
matplot(cppBaseline$time, C.GDP.B, main="Consumption (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppBaseline$time, I.GDP.B, main= " Investment (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppBaseline$time, Ex.GDP.B, main= "Exports (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppBaseline$time, IM.GDP.B, main= "Imports (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppBaseline$time, TB.GDP.B, main= "Trade Balance (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppBaseline$time, CA.GDP.B, main= "Current Account (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppBaseline$time, Un.B, main= "Unemployment Rate (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppBaseline$time, wsh.B, main= "Wage Share (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppBaseline$time, prf.B, main= "Firm profits (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppBaseline$time, prb.B, main= "Bank profits (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))

#Gráficas modelo con choque - Sección 2

PD.GDP.B= cppBaseline$Bg/cppBaseline$GDP #Deuda pública/GDP
PDh.GDP.B= cppBaseline$Bgh/cppBaseline$GDP #Deuda pública hogares/GDP
PDb.GDP.B= cppBaseline$Bgb/cppBaseline$GDP #Deuda pública bancos/GDP
PDw.GDP.B= cppBaseline$Bgrow/cppBaseline$GDP #Deuda pública Resto del mundo/GDP
G.GDP.B= (cppBaseline$G+cppBaseline$Gs)/cppBaseline$GDP #Gasto Público/GDP
T.GDP.B= ((cppBaseline$T_Rev)/cppBaseline$GDP) #Ingresos fiscales/GDP
FD.GDP.B= (cppBaseline$BgDot/cppBaseline$GDP) #Déficit fiscal
FXR.B = cppBaseline$Rfx/(cppBaseline$IM*cppBaseline$pw)

X11()
par(mfcol=c(2,4))
matplot(cppBaseline$time, PD.GDP.B, main="Public Debt (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppBaseline$time, PDh.GDP.B, main="Households holdings of gov.Bonds (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppBaseline$time, PDb.GDP.B, main="Banks holdings of gov.Bonds (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppBaseline$time, PDw.GDP.B, main="Foreign holdings of gov.Bonds (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppBaseline$time, G.GDP.B, main="Public Spending (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppBaseline$time, T.GDP.B, main="Fiscal Revenue (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppBaseline$time, FD.GDP.B, main="Public Deficit (%GDP)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppBaseline$time, FXR.B, main="FX reserves (%Imports)" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))

#Gráficas del modelo con choque-Sección 3

infl.B= cppBaseline$pDot/cppBaseline$p #Inflación
ner.B= cppBaseline$en #Tasa de cambio nominal
rer.B= cppBaseline$er #Tasa de cambio real
r.idep.B= cppBaseline$idep-(cppBaseline$pDot/cppBaseline$p) #Real Deposit Rate
r.ibg.B= cppBaseline$ibg-(cppBaseline$pDot/cppBaseline$p) #Real Public Bond Rate
idL.B= cppBaseline$idL #interest rate on domestic currency loans
idLfx.B= cppBaseline$ifxf #interest rate on FX loans
arb.f.B= cppBaseline$arbf #Firms arbitrage
arb.w.B= cppBaseline$arbrow #Arbitrage of the RoW
rsk.B= cppBaseline$rsk #Country risk

X11()
par(mfcol=c(2,5))
matplot(cppBaseline$time,infl.B, main="Inflation Rate" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20),ylim=c(0.04,0.06))
matplot(cppBaseline$time,ner.B, main="Nominal Exchange Rate" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppBaseline$time,rer.B, main="Real Exchange Rate" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20),ylim=c(0.8,1.1))
matplot(cppBaseline$time,r.idep.B, main="Real Deposit Rate" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20),ylim=c(0.015,0.03))
matplot(cppBaseline$time,r.ibg.B, main="Real Public Bonds Rate" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20),ylim=c(0.015,0.03))
matplot(cppBaseline$time,idL.B, main="Interest rate on Dom. Currency Loans" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppBaseline$time,idLfx.B, main="Interest rate on FX Loans" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppBaseline$time,arb.f.B, main="Firms arbitrage" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppBaseline$time,arb.w.B, main="RoW arbitrage" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
matplot(cppBaseline$time,rsk.B, main="Country Risk" ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))

#-------------------------------------------------------------------------------------------

##Sensitivity Analysis: Exchange Rate Adjustment

pars1=changeParameters(Baseline,list(alphagw=0.045,infls=0.055,beta12=0.01,beta1=2,ji0=0.8))
pars2=changeParameters(Baseline,list(alphagw=0.015,infls=0.035,beta12=0.0025,beta1=0.75,ji0=0.5))

#Run the model
cpp_scen1 <- cppRK4(Baseline,parms = pars1)
cpp_scen2 <- cppRK4(Baseline, parms=pars2)

#Sensitivity Analysis - Gráficas 1

C.GDP_scen1= cpp_scen1$Con/cpp_scen1$GDP #Consumo/GDP
I.GDP_scen1 = (cpp_scen1$Ir*cpp_scen1$p)/cpp_scen1$GDP #Inversión/GDP
Ex.GDP_scen1= (cpp_scen1$EX)/cpp_scen1$GDP #Exportaciones/GDP
IM.GDP_scen1= (cpp_scen1$IM*cpp_scen1$pw*cpp_scen1$en)/cpp_scen1$GDP #Exportaciones/GDP
TB.GDP_scen1= cpp_scen1$TB  #Balance comercial
CA.GDP_scen1= cpp_scen1$CA #Balance en cuenta corriente
Un_scen1= (cpp_scen1$Pop-cpp_scen1$L)/cpp_scen1$Pop  #Tasa de desempleo
Wsh_scen1= (cpp_scen1$w*cpp_scen1$L)/cpp_scen1$GDP  #Participación salarial/GDP
prf_scen1= cpp_scen1$gproff/cpp_scen1$GDP  #Beneficios de las firmas/GDP
prb_scen1= cpp_scen1$gprofb/cpp_scen1$GDP  #Beneficios de los bancos/GDP

C.GDP_scen2 = cpp_scen2$Con/cpp_scen2$GDP #Consumo/GDP
I.GDP_scen2 = (cpp_scen2$Ir*cpp_scen2$p)/cpp_scen2$GDP #Inversión/GDP
Ex.GDP_scen2= (cpp_scen2$EX)/cpp_scen2$GDP #Exportaciones/GDP
IM.GDP_scen2= (cpp_scen2$IM*cpp_scen2$pw*cpp_scen2$en)/cpp_scen2$GDP #Exportaciones/GDP
TB.GDP_scen2= cpp_scen2$TB  #Balance comercial
CA.GDP_scen2= cpp_scen2$CA #Balance en cuenta corriente
Un_scen2= (cpp_scen2$Pop-cpp_scen2$L)/cpp_scen2$Pop  #Tasa de desempleo
Wsh_scen2= (cpp_scen2$w*cpp_scen2$L)/cpp_scen2$GDP  #Participación salarial/GDP
prf_scen2= cpp_scen2$gproff/cpp_scen2$GDP  #Beneficios de las firmas/GDP
prb_scen2= cpp_scen2$gprofb/cpp_scen2$GDP  #Beneficios de los bancos/GDP

X11()
par(mfcol=c(2,5))
matplot(cppSimul_scen1$time, cbind(C.GDP.B, C.GDP_scen1, C.GDP_scen2), main="Consumption (%GDP)",lty=c(1,2,2),col=c("red","black","blue") ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
legend("bottomright", bty='n',legend=c("Baseline","Scenario 1 (B=2.7)","Scenario 2 (B=4.7)"),lty=c(1,2,2),col=c("red","black","blue"))
matplot(cppSimul_scen1$time, cbind(I.GDP.B, I.GDP_scen1, I.GDP_scen2), main="Investment (%GDP)",lty=c(1,2,2),col=c("red","black","blue") ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
legend("bottomright", bty='n',legend=c("Baseline","Scenario 1 (B=2.7)","Scenario 2 (B=4.7)"),lty=c(1,2,2),col=c("red","black","blue"))
matplot(cppSimul_scen1$time, cbind(Ex.GDP.B, Ex.GDP_scen1, Ex.GDP_scen2), main="Exports (%GDP)",lty=c(1,2,2),col=c("red","black","blue") ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
legend("bottomright", bty='n',legend=c("Baseline","Scenario 1 (B=2.7)","Scenario 2 (B=4.7)"),lty=c(1,2,2),col=c("red","black","blue"))
matplot(cppSimul_scen1$time, cbind(IM.GDP.B, IM.GDP_scen1, IM.GDP_scen2), main="Imports (%GDP)",lty=c(1,2,2),col=c("red","black","blue") ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
legend("bottomright", bty='n',legend=c("Baseline","Scenario 1 (B=2.7)","Scenario 2 (B=4.7)"),lty=c(1,2,2),col=c("red","black","blue"))
matplot(cppSimul_scen1$time, cbind(TB.GDP.B, TB.GDP_scen1, TB.GDP_scen2), main="Trade Balance (%GDP)",lty=c(1,2,2),col=c("red","black","blue") ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
legend("bottomright", bty='n',legend=c("Baseline","Scenario 1 (B=2.7)","Scenario 2 (B=4.7)"),lty=c(1,2,2),col=c("red","black","blue"))
matplot(cppSimul_scen1$time, cbind(CA.GDP.B, CA.GDP_scen1, CA.GDP_scen2), main="Current Account (%GDP)",lty=c(1,2,2),col=c("red","black","blue") ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
legend("bottomright", bty='n',legend=c("Baseline","Scenario 1 (B=2.7)","Scenario 2 (B=4.7)"),lty=c(1,2,2),col=c("red","black","blue"))
matplot(cppSimul_scen1$time, cbind(Un.B, Un.GDP_scen1, Un.GDP_scen2), main="Unemployment Rate (%)",lty=c(1,2,2),col=c("red","black","blue") ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
legend("bottomright", bty='n',legend=c("Baseline","Scenario 1 (B=2.7)","Scenario 2 (B=4.7)"),lty=c(1,2,2),col=c("red","black","blue"))
matplot(cppSimul_scen1$time, cbind(Wsh.B, Wsh_scen1, Wsh_scen2), main="Wage Share (%GDP)",lty=c(1,2,2),col=c("red","black","blue") ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
legend("bottomright", bty='n',legend=c("Baseline","Scenario 1 (B=2.7)","Scenario 2 (B=4.7)"),lty=c(1,2,2),col=c("red","black","blue"))
matplot(cppSimul_scen1$time, cbind(prf.B, prf_scen1, prf_scen2), main="Firms Profits (%GDP)",lty=c(1,2,2),col=c("red","black","blue") ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
legend("bottomright", bty='n',legend=c("Baseline","Scenario 1 (B=2.7)","Scenario 2 (B=4.7)"),lty=c(1,2,2),col=c("red","black","blue"))
matplot(cppSimul_scen1$time, cbind(prb.B, prb_scen1, prb_scen2), main="Banks Profits (%GDP)",lty=c(1,2,2),col=c("red","black","blue") ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
legend("bottomright", bty='n',legend=c("Baseline","Scenario 1 (B=2.7)","Scenario 2 (B=4.7)"),lty=c(1,2,2),col=c("red","black","blue"))



#Sensitivity Analysis - Gráficas 2

PD.GDP_scen1= cpp_scen1$Bg/cpp_scen1$GDP #Deuda pública/GDP
PDh.GDP_scen1= cpp_scen1$Bgh/cpp_scen1$GDP #Deuda pública hogares/GDP
PDb.GDP_scen1= cpp_scen1$Bgb/cpp_scen1$GDP #Deuda pública bancos/GDP
PDw.GDP_scen1= cpp_scen1$Bgrow/cpp_scen1$GDP #Deuda pública Resto del mundo/GDP
G.GDP_scen1= (cpp_scen1$G+cpp_scen1$Gs)/cpp_scen1$GDP #Gasto Público/GDP
T.GDP_scen1= ((cpp_scen1$T_Rev)/cpp_scen1$GDP) #Ingresos fiscales/GDP
FD.GDP_scen1= (cpp_scen1$BgDot/cpp_scen1$GDP) #Déficit fiscal
FXR_scen1 = cpp_scen1$Rfx/(cpp_scen1$IM*cpp_scen1$pw)

PD.GDP_scen2= cpp_scen2$Bg/cpp_scen2$GDP #Deuda pública/GDP
PDh.GDP_scen2= cpp_scen2$Bgh/cpp_scen2$GDP #Deuda pública hogares/GDP
PDb.GDP_scen2= cpp_scen2$Bgb/cpp_scen2$GDP #Deuda pública bancos/GDP
PDw.GDP_scen2= cpp_scen2$Bgrow/cpp_scen2$GDP #Deuda pública Resto del mundo/GDP
G.GDP_scen2= (cpp_scen2$G+cpp_scen2$Gs)/cpp_scen2$GDP #Gasto Público/GDP
T.GDP_scen2= ((cpp_scen2$T_Rev)/cpp_scen2$GDP) #Ingresos fiscales/GDP
FD.GDP_scen2= (cpp_scen2$BgDot/cpp_scen2$GDP) #Déficit fiscal
FXR_scen2 = cpp_scen2$Rfx/(cpp_scen2$IM*cpp_scen2$pw)

x11()
par(mfcol=c(2,4))
matplot(cppSimul_scen1$time, cbind(PD.GDP.B, PD.GDP_scen1, PD.GDP_scen2), main="Public Debt (%GDP)",lty=c(1,2,2),col=c("red","black","blue") ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
legend("bottomright", bty='n',legend=c("Baseline","Scenario 1 (B=2.7)","Scenario 2 (B=4.7)"),lty=c(1,2,2),col=c("red","black","blue"))
matplot(cppSimul_scen1$time, cbind(PDh.GDP.B, PDh.GDP_scen1, PDh.GDP_scen2), main="Households holdings of gov.Bonds (%GDP)",lty=c(1,2,2),col=c("red","black","blue") ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
legend("bottomright", bty='n',legend=c("Baseline","Scenario 1 (B=2.7)","Scenario 2 (B=4.7)"),lty=c(1,2,2),col=c("red","black","blue"))
matplot(cppSimul_scen1$time, cbind(PDb.GDP.B, PDb.GDP_scen1, PDb.GDP_scen2), main="Banks holdings of gov.Bonds (%GDP)",lty=c(1,2,2),col=c("red","black","blue") ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
legend("bottomright", bty='n',legend=c("Baseline","Scenario 1 (B=2.7)","Scenario 2 (B=4.7)"),lty=c(1,2,2),col=c("red","black","blue"))
matplot(cppSimul_scen1$time, cbind(PDw.GDP.B, PDw.GDP_scen1, PDw.GDP_scen2), main="Foreign holdings of gov.Bonds (%GDP)",lty=c(1,2,2),col=c("red","black","blue") ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
legend("bottomright", bty='n',legend=c("Baseline","Scenario 1 (B=2.7)","Scenario 2 (B=4.7)"),lty=c(1,2,2),col=c("red","black","blue"))
matplot(cppSimul_scen1$time, cbind(G.GDP.B, G.GDP_scen1, G.GDP_scen2), main="Government Spending (%GDP)",lty=c(1,2,2),col=c("red","black","blue") ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
legend("bottomright", bty='n',legend=c("Baseline","Scenario 1 (B=2.7)","Scenario 2 (B=4.7)"),lty=c(1,2,2),col=c("red","black","blue"))
matplot(cppSimul_scen1$time, cbind(T.GDP.B, T.GDP_scen1, T.GDP_scen2), main="Fiscal revenue (%GDP)",lty=c(1,2,2),col=c("red","black","blue") ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
legend("bottomright", bty='n',legend=c("Baseline","Scenario 1 (B=2.7)","Scenario 2 (B=4.7)"),lty=c(1,2,2),col=c("red","black","blue"))
matplot(cppSimul_scen1$time, cbind(FD.GDP.B, FD.GDP_scen1, FD.GDP_scen2), main="Public Deficit (%GDP)",lty=c(1,2,2),col=c("red","black","blue") ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
legend("bottomright", bty='n',legend=c("Baseline","Scenario 1 (B=2.7)","Scenario 2 (B=4.7)"),lty=c(1,2,2),col=c("red","black","blue"))
matplot(cppSimul_scen1$time, cbind(FXR.B, FXR_scen1, FXR_scen2), main="FX reserves (%Imports)",lty=c(1,2,2),col=c("red","black","blue") ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
legend("bottomright", bty='n',legend=c("Baseline","Scenario 1 (B=2.7)","Scenario 2 (B=4.7)"),lty=c(1,2,2),col=c("red","black","blue"))



#Sensitivity Analysis - Gráficas 3
infl_scen1= cpp_scen1$pDot/cpp_scen1$p #Inflación
ner_scen1= cpp_scen1$en #Tasa de cambio nominal
rer_scen1= cpp_scen1$er #Tasa de cambio real
r.idep_scen1= cpp_scen1$idep-(cpp_scen1$pDot/cpp_scen1$p) #Real Deposit Rate
r.ibg_scen1= cpp_scen1$ibg-(cpp_scen1$pDot/cpp_scen1$p) #Real Public Bond Rate
idL_scen1= cpp_scen1$idL #interest rate on domestic currency loans
idLfx_scen1= cpp_scen1$ifxf #interest rate on FX loans
arb.f_scen1= cpp_scen1$arbf #Firms arbitrage
arb.w_scen1= cpp_scen1$arbrow #Arbitrage of the RoW
rsk_scen1= cpp_scen1$rsk #Country risk


infl_scen2= cpp_scen2$pDot/cpp_scen2$p #Inflación
ner_scen2= cpp_scen2$en #Tasa de cambio nominal
rer_scen2= cpp_scen2$er #Tasa de cambio real
r.idep_scen2= cpp_scen2$idep-(cpp_scen2$pDot/cpp_scen2$p) #Real Deposit Rate
r.ibg_scen2= cpp_scen2$ibg-(cpp_scen2$pDot/cpp_scen2$p) #Real Public Bond Rate
idL_scen2= cpp_scen2$idL #interest rate on domestic currency loans
idLfx_scen2= cpp_scen2$ifxf #interest rate on FX loans
arb.f_scen2= cpp_scen2$arbf #Firms arbitrage
arb.w_scen2= cpp_scen2$arbrow #Arbitrage of the RoW
rsk_scen2= cpp_scen2$rsk #Country risk


x11()
par(mfcol=c(2,5))
matplot(cppSimul_scen1$time, cbind(infl.B, infl_scen1, infl_scen2), main="Inflation Rate",lty=c(1,2,2),col=c("red","black","blue") ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20),ylim=c(0.04,0.06))
legend("bottomright", bty='n',legend=c("Baseline","Scenario 1 (B=2.7)","Scenario 2 (B=4.7)"),lty=c(1,2,2),col=c("red","black","blue"))
matplot(cppSimul_scen1$time, cbind(ner.B, ner_scen1, ner_scen2), main="Nominal Exchange Rate",lty=c(1,2,2),col=c("red","black","blue") ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20),)
legend("bottomright", bty='n',legend=c("Baseline","Scenario 1 (B=2.7)","Scenario 2 (B=4.7)"),lty=c(1,2,2),col=c("red","black","blue"))
matplot(cppSimul_scen1$time, cbind(rer.B, rer_scen1, rer_scen2), main="Real Exchange Rate",lty=c(1,2,2),col=c("red","black","blue") ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20),ylim=c(0.8,1.1))
legend("bottomright", bty='n',legend=c("Baseline","Scenario 1 (B=2.7)","Scenario 2 (B=4.7)"),lty=c(1,2,2),col=c("red","black","blue"))
matplot(cppSimul_scen1$time, cbind(r.idep.B, r.idep_scen1, r.idep_scen2), main="Real Deposit Rate",lty=c(1,2,2),col=c("red","black","blue") ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20),ylim=c(0.015,0.03))
legend("bottomright", bty='n',legend=c("Baseline","Scenario 1 (B=2.7)","Scenario 2 (B=4.7)"),lty=c(1,2,2),col=c("red","black","blue"))
matplot(cppSimul_scen1$time, cbind(r.ibg.B, r.ibg_scen1, r.ibg_scen2), main="Real Public Bonds Rate",lty=c(1,2,2),col=c("red","black","blue") ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20),ylim=c(0.015,0.03))
legend("bottomright", bty='n',legend=c("Baseline","Scenario 1 (B=2.7)","Scenario 2 (B=4.7)"),lty=c(1,2,2),col=c("red","black","blue"))
matplot(cppSimul_scen1$time, cbind(idL.B, idL_scen1, idL_scen2), main="Interest rate on Dom. Currency Loans",lty=c(1,2,2),col=c("red","black","blue") ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
legend("bottomright", bty='n',legend=c("Baseline","Scenario 1 (B=2.7)","Scenario 2 (B=4.7)"),lty=c(1,2,2),col=c("red","black","blue"))
matplot(cppSimul_scen1$time, cbind(idLfx.B, idLfx_scen1, idLfx_scen2), main="Interest rate on FX Loans",lty=c(1,2,2),col=c("red","black","blue") ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20),ylim=c(0.05,0.12))
legend("bottomright", bty='n',legend=c("Baseline","Scenario 1 (B=2.7)","Scenario 2 (B=4.7)"),lty=c(1,2,2),col=c("red","black","blue"))
matplot(cppSimul_scen1$time, cbind(arb.f.B, arb.f_scen1, arb.f_scen2), main="Firms Arbitrage",lty=c(1,2,2),col=c("red","black","blue") ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
legend("bottomright", bty='n',legend=c("Baseline","Scenario 1 (B=2.7)","Scenario 2 (B=4.7)"),lty=c(1,2,2),col=c("red","black","blue"))
matplot(cppSimul_scen1$time, cbind(arb.w.B, arb.w_scen1, arb.w_scen2), main="RoW Arbitrage",lty=c(1,2,2),col=c("red","black","blue") ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
legend("bottomright", bty='n',legend=c("Baseline","Scenario 1 (B=2.7)","Scenario 2 (B=4.7)"),lty=c(1,2,2),col=c("red","black","blue"))
matplot(cppSimul_scen1$time, cbind(rsk.B, rsk_scen1, rsk_scen2), main="Country Risk",lty=c(1,2,2),col=c("red","black","blue") ,type="l", ylab="", xlab="", lwd=2,xlim=c(0.8,20))
legend("bottomright", bty='n',legend=c("Baseline","Scenario 1 (B=2.7)","Scenario 2 (B=4.7)"),lty=c(1,2,2),col=c("red","black","blue"))



plot(cppBaseline$Ir)
 
#Checking Consistency 
#Households
Check.Hou=-cppBaseline$Con +(cppBaseline$w *cppBaseline$L) + (cppBaseline$idep*cppBaseline$Dep)+(cppBaseline$ibg*cppBaseline$Bgh)+ (cppBaseline$Divf + cppBaseline$Divb) +(cppBaseline$Rem*cppBaseline$en)-(0.2*cppBaseline$w*cppBaseline$L)+cppBaseline$Ge-cppBaseline$DepDot-cppBaseline$BghDot
Check.Hou
#Firms
Check.Fir.Cur= cppBaseline$Con+cppBaseline$Gd+cppBaseline$Ir*cppBaseline$p-cppBaseline$IM*cppBaseline$pw*cppBaseline$en+cppBaseline$EX-cppBaseline$w*cppBaseline$L-cppBaseline$idL*cppBaseline$Ldf-cppBaseline$ifxf*cppBaseline$Lfxf*cppBaseline$en-cppBaseline$Divf-0.2*cppBaseline$gproff-cppBaseline$REf
Check.Fir.Cur
Check.Fir.Cap=-cppBaseline$Ir*cppBaseline$p+cppBaseline$REf+(cppBaseline$LdfDot-cppBaseline$DfxfDot*cppBaseline$en+cppBaseline$LfxfDot*cppBaseline$en)
Check.Fir.Cap

#Government
Check.Gov=-cppBaseline$G-cppBaseline$ibg*cppBaseline$Bg+(cppBaseline$ip*cppBaseline$Ad+cppBaseline$ibg*cppBaseline$Bgcb)+cppBaseline$Tax+cppBaseline$BgDot
Check.Gov
#Banks
Check.Ban.Cur=-cppBaseline$idep*cppBaseline$Dep+cppBaseline$idL*cppBaseline$Ldf+cppBaseline$ifxf*cppBaseline$Lfxf*cppBaseline$en-cppBaseline$ifxb*cppBaseline$Lfxb*cppBaseline$en+cppBaseline$ibg*cppBaseline$Bgb-cppBaseline$ip*cppBaseline$Ad-cppBaseline$Divb-0.2*cppBaseline$gprofb-cppBaseline$REb
Check.Ban.Cur
Check.Ban.Cap=cppBaseline$REb+cppBaseline$DepDot-cppBaseline$ResDot-cppBaseline$LdfDot-cppBaseline$BgbDot+cppBaseline$AdDot+cppBaseline$DfxfDot*cppBaseline$en-cppBaseline$RfxbDot*cppBaseline$en-cppBaseline$LfxfDot*cppBaseline$en+cppBaseline$LfxbDot*cppBaseline$en
Check.Ban.Cap
#Central Bank
Check.CB.Cur=cppBaseline$ibg*cppBaseline$Bgcb+cppBaseline$ip*cppBaseline$Ad-(cppBaseline$ibg*cppBaseline$Bgcb+cppBaseline$ip*cppBaseline$Ad)
Check.CB.Cur
Check.CB.Cap=cppBaseline$ResDot-cppBaseline$BgcbDot-cppBaseline$AdDot-cppBaseline$RfxcbDot*cppBaseline$en
Check.CB.Cap
#Rest of the World
Check.RoW=cppBaseline$IM*cppBaseline$pw*cppBaseline$en-cppBaseline$EX+cppBaseline$ifxb*cppBaseline$Lfxb*cppBaseline$en+cppBaseline$ibg*cppBaseline$Bgrow-cppBaseline$Rem*cppBaseline$en-cppBaseline$BgrowDot+cppBaseline$RfxDot*cppBaseline$en-cppBaseline$LfxbDot*cppBaseline$en
Check.RoW




