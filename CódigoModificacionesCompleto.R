##intermediate variables

#--------
#Firms
#--------

#Aggregate demand
Yd=Con+Gd+Ir*p+EX #(*Nominal,domestic currency*)(*2*)
#Desired inventories
Vd=alphav*Ye # (*Real*)(*4*)
#Desired level of inventories investment
Ivd=(Vd-V)#(*Real*)(*5*)
#Production
Yp=Ye+Ivd #(*Real*)(*6*)
#Domestic Production
Ypd=Yp-IM #(*Real*)(*7*)
#Utilization rate of capital
u=Ypd/(Kap*alphak)#(*Not Numbered*)#Check paper
#Imports Demand
IM=sigmaMC*(Con)/p + sigmaMg*(Gd)/p + sigmaMI*(Ir) + sigmaMX*EX/(en*pw) #(*Real*)
#Target imports propensities
sigmaMCtar=max(sigmaMCmin,1/(1+(betac/(1-betac)*er*(1+taumC))^epsilonc)) #*10a*
sigmaMgtar=max(sigmaMGmin,1/(1+(betagg/(1-betagg)*er*(1+taumC))^epsilong))#*10b*
sigmaMXtar=max(sigmaMXmin,1/(1+(betax/(1-betax)*er*(1+taumx))^epsilonX)) #*10c*
sigmaMItar=max(sigmaMImin,1/(1+(betain/(1-betain)*er*(1+taumI))^epsilonin)) #*10d*
#Real exchange rate
er=pw*en/p #(*11*)
#Exports 
EX=sigmaX*GDPw*en*pw + X_a #(*Nominal,Domestic Currency)(*12*)
#Target exports elasticity
sigmaXtar=sigmaX0*(er)^sigmaer #(*14*) #Check export taxes
#Desired price level
pd=(1+mu)*HUC #(*Nominal, domestic currency*)(*15*)
#Mark-up
mu=mu0-mu1*(V/Ye-alphav) #(*16*)
#Unitary costs
UC=(w*L+pw*en*IM+TIM)/Yp #(*Nominal, domestic currency*)(*18*)
#Realized investment (*real*)
Ir=Id + (Fdi_G*en)/p
#Desired Investment function (*real*)
Id=(kappa0 + kappa1*(rfe-pDot/p)+kappa2*(u))*Kap#(*Real*)(*21*)
#Gross expected profits for firms
GFFe=p*Ye-HUC*Yp-idLf*Ldf-ifxf*Lfxf*ene#(*Nominal, domestic currency*)(*22*) #Check interests on deposits
#Net expected profits for firms
FFe=(1-tauPif)*(GFFe)#(*Nominal, domestic currency*)(*23*)
#expected return rate
rfe=FFe/(p*Kap)#(*24*)
#Total financial needs for firms
TFNf=Id*p-REf-xi_f*Fdi_NG*en#(*Nominal, domestic currency*)(*25*)
#Retained earnings for firms
REf=sf*FFe#(*26*)
#arbitrage parameter for firms loans
betalf=betalfmin+beta1*(arbf)#(*31*)
#arbitrage condition 
arbf= cd-cfx#(*32*)
cd=1+idLftar#(*32.a*)
cfx=(1+ifxf)*(ene+eneDot)/en#(*32.b*)
#Actual Gross Profits
gproff=Yd-w*L-pw*en*IM-tau_r*X_a-idLf*Ldf-ifxf*Lfxf*en#(*Nominal, domestic currency*)(*34*) #Check deposit interests
#Actual Net Profits
proff=(1-tauPif)*(gproff)#(*Nominal, domestic currency*)(*35*)
#Actual Return rate
rf=proff/(p*Kap)#(*36*)
#Expected firms dividends
Divfe=FFe-sf*FFe-DfxfDot*en
#Firms dividends
Divf=proff-REf-DfxfDot*en#(*37*)
#Firms Dividends paid to the RoW
Divf_W = (EQf_W/EQf)*Divf #(*Nominal,domestic currency*)
#Firms Dividends paid to the households
Divf_H = (EQf_H/EQf)*Divf #(*Nominal,domestic currency*)  
#Real expected imports
IMe=(1/ere)*sigmaM*Yp#(*Real*)(*Not numbered*)(Check)
#Real expected exchange rate
ere=ene*pw/p #(*Not numbered*)
#Unemployment rate 
Unemp = 1-L/Pop #(*Not numbered*)

#---------
#Banks
#---------

#Cross border lending rationing parameter
ji_Lfx =ji0 + ji1*NIIP + ji2*((YeDot+Ivd)/Yp)
#Cross border lending rate charged to the banks
ifxb=(1+theta_Lfxb)*ifp
#Cross border lending risk premium
theta_Lfxb = theta0_Lfxb*rsk 
#Interest rate on Foreign Exchange Loans charged to the firms
ifxf =ifxb+prem #(*46*)
#Target premium on FX loans
premtar=phi0+phi1*((Lfxf*ene+Ldf)/FFe)^phi3 #(*48*)
#Interest rate on households' loans
idLh = (1+phiLh)*idLf
#Required regulatory change in bank foreign reserves
RfxbnopDot=DfxfDot#(*Nominal, foreign currency*)(*49*)
#Final change in FX reserves owned by the Central Bank
RfxmbDot=RfxDot-RfxcbintDot#(*Nominal, foreign currency*)(*50*)
#Domestic Bank Reserves 
#Total Financial Needs of Banks
TFNb=(LdfDot+LdhDot+BgbDot)+rrr*(Deph+DephDot+Depg+DepgDot)-(DephDot+DepgDot+OFbDot)-Res-(1-xi_f)*Fdi_NG*en #(*Nominal, domestic currency*)(*51*)
#Own funds needed to respect capital adequacy requirement
OFbT = digamma*(Ldf+Ldh+Lfxf*en)#(*Nominal, domestic currency*)(*53*)
#Retained earnings by the banks
REb=Delta*(OFbT-OFb)#(*Nominal, domestic currency*)(*54*)
#Actual gross profits
gprofb=idLf*Ldf+idLh*Ldh+ifxf*Lfxf*en+ibg*Bgb-idep*(Deph+Depg)-ip*Ad-ifxb*Lfxb*en#(*Nominal, domestic currency*)(*56)
#Interest rate on deposits
idep=ip-rho3 *((Ldf+Ldh+Bgb)/Ad)^phi #(*57*)
#Target interest rate on domestic currency loans - idLfftar= AFC+phi0+phi1*((Lfxf*ene+Ldf)/FFe)^phi3 #(*58*)#Check Equation
idLftar= AFC+prem #(*58*)#Check Equation
#Average Funding Cost
AFC=(Ad*ip+idep*(Deph+Depg))/(Ad+Deph+Depg)#(*Nominal, domestic currency*)(*59*)
#Actual net profits
profb=(1-tauPib)*(gprofb)#(*Nominal, domestic currency*)(*61*)
#Dividends
Divb=profb-REb #(*Nominal, domestic currency*)(*62*)
#Bank Dividends paid to the RoW
Divb_W = (EQb_W/EQb)*Divb    
#Bank Dividends paid to the households
Divb_H = (EQb_H/EQb)*Divb     
#----------
#Households
#----------

#Desired target Consumption 
ConTd =mpc1*(YF_Lh)+mpc3*(YF_Lh)+mpc2*(Deph+Bgh) #(*Nominal, domestic currency*)(*80*) #Check Equation: Expected Firms profits
#Households net labour income
YD_Lh=((1-tauw)*w*L+Ge) #(*nominal,domestic currency*)
#Households financial income
YF_Lh=idep*Deph+Rem*en+ibg*Bgh+Divb_H+(EQf_H/EQf)*Divfe
#Consumption
Con = Cond + LdhDot
#Households savings  
Sh =(YD_Lh+YF_Lh)-(Con+idLh*Ldh)#(*Nominal, domestic currency*)(*85*) 
#Marginal propensity to consume labour income
mpc1=1-lambda0w*(idep-pDot/p)^lambda1w #(*81*)
#Marginal propensity to consume financial income
mpc3=1-lambda0a*(idep-pDot/p)^lambda1a #(*82*)
#Marginal propensity to consume households wealth
mpc2=1-lambda0wl*(idep-pDot/p)^lambda1wl #(*83*)
#Sensitivity oh households' loans demand to desired consumption
ipsilon1 = 1-Bur  
#Burden of the households
Bur =((rep+idLh)*Ldh/(1-tauw)*w*L)    
#Allocation parameter of households savings
upsilonH=Omega0BA+Omega1BA*(((1+ibg)/(1+idep)))^sigmaBA #(*88*)
#Employment
L=Ypd/a #(*Real*)(*75*) 

#-----------
#Government
#-----------

#Total Government spending (with interest payments)
G = Gd + Gs + Bg*ibg  # (*Nominal, domestic currency*)(*Not numbered*)
#Government demand
Gd = Go + Gi  #(*Nominal, domestic currency*)
#Government Operating spending
Go = fi1*(Yp*p)#(*Nominal, domestic currency*)(*63*)
#Target Public Investment
GiT = fi2*(Yp*p) + (1-thetar)*(tau_r*X_a)
#Government transfers to households
Gs = fi3*w*(fi4*pop)#(*Nominal, domestic currency*)(*64*)
#Total revenue
T_Rev= Tax + tau_r*X_a +idep*Depg 
#Tax revenue
Tax =tauw*w*L+tauPif*gproff+tauPib*gprofb + TIM #(*Nominal, domestic currency*)(*66*)
#Total Import Taxes
TIM= (taumC*(sigmaMC*(Con)/p)*pw*en) + (taumC*(sigmaMg*(Gd)/p)*pw*en)+(taumx*(sigmaMX*EX/(en*pw))*pw*en)+ (taumI*(sigmaMI*(Ir))*pw*en) #(*67*)#Check Equation
#Interest rate on Government Bonds 
ibg =pDot/p+(iota5)*(Bg/(Yp*p-IM*pw*ene)) #(*69*)#Check Equation

 
#--------------
#Central Bank
#-------------

#Domestic Policy Rate - ip = iota7+(pDot/p)+iota8*(pDot/p-iota9)#(*71*)#Check Equation
ip = iota7+pDot/p+iota8*(pDot/p-iota9)#(*71*)#Check Equation
#RfxcbintDot=Piecewise[{{max(Sigma*IM*pw-Rfxcb,0],(BgrowDot+LfxfDot)>=0.001},{Sigma*IM*pw-Rfxcb,(BgrowDot+LfxfDot)<0.001}})#
RfxcbintDot=ifelse((BgrowDot+LfxfDot)>=0.001, max(Sigma*IM*pw-Rfxcb,0), Sigma*IM*pw-Rfxcb)#(*73*)#Check Equation #BgrowDot*en 

#-------------------------------
#World Trends & Portfolio flows
#-------------------------------
#Global Capital Flows
GFF=alphagff*GDPw*pw# (*Nominal, foreign currency*)(*90*)
#Share of portfolio flows/government bonds entering to the economy*****
psi_wffd = psi0 + psi1*(arbrow)      
#Rest of the world arbitrage condition
arbrow= rBG_e - rW_e #(*Not numbered*)
#Expected domestic yield
rBG_e = ((1+ibg)*(1-rsk))/((eneDot+ene)/en) #(*92*)
#Expected foreign yield
rW_e = (1+ifb)#(*93*)
#Interest rate on foreign bonds
ifb=ifp+(ifbr-ifpr) #(*Not numbered*)(*Check Equation*)
#Country Risk
rsk=((0.015/(1+exp(-beta10*NIIP+beta11)))+kuku)#(*94*)
#Net International Investment Position
NIIP=-((Rfx*en-Lfxb*en-Bgrow-EQ_W)/(Yp*p-IM*pw*en)) #(*95)
#Foreign Policy Rate
ifp=ifpr+switchpolicy*valuepolicy
#Greenfield Investment (*Nominal,foreign currency*)
Fdi_G =etafg*Fdi
#Non-greenfield FDI
Fdi_NG =(1-etafg)*Fdi#(*nominal, foreign currency*)

#--------------------------------------------
#Balance of Payments & Exchange Rate Dynamics
#---------------------------------------------
#Foreign Exchange Demand - Dfx=IM*pw+IA+RfxbnopDot-ibg*Bgrow/en #(*Nominal, foreign currency*)#(*97)#Check Equation
Dfx=IM*pw+ifxb*Lfxb+ibg*Bgrow/en+Divf_W/en+Divb_W/en+RfxbnopDot #(*Nominal, foreign currency*)#(*97)#Check Equation
#Foreign Exchange supply
Sfx=EX/en+LfxbDot+Rem+Fdi+BgrowDot/en-RfxcbintDot #(*Nominal, foreign currency*)(*98*)
# Income Account - IA= ifxb*Lfxb #(*100*)
IA = Rem*en-ibg*Bgrow-ifxb*Lfxb*en-Divf_W-Divb_W #(Nominal, Domestic Currency)(*100*)

#-----------------------------
# Other Variables
#----------------------------
#Gross Domestic Product
GDP = Ye*p-IM*pw*ene #(Check Equation)
#Check
RfxoverpoverYpd = Rfx/p/Ypd
#Trade Balance (%GDP)
TB = (EX-IM*en*pw)/GDP
#Check
YdoverpoverYpd = Yd/p/Ypd
#Current Account (%GDP)
CA = (EX-IM*en*pw-ifxb*Lfxb*en+Rem*en-ibg*Bgrow-Divf_W-Divb_W)/GDP
#Government Debt (%GDP)
BgoverGDP = Bg/GDP
#Investment (%GDP)
IroverYpd = Ir/Ypd
#Inflation
Infl = pDot/p
#Government External Debt (%GDP)
BgrowoverGDP = Bgrow/GDP

##time derivatives
Ye=betay*(Yd/p-Ye)+(alphaa+alphap)*Ye #Expected sales variation (*Real*)(*1*)
V=Yp-Yd/p #Change in inventories (*Real*)(*2*)
sigmaMX=betaimp*(sigmaMXtar-sigmaMX) #Change in target imports propensity (*9*)
sigmaMI=betaimp*(sigmaMItar-sigmaMI) #Change in target imports propensity (*9*)
sigmaMC=betaimp*(sigmaMCtar-sigmaMC) #Change in target imports propensity (*9*)
sigmaMg=betaimp*(sigmaMgtar-sigmaMg) #Change in target imports propensity (*9*)
X_a=sigmaX_aut*X_a #Autonomous exports growth
sigmaX=betaexp*(sigmaXtar-sigmaX) #Change in exports elasticity (*13*)
HUC=zeta*(UC-HUC)#Change in Historical Unitary Cost (*Nominal, domestic currency*)(*17*)
p=nu2*(pd-p)#Change in price level(*19*)
Kap=Ir-delta*Kap #Gross Capital Formation (*Real*) (*20*)
Lfxfdes=betalf*TFNf/en#(*27*)#Check equation 
Lfxf = Lfxb #New FX Loans borrowed by the firms(*Nominal, foreign currency*)(*40*)
Ldf=TFNf-LfxfDot*en# New Domestic Loans Borrowed by the firms (*Nominal, domestic currency*)(*29*)
Dfxf=eta*LfxfDot #New Firms FX Deposits(*Nominal, foreign currency*)(*33*)
a=a*(alphaa) #Labour Productivity Growth (*76*)
Pop=Pop*alphap #Population Growth (*Not numbered)
w=w*(omega0+omega1*(L/Pop-omega3)+omega2*pDot/p)#Change in nominal wages (*Nominal, domestic currency*)(*77*)
Cond=betacon*(ConTd-Cond)#Change in Consumption level (*Nominal, domestic currency*) (*84)
Ldh = ipsilon1*(Cond)^ipsilon2   #Households loans demand
Bgh =upsilonH*Sh #Government Bonds Purchased by the Households(*Nominal, domestic currency*)(*86*)
Deph=Sh-BghDot #New Households Deposits (*Nominal, domestic currency*)(*87*)
Depg =  thetar*(tau_r*X_a) #New Government Deposits
Gi = betagi*(GiT-Gi) #Change in Public Investment
Bg=G-(T_Rev-thetar*(tau_r*X_a))-ip*Ad  #New bonds supply issued by the Government (*Nominal, domestic currency*) (*68*)
Bgb= BgDot-BghDot-BgrowDot #Quantity bonds purchased by the banks
Lfxbdes = LfxfdesDot     #Desired Banks demand for cross border lending.
Lfxb=  ji_Lfx*LfxbdesDot  #Desired Banks demand for cross border lending.              #Effective Banks demand for cross border lending
prem=2*(premtar-prem)#Premium variation(*47*)
Rfxb=RfxbnopDot #required regulatory change in bank foreign reserves (*Nominal, foreign currency*)(*49*)
Rfxcb=RfxDot-RfxbnopDot #Change in Central Bank FX Reserves(*Nominal, foreign currency*)(*50*)
Ad=TFNb #Liquidity advances from the central bank to the banks(*Nominal, domestic currency*)(*52*)
Res=rrr*(DephDot+DepgDot)#(*Nominal, domestic currency*)
OFb = REb #Change in the funds owned by the banks (*Nominal, domestic currency*)(*55*)
idLf=betaidLf*(idLftar-idLf)#Change in interest rate on domestic loans (*60*)
Fdi = gammafdi*Fdi #Total FDI growth
EQ_W = EQf_WDot + EQb_WDot    #Change in total equities owned by the RoW
EQf_W = Fdi_G*en + xi_f*Fdi_NG*en +(REf-delta*Kap)*(EQf_W/EQf) #Change in Firms equities owned by the RoW
EQb_W=(1-xi_f)*Fdi_NG*en+REb*(EQf_H/EQf)    #Change in Banks equities owned by the RoW
EQ_H = EQf_HDot + EQb_HDot  #Change in total equities owned by the household
EQf_H=EQfDot-EQf_WDot#Change in Firms equities owned by the households***
EQb_H=EQbDot-EQb_WDot#Change in Banks equities owned by the households***
EQf=Fdi_G*en + xi_f*Fdi_NG*en +REf -delta*Kap  #Change in Firms equities
EQb=(LdfDot+LdhDot+BgbDot)-(AdDot+(1-rrr)*(DephDot+DepgDot)) #Change in Banks equities 
en =en*betae*(Dfx -Sfx)/Sfx #Change in Nominal Exchange Rate (*96*)
ene=betaene*(Upsilon*((((1+ifp)/((1+ip)*(1-rsk))))^sigmaene)*en-ene) #Change in the expected nominal exchange rate (*99*)
Rem= fi_r*Rem      #Remittances growth
Rfx=EX/en-IM*pw+BgrowDot/en+Fdi+LfxbDot+IA/en  #Change in Domestic FX reserves (*Nominal, foreign currency*)
Bgrow= psi_wffd*GFF*en #Government Bonds Purchased by RoW (*Nominal, domestic currency*)(*103*)***
GDPw=(alphap+alphaa)*GDPw #World GDP growth (*Not numbered)
pw=pw*infls #World Inflation Rate (*Not numbered)
##initial values
Ye=100 
V=10
sigmaMX=0.2
sigmaMI=0.3
sigmaMC=0.20406910810132728
sigmaMg=0.06
sigmaX=0.008023300970873786
X_a=7
HUC=0.5275 
p=1
Kap=185  
Lfxb=10
Lfxf=10
Ldf=30
Dfxf = 2.89
a=1.35
Pop=92  
w=0.481
Cond=65
Ldh=10
Bgh=12.5
Deph=30
Depg=3
Gi=2
Bg=50
Bgb=25
prem=0.04
Rfxb= 2.89
Rfxcb= 15
Ad=4
Res=3.3
OFb=3.3
idLf=0.10
Fdi=4
EQf_W = 14.5 
EQb_W= 0.78
EQ_H = 137.52  
EQf_H= 130.5 
EQb_H= 7.02
EQf=145
EQb=7.8
en=1
ene=1
Rfx= 17.89
Bgrow=12.5
GDPw= 3000
pw=1
##parameters
alphaa = 0.02
alphap = 0.01
alphak = 0.55
delta = 0.065
alphagff = 0.07
omega1 = 0.1
omega2 = 1
omega3 = 0.9
nu2 = 0.25
betacon = 4
zeta = 10
betay = 4
betaimp = 1
betaexp = 1
betapar = 0.5
alphav = 0.1
sf = 0.5
kappa0 = 0.015
kappa1 = 1.5
mu0 = 0.8
mu1 = 2
digamma = 0.12
rrr = 0.1
Delta = 1
betae = 0.7
betaene = 0.75
Upsilon = 1
sigmaene = 1
iota7 = 0.015
iota8 = 0.3
iota9 = 0.02
tauw = 0.2
tauPib = 0.2
tauPif = 0.2
iota5 = 0.05
sigmaMGmin = 0
sigmaMXmin = 0.1
sigmaMImin = 0.1
sigmaMCmin = 0.1
sigmaM = 0.2
betalfmin = 0.1
beta11 = 5
betaidLf = 0.2
beta1 = 2
beta10 = 300
sigmaer = 0.6
eta=0.289
Sigma=0.418267211437954
omega0=0.018564821480475
betain=0.967365028203062
betax=0.996108949416342
betac=0.859933555894369
betagg=0.999998940462032
sigmaX0=0.008023300970874
lambda0a=0.908093285989112
lambda0wl=1.144122577241310
lambda0w=1.191207257567350
Omega1BA=0.018751579439052
rho3=0.000670098091130
phi1=0.000983225146863
ifpr=0.053009242610299
ifbr=0.050773586458241
kuku=0.014899607236136
epsilonX=0.25
epsilonin=0.25
epsilonc=0.75
epsilong=0.2
lambda1a = 0.1
lambda1wl = 0.05
lambda1w = 0.5
sigmaBA = 2
phi = 0.7
phi0 = 0.005
phi3 = 2
Omega0BA = 0.02
infls=0.046188397196641
sigmaX_aut = 0.01   
kappa2= 1
xi_f= 0.7 
tau_r=0.05 
ji0= 0.6 
ji1= -0.2
ji2=  0.1
theta0_Lfxb=2
phiLh=0.5
rep=0.01
fi1=0.14
fi2=0.02
fi3=0.1
fi4=0.15
thetar=0.2
taumC=0.05
taumx=0.025
taumI=0.025
psi0=0.005
psi1=1
etafg=0.1
ipsilon2=0.02
betagi=0.75
fi_r=0.01      
gammafdi=0.01
switchpolicy=0              
valuepolicy=-0.0075          
##time
begin = 0
end = 50
by = 0.01
