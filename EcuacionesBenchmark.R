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
u=Ypd/(Kap*alphak)#(*Not Numbered*)#Check paper #Changed
#Expected utilization rate of capital
u.e=Ye/(Kap*alphak)
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
EX=sigmaX*GDPw*en*pw #(*Nominal,Domestic Currency)(*12*)
#Target exports elasticity
sigmaXtar=sigmaX0*(er)^sigmaer #(*14*) #Check export taxes
#Desired price level
pd=(1+mu)*HUC #(*Nominal, domestic currency*)(*15*)
#Mark-up
mu=mu0-mu1*(V/Ye-alphav) #(*16*)
#Unitary costs
UC=(w*L+pw*en*IM+TIM)/Yp #(*Nominal, domestic currency*)(*18*)
#Investment function
Id=(kappa0 + kappa1*(rfe-pDot/p))*Kap#(*Real*)(*21*)
#Gross expected profits for firms
GFFe=p*Ye-HUC*Yp-idL*Ldf-ifxf*Lfxf*en#(*Nominal, domestic currency*)(*22*) #Check interests on deposits
#Net expected profits for firms
FFe=(1-tauPif)*(GFFe)#(*Nominal, domestic currency*)(*23*)
#expected return rate
rfe=FFe/(p*Kap)#(*24*)
#Total financial needs for firms
TFNf=Id*p-REf#(*Nominal, domestic currency*)(*25*)
#Retained earnings for firms
REf=sf*FFe#(*26*)
#Target arbitrage parameter for firms loans
betalftar=betalfmin+tanh(beta1*(arbf))#(*31*)
#arbitrage condition 
arbf=(cd-cfx)/idLtar#(*32*)
cd=1+idLtar#(*32.a*)
cfx=(1+ifxf)*(ene+eneDot)/en#(*32.b*)
#Realized investment
#(*Ir=(LdfDot+LfxfDot*en+sf*FFe)/p#*)(*Real*) #TFN = Id + sf*FFE
Ir=Id#(*Not numbered*)
#Actual Gross Profits
gproff=Yd-w*L-pw*en*IM-idL*Ldf-ifxf*Lfxf*en#(*Nominal, domestic currency*)(*34*) #Check deposit interests
#Actual Net Profits
proff=(1-tauPif)*(gproff)#(*Nominal, domestic currency*)(*35*)
#Actual Return rate
rf=proff/(p*Kap)#(*36*)
#Firms dividends
Divf=proff-REf-DfxfDot*en#(*37*)
#Real expected imports
IMe=(1/ere)*sigmaM*Yp#(*Real*)(*Not numbered*)
#Real expected exchange rate
ere=ene*pw/p #(*Not numbered*)
#Desired FX loans demand
LfxfDotdes=max(betalf*TFNf/en,-Lfxf)#(*27*)#Check equation 
#Unemployment rate 
Unemp = 1-L/Pop #(*Not numbered*)

#---------
#Banks
#---------
#Desired quantity bonds purchased by the banks
BgbdesDot=upsilon*BgDot#(*Nominal, domestic currency*)(*38*)
# fraction of newly supplied government bonds purchased by the banks
upsilon=Omega0BB+Omega1BB *((1+ibg)/(1+idL))^sigmaBB#(*39*)
#Desired Cross Border Lending Supply
CBLS=koppa*(OFGB/(1-lambda_bar*(1+ifxb)/(1+ifp)))#(*Nominal, foreign currency*)(*41*)
#Leverage ratios of global banks
lambda_bar=lambdalev*ifp^(-4)*rsk^(-4) #(*42*)#Added
#Global Banks' own funds
OFGB=sampi2*GDPw*pw#(*Nominal, foreign currency*)(*45*)
#Cross Border Lending Demand
CBLD=LfxfDotdes+Lfxf#(*Nominal, foreign currency*)(*42*)
#Interest rate on Foreign Exchange Loans
ifxf =ifxb+prem #(*46*)
#Target premium on FX loans
premtar=phi0+phi1*((Lfxf*ene+Ldf)/FFe)^phi3 #(*48*)
#Required regulatory change in bank foreign reserves
RfxbnopDot=DfxfDot#(*Nominal, foreign currency*)(*49*)
#Final change in FX reserves owned by the Central Bank
RfxmbDot=RfxDot-RfxcbintDot#(*Nominal, foreign currency*)(*50*)
#Total Financial Needs of Banks
TFNb=(LdfDot+BgbDot)+rrr*(Dep+DepDot)-(DepDot+OFbDot)-Res #(*Nominal, domestic currency*)(*51*)
#Own funds needed to respect capital adequacy requirement
OFbT = digamma*(Ldf+Lfxf*en)#(*Nominal, domestic currency*)(*53*)
#Retained earnings by the banks
REb=Delta*(OFbT-OFb)#(*Nominal, domestic currency*)(*54*)
#Actual gross profits
gprofb=idL*Ldf+ibg*Bgb-idep*Dep-ip*Ad+ifxf*Lfxf*en -ifxb*Lfxb*en#(*Nominal, domestic currency*)(*56)
#Interest rate on deposits
idep=ip-rho3 *((Ldf+Bgb)/Ad)^phi #(*57*)
#Target interest rate on domestic currency loans - idLtar= AFC+phi0+phi1*((Lfxf*ene+Ldf)/FFe)^phi3 #(*58*)#Check Equation
idLtar= AFC+prem #(*58*)#Check Equation #Changed
#Average Funding Cost
AFC=(Ad*ip+idep*Dep)/(Ad+Dep)#(*Nominal, domestic currency*)(*59*)
#Actual net profits
profb=(1-tauPib)*(gprofb)#(*Nominal, domestic currency*)(*61*)
#Dividends
Divb=profb-REb #(*Nominal, domestic currency*)(*62*)

#----------
#Households
#----------

#Target Consumption 
ConT =mpc1*((1-tauw)*w*L+Ge)+mpc3*(idep*Dep+Rem*en+ibg*Bgh+(FFe-sf*FFe-DfxfDot*ene)+Divb)+mpc2*(Dep+Bgh) #(*Nominal, domestic currency*)(*80*) #Check Equation: Expected Firms profits
#Households savings  
Sh =(1-tauw)*w*L+Ge+idep*Dep+Rem*en+ibg*Bgh+ Divf+Divb-Con#(*Nominal, domestic currency*)(*85*) 
#Marginal propensity to consume labour income
mpc1=1-lambda0w*(idep-pDot/p)^lambda1w #(*81*)
#Marginal propensity to consume financial income
mpc3=1-lambda0a*(idep-pDot/p)^lambda1a #(*82*)
#Marginal propensity to consume households wealth
mpc2=1-lambda0wl*(idep-pDot/p)^lambda1wl #(*83*)
#Allocation parameter of households savings
upsilonH=Omega0BA+Omega1BA*(((1+ibg)/(1+idep)))^sigmaBA #(*88*)
#Employment
L=Ypd/a #(*Real*)(*75*) 

#-----------
#Government
#-----------

#Government spending (without interest payments)
G = Gd+Ge# (*Nominal, domestic currency*)(*Not numbered*)
#Operating Government spending
Gd = rho1*(Yp*p)#(*Nominal, domestic currency*)(*63*)
#Government transfers to households
Ge =rho2*w*(Pop-L)#(*Nominal, domestic currency*)(*64*)
#Tax revenue
Tax =tauw*w*L+tauPif*gproff+tauPib*gprofb + TIM #(*Nominal, domestic currency*)(*66*)
#Total Import Taxes
TIM=0 #(*67*)#Check Equation
#Interest rate on Government Bonds 
ibg =pDot/p+(iota5)*(Bg/(Yp*p-IM*pw*ene)) #(*69*)#Check Equation

#--------------
#Central Bank
#-------------

#Domestic Policy Rate - ip = iota7+(pDot/p)+iota8*(pDot/p-iota9)#(*71*)#Check Equation
ip = iota7+pDot/p+iota8*(pDot/p-iota9)#(*71*)#Check Equation - Changed_ THIS IS THE MISTAKE
#RfxcbintDot=Piecewise[{{max(Sigma*IM*pw-Rfxcb,0],(BgrowDot+LfxfDot)>=0.001},{Sigma*IM*pw-Rfxcb,(BgrowDot+LfxfDot)<0.001}})#
RfxcbintDot=ifelse((BgrowDot/en+LfxfDot)>=0.001, max(Sigma*IM*pw-Rfxcb,0), Sigma*IM*pw-Rfxcb)#(*73*)#Check Equation #Changed


#-------------------------------
#World Trends & Portfolio flows
#-------------------------------
#Global Capital Flows
GFF=alphagff*GDPw*pw# (*Nominal, foreign currency*)(*90*)
#Share of Global Capital Flows entering to the domestic economy
betagff=beta12*tanh(beta1*(arbrow)) #(*91*)
#Rest of the world arbitrage condition
arbrow= (rBG_e - rW_e)/rW_e #(*Not numbered*)
#Expected domestic yield
rBG_e = ((1+ibg)*(1-rsk))/((eneDot+ene)/en) #(*92*)
#Expected foreign yield
rW_e = (1+ifb)#(*93*)
#Interest rate on foreign bonds
ifb=ifp+(ifbr-ifpr) #(*Not numbered*)
#Country Risk
rsk=((0.015/(1+exp(-beta10*resrat+beta11)))+kuku)#(*94*)
#Net International Investment Position
resrat=-((Rfx*en-Lfxb*en-Bgrow)/(Yp*p-IM*pw*en)) #(*95)
#Foreign Policy Rate
ifp=ifpr+switchpolicy*valuepolicy

#--------------------------------------------
#Balance of Payments & Exchange Rate Dynamics
#---------------------------------------------

#Foreign Exchange Demand - Dfx=IM*pw+IA+RfxbnopDot-ibg*Bgrow/en #(*Nominal, foreign currency*)#(*97)#Check Equation
Dfx=IM*pw+ifxb*Lfxb+RfxbnopDot+ibg*Bgrow/en #(*Nominal, foreign currency*)#(*97)#Changed
#Foreign Exchange supply
Sfx=EX/en+LfxbDot+Rem-RfxcbintDot+BgrowDot/en #(*Nominal, foreign currency*)(*98*)#Changed
# Income Account - IA= ifxb*Lfxb #(*100*)
IA = Rem*en - ibg*Bgrow- ifxb*Lfxb*en #(Nominal, Domestic Currency)(*100*)#Changed
#Remmitances
Rem=alpharem*GDPw*pw #(Nominal, foreign currency)(*101)

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
CA = (EX-IM*en*pw-ifxb*Lfxb*en+Rem*en-ibg*Bgrow)/GDP
#Government Debt (%GDP)
BgoverGDP = Bg/GDP
#Investment (%GDP)
IdoverYpd = Id/Ypd
#Inflation
Infl = pDot/p
#Government External Debt (%GDP)
BgrowoverGDP = Bgrow/GDP

##time derivatives
Ye=betay*(Yd/p-Ye)+(alphaa+alphap)*Ye #Expected sales variation (*Real*)(*1*)
V=Yp-Yd/p #Change in inventories (*Real*)(*2*)
Kap=Ir-delta*Kap #Gross Capital Formation (*Real*) (*20*)
w=w*(omega0+omega1*(L/Pop-omega3)+omega2*pDot/p)#Change in nominal wages (*Nominal, domestic currency*)(*77*)
Dep=Sh-BghDot #New Households Deposits (*Nominal, domestic currency*)(*87*)
Ldf=TFNf-LfxfDot*en# New Domestic Loans Borrowed by the firms (*Nominal, domestic currency*)(*29*)
Bgb=min(BgDot-BghDot-BgcbintDot-BgrowDot,BgbdesDot)#Government Bonds Purchased by the banks (*Nominal, domestic currency*)(*70*)
Ad=TFNb #Liquidity advances from the central bank to the banks(*Nominal, domestic currency*)(*52*)
Res=rrr*DepDot#(*Nominal, domestic currency*)
Bg=Bg*ibg+G-Tax-ip*Ad -ibg*Bgcb #New bonds supply issued by the Government (*Nominal, domestic currency*) (*68*)
Bgh =upsilonH*Sh #Government Bonds Purchased by the Households(*Nominal, domestic currency*)(*86*)
Bgcb=BgDot-BghDot-BgbDot-BgrowDot #Government Bonds Purchased by the Central Bank(*Nominal, domestic currency*)(*72*)
HUC=zeta*(UC-HUC)#Change in Historical Unitary Cost (*Nominal, domestic currency*)(*17*)
Con=betacon*(ConT-Con)#Change in Consumption level (*Nominal, domestic currency*) (*84)
OFb = REb #Change in the funds owned by the banks (*Nominal, domestic currency*)(*55*)
sigmaMX=betaimp*(sigmaMXtar-sigmaMX) #Change in target imports propensity (*9*)
sigmaMI=betaimp*(sigmaMItar-sigmaMI) #Change in target imports propensity (*9*)
sigmaMC=betaimp*(sigmaMCtar-sigmaMC) #Change in target imports propensity (*9*)
sigmaMg=betaimp*(sigmaMgtar-sigmaMg) #Change in target imports propensity (*9*)
sigmaX=betaexp*(sigmaXtar-sigmaX) #Change in exports elasticity (*13*)
betalf=betapar*(betalftar-betalf) #Change in the firms arbitrage condition (*30*)
GDPw=(alphap+alphaa)*GDPw #World GDP growth (*Not numbered)
Pop=Pop*alphap #Population Growth (*Not numbered)
a=a*(alphaa) #Labour Productivity Growth (*76*)
p=nu2*(pd-p)#Change in price level(*19*)
pw=pw*infls #World Inflation Rate (*Not numbered)
en =en*betae*(Dfx -Sfx)/Sfx #Change in Nominal Exchange Rate (*96*)
ene=betaene*(Upsilon*((((1+ifp)/((1+ip)*(1-rsk))))^sigmaene)*en-ene) #Change in the expected nominal exchange rate (*99*)
prem=2*(premtar-prem)#Premium variation(*47*)
idL=betaidL*(idLtar-idL)#Change in interest rate on domestic loans (*60*)
Lfxb = LfxfDot #New FX Loans borrowed by the banks(*Nominal, foreign currency*)(*40*)
Lfxf=min(CBLS-Lfxf,max(betalf*TFNf/en,-Lfxf))#New FX Loans borrowed by the firms(*Nominal, foreign currency*)(*28*)
Dfxf=eta*LfxfDot #New Firms FX Deposits(*Nominal, foreign currency*)(*33*)
ifxb=betaib*ifxb*(CBLD-CBLS)/CBLS #Change in FX interest rate(*44*)
Bgcbint=0#(*Nominal, domestic currency*)
Rfx=EX/en-IM*pw-ifxb*Lfxb+LfxbDot+Rem+betagff*GFF-ibg*Bgrow/en #Change in Domestic FX reserves (*Nominal, foreign currency*)#Changed
Rfxcb=RfxDot-RfxbnopDot #Change in Central Bank FX Reserves(*Nominal, foreign currency*)(*50*)
Rfxb=RfxbnopDot #required regulatory change in bank foreign reserves (*Nominal, foreign currency*)(*49*)
Bgrow=max(betagff*GFF*en,-Bgrow)#Government Bonds Purchased by RoW (*Nominal, domestic currency*)(*103*)
##initial values
Ye=54.131971310113215
V=5.255531195156623
Kap=100
w=0.5451854311441371
Dep=66.23931691486177
Ldf=53.44018611708843
Bgb=15.2537459874519
Ad=2.403931691486177
Res=6.6239316914861766
Bg=17.88241365015353
Bgh=2.68236204752303
Bgcb=0
HUC=0.656073223897936
Con=26.444942832713963
OFb=6.620920804857162
sigmaMX=0.2
sigmaMI=0.3
sigmaMC=0.20406910810132728
sigmaMg=0.06
sigmaX=0.008023300970873786
betalf=0.1
GDPw=1353.2992827528303
Pop=50
a=0.95
p=1
pw=1
en=1
ene=1
prem=0.04
idL=0.10229006095735384
Lfxb=5.93779845745427
Lfxf=5.93779845745427
Dfxf = 1.7177984574542697
ifxb=0.062290060957353836
Bgcbint = 0
Rfx=5.93779845745427
Rfxcb=4.22
Rfxb=1.7177984574542697
Bgrow=0
##parameters
alphaa = 0.02
alphap = 0.01
beta5 = 25
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
mpc1s = 0.85
mpc2s = 0.07
mpc3s = 0.4
alphav = 0.1
sf = 0.5
kappa0 = 0.015
kappa1 = 1.5
mu0 = 0.8
mu1 = 2
digamma = 0.12
upsilons = 0.85
iota1 = 0.007
iota2 = 0.04
rrr = 0.1
Delta = 1
premfs = 0.04
betae = 0.7
betaene = 0.75
Upsilon = 1
risks = 0
levs = 0.1
sampi2 = 0.1
rskmin = 0.015
sigmaene = 1
iota7 = 0.015
iota8 = 0.3
iota9 = 0.02
tauw = 0.2
tauPib = 0.2
tauPif = 0.2
rho1 = 0.135
rho2 = 0.4
iota5 = 0.05
sigmaMGmin = 0
sigmaMXmin = 0.1
sigmaMImin = 0.1
sigmaMCmin = 0.1
sigmaM = 0.2
taumx = 0
taumI = 0
taumC = 0
betalfmin = 0.1
beta11 = 5
betaib = 1
betaidL = 0.2
beta1 = 0.9
beta10 = 300
sigmaer = 0.6
beta12 = 0.1
alpharem=0.000273306749350
eta=0.289298882365695
Sigma=0.418267211437954
omega0=0.018564821480475
sigmaMCs=0.204069108101327
betain=0.967365028203062
betax=0.996108949416342
betac=0.859933555894369
betagg=0.999998940462032
sigmaX0=0.008023300970874
lambda0a=0.908093285989112
lambda0wl=1.144122577241310
lambda0w=1.191207257567350
sigmaX1=0.0008
Omega1BB=0.154993767073456
Omega1BA=0.018751579439052
rho3=0.000670098091130
phi1=0.000983225146863
ifpr=0.053009242610299
ifbr=0.050773586458241
sampi1=0.042455788088413
koppa=0.042455788088413
kuku=0.014899607236136
lambdalev=0.00000000000004
epsilonX=0.25
epsilonin=0.25
epsilonc=0.75
epsilong=0.2
lambda1a = 0.1
lambda1wl = 0.05
lambda1w = 0.5
sigmaBB = 1
sigmaBA = 2
phi = 0.7
phi0 = 0.005
phi3 = 2
Omega0BB = 0.7
Omega0BA = 0.02
idLss=0.102290060957353
ifxbss=0.062290060957354
infls=0.046188397196641
Yes=54.131971310113200
Vs=5.255531195156620
ws=0.545185431144137
Ads=2.403931691486170
Bgs=17.882413650153500
Bgbs=15.200051602630500
Bghs=2.682362047523020
HUCs=0.656073223897936
Ldfs=53.440186117088400
Deps=66.239316914861700
Ress=6.623931691486170
sigmaXs=0.008023300970874
Cons=26.444942832713900
OFbs=6.620920804857160
Bgcbs=0
Lfxbs=5.937798457454260
etas=0.289298882365695
alpharems=0.000273306749350
omega0s=0.018564821480475
Rfxbs=1.717798457454260
Rfxcbs=4.220000000000000
sigmas=0.418267211437954
iFs=0.069044916355633
IMi=2.85
IMx=2.171585489838710
IMg=0.439746061692340
IMc=5.396595897662520
switchpolicy=0
valuepolicy=-0.0075
##time
begin = 0
end = 50
by = 0.01
