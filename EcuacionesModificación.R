##################################
# New Variables and Parameters
##################################


X_a #Autonomous exports
Divf_W = (EQf_W/EQf)*Divf    #Firms Dividends paid to the RoW
Divf_H = (EQf_H/EQf)*Divf    #Firms Dividends paid to the households
EQf       #Firms equities
EQf_W     #Firms equities owned by the RoW (Stock)
EQf_H     #Firms equities owned by the households (Stock)
ConTd     #Desired Target Consumption
Cond      #Desired Consumption
Ldh       #Households indebtness
ipsilon1 = 1-Bur  #sensitivity to desired consumption
Bur =((rep+idLh)*Ldh/(1-tauw)*w*L)    #Burden of the households
Con = Cond + LdhDot #Consumption
Depg      #Government Deposits
Gd = Go + Gi    #Government demand
Go = fi1*(Yp*p)  #Government operating expenses
GiT = fi2*(Yp*p) + (1-thetar)*(tau_r*X_a) #Target Public Investment
Gi        #Public investment
Gs = fi3*w*(fi4*pop)   #Government transfers
ji_Lfx =ji0 + ji1*NIIP + ji2*((YeDot+Ivd)/Yp)    #Cross border lending rationing parameter
ifxb=(1+theta_Lfx)*ifp #Cross border lending rate
theta_Lfxb = theta0_Lfxb*rsk #Cross border lending risk premium
idLh = (1+phiLh)*idLf    #Interest rate on Households Loans
Divb_W = (EQb_W/EQb)*Divb    #Bank Dividends paid to the RoW
Divb_H = (EQb_H/EQb)*Divb     #Bank Dividends paid to the households
EQb       #Banks equities
EQb_W     #Banks equities owned by the RoW (Stock)
EQb_H     #Banks equities owned by the households (Stock)
psi_wffd = psi0 + psi1*(rBG_e-rW_e)       #Share of portfolio flows/government bonds entering to the economy*****
Fdi= #Total FDI flows
Fdi_G =etafg*Fdi #Greenfield FDI
Fdi_NG = (1-etafg)*Fdi #Non-Greenfield FDI
EQ_H      #Total equities owned by the household
EQ_W      #Total equities owned by the RoW
Rem       #Remittances

#Time Derivatives
X_aDot= sigmaX_aut*X_a  #Autonomous exports growth
CondDot = betacon*(CondT-Cond) #Change in Desired Consumption
LdhDot = ipsilon0 + ipsilon1*(Cond)^ipsilon2   #Households loans demand
DepgDot =  thetar*(tau_r*X_a)  #Government deposits variation
GiDot = betagi*(GiT-Gi) #Change in Public Investment
LfxbdesDot = LfxfdesDot     #Desired Banks demand for cross border lending.
FdiDot = gammafdi*Fdi #Total FDI growth
EQfDot=EQf_WDot+EQf_HDot   #Change in Firms equities
EQf_WDot = Fdi_G*en + xi_f*Fdi_NG*en  #Change in Firms equities owned by the RoW
EQf_HDot=????#Change in Firms equities owned by the households***
EQbDot=EQb_WDot+EQb_HDot#Change in Banks equities
EQb_WDot=(1-xi_f)*Fdi_NG*en    #Change in Banks equities owned by the RoW
EQb_HDot=????#Change in Banks equities owned by the households***
EQ_HDot = EQf_HDot + EQb_HDot  #Change in total equities owned by the household
EQ_WDot = EQf_WDot + EQb_WDot    #Change in total equities owned by the RoW
RemDot  = fi_r*Rem      #Remittances growth 

#Parameters
sigmaX_aut =          #Autonomous exports growth rate
kappa2 =              #Investment sensitivity to utilization rate of capital
xi_f=                 #Share of non - greenfield FDI allocated into the firms sector
tau_r                 #Tax rate on autonomous exports
ipsilon0              #Autonomous households loans demand 
ipsilon2              #Elasticity of credit demand with respect to desired consumption
rep                   #Average Repayment ratio
thetar                #Share of royalties that are not invested
fi1                   #Share of GDP allocated to public consumption
fi2                   #Share of GDP invested by the government
fi3                   #Share of nominal wages in social transfers equation
fi4                   #Share of the total population that receive government transfers
betagi                #Public investment adjustment parameter
ji0                   #Autonomous cross border lending rationing
ji1                   #Sensitivity of cross border lending to NIIP 
ji2                   #Sensitivity of cross border lending to GDP growth rate.
theta0_Lfxb            #Sensitivity of cross border lending to country risk
phiLh                #Mark-up over firms interest rate
psi0                  #Autonomous share of portfolio flows/government bonds entering to the economy
psi1                  #sensitivity of portfolio flows to the bonds yield gap
gammafdi              #Total FDI growth rate
etafg                 #Ratio of greenfield FDI to total FDI
fi_r                  #Remittances growth rate

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
#Target arbitrage parameter for firms loans
betalftar=betalfmin+tanh(beta1*(arbf))#(*31*)
#arbitrage condition 
arbf=(cd-cfx)/idLftar#(*32*)
cd=1+idLftar#(*32.a*)
cfx=(1+ifxf)*(ene+eneDot)/en#(*32.b*)
#Actual Gross Profits
gproff=Yd-w*L-pw*en*IM-tau_r*X_a-idLf*Ldf-ifxf*Lfxf*en#(*Nominal, domestic currency*)(*34*) #Check deposit interests
#Actual Net Profits
proff=(1-tauPif)*(gproff)#(*Nominal, domestic currency*)(*35*)
#Actual Return rate
rf=proff/(p*Kap)#(*36*)
#Expected firms dividends
Divfe=FFe-sf*FFe-DfxfDot*ene
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
ConTd =mpc1*(Y_Lh)+mpc3*(YF_Lh)+mpc2*(Deph+Bgh) #(*Nominal, domestic currency*)(*80*) #Check Equation: Expected Firms profits
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
#Global Capital Flows*****
GFF=alphagff*GDPw*pw# (*Nominal, foreign currency*)(*90*)
#Share of portfolio flows/government bonds entering to the economy*****
psi_wffd = psi0 + psi1*(arbrow)      
#Rest of the world arbitrage condition
arbrow= (rBG_e - rW_e)/rW_e #(*Not numbered*)
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
betalf=betapar*(betalftar-betalf) #Change in the firms arbitrage condition (*30*)
Dfxf=eta*LfxfDot #New Firms FX Deposits(*Nominal, foreign currency*)(*33*)
a=a*(alphaa) #Labour Productivity Growth (*76*)
Pop=Pop*alphap #Population Growth (*Not numbered)
w=w*(omega0+omega1*(L/Pop-omega3)+omega2*pDot/p)#Change in nominal wages (*Nominal, domestic currency*)(*77*)
Cond=betacon*(ConTd-Cond)#Change in Consumption level (*Nominal, domestic currency*) (*84)
Ldh = ipsilon0 + ipsilon1*(Cond)^ipsilon2   #Households loans demand
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
EQf_W = Fdi_G*en + xi_f*Fdi_NG*en  #Change in Firms equities owned by the RoW
EQb_W=(1-xi_f)*Fdi_NG*en    #Change in Banks equities owned by the RoW
EQ_H = EQf_HDot + EQb_HDot  #Change in total equities owned by the household
EQf_H=????#Change in Firms equities owned by the households***
EQb_H=????#Change in Banks equities owned by the households***
EQf=EQf_WDot+EQf_HDot   #Change in Firms equities
EQb=EQb_WDot+EQb_HDot#Change in Banks equities 
en =en*betae*(Dfx -Sfx)/Sfx #Change in Nominal Exchange Rate (*96*)
ene=betaene*(Upsilon*((((1+ifp)/((1+ip)*(1-rsk))))^sigmaene)*en-ene) #Change in the expected nominal exchange rate (*99*)
Rem= fi_r*Rem      #Remittances growth
Rfx=EX/en-IM*pw+BgrowDot/en+Fdi+LfxbDot+IA/en  #Change in Domestic FX reserves (*Nominal, foreign currency*)
Bgrow= betagff*GFF*en #Government Bonds Purchased by RoW (*Nominal, domestic currency*)(*103*)***
GDPw=(alphap+alphaa)*GDPw #World GDP growth (*Not numbered)
pw=pw*infls #World Inflation Rate (*Not numbered)
##initial values
Ye=54.131971310113215
V=5.255531195156623
sigmaMX=0.2
sigmaMI=0.3
sigmaMC=0.20406910810132728
sigmaMg=0.06
sigmaX=0.008023300970873786
X_a=
HUC=0.656073223897936
p=1
Kap=100
Lfxfdes=
Lfxb=5.93779845745427
Lfxf=5.93779845745427
Ldf=53.44018611708843
betalf=0.1
Dfxf = 1.7177984574542697
a=0.95
Pop=50
w=0.5451854311441371
Cond=26.444942832713963
Ldh=
Bgh=2.68236204752303
Deph=66.23931691486177
Depg=
Gi=
Bg=17.88241365015353
Bgb=15.2537459874519
Lfxbdes=
Lfxb
prem=0.04
Rfxb=1.7177984574542697
Rfxcb=4.22
Ad=2.403931691486177
Res=6.6239316914861766
OFb=6.620920804857162
idLf=0.10229006095735384
Fdi=
EQf_W = 
EQb_W= 
EQ_H =  
EQf_H=
EQb_H=
EQf=
EQb=
en=1
ene=1
Rfx=5.93779845745427
Bgrow=0
GDPw=1353.2992827528303
pw=1
##parameters
alphaa = 0.00384
alphap = 0.015
beta5 = 25 #Ningún parámetro aparece en las ecuaciones así.
alphak = 0.4
delta = 0.2
alphagff = - #No está calibrado por DNP. 
omega1 = 0.12
omega2 = 0.8
omega3 = 1
nu2 = 0.2
betacon = 4
zeta = 10 #Ningún parametro aparece así. 
betay = 4
betaimp = 1
betaexp = 1
betapar = 0
mpc1s = 0.85 #Ningún parametro aparece así.
mpc2s = 0.07 #Ningún parametro aparece así.
mpc3s = 0.4  #Ningún parametro aparece así.
alphav = 0.25
sf = 0.45
kappa0 = 0.23
kappa1 = - #No calibrado por DNP.
mu0 = 0
mu1 = 0
digamma = 0.12
upsilons = 0.85 #Ningún parametro aparece así.
iota1 = 0.007  #Ningún parametro aparece así.
iota2 = 0.04   #Ningún parametro aparece así.
rrr = 0.1
Delta = 1
premfs = 0.04   #Ningún parametro aparece así.
betae = 0.8 
betaene = 0.9 
Upsilon = 1   #No calilibrado por DNP
risks = 0     #Ningún parametro aparece así.
levs = 0.1    #Ningún parametro aparece así.
sampi2 = 0.1
rskmin = 0.015 #Ningún parametro aparece así.
sigmaene = 1
iota7 = 0.0325 
iota8 = 1
iota9 = 0.02  #No se calibro por DNP
tauw = 0.025  #Supuse que es sobre los salarios
tauPib = 0.2  
tauPif = 0.253
rho1 = 0.2743
rho2 = 0.08
iota5 = 0.05
sigmaMGmin = - #No se calibro por DNP
sigmaMXmin = - #No se calibro por DNP
sigmaMImin = - #No se calibro por DNP
sigmaMCmin = - #No se calibro por DNP
sigmaM = 0.2  ##No se calibro por DNP*
betalfmin = 0
beta11 = 5
betaib = 2
betaidLff = 0.2
beta1 = 0
beta10 = 300
sigmaer = 0.75
beta12 = 0.1
alpharem=0.000273306749350    #Ningún parámetro aparece así
eta=0
Sigma=0.418267211437954       #No calibardo por DNP
omega0=0.06
sigmaMCs=0.204069108101327    #Ningún parámetro aparece así
betain=0.967365028203062      #No calibrado (desconocido)
betax=0.996108949416342       #No calibrado (desconocido)
betac=0.859933555894369       #No calibrado (desconocido)
betagg=0.999998940462032      #No calibrado (desconocido)
sigmaX0=0.008023300970874     #No calibrado (desconocido)
lambda0a=0.0902923
lambda0wl=1.14086
lambda0w=1.15768     
sigmaX1=0.0008               #No está en el c?digo       
Omega1BB=0.155118
Omega1BA=0
rho3=0.8
phi1=0.00096
ifpr=0.053009242610299      #no está en las ecuaciones del paper.
ifbr=0.050773586458241      #No está en las ecuaciones del paper.
sampi1=0.042455788088413    #No aparece en las ecuaciones.
koppa=0.0426
kuku=0.014899607236136      #No calibrada por DNP.
lambdalev=0
epsilonX=0.25
epsilonin=0.25
epsilonc=0.75
epsilong=0.2
lambda1a = 0
lambda1wl = 0
lambda1w = 0
sigmaBB = 1                 #No calibrado por DNP
sigmaBA = 0
phi = 0.00047868
phi0 = 0.005
phi3 = 2
Omega0BB = 0.7
Omega0BA = 0.02             #No calibrado por DNP
idLffss=0.102290060957353     #No aparece en el codigo
ifxbss=0.062290060957354    #No aparece en el codigo 
infls=0.046188397196641     #No calibrado por DNP
Yes=54.131971310113200      #No aparece en el codigo
Vs=5.255531195156620        #No aparece en el codigo
ws=0.545185431144137        #No aparece en el codigo
Ads=2.403931691486170       #No aparece en el codigo
Bgs=17.882413650153500      #No aparece en el codigo
Bgbs=15.200051602630500     #No aparece en e codigo
Bghs=2.682362047523020      #No aparece en e codigo
HUCs=0.656073223897936      #No aparece en e codigo
Ldfs=53.440186117088400     #No aparece en e codigo
Deps=66.239316914861700     #No aparece en e codigo
Ress=6.623931691486170      #No aparece en e codigo
sigmaXs=0.008023300970874   #No aparece en e codigo
Cons=26.444942832713900     #No aparece en e codigo
OFbs=6.620920804857160      #No aparece en e codigo
Bgcbs=0                     #No aparece en e codigo
Lfxbs=5.937798457454260     #No aparece en e codigo
etas=0.289298882365695      #No aparece en e codigo
alpharems=0.000273306749350 #No aparece en e codigo
omega0s=0.018564821480475   #No aparece en e codigo
Rfxbs=1.717798457454260     #No aparece en e codigo
Rfxcbs=4.220000000000000    #No aparece en e codigo
sigmas=0.418267211437954    #No aparece en e codigo
iFs=0.069044916355633       #No aparece en e codigo
IMi=2.85                    #No aparece en e codigo
IMx=2.171585489838710       #No aparece en e codigo
IMg=0.439746061692340       #No aparece en e codigo
IMc=5.396595897662520       #No aparece en e codigo
sigmaX_aut =   
kappa2=
xi_f=  
tau_r=
ji0=
ji1=
ji2=
theta_Lfxb=
theta0_Lfxb=
phiLh=
rep=
fi1=
fi2=
fi3=
fi4=
thetar=
taumC=
taumx=
taumI=
psi0=
psi1=
etafg=
ipsilon0=
ipsilon2=
betagi=
fi_r=
switchpolicy=0              #No calibrado por DNP
valuepolicy=-0.0075         #No aparece en e codigo 
##time
begin = 0
end = 50
by = 0.01
