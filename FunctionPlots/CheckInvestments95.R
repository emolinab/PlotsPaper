#Test for equations step 1

gdx<-"C:/Users/mbacca/Documents/PIK/Papers/Paper one/Runs_LDOn_15/PaRun_LDON15_GFDL-ESM4_sticky_feb18_dynamic_cc__2021-08-27_10.49.47/fulldata.gdx"


stocks<-readGDX(gdx,"p38_capital_immobile")

stocks_95<-stocks[,1995,] #from the parameter

##stocks 95 from the calculation

capital_need<-collapseNames(readGDX(gdx,"p38_capital_need")[,,"immobile"])
area_start<-readGDX(gdx,"p38_croparea_start")
yield_start<-readGDX(gdx,"f38_region_yield")
tau_start<-readGDX(gdx,"fm_tau1995")

stocks_95_calc<-setYears(capital_need[,1995,]*area_start*yield_start*tau_start,1995)

summary(stocks_95_calc-stocks_95)

##########
prod<-production(gdx,level = "cell",products = "kcr",product_aggr = FALSE)
need<-prod["SSA.161",1995,"rice_pro"]*
