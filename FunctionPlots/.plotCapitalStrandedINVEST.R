#Plot capital distribution
library(ggplot2)
library(magpie4)
library(luplot)
library(luscale)
library(tidyr)
library(mrcommons)

.plotCapitalStranded<-function(gdx_c,scenario,tag,folder,mobility=FALSE){
  aux<-1
  global<-list()
  for(gd in gdx_c){
    
    gdx<-paste0(gd,"/fulldata.gdx")
    
    invest_im<-collapseNames(readGDX(gdx,"ov38_investment_immobile")[,,"level"])
    invest_m<-collapseNames(readGDX(gdx,"ov38_investment_mobile")[,,"level"])
    
    Production<-production(gdx,level="cell",products="kcr",product_aggr=FALSE)
    iRequImmobile<-collapseNames((readGDX(gdx,"p38_capital_need")[,,"immobile"]))
    iRequMobile<-readGDX(gdx,"p38_capital_need")[,,"mobile"]  
    RequImmobile<-collapseNames(Production*iRequImmobile)
    RequMobile<-collapseNames(dimSums(Production*iRequMobile,dim=3))
    
    ImmobileStocks <- readGDX(gdx,"p38_capital_immobile")
    MobileStocks <- readGDX(gdx,"p38_capital_mobile")
    
    check<-invest_im+ImmobileStocks-RequImmobile
    if(any(check)<0) stop ("ERROR, requirements larger than available stocks + investment")
    
    }
  return(list(global,map))
}

gdx_c<-c("C:/Users/mbacca/Documents/PIK/Papers/Paper one/Runs_LDOn_15/PaRun_LDON15_GFDL-ESM4_sticky_feb18_dynamic_cc__2021-08-27_10.49.47/",
         "C:/Users/mbacca/Documents/PIK/Papers/Paper one/Runs_LDOn_15/PaRun_LDON15_GFDL-ESM4_sticky_feb18_dynamic_nocc_hist__2021-08-27_10.39.53/")

scenario<-c("GFDL-cc","GFDL-nocc")
tag<-"Paper_plots_fix"
folder<-"C:/Users/mbacca/Documents/PIK/Papers/Paper one/Images/LDON_paper/"

a<-.plotCapitalStranded(gdx_c,scenario,tag,folder)