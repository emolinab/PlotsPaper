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
    
    stranded<-invest_im-(ImmobileStocks+RequImmobile)
    
    }
  return(list(global,map))
}

gdx_c<-c("C:/Users/mbacca/Documents/PIK/GitHub_downloads/MAGPIE_Versions/split2020UpToDate/magpie/output/Paper_LDON_UKESM1-0-LL_sticky_feb18_dynamic_cc__2021-08-15_11.57.48",
         "C:/Users/mbacca/Documents/PIK/GitHub_downloads/MAGPIE_Versions/split2020UpToDate/magpie/output/Paper_LDON_UKESM1-0-LL_sticky_feb18_dynamic_nocc_hist__2021-08-15_11.48.02")

scenario<-c("UKESM-cc","UKESM-nocc")
tag<-"Paper_plots_fix"
folder<-"C:/Users/mbacca/Documents/PIK/Papers/Paper one/Images/LDON_paper/"

a<-.plotCapitalStranded(gdx_c,scenario,tag,folder)