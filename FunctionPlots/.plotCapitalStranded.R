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
    ImmobileStocks <- readGDX(gdx,"p38_capital_immobile")
    MobileStocks <- readGDX(gdx,"p38_capital_mobile")
    
    Years<-getYears(ImmobileStocks,as.integer = TRUE)
    t<-Years[1:length(Years)-1]
    t_1<-Years[2:length(Years)]
    t_step<-t_1-t
    t_step<-c(t_step)
    depreciation<-readGDX(gdx,"s38_depreciation_rate")
    factor_Im<-ImmobileStocks
    factor_Mo<-MobileStocks
    factor_Im[,1,]<-0
    factor_Mo[,1,]<-0
    
    for(i in 1:(length(Years)-1)){
      factor_Im[,i+1,]<-(1-(depreciation))^t_step[i]
      factor_Mo[,i+1,]<-(1-(depreciation))^t_step[i]
    }
    
    ImmobileStocks2<-ImmobileStocks[,2:length(Years),]/factor_Im[,2:length(Years),]
    MobileStocks2<-MobileStocks[,2:length(Years),]/factor_Mo[,2:length(Years),]
    
    Production<-production(gdx,level="cell",products="kcr",product_aggr=FALSE)
    iRequImmobile<-collapseNames((readGDX(gdx,"p38_capital_need")[,,"immobile"]))
    iRequMobile<-readGDX(gdx,"p38_capital_need")[,,"mobile"]  
    RequImmobile<-collapseNames(Production*iRequImmobile)
    RequMobile<-collapseNames(dimSums(Production*iRequMobile,dim=3))
    
    StrandedImmobile<-setYears(ImmobileStocks2,t)-setYears(RequImmobile[,t,],t)
    StrandedMobile<-setYears(MobileStocks[,t_1,],t)-setYears(RequMobile[,t,],t)
    
    StrandedImmobile[StrandedImmobile<=0]<-0 #as a check
    StrandedImmobile<-dimSums(StrandedImmobile,dim=3)
    StrandedMobile[StrandedMobile<=0]<-0
    
    StrandedStocks<-StrandedImmobile+StrandedMobile
    global[[scenario[aux]]]<-superAggregate(StrandedStocks,aggr_type = "sum",level="regglo")
    
    Area_grid<-croparea(gdx,product_aggr = TRUE,level="grid",dir=gd)

    
    Stranded_Stocks_grid<-gdxAggregate(gdx,StrandedStocks,weight = Area_grid, from= "cell",to="grid", absolute = TRUE, dir = gd)
    getNames(Stranded_Stocks_grid)<-scenario[aux]
    
    map<-plotmap2(Stranded_Stocks_grid[,2090,]*1000,file=paste0(folder,"/",tag,"_",scenario[aux],".pdf"),title="Stranded Capital stocks",
             legendname = "(1000 USD05$)",lowcol="white",midcol = "chocolate",
             highcol="brown",facet_style="paper",text_size=25)

    aux<-aux+1
  }
  return(list(global,map))
}

gdx_c<-c("C:/Users/mbacca/Documents/PIK/GitHub_downloads/MAGPIE_Versions/split2020UpToDate/magpie/output/Paper_LDON_UKESM1-0-LL_sticky_feb18_dynamic_cc__2021-08-15_11.57.48",
         "C:/Users/mbacca/Documents/PIK/GitHub_downloads/MAGPIE_Versions/split2020UpToDate/magpie/output/Paper_LDON_UKESM1-0-LL_sticky_feb18_dynamic_nocc_hist__2021-08-15_11.48.02")

scenario<-c("UKESM-cc","UKESM-nocc")
tag<-"Paper_plots_fix"
folder<-"C:/Users/mbacca/Documents/PIK/Papers/Paper one/Images/LDON_paper/"

a<-.plotCapitalStranded(gdx_c,scenario,tag,folder)
