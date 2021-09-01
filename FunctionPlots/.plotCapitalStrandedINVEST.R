#Plot capital distribution
library(ggplot2)
library(magpie4)
library(luplot)
library(luscale)
library(tidyr)
library(mrcommons)

.plotCapitalStrandedINVEST<-function(gdx_c,scenario,tag,folder,mobility=FALSE){
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
    
    StrandedImmobile<-invest_im+ImmobileStocks-RequImmobile
    if(any((StrandedImmobile)<(-1e-4))) stop ("ERROR, requirements larger than available stocks + investment")
    StrandedImmobile<-dimSums(StrandedImmobile,dim=3)
    
    StrandedMobile<-invest_m+MobileStocks-RequMobile
    if(any((StrandedMobile)<(-1e-4))) stop ("ERROR, requirements larger than available stocks + investment")
    
    StrandedStocks<-StrandedImmobile+StrandedMobile
    global[[scenario[aux]]]<-superAggregate(StrandedStocks,aggr_type = "sum",level="regglo")
    
    Area_grid<-croparea(gdx,product_aggr = TRUE,level="grid",dir=gd)
    
    
    Stranded_Stocks_grid<-gdxAggregate(gdx,StrandedStocks,weight = Area_grid, from= "cell",to="grid", absolute = TRUE, dir = gd)
    getNames(Stranded_Stocks_grid)<-scenario[aux]
    
    map<-plotmap2(Stranded_Stocks_grid[,2090,]*1000,file=paste0(folder,"/",tag,"_",scenario[aux],".pdf"),title="Stranded Capital stocks",
                  legendname = "(1000 USD05$)",lowcol="white",midcol = "chocolate",
                  highcol="brown",facet_style="paper",text_size=25,legend_range = c(0,700),midpoint = 700/2)
    
    aux<-aux+1
  }
  
  return(list(global,map))
}

# gdx_c<-c("C:/Users/mbacca/Documents/PIK/Papers/Paper one/Runs_LDON_rev463/PaIn_rev463_GFDL-ESM4_sticky_feb18_dynamic_cc__2021-08-31_19.50.08/",
#          "C:/Users/mbacca/Documents/PIK/Papers/Paper one/Runs_LDON_rev463/PaIn_rev463_GFDL-ESM4_sticky_feb18_dynamic_nocc_hist__2021-08-31_19.35.59/")

gdx_c<-c("C:/Users/mbacca/Documents/PIK/Papers/Paper one/Runs_LDON_rev463/PaIn_rev463_UKESM1-0-LL_sticky_feb18_dynamic_cc__2021-08-31_19.55.23/",
         "C:/Users/mbacca/Documents/PIK/Papers/Paper one/Runs_LDON_rev463/PaIn_rev463_UKESM1-0-LL_sticky_feb18_dynamic_nocc_hist__2021-08-31_19.41.49/")


scenario<-c("UKESM-cc","UKESM-nocc")
tag<-"Paper_plots_InvStrand_UKESM"
folder<-"C:/Users/mbacca/Documents/PIK/Papers/Paper one/Images/LDON_paper/ThursdaySept02/"

a<-.plotCapitalStrandedINVEST(gdx_c,scenario,tag,folder)
