#Plot capital distribution
library(ggplot2)
library(magpie4)
library(luplot)
library(luscale)
library(tidyr)
library(mrcommons)

.plotCapitalRequirements<-function(gdx_c,scenario,tag,folder,year){
  aux<-1
  global<-list()
  for(gd in gdx_c){
    
    gdx<-paste0(gd,"/fulldata.gdx")
    
    
    iRequImmobile<-collapseNames((readGDX(gdx,"p38_capital_need")[,,"immobile"]))
    iRequMobile<-readGDX(gdx,"p38_capital_need")[,,"mobile"]
    RequImmobile<-dimSums(collapseNames(Production*iRequImmobile),dim=3)
    RequMobile<-dimSums(collapseNames(dimSums(Production*iRequMobile,dim=3)))
    
    #### FACTOR NOT SURE, ASK SUPERVISORS. WITHOUT THIS REQUIREMENTS MAX EQUAL TO 3700 mio. USD
    interest<-readGDX(gdx,"pm_interest")
    depreciation<-readGDX(gdx,"s38_depreciation_rate")
    factor<-(interest+depreciation)[,getYears(RequImmobile),]
    #####
    
    
    RequTotal<-(RequImmobile+RequMobile)*factor
    
    Area_grid<-croparea(gdx,product_aggr = TRUE,level="grid",dir=gd)
    
    Requirements_grid<-gdxAggregate(gdx,RequTotal,weight = Area_grid, from= "cell",to="grid", absolute = TRUE, dir = gd)
    getNames(Requirements_grid)<-scenario[aux]
    
    map<-plotmap2(Requirements_grid[,year,],file=paste0(folder,"/",tag,"_",scenario[aux],".pdf"),title="Capital requirements",
                  legendname = "(mio. USD05$)",lowcol="white",midcol = "chocolate",
                  highcol="brown",facet_style="paper",text_size=25)
    
    aux<-aux+1
  }
  return(list(global,map))
}

gdx_c<-c("C:/Users/mbacca/Documents/PIK/GitHub_downloads/MAGPIE_Versions/split2020UpToDate/magpie/output/Paper_LDON_UKESM1-0-LL_sticky_feb18_dynamic_cc__2021-08-15_11.57.48",
         "C:/Users/mbacca/Documents/PIK/GitHub_downloads/MAGPIE_Versions/split2020UpToDate/magpie/output/Paper_LDON_UKESM1-0-LL_sticky_feb18_dynamic_nocc_hist__2021-08-15_11.48.02")

scenario<-c("UKESM-cc","UKESM-nocc")
tag<-"Paper_requirements"
folder<-"C:/Users/mbacca/Documents/PIK/Papers/Paper one/Images/LDON_paper/"
year<-2090

a<-.plotCapitalRequirements(gdx_c,scenario,tag,folder,year)
