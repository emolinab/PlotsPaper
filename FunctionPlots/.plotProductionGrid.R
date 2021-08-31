#Plot capital distribution
library(ggplot2)
library(magpie4)
library(luplot)
library(luscale)
library(tidyr)
library(mrcommons)

.plotProductionGrid<-function(gdx_cc,gdx_nocc,scenario,tag,folder,gcm,rcp){

  map<-list()
  for(gd in 1:length(gdx_c)){
    
    gd_cc<-paste0(gdx_cc[gd],"/fulldata.gdx")
    gd_nocc<-paste0(gdx_nocc[gd],"/fulldata.gdx")
    
    Production_cc<-production(gd_cc,level="cell",products="kcr",product_aggr=TRUE)
    Production_nocc<-production(gd_nocc,level="cell",products="kcr",product_aggr=TRUE)
    
    Area_grid_cc<-croparea(gd_cc,product_aggr = TRUE,level="grid",dir=gdx_cc[gd])
    Area_grid_nocc<-croparea(gd_nocc,product_aggr = TRUE,level="grid",dir=gdx_nocc[gd])
    
    Prod_grid_cc<-gdxAggregate(gdx,Area_grid_cc,weight = Area_grid_cc, from= "cell",to="grid", absolute = TRUE, dir = gdx_cc[gd])
    Prod_grid_nocc<-gdxAggregate(gdx,Area_grid_nocc,weight = Area_grid_nocc, from= "cell",to="grid", absolute = TRUE, dir = gdx_nocc[gd])
    
    Prod_diff<-Prod_grid_cc-Prod_grid_nocc
    getNames(Prod_diff)<-scenario[gd]
    
    map[[scenario[gd]]]<-plotmap2(Prod_diff[,2100,]*1000,title=paste0("Difference in crops production (",gcm,"-",rcp,")"),
                  legendname = "Thousand tDM",lowcol="red",midcol = "white",
                  highcol="darkgreen",facet_style="paper",text_size=25,legend_range = c(-50,50),midpoint = 0,
                  file=paste0(folder,tag,"_",scenario[gd],".pdf"))
    

  }
  
  return(map)
}

gdx_cc<-c("C:/Users/mbacca/Documents/PIK/Papers/Paper one/Runs_LDOn_15/PaRun_LDON15_GFDL-ESM4_sticky_feb18_free_cc__2021-08-27_11.19.14//",
         "C:/Users/mbacca/Documents/PIK/Papers/Paper one/Runs_LDOn_15/PaRun_LDON15_GFDL-ESM4_sticky_feb18_dynamic_cc__2021-08-27_10.49.47//")

gdx_nocc<-c("C:/Users/mbacca/Documents/PIK/Papers/Paper one/Runs_LDOn_15/PaRun_LDON15_GFDL-ESM4_sticky_feb18_free_nocc_hist__2021-08-27_11.09.34//",
          "C:/Users/mbacca/Documents/PIK/Papers/Paper one/Runs_LDOn_15/PaRun_LDON15_GFDL-ESM4_sticky_feb18_dynamic_nocc_hist__2021-08-27_10.39.53/")


scenario<-c("Free","Sticky")
tag<-"ProdDiff_grid"
folder<-"C:/Users/mbacca/Documents/PIK/Papers/Paper one/Images/LDON_paper/ThursdaySept02/"
gcm<-"GFDL-ESM4"
rcp<-"rcp 8p5"
a<-.plotProductionGrid(gdx_cc,gdx_nocc,scenario,tag,folder,gcm,rcp)
