#Plot capital distribution
library(ggplot2)
library(magpie4)
library(luplot)
library(luscale)
library(tidyr)
library(mrcommons)

.plotAreaGrid<-function(gdx_cc,gdx_nocc,scenario,tag,folder,gcm,rcp){
  
  map<-list()
  for(gd in 1:length(gdx_c)){
    
    gd_cc<-paste0(gdx_cc[gd],"/fulldata.gdx")
    gd_nocc<-paste0(gdx_nocc[gd],"/fulldata.gdx")
    
    Area_grid_cc<-croparea(gd_cc,product_aggr = TRUE,level="grid",dir=gdx_cc[gd])
    Area_grid_nocc<-croparea(gd_nocc,product_aggr = TRUE,level="grid",dir=gdx_nocc[gd])
  
    Area_grid<-Area_grid_cc-Area_grid_nocc
    getNames(Area_grid)<-scenario[gd]
    
    map[[scenario[gd]]]<-plotmap2(Area_grid[,2100,]*1000,title=paste0("Difference in crops production (",gcm,"-",rcp,")"),
                                  legendname = "Thousand ha",lowcol="red",midcol = "white",
                                  highcol="darkgreen",facet_style="paper",text_size=25,legend_range = c(-5,5),midpoint = 0,
                                  file=paste0(folder,tag,"_",scenario[gd],".pdf"))
    
    
  }
  
  return(map)
}

# gdx_cc<-c("C:/Users/mbacca/Documents/PIK/Papers/Paper one/Runs_LDON_rev463/PaIn_rev463_GFDL-ESM4_sticky_feb18_free_cc__2021-08-31_20.26.39/",
#           "C:/Users/mbacca/Documents/PIK/Papers/Paper one/Runs_LDON_rev463/PaIn_rev463_GFDL-ESM4_sticky_feb18_dynamic_cc__2021-08-31_19.50.08/")
# 
# gdx_nocc<-c("C:/Users/mbacca/Documents/PIK/Papers/Paper one/Runs_LDON_rev463/PaIn_rev463_GFDL-ESM4_sticky_feb18_free_nocc_hist__2021-08-31_20.14.28/",
#             "C:/Users/mbacca/Documents/PIK/Papers/Paper one/Runs_LDON_rev463/PaIn_rev463_GFDL-ESM4_sticky_feb18_dynamic_nocc_hist__2021-08-31_19.35.59/")

gdx_cc<-c("C:/Users/mbacca/Documents/PIK/Papers/Paper one/Runs_LDON_rev463/PaIn_rev463_UKESM1-0-LL_sticky_feb18_free_cc__2021-08-31_20.31.28/",
          "C:/Users/mbacca/Documents/PIK/Papers/Paper one/Runs_LDON_rev463/PaIn_rev463_UKESM1-0-LL_sticky_feb18_dynamic_cc__2021-08-31_19.55.23/")

gdx_nocc<-c("C:/Users/mbacca/Documents/PIK/Papers/Paper one/Runs_LDON_rev463/PaIn_rev463_UKESM1-0-LL_sticky_feb18_free_nocc_hist__2021-08-31_20.19.22/",
            "C:/Users/mbacca/Documents/PIK/Papers/Paper one/Runs_LDON_rev463/PaIn_rev463_UKESM1-0-LL_sticky_feb18_dynamic_nocc_hist__2021-08-31_19.41.49//")


scenario<-c("Free","Dynamic")
tag<-"AreaDiff_grid_UKESM"
folder<-"C:/Users/mbacca/Documents/PIK/Papers/Paper one/Images/LDON_paper/ThursdaySept02/"
gcm<-"UKESM1-0-LL"
rcp<-"rcp 8p5"
a<-.plotAreaGrid(gdx_cc,gdx_nocc,scenario,tag,folder,gcm,rcp)
