library(magpie4)
library(luplot)
library(luscale)
library(madrat)
library(ggplot2)
library(wesanderson)
library(RColorBrewer)
library(grDevices)

#Type can be either initialization or spam

.correlationInputsPreparation<-function(folder_runs,run_names,run_nicknames,gcm,path_in,type="initialization"){
  
  plots_names<-list()
  r2_names<-list()
  
  for (r in 1:length(run_names)){
    
    gdx<-paste0(folder_runs,run_names[r],"/fulldata.gdx")
    cropland<-croparea(gdx=gdx,level="cell",product_aggr = FALSE)
    initialization<-dimSums(read.magpie(paste0(path_in,"/yields_gcms/f30_croparea_w_initialisation_c200_",gcm,".mz")),dim=3.1)
    
    #initialization<-SPAM
   
    years<- if(type=="SPAM") c(2000,2005,2010) else 
      intersect(getYears(initialization,as.integer = TRUE),getYears(cropland,as.integer = TRUE))
    
      for (ye in years){
        if (type=="SPAM"){
          #SPAM's harvested area
          SPAM<-get(load(paste0(path_in,"/Correlation/SPAMMagObj_y",ye,"_HA.Rda")))
          map<-readRDS(paste0(path_in,"/Correlation/clustermap_rev4.62EMB+yield_c200_h12.rds"))
          map$global<-paste0("GLO.",1:59199)
          SPAM<-toolAggregate(SPAM,rel=map,weight = NULL,from = "global",to="cluster",dim = 1)
          SPAM<-SPAM/1e6
        }
        
        names<-if(type=="SPAM") intersect(getNames(SPAM),getNames(cropland)) else 
          intersect(getNames(initialization),getNames(cropland))
        
        for (n in names){
        x=if (type=="SPAM") magpiesort(SPAM[,ye,n]) else magpiesort(initialization[,ye,n])

        y=cropland[,ye,n]
        title=if (n!="maiz") paste0(run_nick[r],"\n",n,"-") else paste0(run_nick[r],"\nMaize - ")
        xlab=if (type=="SPAM") "SPAM (mio. ha)" else "Initialization (mio. ha)"
        ylab="MAgPIE (mio. ha)"
        xlim=c(-0.5,25)
        ylim=c(-0.5,25)
        bins=33
        folder=NULL
        
        
        plot_corr<-correlationDataSets(x=x,y=y,title=title,xlab=xlab,ylab=ylab,xlim=xlim,ylim=ylim,bins=bins, folder=NULL)
        
        plots_names[[run_nam[r]]][[n]][[as.character(ye)]]<-plot_corr[[1]]
        r2_names[[run_nam[r]]][[n]][[as.character(ye)]]<-plot_corr[[2]]
        
      }
    }
  }
  
  
  return(list(plots_names,r2_names))
}

#### Example
folder_runs<-"C:/Users/mbacca/Documents/PIK/Papers/Paper one/Runs_LDON_rev463/"

run_names<-c("PaIn_rev463_GFDL-ESM4_sticky_feb18_dynamic_cc__2021-08-31_19.50.08/",
             "PaIn_rev463_GFDL-ESM4_sticky_feb18_free_cc__2021-08-31_20.26.39/")

run_nicknames<-c("Inertia",
                 "Free")
gcm<-"GFDL-ESM4"
path_in<-getwd()
type<-"initialization"
a<-.correlationInputsPreparation(folder_runs,run_names,run_nicknames,gcm,path_in,type)
