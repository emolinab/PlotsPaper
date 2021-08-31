library(magpie4)
library(mrcommons)
library(ggplot2)
library(wesanderson)

.PlotYieldsAvergaeGCMS<-function(path,gcms){
  
  data_frame<-NULL
  
  for(gc in gcms){
    #Reads cropland
    path_mz<-paste0(path,"/yields_gcms/")
    cropland<-magpiesort(read.magpie(paste0(path_mz,"f30_croparea_w_initialisation_c200_",gc,".mz"))[,2010,])
    cropland<-setYears(collapseNames(cropland),NULL)
    cropland_glo<-dimSums(cropland,dim=1)
    
    #Mapping from cell to glo
    rel<-as.data.frame(getCells(cropland))
    rel$GLO<-"GLO"
    colnames(rel)<-c("cell","glo")
    
    #Reads yields
    data<-magpiesort(read.magpie(paste0(path_mz,"lpj_yields_c200_",gc,".mz")))
    
    #Creates weight
    weight<-dimOrder(cropland,perm=c(2,1),dim=3)
    
    #Aggregation from cell to glo
    data_aggregated<-toolAggregate(data[,,getNames(weight)],rel=rel,weight=weight,from="cell",to="glo",dim=1)
    weight<-data_aggregated
    weight[,,]<-cropland_glo
    
    #Mapping irrigation type to all
    rel2<-as.data.frame(getNames(data_aggregated))
    colnames(rel2)<-"water"
    rel2$all<-"all"
    
    #Aggregation from irrigation type to all
    data_aggregated_w<-toolAggregate(data_aggregated,rel=rel2,weight=weight,from="water",to="all",dim=3)
    
    #Data frame for specific gcm
    data_frame_aux<-as.data.frame(data_aggregated_w)[,c("Year","Value")]
    data_frame_aux$gcm<-gc
    
    #Data frame all gcms
    data_frame<-rbind(data_frame,data_frame_aux)
  }
  
  data_frame$Year<-as.numeric(as.character(data_frame$Year))
  data_frame$Value<-as.numeric(as.character(data_frame$Value))
  
  #Plots percentage change
  data_frame$Value_95<-data_frame[1,"Value"]
  
  lin_22<-1:length(unique(data_frame))*22-21
  
  for (i in 1:length(lin_22)){
    data_frame[lin_22[i]:(lin_22[i]+21),"Value_95"]<-data_frame[lin_22[i],"Value"]
  }
  
  
  data_frame$percent<-(data_frame$Value-data_frame$Value_95)/(data_frame$Value_95)*100
  
  
  average<-ggplot(data_frame,aes(x=Year,y=percent,color=as.factor(gcm)))+geom_line(size=1.5)+geom_point(size=1.5,color="black")+ theme_light()+
    theme(title=element_text(size=18,face="bold"),axis.title= element_text(size=16),axis.text.x = element_text(size=16),axis.text.y = element_text(size=16),legend.title = element_text(size=14,face="bold"),legend.text = element_text(size=12))+#,legend.position = "bottom"
    ylab("% Average relative difference")+labs(color = "gcm")+
    scale_color_manual(values=wes_palette(n=length(unique(data_frame$gcm)), name="Darjeeling1",type="continuous"))+
    ggtitle("(base year = 1995, RCP=8p5)")
  
  return(out=list(data_frame,average))
}
