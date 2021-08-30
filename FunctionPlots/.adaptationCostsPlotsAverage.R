library(wesanderson)
library(luplot)
library(ggplot2)
library(magpie4)


.adaptationCostsPlotsAverage<-function(folder_runs,scenario,runs_cc,runs_nocc,gcms,Inpt_Agg=TRUE){
  
  difference_accumulation<-NULL
  for (gg in 1:length(gcms)){#1:length(ggcm)){
    
    gdx_cc_1<-paste0(folder_runs,runs_cc[gg],"/fulldata.gdx")
    gdx_nocc_1<-paste0(folder_runs,runs_nocc[gg],"/fulldata.gdx")
    
    costs_adaptation_cc<-reportCostsAdaptationCrops(gdx_cc_1,type = 'investment')
    costs_adaptation_nocc<-reportCostsAdaptationCrops(gdx_nocc_1,type = 'investment') 
    
    difference<-costs_adaptation_cc-costs_adaptation_nocc
    
    regions<-getCells(difference)
    years<-getYears(difference,as.integer = TRUE)
    names<-getNames(difference)
    
    difference<-as.ggplot(difference)
    difference$Scenario<-gcms[gg]
    difference_accumulation<-rbind(difference_accumulation,difference)
    
  }
  
  
  plot_data<-NULL
  
  for(r in regions){
    for(y in (years)){
      for(n in unique(difference$Data1)){
        
        diff_sub<-subset(difference_accumulation, Region== r & Year == y & Data1 == n)
        mean<-mean(diff_sub$Value)
        error<-sd(diff_sub$Value)
        
        if (n=="Costs Adaptation (USD$05/yr)"){
          n2="Total"
        }else if (n=="Costs Adaptation|+|Variable (Crops) (USD$05/yr)"){
          n2="Variable"
        }else if (n=="Costs Adaptation|+|Capital (Crops) (USD$05/yr)"){
          n2="Capital"
        }else if (n=="Costs Adaptation|+|Input costs (Crops) (USD$05/yr)"){
          n2="Inputs"
        }else if (n=="Costs Adaptation|+|Trade (Crops) (USD$05/yr)"){
          n2="Trade"
        }else if (n=="Costs Adaptation|+|Technology (USD$05/yr)"){
          n2="Technology"
        }else if (n=="Costs Adaptation|+|AEI (USD$05/yr)"){
          n2="AEI"
        }else if (n=="Costs Adaptation|+|Land Conversion (USD$05/yr)"){
          n2="Land conversion"
        }
        
        
        plot_aux<-as.data.frame(r)
        colnames(plot_aux)<-"Region"
        plot_aux$Years<-y
        plot_aux$Names<-n2
        plot_aux$Mean<-mean
        plot_aux$Error<-error
        plot_aux$Scenario<-scenario
        
        plot_data<-rbind(plot_data,plot_aux)
        
      }
    } 
  }
  
  Inputs<-if(Inpt_Agg==TRUE) c("Total","Variable","Capital") else c("Total","Inputs")
  
  Adap_GLO<-ggplot()+
    geom_bar(data = subset(plot_data,Region=="GLO" & Years>2020 & !Names%in%Inputs), aes(x=Years,y=Mean,fill=as.factor(Names)),width = 5,colour="black",position="stack", stat="identity")+theme_bw()+facet_wrap(~Region,scales="free")+
    theme(title=element_text(size=20,face="bold"),axis.title= element_text(size=18),axis.text.x = element_text(size=16, angle = 45, hjust = 1),
          axis.text.y = element_text(size=16),legend.title = element_blank(),legend.text = element_text(size=12),legend.position="bottom")+#,legend.position = "bottom"
    ylab("mio. USD05$")+xlab("Cost")+ggtitle(scenario)+
    geom_point(data = subset(plot_data,Region=="GLO" & Years>2020 & Names=="Total"), aes(x=Years,y=Mean),size=2,color="black")+
    geom_line(data = subset(plot_data,Region=="GLO" & Years>2020 & Names=="Total"), aes(x=Years,y=Mean),size=1,color="black",linetype = "dashed")+
    scale_fill_manual(values=wes_palette(n=7, name="Darjeeling1",type="continuous"))+ylim(-30000,122000)
  #geom_errorbar(data = subset(plot_data,Region=="GLO" & Years>2020 & Names=="Total"),aes(y=Mean,x=Years,ymin=Mean-Error, ymax=Mean+Error), width=.2,
  #              position=position_dodge(0.05))
  
  Adap_REG<-ggplot()+
    geom_bar(data = subset(plot_data,Region!="GLO" & Years>2020 & !Names%in%Inputs), aes(x=Years,y=Mean,fill=as.factor(Names)),width = 5,colour="black",position="stack", stat="identity")+theme_bw()+facet_wrap(~Region,scales="free")+
    theme(title=element_text(size=20,face="bold"),axis.title= element_text(size=18),axis.text.x = element_text(size=10, angle = 45, hjust = 1),
          axis.text.y = element_text(size=10),legend.title = element_blank(),legend.text = element_text(size=12),legend.position="bottom")+#,legend.position = "bottom"
    ylab("mio. USD05$")+xlab("Cost")+ggtitle(scenario)+
    scale_fill_manual(values=wes_palette(n=6, name="Darjeeling1",type="continuous"))

  
  ####Lines adaptation
  return(list(Adap_GLO,Adap_REG,plot_data))
  
}

