# Comparison Free and dynamic

#dynamic

folder_runs<-"C:/Users/mbacca/Documents/PIK/Papers/Paper one/Runs_LDON_rev463/"

runs_cc<-c("PaIn_rev463_GFDL-ESM4_sticky_feb18_dynamic_cc__2021-08-31_19.50.08",
           "PaIn_rev463_IPSL-CM6A-LR_sticky_feb18_dynamic_cc__2021-08-31_20.00.00",
           "PaIn_rev463_MPI-ESM1-2-HR_sticky_feb18_dynamic_cc__2021-08-31_19.57.43",
           "PaIn_rev463_MRI-ESM2-0_sticky_feb18_dynamic_cc__2021-08-31_19.52.46",
           "PaIn_rev463_UKESM1-0-LL_sticky_feb18_dynamic_cc__2021-08-31_19.55.23")

runs_nocc<-c("PaIn_rev463_GFDL-ESM4_sticky_feb18_dynamic_nocc_hist__2021-08-31_19.35.59",
             "PaIn_rev463_IPSL-CM6A-LR_sticky_feb18_dynamic_nocc_hist__2021-08-31_19.47.24",
             "PaIn_rev463_MPI-ESM1-2-HR_sticky_feb18_dynamic_nocc_hist__2021-08-31_19.44.26",
             "PaIn_rev463_MRI-ESM2-0_sticky_feb18_dynamic_nocc_hist__2021-08-31_19.39.00",
             "PaIn_rev463_UKESM1-0-LL_sticky_feb18_dynamic_nocc_hist__2021-08-31_19.41.49")

gcms<-c("GFDL-ESM4",
        "IPSL-CM6A-LR",
        "MPI-ESM1-2-HR",
        "MRI-ESM2-0",
        "UKESM1-0-LL")

Inpt_Agg=TRUE
scenario<-"Inertia"

b<-.adaptationCostsPlotsAverage(folder_runs,scenario,runs_cc,runs_nocc,gcms,Inpt_Agg)

#free
runs_cc<-c("PaIn_rev463_GFDL-ESM4_sticky_feb18_free_split_cc__2021-08-31_21.02.47",
           "PaIn_rev463_IPSL-CM6A-LR_sticky_feb18_free_split_cc__2021-08-31_21.11.54",
           "PaIn_rev463_MPI-ESM1-2-HR_sticky_feb18_free_split_cc__2021-08-31_21.09.41",
           "PaIn_rev463_MRI-ESM2-0_sticky_feb18_free_split_cc__2021-08-31_21.05.08",
           "PaIn_rev463_UKESM1-0-LL_sticky_feb18_free_split_cc__2021-08-31_21.07.28")

runs_nocc<-c("PaIn_rev463_GFDL-ESM4_sticky_feb18_free_split_nocc_hist__2021-08-31_20.50.56",
             "PaIn_rev463_IPSL-CM6A-LR_sticky_feb18_free_split_nocc_hist__2021-08-31_21.00.33",
             "PaIn_rev463_MPI-ESM1-2-HR_sticky_feb18_free_split_nocc_hist__2021-08-31_20.58.24",
             "PaIn_rev463_MRI-ESM2-0_sticky_feb18_free_split_nocc_hist__2021-08-31_20.53.35",
             "PaIn_rev463_UKESM1-0-LL_sticky_feb18_free_split_nocc_hist__2021-08-31_20.56.09")


Inpt_Agg=TRUE
scenario<-"Free"

c<-.adaptationCostsPlotsAverage(folder_runs,scenario,runs_cc,runs_nocc,gcms,Inpt_Agg)

plot_data<-rbind(b[[3]],c[[3]])

####Lines adaptation

Prod_gloTot<-ggplot(subset(plot_data,Region=="GLO" & Names=="Total" & Years>=2020),aes(x=Years,y=Mean,color=as.factor(Scenario)))+geom_line(size=1)+ theme_bw()+facet_wrap(~Names,scales="free")+
  theme(title=element_text(size=20,face="bold"),axis.title= element_text(size=18),axis.text.x = element_text(size=14,angle = 45, hjust = 1),axis.text.y = element_text(size=14),legend.title = element_text(size=16,face="bold"),legend.text = element_text(size=14))+#,legend.position = "bottom"
  ylab("Costs mio. USD05")+
  scale_color_manual(values=wes_palette(n=2, name="Darjeeling1",type="continuous"))+
  ggtitle("Adaptation costs - Globe")+labs(colour="Calculation")+
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
  geom_errorbar(aes(ymin=Mean-Error, ymax=Mean+Error), width=.2,
                position=position_dodge(0.05))
