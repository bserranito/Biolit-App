#list of packages required
list.of.packages <- c("shiny","dplyr","ggplot2","reshape2","ggpubr","Rcpp","units","Hmisc","lifecycle")

#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install missing ones
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

######  server 
library(shiny)
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(Rcpp)
library(Hmisc)
library(lifecycle)

load('Biolit_app_datasets.RData')

server <- function(input, output) {
  
  output$ParamPlot <- renderPlot({
    # Selectionne du parametre
    # k=Selection
    Var=unlist(DF[,input$variable])
    
    req(input$Sites) # Requiert une valeur de sites pour afficher la suite
    
    Sel.var=  as.data.frame(as.matrix(DF[which(DF$Abb %in% input$Sites),c('Abb',input$variable)])) %>%
      rename(Val =input$variable) %>% as.data.frame()#
    
    
    unit=get(paste0(input$variable,'_unit'),DF.unit) # Récupération de l'unité de la variable selectionnée
    
    
    # Sel.var=DF %>% filter(Abb == 'SEN') %>% select(Abb,Nit)  %>% as.data.frame()
    probs <- c(.25,.5, 0.75)
    dens <- density(Var)
    df.quant=data.frame(x=dens$x, y=dens$y)
    quantiles <- quantile(Var, prob=probs)
    df.quant$quant <- factor(findInterval(dens$x,quantiles))
    # 
    # 
    df.test=df.quant %>%  group_by(quant) %>%
      dplyr::summarize(minX = min(x),
                       maxX = max(x))
    
    ggplot()+
      geom_rect(data=df.test,aes(xmin=minX, xmax=maxX, ymin=-.5,ymax=.5 , fill=quant))+
      scale_fill_brewer(direction=1,name = "Distribution \n des valeurs", labels = c("0-25%", "25-50%", "50-75%","75-100%"))+
      geom_hline(yintercept=-0.52,size=1.5)+
      
      geom_segment(data=Sel.var,aes(x=as.numeric(as.character(Val)),
                                    xend=as.numeric(as.character(Val)),
                                    y=0, yend=-.5, col=Abb),
                   arrow = arrow(length = unit(0.5, "cm")),
                   size=1.5,
                   lineend = c('round'),
                   linejoin = c('mitre'))+ # Vecteur déterminant la valeur de la station
      guides(col=guide_legend(title="Site"))+
      # scale_fill_discrete(name = "Distribution", labels = c("0-25%", "25-50%", "50-75%"))+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks.length=unit(0.2,"inch"),
            axis.ticks.y=element_blank(),
            axis.text.x=element_text(size=12),
            axis.text.y=element_blank())+
      ylab('')+ xlab(paste0('gradient de la variable (',unit,')'))
  })
  
  
  ## Plot 2 
  output$RadarPlot <- renderPlot({


    df.radar3=df.radar2 %>% filter(Abb %in% input$Sites)


    ggplot()+
      geom_hline(yintercept=c(0.5,1), linetype=2, col='grey40')+
      geom_polygon(data=df.radar3,aes(Var,Mval, group=Abb, fill=Abb),alpha=.2, col='black')+
      # geom_line(col='black')+
      geom_point(data=df.radar3,aes(Var,Mval,fill=Abb),shape=21, size=4)+
      geom_text(data=df.radarVal,aes(Var,y=as.numeric(as.character(Perc)),label=as.character(ValPerc)), size=3.5, col='grey60')+
      coord_polar()+
      theme_minimal()+
      theme(axis.text.x = element_text(size=11, face='bold'),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())+

      # scale_x_discrete(labels=c("hESC1","hESC2","hESC3","hESC4"))+
      xlab('') + ylab('')

  })
  
  
  # ggplot()+
  #     # geom_density(data=DATA,aes(Inor), alpha=.4)+
  #     geom_line(data=df.quant,aes(x,y), size=1)+
  #     geom_ribbon(data=df.quant,aes(x=x,ymin=0, ymax=y, fill=quant))+
  #     # scale_x_continuous(breaks=quantiles) + 
  #     # scale_fill_brewer(guide="none", palette="Spectral", direction=-1)+
  #     theme_light()+
  #      geom_vline(data=Sel.var, aes(xintercept=Sel.var[,2], col=Abb), linetype=2, size=1.5)+
  #     scale_fill_brewer(direction=-1)+
  #     guides(fill=F)+
  #     # geom_vline(xintercept=(DF.tar$SST.M), linetype=2, size=1, col='red')+
  #     # scale_fill_brewer(direction=-1)+
  #     ylab('')+
  #     xlab('')
  
  # # Plot 2
  # output$bioPlot <- renderPlot({
  #   
  #   # mutate(Site=case_when(Abb %in% input$Sites_Ab ~ "Stations", TRUE ~ "Autres"))
  #   
  #   Quant=as.vector(quantile(StJacu[,input$Var_resp] , probs = c(.25,.5,.75)))
  #   
  #   Sites= StJacu  %>% filter(Abb %in% input$Sites_Ab)
  #   StJacu=StJacu %>% mutate(Site=rep('All',nrow(StJacu)))
  #   Sites=Sites %>% mutate(Site=rep('Sites select',nrow(Sites)))
  #   
  #   nDF=rbind(StJacu,Sites)
  #   
  #   ggplot(data=nDF,aes_string('Site',y=input$Var_resp))+
  #     # geom_jitter(size=1, shape=21)+
  #     geom_violin(aes(fill=Site))+
  #     geom_boxplot(alpha=.8, width=.1)+
  #     # geom_split_violin(alpha=.8)+
  #     # geom_hline(yintercept=Quant,linetype=2, size=.9)+
  #     # # geom_split_violin()
  #     # # geom_violin(aes(fill=substrat3), alpha=.2)+ 
  #     # # # geom_jitter(data=subset(StJacu, Abb %in%   c("IDB","ILC")), col='green')+
  #     # # geom_boxplot(data=subset(StJacu, Abb %in%   c("IDB","ILC")),width=.2)+
  #     # scale_y_log10()+
  #     facet_grid(substrat3~N.couv)+
  #     theme_light()+
  #     xlab('')
  # })
  
  # Plot 3
  output$Estran_plot <- renderPlot({
    
    
    
    
    req(input$sel_spe) # Requiert une valeur de sites
    DF_bio2=DF_bio %>% filter(Spe %in% input$sel_spe)
    
    p1=ggplot(data=DF_bio2,aes(recouv,val.scaled, col=Spe))+
      # geom_jitter()+
      geom_smooth(aes(group=Spe), method='loess')+
      theme_minimal()+
      ylab('Fréquence des abondances (%)')+
      xlab('Classe de recouvrement')+
      guides(col=guide_legend(title="Taxons"))
    
    p2=ggplot(data=DF_bio2,aes(substrat3,as.numeric(val.scaled), col=Spe))+
      # geom_jitter()+
      stat_summary( aes(group=Spe, col=Spe),fun.y=mean, geom="line",size=1.5)+
      # scale_y_log10()+
      stat_summary( fun.data="mean_cl_normal",size=2)+
      theme_minimal()+
      ylab('Fréquence des abondances (%)')+
      xlab('Ceinture algues brunes')+
      guides(col=guide_legend(title="Taxons"))
    
    
    ggpubr::ggarrange(p1,p2, ncol=1, common.legend=T)# mutate(Site=case_when(Abb %in% input$Sites_Ab ~ "Stations", TRUE ~ "Autres"))
  })
  
  
  # Plot 4
  output$Rank_species <- renderPlot({
    
    
    req(input$Site_compo,input$data_type)
    
    p.label=data.frame(value='Abondance moyenne(ind/m²)',
                       PA='Fréquence occurrence (%)')
    
    lab.y=get(input$data_type,p.label) # Récupération de l'unité de la variable selectionnée
    
    
    # Composition station
    DF_bio_site= DF_bio %>% filter(Abb %in% input$Site_compo) %>%
      group_by(substrat3, Spe) %>%
      dplyr::summarize(count= n(),
                       M=mean(eval(as.symbol(input$data_type)), na.rm=F),
                       SD=sd(eval(as.symbol(input$data_type)), na.rm=F))%>%
      group_by(substrat3) %>%
      mutate(rank = dense_rank(-M)) %>% mutate(count2=paste0('n=',count))
    
    #Composition totale
    DF_bio_tot= DF_bio %>%
      group_by(substrat3, Spe) %>%
      dplyr::summarize(count= n(),
                       M=mean(eval(as.symbol(input$data_type)), na.rm=F),
                       SD=sd(eval(as.symbol(input$data_type)), na.rm=F))%>%
      group_by(substrat3) %>%
      mutate(rank = dense_rank(-M)) %>% mutate(count2=paste0('n=',count))
    
    
    comp1=ggplot(DF_bio_site,aes(as.factor(rank), M))+
      geom_bar(stat='identity',aes(fill=Spe), col='black',  position = position_dodge(width = 1))+
      # geom_errorbar(aes(ymin=M-SD, ymax=M+SD), width=.2)+
      facet_grid(.~substrat3, scale="free_y")+
      # scale_y_log10()+
      guides(fill=guide_legend(title="Taxons"))+
      scale_fill_brewer(palette="Spectral")+
      ylab(lab.y)  +
      geom_text( aes(x=max(rank)-.2, y=max(M), label=count2))+
      theme_light()+
      xlab('rang des taxons')+
      ggtitle("Composition de l'estran choisi")
    
    
    comp2=ggplot(DF_bio_tot,aes(as.factor(rank), M))+
      geom_bar(stat='identity',aes(fill=Spe), col='black',  position = position_dodge(width = 1))+
      # geom_errorbar(aes(ymin=M-SD, ymax=M+SD), width=.2)+
      facet_grid(.~substrat3, scale="free_y")+
      # scale_y_log10()+
      guides(fill=guide_legend(title="Taxons"))+
      scale_fill_brewer(palette="Spectral")+
      ylab(lab.y)  +
      geom_text( aes(x=max(rank)-1, y=max(M), label=count2))+
      theme_light()+
      xlab('rang des taxons')+
      ggtitle("Composition de l'ensemble des stations")
    
    
    ggpubr::ggarrange(comp1,comp2, ncol=1, common.legend=T)#
    
  })
}
