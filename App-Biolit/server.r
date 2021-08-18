#list of packages required
list.of.packages <- c("shiny","dplyr","ggplot2","reshape2","ggpubr","Rcpp","units","Hmisc","lifecycle","gridExtra","grid",
                      "ggrepel","DT","maptools")

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
library(gridExtra)
library(grid)
library(ggrepel)
library(DT)
library(maptools)

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
  
  
  ## Radar plot
  output$RadarPlot <- renderPlot({


    df.radar3=df.radar2 %>% filter(Abb %in% input$Sites)


    ggplot()+
      geom_hline(yintercept=c(0.5,1), linetype=2, col='grey40', size=.5)+
      geom_polygon(data=df.radar3,aes(Var,Mval, group=Abb, fill=Abb),alpha=.2, col='black')+
      # geom_line(col='black')+
      geom_point(data=df.radar3,aes(Var,Mval,fill=Abb),shape=21, size=4)+
      geom_text(data=df.radarVal,aes(Var,y=as.numeric(as.character(Perc)),label=as.character(ValPerc)), size=4.5, col='black')+
      coord_polar()+
      theme_minimal()+
      scale_x_discrete(labels=c("Bath" = "Bath \n (m)",
                                "Pop" = "Indice \n de population",
                                'Nit' ='Nit \n (µmol.l-1) ',
                                'TSM'='TSM \n (mg.l-1)',
                                'Sal'='Sal',
                                'Chloa'='Chloa \n (mg.m3)',
                                'WKE'='WKE \n (N.m2.s1)',
                                'TAmp'='Coef Mar \n (m)',
                                'SST'='SST \n (°C)',
                                'SSTco'='SSTco \n (°C)'))+
      theme(axis.text.x = element_text(size=14, face='bold'),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())+

      # scale_x_discrete(labels=c("hESC1","hESC2","hESC3","hESC4"))+
      xlab('') + ylab('')

  }, height = 800, width = 1200)
  
  
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
  
  ## Plot 2
  output$Site_identity <- renderPlot({

    # mutate(Site=case_when(Abb %in% input$Sites_Ab ~ "Stations", TRUE ~ "Autres"))
    # france.form <- raster::getData(name="GADM", country="FRA", level=0)
    # france=fortify(france.form)
    # 
    # 
    # DF=DF%>% mutate(Vern.lab=paste0(Abb,'=',Vern))
    
    df.geo3=DF %>% filter(Abb %in% input$Sites)
    
    ## ajout échelle
    create_scale_bar <- function(lon,lat,distance_lon,distance_lat,distance_legend, dist_units = "km"){
      # First rectangle
      bottom_right <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distance_lon, dist.units = dist_units, model = "WGS84")
      
      topLeft <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance_lat, dist.units = dist_units, model = "WGS84")
      rectangle <- cbind(lon=c(lon, lon, bottom_right[1,"long"], bottom_right[1,"long"], lon),
                         lat = c(lat, topLeft[1,"lat"], topLeft[1,"lat"],lat, lat))
      rectangle <- data.frame(rectangle, stringsAsFactors = FALSE)
      
      # Second rectangle t right of the first rectangle
      bottom_right2 <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distance_lon*2, dist.units = dist_units, model = "WGS84")
      rectangle2 <- cbind(lon = c(bottom_right[1,"long"], bottom_right[1,"long"], bottom_right2[1,"long"], bottom_right2[1,"long"], bottom_right[1,"long"]),
                          lat=c(lat, topLeft[1,"lat"], topLeft[1,"lat"], lat, lat))
      rectangle2 <- data.frame(rectangle2, stringsAsFactors = FALSE)
      
      # Now let's deal with the text
      on_top <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance_legend, dist.units = dist_units, model = "WGS84")
      on_top2 <- on_top3 <- on_top
      on_top2[1,"long"] <- bottom_right[1,"long"]
      on_top3[1,"long"] <- bottom_right2[1,"long"]
      
      legend <- rbind(on_top, on_top2, on_top3)
      legend <- data.frame(cbind(legend, text = c(0, distance_lon, distance_lon*2)), stringsAsFactors = FALSE, row.names = NULL)
      return(list(rectangle = rectangle, rectangle2 = rectangle2, legend = legend))
    }
    scale_bar <- function(lon, lat, distance_lon, distance_lat, distance_legend, dist_unit = "km", rec_fill = "white", rec_colour = "black", rec2_fill = "black", rec2_colour = "black", legend_colour = "black", legend_size = 3, orientation = TRUE, arrow_length = 500, arrow_distance = 300, arrow_north_size = 6){
      the_scale_bar <- create_scale_bar(lon = lon, lat = lat, distance_lon = distance_lon, distance_lat = distance_lat, distance_legend = distance_legend, dist_unit = dist_unit)
      # First rectangle
      rectangle1 <- geom_polygon(data = the_scale_bar$rectangle, aes(x = lon, y = lat), fill = rec_fill, colour = rec_colour)
      
      # Second rectangle
      rectangle2 <- geom_polygon(data = the_scale_bar$rectangle2, aes(x = lon, y = lat), fill = rec2_fill, colour = rec2_colour)
      
      # Legend
      scale_bar_legend <- annotate("text", label = paste(the_scale_bar$legend[,"text"], dist_unit, sep=""), x = the_scale_bar$legend[,"long"], y = the_scale_bar$legend[,"lat"], size = legend_size, colour = legend_colour)
      
      res <- list(rectangle1, rectangle2, scale_bar_legend)
      
      if(orientation){# Add an arrow pointing North
        coords_arrow <- create_orientation_arrow(scale_bar = the_scale_bar, length = arrow_length, distance = arrow_distance, dist_unit = dist_unit)
        arrow <- list(geom_segment(data = coords_arrow$res, aes(x = x, y = y, xend = xend, yend = yend)), annotate("text", label = "N", x = coords_arrow$coords_n[1,"x"], y = coords_arrow$coords_n[1,"y"], size = arrow_north_size, colour = "black"))
        res <- c(res, arrow)
      }
      return(res)
    }
    
    ggplot()+
      geom_polygon(data=france, aes(long,lat,group=group),colour="black", fill='grey70', size=.5)+
      geom_point(data=DF,aes(longit,lat),fill='Black', shape=21,size=5)+
      geom_point(data=df.geo3,aes(longit,lat, fill=Abb), shape=21, size=9)+
      geom_label_repel(data=df.geo3,aes(longit,lat, label=Vern.lab), size=8)+
      coord_cartesian(xlim=c(-5,2), ylim=c(45,50.5))+
      guides(fill=F)+
      theme_void()+
      scale_bar(lon = -4.5, lat = 45, 
                distance_lon = 80, distance_lat = 10, distance_legend = 20, 
                dist_unit = "km", orientation = FALSE)
  })
  
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
  
  # Plot 3 : relation taxons- canopées
  output$Estran_plot <- renderPlot({
    
    
    
    
    req(input$sel_spe) # Requiert une valeur de sites
    DF_bio2=DF_bio %>% filter(Spe %in% input$sel_spe)
    
    p1=ggplot(data=DF_bio2,aes(recouv,val.scaled, col=Spe))+
      # geom_jitter()+
      geom_smooth(aes(group=Spe), method='loess')+
      theme_minimal()+
      ylab('Fréquence des abondances (%)')+
      xlab('Classe de recouvrement')+
      guides(col=guide_legend(title="Taxons"))+
    theme(axis.title = element_text(size = 12),
          plot.title=element_text(size=22, face="bold"))
    
    p2=ggplot(data=DF_bio2,aes(substrat3,as.numeric(val.scaled), col=Spe))+
      # geom_jitter()+
      stat_summary( aes(group=Spe, col=Spe),fun.y=mean, geom="line",size=1.5)+
      # scale_y_log10()+
      stat_summary( fun.data="mean_cl_normal",size=2)+
      theme_minimal()+
      ylab('Fréquence des abondances (%)')+
      xlab('Ceinture algues brunes')+
      guides(col=guide_legend(title="Taxons"))+
      theme(axis.title = element_text(size = 12),
            plot.title=element_text(size=22, face="bold"))
    
    
    ggpubr::ggarrange(p1,p2, ncol=1, common.legend=T)# mutate(Site=case_when(Abb %in% input$Sites_Ab ~ "Stations", TRUE ~ "Autres"))
  })
  
  
  
  # Table gastéropode
  output$table_spe = DT::renderDataTable({
    dat.spe <- data.frame(
      Nom_vernaculaire = c('gibbule commune', 'gibbule ombiliquée','littorine fabalis'),
      Noms_scientifiques=c('Steromphala pennanti','Steromphala umbilicalis','Littorina fabalis'),
      Photo = c('<img src="spennanti.jpeg" height="150"></img>',
               '<img src="sumbilicalis.jpeg" height="150"></img>',
               '<img src="L.obtusata.jpeg" height="150"></img>'))
    
    
      DT::datatable(dat.spe, escape = FALSE) # HERE
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
      scale_fill_brewer(palette="Spectral", labels=c('Bigorneau','Calliostome',
                                                     'Gibbule de Pennant',
                                                     'Gibbule ombiliquée',
                                                     'Littorine lignes noires/des rochers',
                                                     'Littorine fabalis/obtuse',
                                                     'Monodonte',
                                                     'Nasse reticulée',
                                                     'Pourpre',
                                                     'Patelle'))+
      ylab(lab.y)+
      geom_text( aes(x=max(rank)-.2, y=max(M), label=count2), size=5)+
      theme_light()+
      theme(legend.text = element_text(size=13),
            legend.title = element_text(size=15, face="bold"),
        axis.title = element_text(size = 12),
        axis.text.x=element_text(size=12),
        axis.title.y=element_text(size=15, face='bold'),
        axis.title.x =element_text(size=15, face='bold'),
        strip.text.x = element_text(size = 16, face='bold'),
            plot.title=element_text(size=22, face="bold"))+
      xlab('rang des taxons')+
      ggtitle("Composition de l'estran choisi")
    
    
    comp2=ggplot(DF_bio_tot,aes(as.factor(rank), M))+
      geom_bar(stat='identity',aes(fill=Spe), col='black',  position = position_dodge(width = 1))+
      # geom_errorbar(aes(ymin=M-SD, ymax=M+SD), width=.2)+
      facet_grid(.~substrat3, scale="free_y")+
      # scale_y_log10()+
      guides(fill=guide_legend(title="Taxons"))+
      scale_fill_brewer(palette="Spectral", labels=c('Bigorneau','Calliostome',
                                                     'Gibbule de Pennant',
                                                     'Gibbule ombiliquée',
                                                     'Littorine lignes noires/des rochers',
                                                     'Littorine fabalis/obtuse',
                                                     'Monodonte',
                                                     'Nasse reticulée',
                                                     'Pourpre',
                                                     'Patelle'))+
      ylab(lab.y)  +
      geom_text( aes(x=max(rank)-1, y=max(M), label=count2))+
      theme_light()+
      theme(legend.text = element_text(size=13),
            legend.title = element_text(size=15, face="bold"),
            axis.title = element_text(size = 12),
            axis.title.x =element_text(size=15, face='bold'),
            axis.title.y=element_text(size=15, face='bold'),
            strip.text.x = element_text(size = 16, face='bold'),
            axis.text.x=element_text(size=12),
            plot.title=element_text(size=22, face="bold"))+
      xlab('rang des taxons')+
      ggtitle("Composition de l'ensemble des stations")
    
    
    ggpubr::ggarrange(comp1,comp2, ncol=1, common.legend=T, legend='right')#
    
  },height = 800, width = 1200)
  
  ## Carte compo
  output$Site_identity_compo <- renderPlot({
    
    # mutate(Site=case_when(Abb %in% input$Sites_Ab ~ "Stations", TRUE ~ "Autres"))
    # france.form <- raster::getData(name="GADM", country="FRA", level=0)
    # france=fortify(france.form)
    # 
    # 
    # DF=DF%>% mutate(Vern.lab=paste0(Abb,'=',Vern))
    
    df.geo3=DF %>% filter(Abb %in% input$Site_compo)
    
    
    ## ajout échelle
    create_scale_bar <- function(lon,lat,distance_lon,distance_lat,distance_legend, dist_units = "km"){
      # First rectangle
      bottom_right <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distance_lon, dist.units = dist_units, model = "WGS84")
      
      topLeft <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance_lat, dist.units = dist_units, model = "WGS84")
      rectangle <- cbind(lon=c(lon, lon, bottom_right[1,"long"], bottom_right[1,"long"], lon),
                         lat = c(lat, topLeft[1,"lat"], topLeft[1,"lat"],lat, lat))
      rectangle <- data.frame(rectangle, stringsAsFactors = FALSE)
      
      # Second rectangle t right of the first rectangle
      bottom_right2 <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distance_lon*2, dist.units = dist_units, model = "WGS84")
      rectangle2 <- cbind(lon = c(bottom_right[1,"long"], bottom_right[1,"long"], bottom_right2[1,"long"], bottom_right2[1,"long"], bottom_right[1,"long"]),
                          lat=c(lat, topLeft[1,"lat"], topLeft[1,"lat"], lat, lat))
      rectangle2 <- data.frame(rectangle2, stringsAsFactors = FALSE)
      
      # Now let's deal with the text
      on_top <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance_legend, dist.units = dist_units, model = "WGS84")
      on_top2 <- on_top3 <- on_top
      on_top2[1,"long"] <- bottom_right[1,"long"]
      on_top3[1,"long"] <- bottom_right2[1,"long"]
      
      legend <- rbind(on_top, on_top2, on_top3)
      legend <- data.frame(cbind(legend, text = c(0, distance_lon, distance_lon*2)), stringsAsFactors = FALSE, row.names = NULL)
      return(list(rectangle = rectangle, rectangle2 = rectangle2, legend = legend))
    }
    scale_bar <- function(lon, lat, distance_lon, distance_lat, distance_legend, dist_unit = "km", rec_fill = "white", rec_colour = "black", rec2_fill = "black", rec2_colour = "black", legend_colour = "black", legend_size = 3, orientation = TRUE, arrow_length = 500, arrow_distance = 300, arrow_north_size = 6){
      the_scale_bar <- create_scale_bar(lon = lon, lat = lat, distance_lon = distance_lon, distance_lat = distance_lat, distance_legend = distance_legend, dist_unit = dist_unit)
      # First rectangle
      rectangle1 <- geom_polygon(data = the_scale_bar$rectangle, aes(x = lon, y = lat), fill = rec_fill, colour = rec_colour)
      
      # Second rectangle
      rectangle2 <- geom_polygon(data = the_scale_bar$rectangle2, aes(x = lon, y = lat), fill = rec2_fill, colour = rec2_colour)
      
      # Legend
      scale_bar_legend <- annotate("text", label = paste(the_scale_bar$legend[,"text"], dist_unit, sep=""), x = the_scale_bar$legend[,"long"], y = the_scale_bar$legend[,"lat"], size = legend_size, colour = legend_colour)
      
      res <- list(rectangle1, rectangle2, scale_bar_legend)
      
      if(orientation){# Add an arrow pointing North
        coords_arrow <- create_orientation_arrow(scale_bar = the_scale_bar, length = arrow_length, distance = arrow_distance, dist_unit = dist_unit)
        arrow <- list(geom_segment(data = coords_arrow$res, aes(x = x, y = y, xend = xend, yend = yend)), annotate("text", label = "N", x = coords_arrow$coords_n[1,"x"], y = coords_arrow$coords_n[1,"y"], size = arrow_north_size, colour = "black"))
        res <- c(res, arrow)
      }
      return(res)
    }
    
    ggplot()+
      geom_polygon(data=france, aes(long,lat,group=group),colour="black", fill='grey70', size=.5)+
      geom_point(data=DF,aes(longit,lat),fill='Black', shape=21,size=5)+
      geom_point(data=df.geo3,aes(longit,lat, fill=Abb), shape=21, size=9)+
      geom_label_repel(data=df.geo3,aes(longit,lat, label=Vern.lab), size=8)+
      coord_cartesian(xlim=c(-5,2), ylim=c(45,50.5))+
      guides(fill="none")+
      theme_void()+
      scale_bar(lon = -4.5, lat = 45, 
                distance_lon = 80, distance_lat = 10, distance_legend = 20, 
                dist_unit = "km", orientation = FALSE)
  })
}
