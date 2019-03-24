library(shiny)
library(shinyBS)
library(shinyjs)
library(colourpicker)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(metricsgraphics)
library(RColorBrewer)
library(scales)
library(ggrepel)
library(data.table)
library(FactoMineR)
library(factoextra)
library(plotly)
library(ggcorrplot)
library(dendextend)
library(DT)
library(knitr)
library(markdown)
library(bit64)

#load helper functions
source(file.path("helper_functions/ggcorrplot_edited.R"))
source(file.path("helper_functions/multiplot.R"))
source(file.path("helper_functions/ggplot_extract_legend.R"))
#source(file.path("helper_functions/hcpc_plot_edited.R"))
source(file.path("helper_functions/session_info_extract_pretty.R"))




options(shiny.usecairo=TRUE,shiny.maxRequestSize=200*1024^2)

shinyServer(function(input, output, session) {
  
 
  example<-fread(file.path("data/#protein quantification_mean_peptide_intensities_Rho_cytoplasmic_qval_0-001.txt"),sep = "\t",header=T,data.table = F,na.strings = "NA")
  example[,-1]<-apply(example[,-1],2,function(x) log2(as.numeric(as.character(x))))
  
  example.meta<-fread(file.path("data/#protein quantification_mean_peptide_intensities_Rho_cytoplasmic_qval_0-001_META-data.txt"),sep = "\t",header=T,data.table = F)
  
   
    data <-  eventReactive(input$inputButton,{
      if(is.null(input$file1)){
            dataframe <- example          
        } else {
        dataframe <- fread(
          input$file1$datapath, 
            sep=input$sep,
            header = T,data.table = F,na.strings = "NA",check.names = F)
              
        }
        return(dataframe)
    })
    
    data.meta <-  eventReactive(input$inputButton,{
      if(is.null(input$file2)){
        dataframe <- example.meta          
      } else {
        dataframe <- fread(
          input$file2$datapath, 
          sep=input$sep,
          header = T,data.table = F,na.strings = "NA",check.names = F)
          } 
        
      return(dataframe)
    }) 
    
    
      
    meta.names<-reactive({
      meta.in<-data.frame(data.meta())
      meta.select<-input$meta.sele
      ele<-unique(as.character(meta.in[,meta.select]))
      if(length(ele)<15){
        ele<-c(ele,rep("not in use",15-length(ele)))
      }
      return(ele)
    })
    
    meta.names.ele<-reactive({
      meta.in<-data.frame(data.meta())
      meta.select<-input$meta.sele
      ele<-unique(as.character(meta.in[,meta.select]))
      return(length(ele))
    })
    
    output$colorselector <- renderUI({
      if(meta.names.ele()>15){
        helpText("more than 15 elements in selected meta data. A color gradient will be used.")
      }else{
        fluidRow(column(6,        
                        colourpicker::colourInput(inputId = "color1",label=meta.names()[1],value="#0057e7",showColour="background"),
                        colourpicker::colourInput(inputId = "color2",label=meta.names()[2],value="#d62d20",showColour="background"),
                        colourpicker::colourInput(inputId = "color3",label=meta.names()[3],value="#008744",showColour="background"),
                        colourpicker::colourInput(inputId = "color4",label=meta.names()[4],value="#ffa700",showColour="background"),
                        colourpicker::colourInput(inputId = "color5",label=meta.names()[5],value="#03A9F4",showColour="background"),
                        colourpicker::colourInput(inputId = "color6",label=meta.names()[6],value="#E91E63",showColour="background"),
                        colourpicker::colourInput(inputId = "color7",label=meta.names()[7],value="#009688",showColour="background")
        ),
        column(6,
               colourpicker::colourInput(inputId = "color8",label=meta.names()[8],value="#4CAF50",showColour="background"),
               colourpicker::colourInput(inputId = "color9",label=meta.names()[9],value="#B71C1C",showColour="background"),
               colourpicker::colourInput(inputId = "color10",label=meta.names()[10],value="#FF5722",showColour="background"),
               colourpicker::colourInput(inputId = "color11",label=meta.names()[11],value="#3F51B5",showColour="background"),
               colourpicker::colourInput(inputId = "color12",label=meta.names()[12],value="#795548",showColour="background"),
               colourpicker::colourInput(inputId = "color13",label=meta.names()[13],value="#607D8B",showColour="background"),
               colourpicker::colourInput(inputId = "color14",label=meta.names()[14],value="#673AB7",showColour="background"),
               colourpicker::colourInput(inputId = "color15",label=meta.names()[15],value="#FFC107",showColour="background")
        )
        ) 
      }
      
      
    })
    
    
    #manual colors
    #general.colors<-function()c("#0057e7","#d62d20","#008744","#ffa700","#03A9F4","#E91E63","#009688","#4CAF50","#B71C1C","#FF5722","#3F51B5","#795548","#607D8B","#673AB7","#FFC107")
    general.colors<-reactive({c(input$color1,input$color2,input$color3,input$color4,input$color5,input$color6,input$color7,input$color8,input$color9,input$color10,input$color11,input$color12,input$color13,input$color14,input$color15)})
    #gradient colors
    general.colors.gradient<-colorRampPalette(c("black","#0057e7","#d62d20","#008744","#ffa700","grey"))
    
  #  barplot(rep(1,length(general.colors.gradient)),col=general.colors.gradient)
  # barplot(rep(1,length(general.colors)),col=general.colors,names.arg = seq(1:length(general.colors)))
    
    #data<-function()fread("data file DCM_STC_resids_test2.txt",sep = "\t",header = T)
    #data.meta<-function()fread("metadata STC and DCM_test2.txt",sep = "\t",header = T)
    ## DO PCA reactive element
  ###########################
  pca <- reactive({
    
    withProgress(message = 'Process:', value = 0, {
    # Increment the progress bar, and update the detail text.
    incProgress(1/8, detail = paste("prepare data..."))
    
        table.in <- data.frame(data())
        
        raw.dim<-dim(table.in)
        
        meta.in <- data.frame(data.meta())
        
        #table.in<-example
        #meta.in<-example.meta
        
        colnames(meta.in)[1]<-"sample" # overwrite first column header to be more universal
        meta.in[,1]<-as.character(meta.in[,1]) #as factor important when numbers are used
        
        row.nam<-table.in[,1]
        table.in<-table.in[,-1]
        table.in<-as.data.frame(sapply(table.in, as.numeric))# transform all to numeric variables
        rownames(table.in)<-row.nam
        
        
        #input<-list(meta.sele="sex")
        
                #meta selection + colorimng
                meta.select<-input$meta.sele
                meta.select.number.of.elements<-length(unique(as.factor(as.character(meta.in[,meta.select]))))
                if(meta.select.number.of.elements>15){
                  used.col<-general.colors.gradient(meta.select.number.of.elements)
                }else{
                  used.col<-general.colors()[1:meta.select.number.of.elements]
                  }
        ## perform minimum imputation if missing values
        if(input$missval) {table.in[is.na(table.in)]<-min(table.in[table.in!=0],na.rm=T)/2}else{table.in<-na.omit(table.in);row.nam <- rownames(table.in)} #only 100% valid values or missing value imputation lowest int
              
                
                #table.in[is.na(table.in)]<-min(table.in,na.rm=T)
        
        #log2 transform
        if(input$logtrans) {table.in<-apply(table.in,2,log2)} #log transformation
        rownames(table.in)<-row.nam

          
        
        #scaling
        table.transform <-if(input$scaling) scale(table.in,center = T,scale = T) else scale(table.in,scale = F,center = T)
        
      
        p.mat <- cor_pmat(table.in)
        
        
        # Increment the progress bar, and update the detail text.
        incProgress(2/8, detail = paste("correlation plot ..."))
        #correlation plot
        corr <- round(cor(table.in,method = "spearman"), 2)
        
        # using hierarchical clustering
        correlation.plot<-ggcorrplot(corr, colors = c(input$col1, input$colneutral, input$col2),
                        legend.title = "correlation",leg.lim=input$leg.lim,lab_size = input$theme.cex/input$corr.size,
                   sig.level = input$sig.level,hc.order = input$corr.reorder,hc.method = input$linkage,method = "square",type = "full",lab = TRUE, 
                   p.mat = p.mat, insig = "blank")+
          theme_classic(base_size = input$theme.cex)+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5,hjust = 1))+
          labs(title="correlation plot",subtitle=paste("spearman corr. & reord. (linkage:",input$linkage,")"),y="",x="")
          
        
        # Increment the progress bar, and update the detail text.
        incProgress(3/8, detail = paste("dendrogram plot ..."))
        ## dendrogramm:
        dend <- as.dendrogram(hclust(dist(t(table.in),method = input$metrics), input$linkage))
        den.out.plot<-dend%>% 
          set("hang_leaves")%>%
          set("labels_colors",k=input$num.cluster)%>%
          set("labels_cex",c(1))%>%
          set("branches_lwd",c(4))%>%
          set("branches_k_color",k=input$num.cluster) %>% as.dendrogram
       
       
        # Increment the progress bar, and update the detail text.
        incProgress(4/8, detail = paste("box plot ..."))
        #boxplot over data
        box.data<-try(data.table::melt(table.transform),silent = T)
        colnames(box.data)[2]<-"sample" ## unify names
        box.data[,2]<-as.factor(box.data[,2])
        box.data<-try(dplyr::right_join(box.data,meta.in,by="sample"),silent = T)
        box.data$sample<-as.factor(box.data$sample)
        box.data$sample<-factor(box.data$sample,levels = unique(box.data$sample),ordered = TRUE) #reorder data to sample
        boxplot.of.data <- ggplot(box.data,aes(x = sample,y = value,fill = factor(get(meta.select))))+
          geom_boxplot()+
          ylim(c(min(box.data$value,na.rm=T),max(box.data$value,na.rm=T)))+ #min max scaling of plot
          labs(title="Boxplot",subtitle="data without missing values",y="values",x="samples",fill="group")+
          theme_classic(base_size = input$theme.cex)+
          theme(axis.text.x = element_text(angle = 90, hjust = 1))+
          scale_fill_manual(values = c(used.col))
        
        
        
        # Increment the progress bar, and update the detail text.
        incProgress(5/8, detail = paste("PCA ..."))
        #PCA generation
        # numeric variables in columns // individuals rows
        # protein/genes in columns // samples/experiemnts in rows
        #transforming?
        #pca.facto <-if(input$transpose) PCA(t(table.in), graph = FALSE,ncp = 10,scale.unit = input$scaling) else PCA(table.in, graph = FALSE,ncp = 10,scale.unit = input$scaling)
        pca.dim<-dim(table.in)
        pca.facto <-PCA(t(table.in), graph = FALSE,ncp = 10,scale.unit = input$scaling)
        pca.interpret<-dimdesc(pca.facto,axes = 1:3,proba = 0.05)
        
        
        #pca.facto.scree.var,pca.facto.plot
        incProgress(7/8, detail = paste("generating plots ..."))
        pca.facto.scree.var<-#screeplot # eigenvalue
          fviz_eig(pca.facto, choice = c("variance"),addlabels=TRUE, hjust = -0.3,ncp = 10) +
          labs(title="Scree plot", subtitle="explained variances")+
          ylim(c(0,100))+
          theme_bw(base_size = input$theme.cex)
        
        
       
        #vizualize controbutions
        facto.contr.id.1<-fviz_contrib(pca.facto, choice ="ind", axes = 1,top = input$num.ele)+ labs(title="Ind. contribution (1st Dim.)", subtitle="",x="")+theme_bw(base_size = input$theme.cex)+theme(axis.text.x = element_text(angle = 90, hjust = 1))
        facto.contr.id.2<-fviz_contrib(pca.facto, choice ="ind", axes = 2,top = input$num.ele)+ labs(title="Ind. contribution (2nd Dim.)", subtitle="",x="")+theme_bw(base_size = input$theme.cex)+theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        facto.contr.sample.1<-fviz_contrib(pca.facto, choice="var", axes = 1,top = input$num.ele)+labs(title="Var. contribution (1st Dim.)", subtitle="",x="")+theme_bw(base_size = input$theme.cex)+theme(axis.text.x = element_text(angle = 90, hjust = 1))
        facto.contr.sample.2<-fviz_contrib(pca.facto, choice="var", axes = 2, top = input$num.ele)+labs(title="Var. contribution (2nd Dim.)", subtitle="",x="")+theme_bw(base_size = input$theme.cex)+theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        
        
        #contribution plot
        facto.contr.ID.biplot<-fviz_pca_var(pca.facto, select.var = list(contrib = input$num.ele),col.var = "contrib",col.circle = "black")+
          labs(title=paste("TOP", input$num.ele), subtitle="contributions to PCA")+
          theme_bw(base_size = input$theme.cex)

        
        
        
        #generate 3D plotly version of PCA plot FactomineR :: Eigenvalues = coordinates
        plotly3d.facto<-as.data.frame(pca.facto$ind$coord)
        plotly3d.facto.plot <- plot_ly(plotly3d.facto, x = ~Dim.1, y = ~Dim.2, z = ~Dim.3, color = ~as.factor(as.character(meta.in[,meta.select])), colors = c(used.col)) %>%
          add_markers() %>%
          add_text(text = meta.in$sample, textposition = "top right",visible = "legendonly") %>%
          layout(title = "PCA (individuals coordinates)",
                 scene = list(xaxis = list(title = paste("1st PC (", round(pca.facto$eig[1,2]), "%)", sep="")),
                              yaxis = list(title = paste("2nd PC (", round(pca.facto$eig[2,2]), "%)", sep="")),
                              zaxis = list(title = paste("3rd PC (", round(pca.facto$eig[3,2]), "%)", sep=""))))
        
      
        
        
        
        #plot PCA eigenvalues // 1vs2
        
        pca.facto.plot<-fviz_pca_ind(pca.facto,axes = c(1, 2),mean.point=input$show.mean.points,pointsize=input$plot.point.size,pointshape=19, label="none",ellipse.type = input$ellipses.method,ellipse.alpha=input$ellipses.alpha,addEllipses = input$ellipses,ellipse.level=input$ellipses.level,habillage = as.factor(as.character(meta.in[,meta.select])))+ 
          scale_color_manual(values = c(used.col))+
          theme_classic(base_size = input$theme.cex)+
          labs(title="PCA: PC 1vs2",subtitle="individuals coordinates",color="group")+
          guides(color=F,fill=F,shape = F)
        
        if(input$shownam) pca.facto.plot<-pca.facto.plot +geom_text_repel(aes(label=rownames(pca.facto$ind$coord)), label = rownames(pca.facto$ind$coord))
        
       
        #plot PCA eigenvalues // 1vs3
        pca.facto.plot1<-fviz_pca_ind(pca.facto,axes = c(1, 3),mean.point=input$show.mean.points,pointsize=input$plot.point.size,pointshape=19, label="none",ellipse.type = input$ellipses.method,ellipse.alpha=input$ellipses.alpha,addEllipses = input$ellipses,ellipse.level=input$ellipses.level,habillage = as.factor(as.character(meta.in[,meta.select])))+ 
          scale_color_manual(values = c(used.col))+
          theme_classic(base_size = input$theme.cex)+
          labs(title="PCA: PC 1vs3",subtitle="individuals coordinates",color="group")+
          guides(color=F,fill=F,shape = F)
        
        if(input$shownam) pca.facto.plot1<-pca.facto.plot1 +geom_text_repel(aes(label=rownames(pca.facto$ind$coord)), label = rownames(pca.facto$ind$coord))
        
        #plot PCA eigenvalues // 2vs3
        
        pca.facto.plot2<-fviz_pca_ind(pca.facto,axes = c(2, 3),mean.point=input$show.mean.points,pointsize=input$plot.point.size,pointshape=19, label="none",ellipse.type = input$ellipses.method,ellipse.alpha=input$ellipses.alpha,addEllipses = input$ellipses,ellipse.level=input$ellipses.level,habillage = as.factor(as.character(meta.in[,meta.select])))+ 
          scale_color_manual(values = c(used.col))+
          theme_classic(base_size = input$theme.cex)+
          labs(title="PCA: PC 2vs3",subtitle="individuals coordinates",color="group")+
          guides(color=F,fill=F,shape = F)
        
        pca.facto.plot2.leg<-fviz_pca_ind(pca.facto,axes = c(2, 3),pointsize=input$plot.point.size,pointshape=19, label="none",ellipse.type = input$ellipses.method,ellipse.alpha=input$ellipses.alpha,addEllipses = input$ellipses,ellipse.level=input$ellipses.level,habillage = as.factor(as.character(meta.in[,meta.select])))+ 
          scale_color_manual(values = c(used.col))+
          theme_classic(base_size = input$theme.cex)+
          labs(title="PCA: PC 1vs2",subtitle="individuals coordinates",color="group")
        
        if(input$shownam) pca.facto.plot2<-pca.facto.plot2 +geom_text_repel(aes(label=rownames(pca.facto$ind$coord)), label = rownames(pca.facto$ind$coord))
       
        #extract legend
        legend <- g_legend(pca.facto.plot2.leg) 
        
        contri<-get_pca_ind(pca.facto)$contrib[,1:5]
        #contri<-contri[order(rowMeans(contri),decreasing=T),]
        corrplot.contrib.ind<-ggcorrplot(t(contri),method="circle", colors = rev(c("orangered", "steelblue", "grey")),
                   legend.title = "contribution",leg.lim=c(min(contri),max(contri)))+
                    theme_classic(base_size = input$theme.cex)+
                    theme(axis.text.x = element_text(angle = 90, vjust = 0.5,hjust = 1))+
                    labs(title="contribution plot of individuals",y="",x="")
          
        
        # Increment the progress bar, and update the detail text.
        incProgress(8/8, detail = paste("output generation ..."))
        # return all object as a list
        list(
             plotly3d.facto.plot = plotly3d.facto.plot,
             pca.facto.plot = pca.facto.plot,
             pca.facto = pca.facto,
             pca.interpret = pca.interpret,
             pca.facto.plot1 = pca.facto.plot1,
             pca.facto.plot2 = pca.facto.plot2,
             legend = legend,
             pca.facto.scree.var = pca.facto.scree.var,
             boxplot.of.data = boxplot.of.data,
             correlation.plot = correlation.plot,
             facto.contr.id.1 = facto.contr.id.1,
             facto.contr.id.2 = facto.contr.id.2,
             facto.contr.sample.1 = facto.contr.sample.1,
             facto.contr.sample.2 = facto.contr.sample.2,
             facto.contr.ID.biplot = facto.contr.ID.biplot,
             den.out.plot = den.out.plot,
             corrplot.contrib.ind = corrplot.contrib.ind,
             meta.seletion.elements.min = min(table(meta.in[,meta.select]),na.rm=T),
             raw.dim = raw.dim,
             pca.dim = pca.dim
             )
       
    })
        })
      
   
    
      
    
    
    
    #meta reactive
    meta<-reactive({meta.in <- data.frame(data.meta())
    return(colnames(meta.in))})
    #selection ui input for coloring
              output$meta.sele <- renderUI({
                selectInput(inputId = "meta.sele", label = "Choose category for coloring:", choices = as.list(meta()[-1]),selected = "media.phase") 
              })
    #HCPC analysis
            hcpc<-reactive({
              withProgress(message = 'Process HCPC:', value = 0, {
                incProgress(1/1, detail = paste("HCPC calculation..."))
                # Compute HCPC with automatic cutting of clusters
                pca.facto.hcpc <- HCPC(pca()$pca.facto, graph = FALSE,nb.clust=-1,metric = input$metrics,method = input$linkage)
                return(pca.facto.hcpc)
              })
            })
    
    #Plot output
        output$facto.plot.3d<-renderPlotly({pca()$plotly3d.facto.plot})
        output$facto.contr.biplot<-renderPlot({pca()$facto.contr.ID.biplot})
        output$facto.plot.2d<-renderPlot({grid.arrange(pca()$pca.facto.plot,pca()$pca.facto.plot1,pca()$pca.facto.plot2 ,pca()$legend,ncol=2)})
        output$facto.plot.2d.legend<-renderPlot({grid.draw(pca()$legend)})
        output$facto.scree<-renderPlot({pca()$pca.facto.scree.var})
        output$boxplot.data<-renderPlot({pca()$boxplot.of.data})
        output$den.out<-renderPlot({par(mar=c(15,5,5,5))
                                    plot(pca()$den.out.plot,main = paste("clustering (hclust - ",input$metrics,"/",input$linkage,"/ N = ",input$num.cluster,")"),cex.main=2)})
        output$corr.plot<-renderPlot({pca()$correlation.plot})
        output$corrplot.var<-renderPlot({pca()$corrplot.contrib.ind})
        output$facto.contr.id<-renderPlot({multiplot(pca()$facto.contr.id.1,pca()$facto.contr.id.2,cols=2)})
        output$facto.contr.sample<-renderPlot({multiplot(pca()$facto.contr.sample.1,pca()$facto.contr.sample.2,cols=2)})

    
    #value boxes
        output$raw.data.count<-renderValueBox({
          valueBox(
            pca()$raw.dim[1],color="light-blue",
            "raw N of variables",
            icon=icon("table")
          )
        })
        output$pca.data.count<-renderValueBox({
          valueBox(
            pca()$pca.dim[1],color = "black",
            "N of variables used for PCA",
            icon=icon("area-chart")
          )
        })
        output$removed.variables<-renderValueBox({
          valueBox(
            pca()$raw.dim[1]-pca()$pca.dim[1],color = "orange",
            "removed variables due to missing values",
            icon=icon("trash-o")
          )
        })
        output$individual.count<-renderValueBox({
          valueBox(
            pca()$raw.dim[2],color = "light-blue",
            "N of individuals",
            icon=icon("table")
          )
        })
        
    
    ###### render tables #######
    output$table.pca.interpret.dim1 <- renderDataTable({
                                      dat <-  data.frame(pca()$pca.interpret$Dim.1)
                                      colnames(dat)<-c("correlation","p-value")
                                      dat[,1]<-round(dat[,1],digits = 3)
                                      dat[,2]<-format.pval(dat[,2])
                                      datatable(dat)
                                    })
    output$table.pca.interpret.dim2 <- renderDataTable({
                                      dat <-  data.frame(pca()$pca.interpret$Dim.2)
                                      colnames(dat)<-c("correlation","p-value")
                                      dat[,1]<-round(dat[,1],digits = 3)
                                      dat[,2]<-format.pval(dat[,2])
                                      datatable(dat)
                                    })
    output$table.pca.interpret.dim3 <- renderDataTable({
                                    dat <-  data.frame(pca()$pca.interpret$Dim.3)
                                    colnames(dat)<-c("correlation","p-value")
                                    dat[,1]<-round(dat[,1],digits = 3)
                                    dat[,2]<-format.pval(dat[,2])
                                    datatable(dat)
                                  })
   

    
    
    ###### HCPC #######
    # HCPC + tree + 2d + tree + barplot
    output$facto.plot.hcpc.3d<-renderPlot({
                       plot(hcpc(), choice = "3D.map",title="factor map of individuals colored by cluster + tree",clust.col=general.colors)
                                          })
    output$facto.plot.hcpc.2d<-renderPlot({
                       plot(hcpc(), choice = "map",title="factor map of individuals colored by cluster",clust.col=general.colors)
                                          })
    output$facto.plot.hcpc.tree<-renderPlot({
                       plot(hcpc(), choice = "tree",title="tree plot",clust.col=general.colors)
                                          })
    output$facto.plot.hcpc.bar<-renderPlot({
                      plot(hcpc(), choice = "bar",title="bar plot of of inertia gains",clust.col=general.colors)
                                            })
    
    
    
    
    ###### generate outputs for download #######
    output$pca.interpret.dim1<-downloadHandler(
      filename = function() { 
        paste(gsub(".txt","", input$file1),"_variables_most_characteristic_for_1st-Dim.txt",sep='')  
      },
      content = function(file) {
        dat <-  data.frame(pca()$pca.interpret$Dim.1);
        dat <- cbind(rownames(dat),dat);
        colnames(dat)[1]<-"variables";
        write.table(dat, 
                    file, 
                    row.names=FALSE,
                    quote=FALSE,sep="\t")
      }
    )
    
    output$pca.interpret.dim2<-downloadHandler(
      filename = function() { 
        paste(gsub(".txt","", input$file1),"_variables_most_characteristic_for_2nd-Dim.txt",sep='')  
      },
      content = function(file) {
        dat <-  data.frame(pca()$pca.interpret$Dim.2);
        dat <- cbind(rownames(dat),dat);
        colnames(dat)[1]<-"variables";
        write.table(dat, 
                    file, 
                    row.names=FALSE,
                    quote=FALSE,sep="\t")
      }
    )
    
    
    output$pca.interpret.dim3<-downloadHandler(
      filename = function() { 
        paste(gsub(".txt","", input$file1),"_variables_most_characteristic_for_3rd-Dim.txt",sep='')  
      },
      content = function(file) {
        dat <-  data.frame(pca()$pca.interpret$Dim.3);
        dat <- cbind(rownames(dat),dat);
        colnames(dat)[1]<-"variables";
        write.table(dat, 
                    file, 
                    row.names=FALSE,
                    quote=FALSE,sep="\t")
      }
    )
    
    
    
    
    output$file1.data.frame<-downloadHandler(
      filename = function() { 
        paste("example_data_frame.txt", sep='') 
      },
      content = function(file) {
        dat <-  data.frame(example);
        write.table(dat, 
                  file, 
                  row.names=FALSE,
                  quote=FALSE,sep="\t")
      }
    )
    
    output$file2.data.frame<-downloadHandler(
      filename = function() { 
        paste("example_meta_data.txt", sep='') 
      },
      content = function(file) {
        dat <-  data.frame(example.meta);
        write.table(dat, 
                    file, 
                    row.names=FALSE,
                    quote=FALSE,sep="\t")
      }
    )
    
    
    output$plots2D_PCA <- downloadHandler(
      filename = function() {
        paste(gsub(".txt","", input$file1),"_2D_plots_PCA_analysis.pdf",sep='')  },
      content = function(file) {
        pdf(file,width = 20,height = 20)
        grid.arrange(pca()$pca.facto.plot,pca()$pca.facto.plot1,pca()$pca.facto.plot2 ,pca()$legend,ncol=2)
        dev.off()
      }
    )
    
    output$plots2D_PCA_legend <- downloadHandler(
      filename = function() {
        paste(gsub(".txt","", input$file1),"_2D_plots_legend_PCA_analysis.pdf",sep='')  },
      content = function(file) {
        pdf(file)
        grid.draw(pca()$legend)
        dev.off()
      }
    )
    
    output$plot_PCA_scree <- downloadHandler(
      filename = function() {
        paste(gsub(".txt","", input$file1),"_Scree_plot_PCA_analysis.pdf",sep='')  },
      content = function(file) {
        pdf(file)
        print( pca()$pca.facto.scree.var )
        dev.off()
      }
    )
    
    
    output$plot_PCA_contrib <- downloadHandler(
      filename = function() {
        paste(gsub(".txt","", input$file1),"_contributions_barplot_PCA_analysis.pdf",sep='')  },
      content = function(file) {
        pdf(file,width = 20,height = 7)
        print( multiplot(pca()$facto.contr.id.1,pca()$facto.contr.id.2,cols=2) )
        print( multiplot(pca()$facto.contr.sample.1,pca()$facto.contr.sample.2,cols=2) )
        dev.off()
      }
    )
    
    output$plot_PCA_contrib_biplot <- downloadHandler(
      filename = function() {
        paste(gsub(".txt","", input$file1),"_contributions_biplot_PCA_analysis.pdf",sep='')  },
      content = function(file) {
        pdf(file,width = 10,height = 10)
        print( pca()$facto.contr.ID.biplot )
        dev.off()
      }
    )
    
    output$plot_dendro <- downloadHandler(
      filename = function() {
        paste(gsub(".txt","", input$file1),input$metrtics,"_",input$linkage,"_clusterN_",input$num.cluster,"_dendrogram_cluster_analysis.pdf",sep='')  },
      content = function(file) {
        pdf(file,width = 20,height = 13)
        par(mar=c(15,5,5,5))
        print(plot(pca()$den.out.plot,main = paste("clustering (hclust - ",input$metrics,"/",input$linkage,"/ N = ",input$num.cluster,")"),cex.main=2) )
        dev.off()
      }
    )
    output$correlationplot <- downloadHandler(
      filename = function() {
        paste(gsub(".txt","", input$file1),"_correlation_plot_analysis.pdf",sep='')  },
      content = function(file) {
        pdf(file,width = 20,height = 20)
        print( pca()$correlation.plot ) 
        dev.off()
      }
    )
   
    
    output$boxplo <- downloadHandler(
      filename = function() {
        paste(gsub(".txt","", input$file1),"_boxplot_analysis.pdf",sep='')  },
      content = function(file) {
        pdf(file,width = 15,height = 6)
        print( pca()$boxplot.of.data )
        dev.off()
      }
    )

    #plotly out put very low res
    #output$downloadplotly <- downloadHandler(
     # filename = function() { paste(gsub(".txt","", input$file1),"_3D-plot_PCA_analysis.png",sep='') },
     # content = function(file) {
        
     #   plotly_IMAGE(pca()$plotly3d.facto.plot, format = "png",width = 1000,height = 1000,, out_file = file)
    #  }
   # )
    
    #session info output
    output$sessioninfo <- renderUI({
      if(!is.null(input$packages)) sapply(input$packages, function(x) eval(parse(text=paste0("library(",x,")"))))
      includeRmd("SessionInfo.Rmd")
    })
    
    
    output$helper.text.method<-renderUI({
        paste(c(
          paste("The principle component analysis was performed in R (",R.Version()$version.string,") using the factomineR package (version:",packageVersion("FactoMineR"),").",sep=""),
          if(input$missval==T){paste("Missing values were replaced by 0.")}else{paste("Missing values were removed.")},
          if(input$scaling==F){paste("The data centering was done by subtracting the column means (omitting NAs) of the data from their corresponding columns.")
          }else{
            paste("The data centering was done by subtracting the column means (omitting NAs) of the data from their corresponding columns. To unify variances scaling of data was done by dividing the (centered) columns of the data by their standard deviations.")},
          if(input$ellipses==T){paste("Ellipses around the individuals were drawn by using", input$ellipses.level,"size of the concentration ellipse in normal probability.")},
            paste("The Hierarchical Clustering on Principle Components (HCPC) is performing an agglomerative hierarchical clustering on results from a factor analysis like PCA."),
            paste("(metric: ",input$metrics,"; linkage: ",input$linkage,")",sep=""),
          paste("The suggest cutting of the tree into cluster was done by building the sum of the within-cluster inertia that are calculated for each partition and was used for the calculation 
                of the suggested partition, which is the one with the higher relative loss of inertia (i(clusters n+1)/i(cluster n)) (automatic cluster cutting inside the HCPC function; option: 
                nb.clust = -1).")
              ),collapse = " ")
      
      
    })
    
    
    
    
  
    
   
    
   
})
