suppressPackageStartupMessages(library(colourpicker))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(bit64))

dashboardPage(
  dashboardHeader(title = "PCA | HCPC | Clustering of data",titleWidth = 400),
  dashboardSidebar(
    sidebarMenu(
      #action button for run or Demo
      actionButton("inputButton", "DEMO or RUN",width = "90%",icon=icon("youtube-play"),style="color: #fff; background-color: #D84315; border-color: #BF360C"),
      #select_meta_data_coloring
      uiOutput("meta.sele"),
      #File main input
      fileInput('file1', 'MAIN DATA: Choose CSV or TXT',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      
      #File meta input
      fileInput('file2', 'META DATA: Choose CSV or TXT',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      awesomeRadio('sep', 'table separator',
                   c(Tab='\t',
                     Comma=','
                   ),
                   selected='\t'),
      
      
                #download examples
                menuItem("download example", tabName = "download example", icon = icon("table"),
                         #download examples
                         downloadButton(outputId= "file1.data.frame",label = "Download example data frame",class="butt2"),
                         br(),
                         #download examples
                         downloadButton(outputId= "file2.data.frame",label = "Download example meta data",class="butt2"),
                         br(),
                         # making the font italics this time
                         tags$head(tags$style(".butt2{background-color:black;} .butt2{color: white;} .butt2{font-style: italic;}"))
                ),
      
               #data transformations
                menuItem("data transformation", tabName = "data transformation", icon = icon("bar-chart"),
                         materialSwitch(inputId = "logtrans", label = "Do log2 transformation of data?", status = "danger",value = FALSE,width = 400),
                         materialSwitch(inputId = "scaling", label = "Do scaling of data?", status = "danger",value = TRUE,width = 400),
                         materialSwitch(inputId = "missval", label = "missing value imputation of data? (TRUE = replace missing values with half-minimal value)", status = "danger",value = FALSE,width = 400)

                ),
      
              #clustering adjustments
              menuItem("clustering", tabName = "clustering adjustments", icon = icon("cubes"),
                #selection
                sliderInput("num.cluster", "number of cluster (dendrogram plot):" , min = 2, max = 30, value = 3,step = 1),
                selectInput(inputId = "metrics", label = "metrics for clustering:", choices = list("euclidean","manhattan"),selected = "euclidean"),
                selectInput(inputId = "linkage", label = "linkage for clustering:", choices = list("single","complete","ward","average"),selected = "average")
                
                ),
      
      
                  #general adjustments
                  menuItem("general adjustments", tabName = "general adjustments", icon = icon("wrench"),
                           sliderInput("theme.cex", "basic theme text size:", 18, 60, value = 22, step=1),
                           sliderInput("num.ele", "number of top elements to contribution:", 1, 50, value = 10, step=1)
                           
                  ),
      
      
      menuItem("downloads", tabName = "downloads", icon = icon("cloud-download"),
               downloadButton('plots3D_PCA', '3D PCA plot',class="butt2"),
               br(),
               downloadButton('plots2D_PCA', '2D PCA plots',class="butt2"),
               br(),
               downloadButton('plots2D_PCA_legend', '2D PCA legend',class="butt2"),
               br(),
               downloadButton('plot_PCA_scree', 'Scree plot',class="butt2"),
               br(),
               downloadButton('plot_PCA_contrib', 'contribution plots',class="butt2"),
               br(),
               downloadButton('plot_PCA_contrib_biplot', 'contribution biplot plots',class="butt2"),
               br(),
               downloadButton('plot_dendro', 'dendrogram cluster analysis',class="butt2"),
               br(),
               downloadButton('correlationplot', 'correlation plots',class="butt2"),
               br(),
               downloadButton('boxplo', 'Boxplots',class="butt2"),
               br(),
               downloadButton('pca.interpret.dim1', 'characteristic for 1st. Dim.',class="butt2"),
               br(),
               downloadButton('pca.interpret.dim2', 'characteristic for 2nd. Dim.',class="butt2"),
               br(),
               downloadButton('pca.interpret.dim3', 'characteristic for 3rd. Dim.',class="butt2"),
               
               # making the font italics this time
               tags$head(tags$style(".butt2{background-color:black;} .butt2{color: white;} .butt2{font-style: italic;}"))
               
      ),

      #versioning
      tags$hr(),
      em("version 1.4.8 | S. Michalik")
      
      )
    
    
  ),
  dashboardBody(
    
    #hide error messsages in shiny
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    # Also add some custom CSS to make the title background area the same
    # color as the rest of the header.
    tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #080A0D;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #080A0D;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #080A0D;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #080A0D;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #080A0D;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #080A0D;
                              color: #FFFFFF;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #616262;
                              }
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #080A0D;
                              }
                              '))),
  
    fluidRow(
      tabBox(width = 9,
        title = "",
        # The id lets us use input$main on the server to find the current tab
        id = "main",
                    #tabPanel("testx", textOutput("testx")),
                    #tabPanel("table", tableOutput("table.out")),
                  # pdf(NULL) important before plotly out put otherwise Rplot.pdf error occur
                 
                  tabPanel("PCA 3D plots", pdf(NULL),plotlyOutput("facto.plot.3d",height = 600)),
                  tabPanel("PCA 2D plots", 
                           dropdownButton( #add menu directly to plot (shinyWidgets)
                             tags$h3("2D PCA plot adjustments:"),
                             checkboxInput("shownam", "show labels ?", value = FALSE),
                             checkboxInput("ellipses", "add ellipses ?", value = FALSE),
                             checkboxInput("show.mean.points", "show center points ?", value = FALSE),
                             selectInput("ellipses.method","ellipses calculation method",c("norm","convex","t","euclid"),selected = "norm"),
                             numericInput("ellipses.level","size of the concentration ellipse in normal probability:",value=0.95),
                             numericInput("ellipses.alpha","transparency of ellipses:",value=0.1),
                             sliderInput("plot.point.size","size of the points:",min = 1,max = 10,value=3,step = 0.5),
                             circle = TRUE, status = "danger", icon = icon("wrench"), width = "300px",
                             tooltip = tooltipOptions(title = "Click to open plot adjustments !")
                           ),
                           plotOutput("facto.plot.2d",height = 800)
                           #,plotOutput("facto.plot.2d.legend")
                           ),
                  tabPanel("HCPC 3D plot", plotOutput("facto.plot.hcpc.3d",height = 800)),
                  tabPanel("HCPC 2D plot", plotOutput("facto.plot.hcpc.2d",height = 800)),
                  tabPanel("HCPC tree plot", plotOutput("facto.plot.hcpc.tree")),
                  tabPanel("HCPC bar plot", plotOutput("facto.plot.hcpc.bar")),
                  tabPanel("Scree plots", plotOutput("facto.scree",height = "800px")),
                  tabPanel("contribution plots of data input in PCA", 
                           plotOutput("facto.contr.id"),plotOutput("facto.contr.sample")),
                  tabPanel("contribution samples matrix plot",plotOutput("corrplot.var",width = 800,height = 1200)),
                  tabPanel("contribution biplot of PCA", 
                           plotOutput("facto.contr.biplot",height = "800px")),
                  tabPanel("dimension description",
                           fluidRow(
                           box(DT::dataTableOutput("table.pca.interpret.dim1"),title = "1st. Dimension",width = 4,solidHeader = TRUE,status = "primary"),
                           box(DT::dataTableOutput("table.pca.interpret.dim2"),title = "2nd. Dimension",width = 4,solidHeader = TRUE,status = "success"),
                           box(DT::dataTableOutput("table.pca.interpret.dim3"),title = "3rd. Dimension",width = 4,solidHeader = TRUE,status = "danger")
                           )),
                  tabPanel("boxplot of data input in PCA", plotOutput("boxplot.data")),
                  tabPanel("correlation plot of data input in PCA", 
                           dropdownButton( #add menu directly to plot (shinyWidgets)
                             tags$h3("correlation plot adjustments:"),
                             colourpicker::colourInput("col2", "Select upper colour:", "orangered3",palette = "square", returnName = TRUE,showColour = c("both")),
                             colourpicker::colourInput("colneutral", "Select neutral colour:", "white",palette = "square", returnName = TRUE,showColour = c("both")),
                             colourpicker::colourInput("col1", "Select lower colour:", "dodgerblue3",palette = "square", returnName = TRUE,showColour = c("both")),
                             sliderInput("leg.lim", "corrplot color limits:" , min = -1, max = 1, value = c(0.7,1),step = 0.1),
                             numericInput("sig.level", "sig. level:" , value = 0.01),
                             materialSwitch(inputId = "corr.reorder", label = "Do reordering based on clustering?", status = "danger",value = TRUE,width = 400),
                             sliderInput("corr.size", "size text factor :" , min = 5, max = 70, value = 7,step = 1),
                             circle = TRUE, status = "danger", icon = icon("wrench"), width = "300px",
                             tooltip = tooltipOptions(title = "Click to open plot adjustments !")
                           ),
                           plotOutput("corr.plot",width = 800,height = 800)
                           ),
                  tabPanel("dendrogram over samples",         
                           plotOutput("den.out",height = 1000))#,
                  #tabPanel("test table", tableOutput("table.test"))
                  
       
      ),
      tabBox(width = 3, title = tagList(shiny::icon("question"), "help"),side = "right",selected = "color selction",
             tabPanel("general",
                      h3("PCA and HCPC function (FactoMineR package in R)"),
                      h5("scaled = correlation matrix // unscaled = covariation matrix"),
                      h5("Hierarchical Clustering on Principle Components (HCPC)"),
                      h5("Principle Components Analysis (PCA)") ,
                      
                      valueBoxOutput("individual.count",width="100%"),
                      valueBoxOutput("raw.data.count",width="100%"),
                      valueBoxOutput("pca.data.count",width="100%"),
                      valueBoxOutput("removed.variables",width="100%")
                      
             ),
             
             tabPanel("PCA",
                      strong("used here:"),p("When genes/proteins are variables, the analysis creates a set of “principal gene components” that indicate the features of genes
                    that best explain the experimental responses they produce."),
                      tags$hr(),
                      p("When experiments are the variables, the analysis creates a set of “principal experiment components” that indicate the features of the experimental conditions that best explain the gene behaviors they elicit."),
                      
                      h5("Raychaudhuri, S. & Stuart, J. M. Principal components analysis to summarize microarray experiments: application to sporulation time series. Pacific Symposium on … (2000)."),
                      tags$hr(),
                      tags$div(class='success',HTML(
                                      "The amount of variation retained by each PC is called <strong>eigenvalues</strong>. The first PC corresponds to the direction with the maximum amount of variation in the data set.")),
                      strong("cos2:"),
                      p("The squared loadings for variables are called cos2 ( = cor * cor = coord * coord)."),
                      HTML("<div class='success'>
                             <ul>
                             <li>The cos2 values are used to estimate the quality of the representation</li>
                             <li>The closer a variable is to the circle of correlations, the better its representation on the factor map (and the more important it is to interpret these components)</li>
                             <li>Variables that are closed to the center of the plot are less important for the first components.</li>
                             </ul>
                             </div>"),
                      strong("contributions:"),
                      p("The contributions of variables in accounting for the variability in a given principal component are (in percentage) : (variable.cos2 * 100) / (total cos2 of the component)"),
                      p("The larger the value of the contribution, the more the variable contributes to the component."),
                      HTML("<p><span class='question'>What means the red line on the graph?</span></p>
                           <div class='warning'>
                           <ul>
                           <li><p>If the contribution of the variables were uniform, the expected value would be 1/length(variables) = 1/10 = 10%.</p></li>
                           <li>The red dashed line on the graph above indicates the expected average contribution. For a given component, a variable with a contribution larger than this cutoff could be considered as important in contributing to the component.</li>
                           </ul>
                           </div>")
                      
                 ),
             tabPanel("clustering",
                      #explaination cluserting
                      strong("metrics"),
                      h5("euclidean"),
                      h6("Euclidean distances are root sum-of-squares of differences."),
                      h5("manhattan"),
                      h6("Manhattan distances are the sum of absolute differences."),
                      #linkage
                      tags$hr(),            
                      strong("linkage") ,      
                      h5("Single"),
                      h6("With single linkage method (also called nearest neighbor method), the distance between two clusters is the minimum distance between an observation in one cluster and an observation in the other cluster. The single linkage method is a good choice when clusters are obviously separated. When observations lie close together, the single linkage method tends to identify long chain-like clusters that can have a relatively large distance separating observations at either end of the chain."),
                      h5("Average"),
                      h6("With the average linkage method, the distance between two clusters is the mean distance between an observation in one cluster and an observation in the other cluster. Whereas the single or complete linkage methods group clusters are based on single pair distances, the average linkage method uses a more central measure of location."),
                      #h5("Centroid"),
                      #h6("With the centroid linkage method, the distance between two clusters is the distance between the cluster centroids or means. Like the average linkage method, this method is one more averaging technique."),
                      h5("Complete"),
                      h6("With the complete linkage method (also called furthest neighbor method), the distance between two clusters is the maximum distance between an observation in one cluster and an observation in the other cluster. This method ensures that all observations in a cluster are within a maximum distance and tends to produce clusters with similar diameters. The results can be sensitive to outliers."),
                      #h5("Median"),
                      #h6("With the median linkage method, the distance between two clusters is the median distance between an observation in one cluster and an observation in the other cluster. This is a different averaging technique, but uses the median instead of the mean, thus downweighting the effect of outliers."),
                      #h5("McQuitty"),
                      #h6("With McQuitty's linkage method, when two clusters are be joined, the distance of the new cluster to any other cluster is calculated as the average of the distances of the soon to be joined clusters to that other cluster. For example, if clusters 1 and 3 are to be joined into a new cluster, say 1*, then the distance from 1* to cluster 4 is the average of the distances from 1 to 4 and 3 to 4. Here, distance depends on a combination of clusters instead of individual observations in the clusters."),
                      h5("Ward"),
                      h6("With Ward's linkage method, the distance between two clusters is the sum of squared deviations from points to centroids. The goal of Ward's linkage method is to minimize the within-cluster sum of squares. It tends to produce clusters with similar numbers of observations, but it is sensitive to outliers. In Ward's linkage method, it is possible for the distance between two clusters to be larger than dmax, the maximum value in the original distance matrix. If this occurs, the similarity will be negative.")
             ),
             #tabPanel("sessionInfo",
             #           uiOutput("sessioninfo")),
             tabPanel("method",
                      uiOutput("helper.text.method")),
             tabPanel("color selction",
                      uiOutput("colorselector"))
                 
                
                  
                 
        
      
      
     ) #tabbox close
    )#close fluidrow
  )#close dashboard body
)#close dashboard page
    

#### to do !!!
# drastic performance optimization ! split reactive element!!!

#version: 1.4.8
# bugfix: meta data to data table sorting to fix unsorted data bug (coloring ....)

#version: 1.4.7
# add download 3D PCA html
# replace missing value from 0 to half-minimal value


#version: 1.4.6
# add selectable colors helptext if elements in meta data are above 15 elements

#version: 1.4.5
# add selectable colors
# use factoMineR plot function for HCPC (bug was fixed an therefore the sefl edited function is no longer needed)

#version: 1.4.4
# fixed issue if to low number of dimension was generated in PCA for contribution from "contri<-get_pca_ind(pca.facto)$contrib[,1:8]" to "contri<-get_pca_ind(pca.facto)$contrib[,1:5]"

#version: 1.4.3
# use mean.points to show and hide centers
# add correlation plot to ind contrib
#add value boxes for data overview


#version: 1.4.2
# use check.names=F for import to avoid "X" infront of numbers in colnames
# add re-ordering selection for correlation plot

#version: 1.4.1
# fix 64bit number issue
# boxplot issue remianing!

#version: 1.4
# PCA plot from FactoExtra package
# add ellipsis option to 2D plot
# solve issue if numbers are used for sample description

#version: 1.3
# add new shiny dashboard look and feel

#version: 1.2
# do shiny dashboard adpation
# add dimdesc + table (correlation and test)
#         This function is designed to point out the variables and the categories that are the most characteristic according to each dimension obtained by a Factor Analysis. 

#version: 1.1
# implement feedback and resolve some bugs and naming
# resolve input action button bug; now using eventReactive instead of isolate


#version: 1
# release of the app
# add colour picker
# add theme size slidebar
# transform UI to dashboard
# add HCPC
# add dendrogram
# add PCA plotly
# add PCA non-overlapping labels



