# setwd("~/R/MyShinyAPP")
# dataFile <- read.table(file.choose(),header = T, sep = ";", quote = '"', dec = ".")
# as_tibble(dataFile)
# head(dataFile)
# mode(dataFile)
# ####################### # 
## SERVER ----
# ####################### #

server = function(input, output,session) {
  MyData <- reactiveValues()
  MyDataSum <- reactiveValues()
  MyDataBis <- reactiveValues()
  MyDataTVA <- reactiveValues()
  MyDataBisGraph <- reactiveValues()
  
  output$tab_preview <- DT::renderDataTable(filter='none', rownames = F,
                                            colnames = c('Taux de TVA' = 'Ts %', 'Code article' = 'Code'), 
                                            {
                                              options(
                                                DT.options = list(
                                                  autoWidth = TRUE, dom = 'tip',  
                                                  columnDefs = list(list(className = 'dt-center', targets = "")),
                                                  pageLength = 6,
                                                  lengthMenu = c(6, 10, 50, 100)
                                                )
                                              )
                                              req(input$dataFile)
                                              
                                              dataFile <- read_delim(input$dataFile$datapath,
                                                                     ";", escape_double = FALSE,
                                                                     col_types = cols(
                                                                       Date = col_date(format = "%Y-%m-%d"), 
                                                                       Heure = col_time(""),
                                                                       Code = col_factor(),
                                                                       Designation = col_character(),
                                                                       `Qte` = col_number(),
                                                                       # `Ts %` = col_factor(levels = c()),
                                                                       Mont.Total = col_number()),
                                                                     locale = locale(decimal_mark = ",", 
                                                                                     encoding = "ISO-8859-1"), na = "null", 
                                                                     comment = "//", trim_ws = TRUE)
                                              
                                              dataFile <<- dataFile[,-3] 
                                            },  options = list(pageLength = 6)
  )
  
  output$dataFile <- DT::renderDataTable(class = "hover cell-border compact flotter", selection = "none", 
                                         ## caption = "Rapport des ventes",
                                         {
                                           df_expose = MyData$data
                                           # Gerer la selection des codes articles
                                           if(is.null(input$SelectCode)){df_expose = df_expose}
                                           else{df_expose = df_expose[df_expose$Code %in% input$SelectCode, ]}
                                           # Gerer la selection des codes TVA
                                           if(is.null(input$SelectTVA)){df_expose = df_expose}
                                           else{df_expose = df_expose[df_expose$'Ts %' %in% input$SelectTVA, ]}
                                           # Gerer la selection des codes familles
                                           if(is.null(input$SelectFamilles)){df_expose = df_expose}
                                           else{
                                             print(input$SelectFamilles)
                                             print(df_expose[c("Désignation.Famille")])
                                             df_expose = df_expose[df_expose$Désignation.Famille %in% input$SelectFamilles, ]
                                           } 
                                           
                                           if (file.exists("dataCodeRayons.csv"))
                                           { 
                                             df_expose = df_expose[,c("Date", "Heure", "Réf.Doc.", "Famille", "Code", "Désignation", "Qté", "Ts %", "Prix", "Mont.Soumis", "Mont.TVA", "Mont.Total")]
                                           }
                                           else if (file.exists("dataFamilles.csv"))
                                           {
                                             df_expose = df_expose[,c("Date", "Heure", "Réf.Doc.", "Famille", "Code", "Désignation", "Désignation.Famille", "Qté", "Ts %", "Prix", "Mont.Soumis", "Mont.TVA", "Mont.Total")]
                                           }
                                           else
                                           {
                                             df_expose = df_expose[,c("Date", "Heure", "Réf.Doc.", "Code", "Désignation", "Qté", "Ts %", "Prix", "Mont.Soumis", "Mont.TVA", "Mont.Total")]
                                           }
                                           
                                           df <- datatable(df_expose %>% filter(Date >= input$DateRange[1] & Date <= input$DateRange[2]), 
                                                           extension = "Buttons",
                                                           filter='none',
                                                           colnames = c('Taux de TVA' = 'Ts %', 'Code article' = 'Code'),
                                                           options = list(
                                                             autoWidth = TRUE,
                                                             dom = "lftiprB", 
                                                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), 
                                                             columnDefs = list(list(className = 'dt-center', targets = "") ),
                                                             pageLength = 8,
                                                             lengthMenu = c(8, 200, 500, 1000)
                                                           )) %>% formatCurrency("Mont.Total", currency = "\U20AC  ", interval = 3, mark = ",", digits = 2) 
                                           # %>% formatStyle('Prix', color = 'red', backgroundColor = 'yellow', fontWeight = 'bold')
                                         }
  )
  
  output$dataFileSum <- DT::renderDataTable(
    {
      df_expose = MyData$data %>% filter(Date >= input$DateRange[1] & Date <= input$DateRange[2])
      # Gerer la selection des codes articles
      if(is.null(input$SelectCode)){df_expose = df_expose}
      else{df_expose = df_expose[df_expose$Code %in% input$SelectCode, ]}
      # Gerer la selection des TVA
      if(is.null(input$SelectTVA)){df_expose = df_expose}
      else{df_expose = df_expose[df_expose$'Ts %' %in% input$SelectTVA, ]}
      # Gerer la selection des codes familles
      if(is.null(input$SelectFamilles)){df_expose = df_expose}
      else{df_expose = df_expose[df_expose$Désignation.Famille  %in% input$SelectFamilles, ]}
      
      df_expose <- df_expose[,c('Mont.Total','Mont.TVA')]
      df_expose <- data.frame(Sommes=colSums(df_expose))
      
      df <- datatable(df_expose,
                      options=list(
                        autoWidth = FALSE, 
                        dom = "none",
                        columnDefs = list(list(className = 'dt-center')))
      ) %>% formatCurrency("Sommes", currency = "\U20AC  ", interval = 3, mark = ",", digits = 2)
    }) 
  
  output$dataCod.Rayons<- DT::renderDataTable(filter='none', rownames = F, editable = T, {
    
    req(input$dataCod.Rayons)
    
    dataCod.Rayons <- read.csv2(input$dataCod.Rayons$datapath) 
    dataCod.Rayons <- dataCod.Rayons[,c(-6:-53)]
  })
  
  output$dataFamilles <- DT::renderDataTable(filter='none', rownames = F, editable = T, {
    
    req(input$dataFamilles)
    
    dataFamilles <- read.csv2(input$dataFamilles$datapath)
    dataFamilles <- dataFamilles[,c(-4:-14)]
  })
  
  output$MyDataBis <- DT::renderDataTable(filter='none', rownames = F,
                                          {
                                            df_expose = MyData$data
                                            df_expose <- df_expose %>% filter(Date >= input$DateRange[1] & Date <= input$DateRange[2])
                                            
                                            # Gerer la selection des codes articles
                                            if(is.null(input$SelectCode)){df_expose = df_expose}
                                            else{df_expose = df_expose[df_expose$Code %in% input$SelectCode, ]}
                                            # Gerer la selection des TVA
                                            if(is.null(input$SelectTVA)){df_expose = df_expose}
                                            else{df_expose = df_expose[df_expose$'Ts %' %in% input$SelectTVA, ]}
                                            # Gerer la selection des codes familles
                                            if(is.null(input$SelectFamilles)){df_expose = df_expose}
                                            else{df_expose = df_expose[df_expose$Désignation.Famille  %in% input$SelectFamilles, ]}
                                            
                                            df_expose$Mont.Soumis <- as.numeric(df_expose$Mont.Soumis)
                                            df_expose$Mont.TVA <- as.numeric(df_expose$Mont.TVA)
                                            df_expose$Mont.Total <- as.numeric(df_expose$Mont.Total)
                                            
                                            df_expose = aggregate(cbind(df_expose$Mont.Soumis,df_expose$Mont.TVA,df_expose$Mont.Total),
                                                                  by=list(Désignation.Famille=df_expose$Désignation.Famille), FUN=sum)
                                            
                                            print(df_expose)
                                            df <- datatable(df_expose, rownames = F,
                                                            colnames = c('Montant soumis' = 'V1', 'Montant TVA' = 'V2', 'Montant Total' = 'V3'),
                                                            extension = "Buttons",
                                                            filter='none',
                                                            options = list(
                                                              # autoWidth = TRUE,
                                                              dom = "lftiprB",
                                                              buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                              pageLength = 15,
                                                              lengthMenu = c(15, 20, 25, 30)
                                                            )) %>% formatCurrency("Montant Total", ' \U20AC  ', 2)
                                          }
  )
  
  output$MyDataTVA <- DT::renderDataTable(filter='none', rownames = F,
                                          {
                                            df_expose = MyData$data
                                            df_expose <- df_expose %>% filter(Date >= input$DateRange[1] & Date <= input$DateRange[2])
                                            
                                            # Gerer la selection des codes articles
                                            if(is.null(input$SelectCode)){df_expose = df_expose}
                                            else{df_expose = df_expose[df_expose$Code %in% input$SelectCode, ]}
                                            # Gerer la selection des TVA
                                            if(is.null(input$SelectTVA)){df_expose = df_expose}
                                            else{df_expose = df_expose[df_expose$'Ts %' %in% input$SelectTVA, ]}
                                            # Gerer la selection des codes familles
                                            if(is.null(input$SelectFamilles)){df_expose = df_expose}
                                            else{df_expose = df_expose[df_expose$Désignation.Famille  %in% input$SelectFamilles, ]}
                                            
                                            df_expose$Mont.Soumis <- as.numeric(df_expose$Mont.Soumis)
                                            df_expose$Mont.TVA <- as.numeric(df_expose$Mont.TVA)
                                            df_expose$Mont.Total <- as.numeric(df_expose$Mont.Total)
                                            
                                            df_expose <- aggregate(cbind(df_expose$Mont.Soumis,df_expose$Mont.TVA,df_expose$Mont.Total),
                                                                   by=list('Ts %'=df_expose$'Ts %'), FUN=sum)
                                            
                                            print(df_expose)
                                            df <- datatable(df_expose, rownames = F,
                                                            colnames = c('Taux de TVA' = 'Ts %', 'Montant TVA' = 'V2', 'Montant HT'= 'V1', 'Montant TTC' = "V3"),
                                                            extension = "Buttons",
                                                            filter='none',
                                                            options = list(
                                                              # autoWidth = TRUE,
                                                              dom = "lftiprB",
                                                              buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                              pageLength = 5,
                                                              lengthMenu = c(5, 6, 7)
                                                            ))
                                          }
  )
  
  output$MyDataBisGraph <- renderPlot( {
    df_expose = MyData$data
    df_expose <- df_expose %>% filter(Date >= input$DateRange[1] & Date <= input$DateRange[2])
    
    # Gerer la selection des codes articles
    if(is.null(input$SelectCode)){df_expose = df_expose}
    else{df_expose = df_expose[df_expose$Code %in% input$SelectCode, ]}
    # Gerer la selection des TVA
    if(is.null(input$SelectTVA)){df_expose = df_expose}
    else{df_expose = df_expose[df_expose$'Ts %' %in% input$SelectTVA, ]}
    # Gerer la selection des codes familles
    if(is.null(input$SelectFamilles)){df_expose = df_expose}
    else{df_expose = df_expose[df_expose$Désignation.Famille  %in% input$SelectFamilles, ]}
    
    df_expose$Mont.Soumis <- as.numeric(df_expose$Mont.Soumis)
    df_expose$Mont.TVA <- as.numeric(df_expose$Mont.TVA)
    df_expose$Mont.Total <- as.numeric(df_expose$Mont.Total)
    
    df_expose = aggregate(cbind(df_expose$Mont.Soumis,df_expose$Mont.TVA,df_expose$Mont.Total),
                          by=list(Désignation.Famille=df_expose$Désignation.Famille), FUN=sum)
    colnames(df_expose)
    colnames(df_expose) = c('Désignation.Famille','Montant soumis' , 'Montant TVA', 'Montant.Total' )
    ggplot(df_expose[,c('Désignation.Famille','Montant.Total')], aes(x=Désignation.Famille, y=Montant.Total))+geom_bar(stat="identity")   
    
  },height = 'auto',width = 'auto'
  )
  
  # =========================================================================== =
  ## Preview ----
  # =========================================================================== =
  observeEvent(input$UploadFile, {
    updateTabItems(session,"tabs",selected= "tab_readData")
  })
  observeEvent(input$ModFile, {
    updateTabItems(session, "tabs", selected = "tab_visualization")
  })
  
  observeEvent(input$Dashboard, {
    updateTabItems(session, "tabs", selected = "tab_dashboard")
  })
  
  observeEvent(input$merging, {
    
    req(input$dataCod.Rayons)
    
    dataCod.Rayons <- read.csv2(input$dataCod.Rayons$datapath)
    dataCod.Rayons <- dataCod.Rayons[,c('Code', 'Famille')]
    if (file.exists("~/R/ProjetCaisse/ProjetCaisse/dash/dataFamilles.csv"))
    {
      dataFamilles <- read.table("~/R/ProjetCaisse/ProjetCaisse/dash/dataFamilles.csv", header = T, sep = ";", quote = '"', dec = ".")
      dataFamilles <- read.csv2(input$dataFamilles$datapath)
      dataFamilles<- dataFamilles[,c('Code', 'Désignation')]
      MyData$data$Code <- as.numeric(as.character(MyData$data$Code))
      colnames(dataFamilles) <- c("Famille", "Désignation.Famille")
      total <- merge(MyData$data,dataFamilles,by="Famille",all = TRUE)
      total$Famille <- as.factor(total$Famille)
      MyData$data <- total
    }
    else
    {
      MyData$data$Code <- as.numeric(as.character(MyData$data$Code))
      dataCod.Rayons$Code <- as.numeric(as.character(dataCod.Rayons$Code))
      total <- merge(MyData$data,dataCod.Rayons,by="Code",all = TRUE)
      total$Code <- as.factor(total$Code)
      MyData$data <- total
    }
    write.csv2(dataCod.Rayons, file = "dataCodeRayons.csv")
    
    sendSweetAlert(
      session  =  session , 
      title  =  "Succes !!" , 
      text  =  "Ce fichier sera dorénavant utiliser pour libeller les articles ..." , 
      type  =  "success" 
    )
  })
  
  observeEvent(input$mergingF, {
    
    req(input$dataFamilles)
    
    dataFamilles <- read.csv2(input$dataFamilles$datapath)
    dataFamilles<- dataFamilles[,c('Code', 'Désignation')]
    if (file.exists("~/R/ProjetCaisse/ProjetCaisse/dash/dataCodeRayons.csv"))
    {    
      dataCod.Rayons <- read.table("~/R/ProjetCaisse/ProjetCaisse/dash/dataCodeRayons.csv",header = T, sep = ";", quote = '"', dec = ".")
      dataCod.Rayons <- dataCod.Rayons[,c('Code', 'Famille')]
      colnames(dataFamilles) <- c("Famille", "Désignation.Famille")
      MyData$data$Code <- as.numeric(as.character(MyData$data$Code))
      dataCod.Rayons$Code <- as.numeric(as.character(dataCod.Rayons$Code))
      
      total <- merge(MyData$data,dataCod.Rayons,by="Code",all = TRUE)
      total$Code <- as.factor(total$Code)
      MyData$data <- total   
      
      write.csv2(dataFamilles, file = "dataFamilles.csv")
      sendSweetAlert(
        session  =  session ,
        title  =  "Succes !!" ,
        text  =  "Ce fichier sera dorénavant utiliser pour libeller les codes familles ..." ,
        type  =  "success"
      )
    }
    else 
    {
      sendSweetAlert(
        session  =  session ,
        title  =  "attention !!" ,
        text  =  "Veuillez charger la table codes rayons en premier" ,
        type  =  "warning"
      )
    }
  })
  
  observeEvent(input$visualisation, {
    
    if(!is.null(input$dataFile)){
      
      MyData$data <- read_delim(input$dataFile$datapath,
                                ";", escape_double = FALSE,
                                col_types = cols(
                                  Date = col_date(format = "%Y-%m-%d"), 
                                  Heure = col_time(format = ""),
                                  Code = col_factor(),
                                  Designation = col_character(),
                                  `Qte` = col_number(),
                                  # `Ts %` = col_factor(levels = c()),
                                  Mont.Total = col_number()),
                                locale = locale(decimal_mark = ",", 
                                                encoding = "ISO-8859-1"), na = "null", 
                                comment = "//", trim_ws = TRUE 
      )
      
      MyData$data <- MyData$data[,-3,]
      MyData$data %>% filter(Date >= input$DateRange[1] & Date <= input$DateRange[2])
      
      # Merge avec le fichier des codes articles si il existe dans le répertoire courant
      if (file.exists("dataCodeRayons.csv"))
      { 
        print("file dataCodeRayons exist")
        dataCod.Rayons <- read.csv2("dataCodeRayons.csv")
        dataCod.Rayons = subset(dataCod.Rayons, select = -c(X) )
        
        MyData$data$Code <- as.numeric(as.character(MyData$data$Code))
        dataCod.Rayons$Code <- as.numeric(as.character(dataCod.Rayons$Code))
        
        total <- merge(MyData$data,dataCod.Rayons,by="Code",all = TRUE)
        total$Code <- as.factor(total$Code)
        MyData$data <- total
      }
      
      # Merge avec le fichier des codes familles si il existe dans le répertoire courant
      if (file.exists("dataFamilles.csv"))
      {
        print("file dataFamilles exist")
        dataFamilles <- read.csv2("dataFamilles.csv")
        MyData$data$Famille <- as.numeric(as.character(MyData$data$Famille))
        print(colnames( MyData$data))
        dataFamilles$Famille <- as.numeric(as.character(dataFamilles$Famille))
        dataFamilles = subset(dataFamilles, select = -c(X) )
        thecolname  = colnames( dataFamilles)[-1]
        
        total <- merge(MyData$data,dataFamilles,by="Famille",all = TRUE)
        total$Famille <- as.factor(total$Famille)
        MyData$data <- total
        updateSelectInput(session, 'SelectFamilles', choices = unique( MyData$data[c(thecolname)] ) )
      }
      
      updateSelectizeInput(session, 'SelectCode', choices = unique( MyData$data[c("Code")] ) )
      updateSelectizeInput(session, 'SelectTVA', choices = unique( MyData$data[c("Ts %")] ) )
      
      MyDataSum$data <- MyData$data[,c('Prix','Mont.TVA')]
      MyDataSum$data <- data.frame(Sommes=colSums(MyDataSum$data))
      
      sendSweetAlert(
        session = session,
        title = "Le fichier a bien été chargé !",
        text = "Les informations sont disponibles",
        type = "success"
      )
      
      updateTabItems(session,"tabs",selected= "tab_visualization")
    }
    else
    {
      sendSweetAlert(
        session  =  session , 
        title  =  "Attention !!" , 
        text  =  "Veuillez sélectionner un fichier ..." , 
        type  =  "erreur" 
      )
    }
  })
  
  observeEvent(input$saveRBtn, {
    
    dataCod.Rayons <- read.csv2(input$dataCod.Rayons$datapath) 
    dataCod.Rayons <- dataCod.Rayons[,c(-6:-53)]
    dataCod.Rayons <- dataCod.Rayons[,c(-2:-4)]
    write.csv2(dataCod.Rayons, "dataCodeRayons.csv"
    )
    
    sendSweetAlert(
      session = session,
      title = "Le fichier a bien été sauvegarder !",
      text = "Les informations sont disponibles",
      type = "success"
    )
   }
  )
  
  observeEvent(input$saveFBtn, {
    
    dataFamilles <- read.csv2(input$dataFamilles$datapath)
    dataFamilles <- dataFamilles[,c(-4:-14)]
    dataFamilles <- dataFamilles[,-3]
    dataFamilles <- dataFamilles[,c('Code', 'Désignation')]
    colnames(dataFamilles) <- c("Famille", "Désignation.Famille")
    write.csv2(dataFamilles, "dataFamilles.csv")
    
    sendSweetAlert(
      session = session,
      title = "Le fichier a bien été sauvegarder !",
      text = "Les informations sont disponibles",
      type = "success"
    )
   }
  )
}
