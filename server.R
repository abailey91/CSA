
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(RODBC)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
#library(mediacomR)


#source("adx_connect.R",local = T)

#advertisers <- querySQLAdvertisers()
#date_bounds <- querySQLDates() 

cpt_benchmarks <- read.csv("CPT_Benchmarks2.csv",sep=",",header=T,stringsAsFactors = F)

inflation_benchmarks <- read.csv("inflation.csv",sep=",",header=T,stringsAsFactors = F)

regional_universes <- read.csv("regional_universes.csv",sep=",",header=T,stringsAsFactors = F)

test_data <- read.csv("addy_test.csv",sep = ",",header=T,stringsAsFactors = F)
colnames(test_data)[3] <- "Media Channel"

test_data$DATE <- as.Date(test_data$DATE)
test_data$week_start <- as.Date(test_data$week_start)
test_data$month_start <- as.Date(test_data$month_start)
test_data$Year <- as.Date(paste0(test_data$Year,"-01-01"))


shinyServer(function(input, output) {

    output$select_advertiser <- renderUI({
     # textInput("select_advertiser",label = "Advertiser",placeholder = "Enter an advertiser name")
      selectizeInput("select_advertiser",label = "Advertisers",multiple=T,choices = advertiser_names(),options=list(maxOptions=50))
    })
   
   #output$select_advertiser <- renderUI({
     output$select_brands <- renderUI({
       
       selectizeInput("select_brands",label = "Brands",multiple=T,choices = brand_names(),options=list(maxOptions=50)) 
       
     })
     
    # selectizeInput("select_advertiser",label = "Advertiser",choices = advertiser_names())
     
  # })

   advertiser_names <- reactive({
    # need(input$select_advertiser)
     c("IKEA","DFS","IKEASGFD")
     #return(querySQLAdvertisers("ikea"))
   })
   
   brand_names <- reactive({
    # need(input$select_advertiser)
     c("IKEA","DFS","IKEASGFD")
     
     #####add code to return unqiue brands based on a list of selected advertisers#####
     #return(querySQLAdvertisers("ikea"))
   })
   
   output$date_select <- renderUI({
     dateRangeInput("date_select",label="Pick a date range",start=date_bounds[1,1],end = date_bounds[1,2],min=date_bounds[1,1],max = date_bounds[1,2])
   })
  # 
   
   
   
   live_addy_df <- eventReactive(input$btn_getaddy,{
     
     #test_data <<-  querySQLDateAdvertiser(input$select_advertiser,input$date_select[1],input$date_select[2]) 
    
     
     #df <- test_data %>% 
    #   group_by(DATE,Advertiser,`Media Channel`,Region) %>%
    #   summarise(Expenditure=sum(Expenditure)) 
     
     #calculate date aggregators
    # df$Week_id <- lubridate::isoweek(df$DATE)
     #df$Month_id <- lubridate::month(df$DATE)
     #df$Year <- lubridate::year(df$DATE)
     
     #df$week_start <- as.Date(paste(isoyear(df$DATE), isoweek(df$DATE), 1, sep="-"), "%Y-%U-%u")
     #df$month_start <- as.Date(cut(df$DATE,"month"))
     #df$month_name <- months(as.Date(cut(test_df$DATE[1],"month")))
     
     #add in blanks
     
     df <- test_data
     all_dates <- data.frame(seq(min(df$DATE),max(df$DATE),by="days"))  
     
     df <<- df %>%
       spread(key = `Media Channel`,value = Expenditure,fill = 0) %>%
       gather(key="Media Channel",value="Expenditure",-(1:8))
     
     
     
     if(input$select_frequency=="Weekly"){
        df %>% 
         group_by(Advertiser,week_start,`Media Channel`,Region) %>%
         summarise(Expenditure=sum(Expdenditure))
     }else if(input$select_frequency=="Monthly"){
       
       df %>% 
         group_by(Advertiser,month_start,`Media Channel`,Region) %>%
         summarise(Expenditure=sum(Expdenditure))
       
     }else if(input$select_frequency=="Annual"){
       
       df %>% 
         group_by(Advertiser,Year,`Media Channel`,Region) %>%
         summarise(Expenditure=sum(Expdenditure))
       
     }else
     {
       #return daily
       df
     }

   })

   
   output$tbl_addy <- renderDataTable({
     
     df <- live_addy_df() %>%
       spread(key=`Media Channel`,value=Expenditure,fill=0)
     
     datatable(df,options = list(scrollX=T)) %>%
       formatCurrency(columns = 7:(ncol(df)),currency = "£")
   })

   
   
   output$plot_spend <- renderPlot({
     
     df <-  summarise(group_by(live_addy_df(),DATE,week_start,month_start,Year,`Media Channel`),Expenditure=sum(Expenditure)) %>%
       #arrange(`Media Channel`,DATE)#%>%
       spread(key=`Media Channel`,value=Expenditure,fill=0) 
       
     #add in any missing dates
     df <- merge(df,data.frame(DATE=seq(min(df$DATE),max(df$DATE),by="day")),by="DATE",all=T)
     
     #recalc week month year identifiers
     df$Year <- as.Date(paste(lubridate::year(df$DATE),"01","01",sep="-"))
     df$week_start <- floor_date(df$DATE,unit="week")#as.Date(paste(isoyear(df$DATE), isoweek(df$DATE), 1, sep="-"), "%Y-%W-%u")
     #df$week_start <- as.Date(strptime(paste0(df$Year, "Monday", isoweek(df$DATE)), "%Y%A%V"))
     df$month_start <- as.Date(cut(df$DATE,"month"))
     #df$month_name <- months(as.Date(cut(df$DATE[1],"month")))
     
     df[is.na(df)] <- 0
     df <- gather(df,key="Media Channel",value = "Expenditure",-(1:4)) %>%
       dplyr::filter(DATE >=input$date_select[1],DATE <= input$date_select[2]) %>%
       arrange(DATE,week_start,month_start,Year,`Media Channel`)
     
     
     
     
     if(input$select_frequency=="Weekly"){
     
       df <- summarise(group_by(df,week_start,`Media Channel`),Expenditure=sum(Expenditure))
       
       ggplot(df,aes(x=week_start,y=Expenditure)) + geom_area(aes(colour = `Media Channel`, fill= `Media Channel`),stat="identity", position = 'stack') +
         scale_x_date(date_breaks="3 months") +
         scale_y_continuous(labels=scales::dollar_format(prefix = "£")) +
         xlab("Date")+
         ylab("Expenditure")+
         mediacomR::mcom_ggplot(title="Weekly Media Spend",xtitle = "Date",ytitle = "Expenditure (£)",source = "Nielsen Addynamix")
       
     }else if(input$select_frequency=="Monthly"){
       
       df <- summarise(group_by(df,month_start,`Media Channel`),Expenditure=sum(Expenditure))
       
       
       ggplot(df,aes(x=month_start,y=Expenditure)) + geom_area(aes(colour = `Media Channel`, fill= `Media Channel`),stat="identity", position = 'stack') +
         scale_x_date(date_breaks="3 months") +
         scale_y_continuous(labels=scales::dollar_format(prefix = "£"))+
         xlab("Date")+
         ylab("Expenditure")+
         mediacomR::mcom_ggplot(title="Monthly Media Spend",xtitle = "Date",ytitle = "Expenditure (£)",source = "Nielsen Addynamix")
       
     }else if(input$select_frequency=="Annual"){
       
       df <- summarise(group_by(df,Year,`Media Channel`),Expenditure=sum(Expenditure))
       
       
       ggplot(df,aes(x=Year,y=Expenditure)) + geom_bar(aes(colour = `Media Channel`, fill= `Media Channel`),stat="identity", position = 'stack') +
         scale_x_date(date_breaks="1 year") +
         scale_y_continuous(labels=scales::dollar_format(prefix = "£"))+
         xlab("Date")+
         ylab("Expenditure")+
         mediacomR::mcom_ggplot(title="Annual Media Spend",xtitle = "Date",ytitle = "Expenditure (£)",source = "Nielsen Addynamix")
       
     }else
     {
     
       #return daily
       ggplot(df,aes(x=DATE,y=Expenditure)) + 
         geom_area(aes(colour = `Media Channel`, fill= `Media Channel`),stat="identity", position = 'stack')   +
         scale_x_date(date_breaks="3 months") +
         scale_y_continuous(labels=scales::dollar_format(prefix = "£"))+
         xlab("Date")+
         ylab("Expenditure") + 
         mediacomR::mcom_ggplot(title="Daily Media Spend",xtitle = "Date",ytitle = "Expenditure (£)",source = "Nielsen Addynamix") 
     }
     
     #ggplotly(p,width="100%")
     
   })
  
   output$select_region <- renderUI({
     selectInput("select_region",label="Region",choices=unique(live_addy_df()$Region))
   })
   
   #rhandsontable to handle editing of CPT values by month
   output$rhotbl_cpts <- renderRHandsontable({
     
     df <- cpt_benchmarks %>%
       filter(Region==input$select_region) %>%
       mutate(Month = factor(Month,levels=unique(Month))) %>% 
       spread(key="Month",value="CPT")
     
     df <- cpt_benchmarks %>%
       #filter(Region==input$select_region) %>%
       mutate(Month = factor(Month,levels=unique(Month))) %>% 
       spread(key="Month",value="CPT")
     
     rhandsontable(df,selectCallback = T) %>%
       rhandsontable::hot_table(highlightCol = T,highlightRow = T)
   })
   
   
   #add code to edit the HOT and store the updates for the CPTs
   editable_cpts <- reactiveValues()
   
   observe({
     if(!is.null(input$rhotbl_cpts)){
       editable_cpts[["previous"]] <- isolate(editable_cpts[["DF"]])
       DF <- hot_to_r(input$rhotbl_cpts)
     }else{
       if(is.null(editable_cpts[["DF"]]))
       {}
     }
   })
   
   #live data in hands on table
   live_cpts <- reactive({
     hot_to_r(input$rhotbl_cpts) %>%
       gather(key=Month,value=CPT,-Channel,-Region)
   })
   
   
   #inflation table
   output$rhotbl_inflation <- renderRHandsontable({
     df <- inflation_benchmarks %>%
       spread(key="Year",value="Inflation")
     
     rhandsontable(df)
   })
   
   output$select_inf_channel <- renderUI({
     selectInput("select_inf_channel",label="Select a channel",choices = unique(live_addy_df()$`Media Channel`))
   })
   
   
   #inflation plot
   output$plot_inflation <- renderPlot({
     
     df <- hot_to_r(input$rhotbl_inflation) %>%
       gather(key=Year,value=Inflation,-Channel) %>%
       filter(Channel==input$select_inf_channel)
     
     ggplot(df,aes(x=Year,y=Inflation)) + 
       geom_bar(aes(colour = Channel, fill= Channel),stat="identity")   +
       mediacomR::mcom_ggplot(title="Media Cost Inflation",xtitle = "Year",ytitle = "Inflation",source = "MediaCom")
   })
   
   
   #live calculates the number of impacts and GRPs 
   live_ratings <- reactive({

     df <- left_join(live_addy_df() ,live_cpts(),by=c("Region"="Region","Media Channel"="Channel"))

     df <- left_join(df,regional_universes,by=c("Region"="Region"))
     
     national_universe <- regional_universes[regional_universes$Region=="National","Universe"]
     
     df$Impacts <- df$Expenditure/df$CPT * 1000
     df$Nat_Ratings <- df$Impacts/national_universe * 100
     df$Regional_Ratings <- df$Impacts/df$Universe * 100
     
    
     df %>%
       filter(DATE>=input$date_select_reach[1],DATE<=input$date_select_reach[2]) %>%
       filter(`Media Channel` %in% input$select_channels, Region %in% input$select_regions)
   })
   
   
   #outputreadyformat
   ratings_output_ready <- reactive({
     
     #break out useful columns
          df <- live_ratings()[,c("DATE","Media Channel","Region","CPT","Expenditure","Impacts","Regional_Ratings","Nat_Ratings","Universe")]

     #add universe size
     df$Nat_Universe <- regional_universes[regional_universes$Region=="National","Universe"]
     
     #change column names
     colnames(df) <- c("Date","Media Channel","Region","CPT","Expenditure","Impacts","Regional Ratings","National Ratings","Regional Universe","National Universe")
     
     df
   })
   
   #download results
   output$download_results <- downloadHandler(
     filename = function(){
       paste0("CSA_",Sys.Date(),".csv")
     },
     content = function(con){
       write.csv(ratings_output_ready(),con,row.names=F)
     }
   )
   
   #reach table
   output$tbl_reach <- renderDataTable({
     
     datatable(ratings_output_ready(),options = list(scrollX=T))
     
   })

   
   #date selector for ratings page
   output$date_select_reach <- renderUI({
     dateRangeInput("date_select_reach",label = "Date Range",start = min(live_addy_df()$DATE),end = max(live_addy_df()$DATE))
   })
 
   output$select_channels <- renderUI({
     selectizeInput("select_channels",label="Channels",choices=unique(live_addy_df()$`Media Channel`),selected=unique(live_addy_df()$`Media Channel`),multiple=T)
   })
   
   
   output$select_regions <- renderUI({
     selectizeInput("select_regions",label="Regions",choices=unique(live_addy_df()$Region),selected=unique(live_addy_df()$Region),multiple=T)
   })
   
   
   
   output$plot_ratings <- renderPlot({
     
     
     df <- summarise(group_by(ratings_output_ready(),Date,`Media Channel`),`National Ratings`= sum(`National Ratings`))
     
     
     ggplot(df,aes(x=Date,y=`National Ratings`)) + geom_area(aes(colour = `Media Channel`, fill= `Media Channel`),stat="identity", position = 'stack') +
       scale_x_date(date_breaks="3 months") +
       scale_y_continuous(labels=scales::dollar_format(prefix = "£"))+
       xlab("Date")+
       ylab("Expenditure")+
       mediacomR::mcom_ggplot(title="Monthly Media Spend",xtitle = "Date",ytitle = "Expenditure (£)",source = "Nielsen Addynamix")
     
   })
   
})


#test2 <- test_df %>% spread(key=`Media Channel`,value=Expenditure,fill=0) %>% gather(key="Channel",value="Expenditure",-DATE,-week_start,-Advertiser)
