library(RODBC)

# 
# # function to connect to the tables stored in BS_BusinessScience
# # connectSQL <- function(username, password){
# #   odbcDriverConnect(paste0("driver={SQL Server Native Client 11.0};server=10.2.186.143\\SQLINS02,6002;
# #                            database=BS_BusinessScience;Uid=", username, "; Pwd=", password))
# # }


# connectSQL <- function(username, password){
#   odbcDriverConnect(paste0("driver={SQL Server};server=PSCSQLP00121\\SQLINS02,6002;
#                            database=BS_BusinessScience;trusted_connection=true"))
# }




querySQL <- function(query){
  
  df <- sqlQuery(myconn, query,stringsAsFactors=F)
  
  # close connection
  close(myconn)
  
  df
}

#df <- sqlQuery(myconn, "SELECT TOP 10 * FROM [dbo].[ADX_Daily_by_Product_Category_Reweighted]")

#gets advertiser data between a date range
querySQLDateAdvertiser <- function(advertisers,date_min,date_max){
  myconn <- connectSQL("BS_Shared", "marshallsmill7") 
  
  df <- sqlQuery(myconn, 
                 paste0("SELECT * 
                        FROM [dbo].[ADX_Daily_by_Product_Category_Reweighted]
                        WHERE [Advertiser] IN ('", paste(advertisers, collapse = "','"), "') 
                        AND [DATE] >='",paste0(date_min,"'"),
                        "AND [DATE] <='",paste0(date_max,"'")),
                 stringsAsFactors = F)
  
  # close connection
  close(myconn)
  
  df
}

#myconn <- connectSQL("BS_Shared", "marshallsmill7") 
#df <- sqlQuery(myconn, "SELECT * FROM [dbo].[Post]")



#gets list of available advertisers
querySQLAdvertisers <- function(search_text){
  myconn <- connectSQL("BS_Shared", "marshallsmill7") 
  
  df <- sqlQuery(myconn, 
                 paste0("SELECT DISTINCT [Advertiser] 
                        FROM [dbo].[ADX_Daily_by_Product_Category_Reweighted] WHERE [Advertiser] LIKE '%",search_text,"%'"),
                 stringsAsFactors = F)
  
  # close connection
  close(myconn)
  
  df
} 

#gets list of available advertisers
querySQLDates <- function(){
  myconn <- connectSQL("BS_Shared", "marshallsmill7") 
  
  min <- sqlQuery(myconn, 
                 paste0("SELECT min([DATE])
                        FROM [dbo].[ADX_Daily_by_Product_Category_Reweighted]"),
                 stringsAsFactors = F)
  
  max <- sqlQuery(myconn, 
                  paste0("SELECT max([DATE])
                         FROM [dbo].[ADX_Daily_by_Product_Category_Reweighted]"),
                  stringsAsFactors = F)
  
  # close connection
  close(myconn)
  
  data.frame(Min=min,Max=max)
}
