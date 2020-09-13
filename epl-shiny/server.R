#
library(shiny)

# Define server logic
shinyServer(function(input, output) {

    # connect to database
    library(RMySQL)
    library(tidyverse)

    db_user = "root"
    db_password = "Nuggetsbball2020"
    db_name = "scrape_test"
    db_table = "epl_table"
    db_host = "127.0.0.1"
    db_port = 3306

    mydb = dbConnect(MySQL(), user = db_user, password = db_password, dbname = db_name, host = db_host, port = db_port)

    # pull epl_table
    epl_table_query = paste("SELECT * FROM epl_table")
    rs = dbSendQuery(mydb, epl_table_query)
    epl_table = dbFetch(rs)
    epl_table$owner = gsub("Neo", "Neo ⭐", epl_table$owner)

    # pull owner table
    owner_table_query = paste("SELECT * FROM owner_table")
    rs = dbSendQuery(mydb, owner_table_query)
    owner_table = dbFetch(rs)
    
    library(DT)
    output$epl_table = DT::renderDataTable({datatable(epl_table, rownames = F, options = list(
        pageLength = 20
    ))})
    
    output$owner_table = DT::renderDataTable({datatable(owner_table, rownames=F)})
    
    output$w_attend = renderPlot({ggplot(all_stats_img, aes(x = x_g, y = w))+
                                 geom_image(aes(image = image), size = 0.05)+
                                 theme_bw()})
    


})
