#
library(shiny)
library(rdrop2)

# Define server logic
shinyServer(function(input, output) {
    
    # load dropbox credentials
    token = drop_auth()
    
    saveRDS(token, file = "token.rds")
    
    epl_table <- drop_read_csv("epl_table.csv")
    epl_table = epl_table[,-c(1)]
    
    owner_table <- drop_read_csv("owner_table.csv")
    owner_table = owner_table[,-c(1)]
    
    library(DT)
    output$epl_table = DT::renderDataTable({datatable(epl_table, rownames = F, options = list(
        pageLength = 20
    ))})
    
    output$owner_table = DT::renderDataTable({datatable(owner_table, rownames=F)})
    
})
