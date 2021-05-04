# dictionary grouping target implementers of governance measures into the categories: 
# governmental entities, food producing entities, unclear, knowledge broker entities, trade entities, non-food manufacturing/processing entities, food consuming entities, citizen entities, 
# food retail entities, knowledge producing entities, banks/credit suppliers, & non-food retail entities

timpl_class <- list()
timpl_class[["governmental entities"]] <- "government"
timpl_class[["food producing entities"]] <- c("farmer")
timpl_class[["unclear"]] <- c("unclear")
timpl_class[["knowledge broker entities"]] <- c("agricultural extension services", "educators")
timpl_class[["trade entities"]] <- c("trade partnership", "farmer cooperative")
timpl_class[["non-food manufacturing/processing entities"]] <- c("agricultural machinery manufacturing industry", "fertiliser industry")
timpl_class[["food consuming entities"]] <- c("consumer", "resident/consumer")
timpl_class[["citizen entities"]] <- c("citizen", "volunteers", "women", "local community")
timpl_class[["food retail entities"]] <- c("retailer", "stores", "bakeries")
timpl_class[["knowledge producing entities"]] <- c("agricultural researchers")
timpl_class[["banks/credit suppliers"]] <- c("credit suppliers")
timpl_class[["non-food retail entities"]] <- c("input supplier")
timpl_class[["non-food labourers"]] <- c("construction workers")
