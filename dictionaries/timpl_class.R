# dictionary grouping target implementers of governance measures into the categories: 
# governmental entities, food producing entities, unclear, knowledge broker entities, trade entities, non-food manufacturing/processing entities, food consuming entities, citizen entities, 
# food retail entities, knowledge producing entities, banks/credit suppliers, & non-food retail entities

timpl_class <- list()

timpl_class[["governmental entities"]] <- "government"

timpl_class[["food producing entities"]] <- c("farmer", "farming household", "farm", "agricultural entity", "producer", "agro-pastoralist", "pastoralist", "farmer population", "producer household")

timpl_class[["generic, overarching entities"]] <- c("household", "urban household", "labourer", "investor", "foreign sector", "rural household")

timpl_class[["unclear"]] <- c("unclear")

timpl_class[["knowledge broker entities"]] <- c("agricultural extension services", "educators")

timpl_class[["trade entities"]] <- c("trade partnership", "farmer cooperative")

timpl_class[["non-food manufacturing/processing entities"]] <- c("agricultural machinery manufacturing industry", "fertiliser industry", "water supplier", "industrial entity")

timpl_class[["food distribution/storage entities"]] <- c("storing agent", "distribution centre")

timpl_class[["food consuming entities"]] <- c("consumer", "resident/consumer", "consumer household")

timpl_class[["citizen entities"]] <- c("citizen", "volunteers", "women", "local community", "population", "residential entity", "villager", "taxpayer", "village", "volunteer")

timpl_class[["food retail entities"]] <- c("retailer", "stores", "bakeries", "restaurants", "food vendor/store", "store", "restaurant", "shops")

timpl_class[["knowledge producing entities"]] <- c("agricultural researchers", "meteorological agency")

timpl_class[["banks/credit suppliers"]] <- c("credit suppliers")

timpl_class[["non-food retail entities"]] <- c("input supplier", "water manager")

timpl_class[["non-food labourers"]] <- c("construction workers")

timpl_class[["non-agricultural land users"]] <- c("plantation forest owners", "land manager", "ecological entity")

timpl_class[["transport entities"]] <- c("transporters", "public transport vehicle", "transportation vehicle")

timpl_class[["none"]] <- c("none")