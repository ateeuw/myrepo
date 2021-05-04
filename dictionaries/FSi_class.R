# dictionary grouping food security indicators into the categories: 
# availability, accessibility, utilisation, stability, & unclear


FSi_class <- list()

FSi_class[["availability"]] <- c("number of (active/passive) farms", "area used for X", "supply of water for irrigation", "area irrigated", "production of X", "productivity of X", 
                                   "yield of X", "yield gap of X", "availability of X", "area left fallow/unused", "supply of X", "consumption of agricultural water", "fertiliser use for X",
                                 "production of fertiliser", "production of X for self-consumption")

FSi_class[["access - general"]] <- c("land use inequality")

FSi_class[["access - economic"]] <- c("profitability of land", "profitability of water use", "profitability of labour", "farm profit", "marginal cost of producing X", "marginal cost of shipping X",
                             "farm gate/producer price of X", "price of X", "import price of X", "price of irrigation water", "added value of X", 
                             "contribution of X to GDP", "income/wage/salary", "discretionary income", "income from X", "income from agriculture", "income inequality", "poverty incidence/rate",
                             "poverty severity", "poverty depth/gap", "capital assets", "demand for X", "demand for farm land", "production costs",
                             "dietary income differential", "economic benefit/increase in output value of additional production of X", "inflation", "poverty rate/incidence/headcount ratio", 
                             "sales of X", "selling time of X", "unemployed", "demand for agrochemicals", "demand for capital", "demand for farm labour", "demand for agricultural water", 
                             "price of agrochemicals", "price of labour", "price of land", "price of fertilisers", "profitability of livestock", "return to investments (in agriculture)")

FSi_class[["access - physical"]] <- c("import of X", "export of X", "people walking to store as a last resort")

FSi_class[["utilisation"]] <- c("consumption of X", "Healthy Eating Index", "share of X in diet", "quality adjusted life years", "dietary income inequality", "food losses of X", "purchase of X")

FSi_class[["stability"]] <- c("resilience to water scarcity", "water security", "food shortage/deficit", "stocks/surplus of X", "incidence of higher average income with higher variance (%)",
                                "incidence of higher average income with lower variance (%)", "incidence of identical average income and variance (%)", 
                                "incidence of ïdentical average income with lower variance (%)", "incidence of lower average income with higher variance", 
                                "incidence of lower average income with lower variance [%]", "variance in income", "self-sufficiency of X", "food security risk", 
                                "incidence of identical average income with lower variance (%)", "incidence of lower income with higher variance")

FSi_class[["unclear"]] <- "unclear"
