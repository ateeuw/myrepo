# Libraries for the literature review

# Countries x extents
countries <- list()
countries["Australia"] <- 7682300000000
countries["Ghana"] <- 238535000000
countries["Mozambique"] <- 801590000000
countries["Philippines"] <- 300000000000
countries["Niger"]
countries["Brazil"] <- 8516000000000
countries["USA"] <- 9834000000000
countries[["earth"]] <- 510100000000000
countries[["Indonesia"]] <- 1905000000000

# Regions
regions[""]

# Extent and resolution
time <- list()
time["1 month"] <- 30
time["3 months"] <- 91 #also one season, i.e. spring/summer/autumn/winter
time["5 months"] <- 152
time["56 weeks"] <- 56*7
time["2 years"] <- 730
time["5 years"] <- 1826
time["6 years"] <- 2191
time["7 years"] <- 2557
time["8 years"] <- 2922
time["9 years"] <- 3287
time["10 years"] <- 3652
time["11 years"] <- 4018
time["12 years"] <- 4383
time["14 years"] <- 5113
time["15 years"] <- 5479
time["18 years"] <- 6574
time[["21 years"]] <- 7670
time["25 years"] <- 9313
time["30 years"] <- 10957
time[["36 years"]] <- 13149
time["50 years"] <- 18262
time[["64 years"]] <- 23376
time["85 years"] <- 31046
time[["200 years"]] <- 73048

space <- list()
space["3.63 km2"] <- 3.63*1000000                                 #3630000               6
space["1000 ha"] <- 1000*10000                                    #10000000              7
space[["1975.7 km2"]] <- 1975.7*1000000                           #1975700000            7
space["3703 km2"] <- sprintf("%.1f", 3703*1000000)                #3703000000            9
space["461.786 ha"] <- 461786*10000                               #4617860000            9
space["680,000 ha"] <- 680000*10000                               #6800000000            9
space["14000 km2"] <- sprintf("%.1f", 14000*1000000)              #14000000000          10
space[["23,000 km2"]] <- sprintf("%.1f", 23000*1000000)           #23000000000          10 
space["46,717.48 km2"] <- sprintf("%.1f", 46717.48*1000000)       #46717480000          10
space["91.646 km?"] <- sprintf("%.1f", 91646*1000000)             #91646000000          10
space["238.535 km?"] <- sprintf("%.1f",238535*1000000)            #238535000000         11
space["268303 km2"]  <- sprintf("%.1f", 268303*1000000)           #268303000000         11
space["300000 km2"] <- sprintf("%.1f", 300000*1000000)            #300000000000         11
space["4.73 * 10^7 hm2"] <- sprintf("%.1f",4.73*10^7*10000)       #473000000000         11
space[["537,023 sq km"]] <- sprintf("%.1f", 537023*1000000)       #537023000000         11
space["881,913 km2"] <- sprintf("%.1f", 881913*1000000)           #881913000000         11
space["1 100 000 km2"] <- sprintf("%.1f", 1100000*1000000)        #1100000000000        12
space["1.267.000 km?"] <- sprintf("%.1f", 1267000*1000000)        #1267000000000        12
space[["1.905.000 km?"]] <- sprintf("%.1f", 1905000*1000000)      #1905000000000        12
space["7,682,300 km2"] <- sprintf("%.1f", 7682300*1000000)        #7682300000000        12 
space["8.516.000 km?"] <- sprintf("%.1f", 8516000*1000000)        #8516000000000        12
space["9,833,520 km2"] <- sprintf("%.1f", 9833520*1000000)        #9833520000000        12
space["9.834.000 km?"] <- sprintf("%.1f", 9834000*1000000)        #9834000000000        12
space[["510.100.000 km?"]] <- sprintf("%.1f", 510100000*1000000)  #510100000000000      14 
space["87,311 km2"] <- sprintf("%.1f", 87311*1000000)
space["250 m x 250 m"] <- sprintf("%.1f", 250*250)
space["9,596,961 km2"] <- sprintf("%.1f", 9596961*1000000)
space["148 326 000 km2"] <- sprintf("%.1f", 148326000*1000000) #terrestrial land earth
space["1.143.000 km?"] <- sprintf("%.1f", 1143000*1000000) 
space["801.590 km?"] <- sprintf("%.1f", 801590*1000000) 
# many US states:
91646+141300+22608+119283+24923+3144+24217+6446+295254+423970+423970+170312+153909+82931+153909+82931+139390+32133+110786
space["2503062 km2"] <- sprintf("%.1f", 2503062*1000000) 
# many US states + mexico & canada
91646+141300+22608+119283+24923+3144+24217+6446+295254+423970+423970+170312+153909+82931+153909+82931+139390+32133+110786+1973000+9985000
space["14461062 km2"] <- sprintf("%.1f", 14461062*1000000) 
space["300.000 km?"] <- sprintf("%.1f", 300000*1000000) 
space["801.590 km?"] <- sprintf("%.1f", 801590*1000000)

# 2503062000000 + 14461062000000
# 300000000000 + 148326000000000

# Software
software <- list()
software["TreeAge Pro"] <- "https://www.treeage.com/"

# Model types
models <- list("equilibrium","systems dynamics", "agent-based", "micro-simulation", "optimization", "statistical/econometric", "cellular automata", "discrete-event")
names(models) <- c("equilibrium","systems dynamics", "agent-based", "optimization", "statistical/econometric", "cellular automata", "discrete-event")
models[["agent-based"]] <- list(m_types = c("agent-based"), fill = "#E41A1C")
models[["micro-simulation"]] <- list(m_types = c("micro-simulation"), fill = "firebrick1")
models[["cellular automata"]] <- list(m_types = c("cellular automata", "cellular automata-markov"), fill = "#377EB8")
models[["equilibrium"]] <- list(m_types = c("equilibrium", "general-equilibrium","partial-equilibrium", "supply-demand", 
                                            "recursively dynamic general equilibrium", "mixed demand", "dynamic general-equilibrium"), fill = "#4DAF4A")
models[["machine learning"]] <- list(m_types = c("machine learning", "neural-network", "bayesian networks"), fill = "#A65628")
models[["optimization"]] <- list(m_types = c("optimization", "budget allocation", "Genetic Algorithm"), fill = "#984EA3")
models[["statistical/econometric"]] <- list(m_types = c("statistical", "econometric", "simultaneous equations", "social accounting matrix", "grey box", "vector autoregression", "co-integration analysis", 
                                                        "positive mathematical programming", "tradeoff analysis model for multi-dimensional impact assessment", "multistate life table", 
                                                        "spatial regression", "monte-carlo", "generalized method of moments", "multinomial logit", "stochastic frontier",
                                                        "probit regression", "conjoint analysis", "structural demand model"), fill = "#FF7F00")
models[["systems dynamics"]] <- list(m_types = c("systems dynamics", "earth systems", "fuzzy cognitive map", "topical modelling-systems dynamics",
                                                 "stock-flow-feedback", "stochastic systems dynamics"), fill = "#FFFF33")
models[["discrete-event"]] <- list(m_types = c("discrete-event"), fill = "gold")


# Model subtypes
subtypes <- list()
subtypes["Markov model"] <- "statistical"

# Data types
data_types <- list()
data_types[["dis-aggregated statistics"]] <- "census/survey/panel"
data_types[["spatial statistics"]] <- c("spatial_points", "spatial_zones", "spatial_gridded")
data_types[["aggregated statistics"]] <- c("literature", "aggregated_statistics", "social_accounting_matrix")
data_types[["field work"]] <- c("expert_interviews", "stakeholder_interviews", "observational_studies")

# agent types
agent_types <- list()
agent_types[["consumer-type agents"]] <- c("consumer", "resident", "resident/consumer", "consumer household", "residential entity")
agent_types[["retailer-type agents"]] <- c("retailer", "food vendor/store", "store")
agent_types[["distribution-type agents"]] <- "distribution centre"
agent_types[["farmer-type agents"]] <- c("farmer", "farm", "agricultural entity", "producer", "land manager", "farmer household", "agro-pastoralist", "pastoralist")
agent_types[["other type of agents"]] <- c("government", "ecological entity", "water supplier", "water manager", "industrial entity", "public transport vehicle")
agent_types[["overarching agent types"]] <- c("household", "labourer", "population")
agent_types[["not applicable"]] <- c("not applicable")

# goals
goals_class <- list()
goals_class[["availability"]] <- c("increase agricultural productivity",
                             "supply water for irrigation",
                             "increase grain production",
                             "maximize food production",
                             "reduce food waste",
                             "increase agricultural growth rate",
                             "reclaim degraded agricultural land",
                             "maintain land in good agricultural condition",
                             "prevent farm exits",
                             "expand area under irrigation",
                             "increase domestic availability of X",
                             "increase production of X")
goals_class[["accessibility"]] <- c("improve (healthy) food access",
                       "poverty reduction",
                       "reduce income inequality",
                       "reduce food prices",
                       "reduce rice prices",
                       "reduce production costs",
                       "entertain rural livelihoods",
                       "diversify farm incomes",
                       "improve farmer livelihoods",
                       "increase farm incomes",
                       "reduce domestic prices of X")
goals_class[["utilisation"]] <- c("increase consumption of fruits and vegetables",
                            "improve diets (more healthy)",
                            "stimulate consumption")
goals_class[["stability"]] <- c("maintain food self-sufficiency",
                          "obtain rice self-sufficiency",
                          "reduce food shortages",
                          "increase agricultural resilience during droughts",
                          "help farmers adapt to climate change",
                          "increase food consumption during drought",
                          "increase food self-sufficiency during droughts",
                          "stabilise domestic food prices",
                          "increase food consumption during economic shocks",
                          "obtain/maintain food self-sufficiency of X")
goals_class[["environmental"]] <- c("maximise delivery of ecosystem services",
                              "digest organic wastes",
                              "regulate pollution",
                              "sustain ecosystems",
                              "maintain wetlands",
                              "recharge aquifers",
                              "store run-off water",
                              "reduce soil erosion",
                              "improve soil fertility",
                              "maintain land in good environmental condition",
                              "reduce greenhouse gas emissions")
goals_class[["economic"]] <- c("economic growth",
                         "improve market",
                         "increase profitability of businesses",
                         "reduce cost of governance measure",
                         "increase regional income",
                         "equitable rural development",
                         "ensure stability of national export revenues",
                         "increase economic efficiency",
                         "increase household expenditure",
                         "increase public revenues")
goals_class[["logistics"]] <- c("increase supply chain efficiency")
goals_class[["infrastructure & technology"]] <- c("speed up technology diffusion",
                                                  "finance infrastructure")
goals_class[["health & wellbeing"]] <- c("improve public health",
                                   "improve living standards",
                                   "improve welfare of low-income households")
goals_class[["other"]] <- c("reduce population growth",
                            "stimulate employment of less skilled manpower")
measure_class <- list()
measure_class[["awareness raising, education, promotion & training"]] <- c("promotion of improved seed",
                                                                           "improving willingness to walk",
                                                                           "perfect information communication through agricultural extension services",
                                                                           "awareness campaign on water conservation",
                                                                           "improving female workers' education levels",
                                                                           "promotion of improved seed",
                                                                           "promotion of agricultural machinery & equipment",
                                                                           "training and scholarship scheme",
                                                                           "development of farm schools",
                                                                           "education & training")
measure_class[["store policies"]] <- c("creating new stores",
                                       "increase availability of healthy foods in stores",
                                       "subsidies on farm inputs")

measure_class[["infrastructure projects"]] <- c("improving public transport",
                                                "construction of hydro-electric dams",
                                                "sewage treatment")

measure_class[["affordability & pricing policies"]] <- c("cheaper healthy food/relative pricing",
                                                         "food stamps with incentives for fruit and vegetable",
                                                         "cheaper food products that will soon expire",
                                                         "cash transfers to low-income households")

measure_class[["credit policies"]] <- c("credit provision for agricultural production-related expenses",
                                        "adjusted interest rates",
                                        "increased interest rates",
                                        "reduced interest rates",
                                        "credit programme")

measure_class[["targeted subsidies"]] <- c("subsidies on fertilizer",
                                           "elimination of irrigation subsidies",
                                           "subsidies for establishment of perennials",
                                           "subsidies for primary processing and marketing activities",
                                           "subsidies for productive infrastructure",
                                           "subsidies for pure breed livestock acquisition",
                                           "subsidies on agricultural labour",
                                           "subsidies on land rent",
                                           "subsidies on machinery and equipment",
                                           "subsidies on irrigation and drainage projects",
                                           "subsidies on farm inputs")

measure_class[["international trade"]] <- c("trade liberalisation",
                                            "trade deliberalisation",
                                            "accession into trade partnership")

measure_class[["other"]] <- c("relocation of water-demanding crops",
                              "rice self-sufficiency program")

measure_class[["targeted investment"]] <- c("(increased) investment in agricultural sector",
                                            "increased investment general",
                                            "(increased) investment in agricultural research",
                                            "(increased) investment in extension services",
                                            "(increased) investment in agricultural technology development",
                                            "investment in agricultural machinery & equipment",
                                            "(increased) investment in farm inputs",
                                            "(increased) investment in irrigation",
                                            "(increased) investment in infrastructure")

measure_class[["unclear"]] <- "unclear"

measure_class[["tariffication"]] <- c("import tariffs on rice",
                                      "reduce import tariffs on fertilizer",
                                      "abolish quantitative rice import quotas",
                                      "reduced import tariffs on rice")

measure_class[["direct support for food producers"]] <- c("minimum support price for rice",
                                                          "minimum support price for milk")

measure_class[["nature-centered measures"]] <- c("soil erosion control",
                                                 "afforestation",
                                                 "disaster control")

# commodities
comm_classes <- list()
comm_classes[["cereals & cerealproducts"]] <- c("rice", "maize", "wheat", "sorghum", "millet", "grain")
comm_classes[["fruits and derived products"]] <- c("fruit")
comm_classes[["vegetables and derived products"]] <- c("vegetable")
comm_classes[["sugar crops and sweeteners, and derived products"]] <- c("sugar")
comm_classes[["fibres of vegetal and animal origin"]] <- c("cotton")
comm_classes[["not applicable"]] <- c("food", "trees")
comm_classes[["products from live animals"]] <- c("milk", "dairy")
comm_classes[["oil-bearing crops and derived products"]] <- c("groundnut")
comm_classes[["products from slaughtered animals"]] <- c("meat", "fish")
comm_classes[["livestock"]] <- c("cattle", "goat")
comm_classes[["stimulant crops and derived products"]] <- c("tobacco", "coffee")