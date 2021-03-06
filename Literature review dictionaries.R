# Libraries for the literature review

# Countries x extents
countries <- list()
countries[["Algeria"]] <- 2382000000000; countries["Australia"] <- 7682300000000
countries[["Bangladesh"]] <- 148460000000; countries[["Benin"]] <- 114763000000; countries["Brazil"] <- 8516000000000; countries[["Burkina Faso"]] <- 274200000000
countries[["China"]] <- 9597000000000
countries[["Djibouti"]] <- 23200000000 #
countries[["earth"]] <- 510100000000000; countries[["Egypt"]] <- 1010000000000; countries[["Ethiopia"]] <- 1040000000000; countries[["EU-27"]] <- 4476000000000
countries[["France"]] <- 643801000000
countries[["Germany"]] <- 357386000000; countries["Ghana"] <- 238535000000
countries[["Hungary"]] <- 93030000000
countries[["India"]] <- 3287000000000; countries[["Indonesia"]] <- 1905000000000; countries[["Iraq"]] <- 438317000000; countries[["Iran"]] <- 1648000000000 
countries[["Japan"]] <- 377975000000; countries[["Jordan"]] <- 89342000000; 
countries[["Kenya"]] <- 580367000000
countries[["Lebanon"]] <- 10452000000; countries[["Libya"]] <- 1760000000000 #
countries[["Malaysia"]] <- 329847000000; countries[["Malawi"]] <- 118484000000; countries[["Mali"]] <- 1240000000000; countries[["Mauritania"]] <- 1030000000000; countries[["Mexico"]] <- 1973000000000; countries[["Morocco"]] <- 446550000000; countries["Mozambique"] <- 801590000000
countries[["Netherlands"]] <- 41543000000; countries[["Niger"]] <- 1267000000000;countries[["Nigeria"]] <- 923768000000; countries[["Norway"]] <- 385207000000
countries[["Philippines"]] <- 300000000000
countries[["Quatar"]] <- 11571000000
countries[["Somalia"]] <- 637657000000; countries[["South Africa"]] <- 1220000000000; countries[["South Korea"]] <- 100210000000; countries[["Spain"]] <- 505990000000; countries[["Sudan"]] <- 1886000000000; countries[["Switzerland"]] <- 41285000000; countries[["Syria"]] <- 185180000000 #
countries[["Thailand"]] <- 513120000000; countries[["Togo"]] <- 56785000000; countries[["Tunisia"]] <- 163610000000; countries[["Turkey"]] <- 783562000000
countries["USA"] <- 9834000000000
countries[["Vietnam"]] <- 331212000000
countries[["Zimbabwe"]] <- 390757000000

t <- countries[["Japan"]] + countries[["China"]] + countries[["India"]] + countries[["Indonesia"]] + countries[["Bangladesh"]] + countries[["Vietnam"]] + countries[["Thailand"]] + countries[["Philippines"]] + countries[["USA"]] + countries[["Australia"]]
sprintf("%.0f", t) 

# Extent and resolution
time <- list()
time["1 month"] <- 30
time["3 months"] <- 91 #also one season, i.e. spring/summer/autumn/winter
time["5 months"] <- 152
time["56 weeks"] <- 56*7
time["2 years"] <- 730
time[["3 years"]] <- 1096
time[["4 years"]] <- 1461
time["5 years"] <- 1826 #also 60 months
time["6 years"] <- 2191
time["7 years"] <- 2557
time["8 years"] <- 2922
time["9 years"] <- 3287
time["10 years"] <- 3652 #also 120 months
time["11 years"] <- 4018
time["12 years"] <- 4383
time[["13 years"]] <- 4748
time["14 years"] <- 5113
time["15 years"] <- 5479
time["18 years"] <- 6574
time[["20 years"]] <- 7305
time[["21 years"]] <- 7670
time[["23 years"]] <- 8401
time["25 years"] <- 9313
time["30 years"] <- 10957
time[["35 years"]] <- 12783
time[["36 years"]] <- 13149
time[["40 years"]] <- 14610
time["50 years"] <- 18262
time[["55 years"]] <- 20088
time[["60 years"]] <- 21914
time[["64 years"]] <- 23376
time["85 years"] <- 31046
time[["103 years"]] <- 37620
time[["200 years"]] <- 73048


space <- list()
space["100 ha"] <- 100*10000                                      #1000000              6
space["3.63 km2"] <- 3.63*1000000                                 #3630000               6
space["1000 ha"] <- 1000*10000                                    #10000000              7
space[["24 km2"]] <- 24*1000000                                   #24000000              7
space[["69,56 km²"]] <- 69.56*1000000                             #69560000              7
space[["127 km²"]] <- sprintf("%.0f", 127*1000000)                #127000000             8
space[["136 km²"]] <- sprintf("%.0f", 136*1000000)                #136000000             8
space[["141 km2"]] <- sprintf("%.0f", 141*1000000)                #141000000             8
space[["183,19 km²"]] <- sprintf("%.0f", 183.19*1000000)          #183190000             8
space[["19,637 ha"]] <- sprintf("%.0f", 19637*10000)              #196370000             8
space[["23.325 ha"]] <- 23325*10000                               #233250000             8
space[["237 km"]] <- sprintf("%.0f", 237*1000000)                 #237000000             8
space[["458.50 km2"]] <- sprintf("%.0f", 458.50*1000000)          #458500000             8
space[["577,85 km²"]] <- 577.85*1000000                           #577850000             8
space[["667 km²"]] <- sprintf("%.0f", 667*1000000)                #667000000             8
space[["730 km²"]] <- sprintf("%.0f", 730*1000000)                #667000000             8
space[["783,8 km²"]] <- 783.8*1000000                             #783800000             8
space[["1593 km2"]] <- sprintf("%.0f", 1593*1000000)              #1593000000            9
space[["1.602 km²"]] <- sprintf("%.0f", 1602*1000000)             #1602000000
space[["1,707.84 平方千米"]] <- 1707.7*1000000                    #1707700000            9
space[["1975.7 km2"]] <- 1975.7*1000000                           #1975700000            9
space[["3.233 km²"]] <- sprintf("%.0f", 3233*1000000)             #3233000000            9
space[["3,467.89 km2"]] <- sprintf("%.0f", 3467.89*1000000)       #3467890000
space["3703 km2"] <- sprintf("%.1f", 3703*1000000)                #3703000000            9
space[["383,990 ha"]] <- 383990*10000                             #3839900000            9
space[["5.343 km²"]] <- sprintf("%.0f", 5343*1000000)  
space[["7.415 km2"]] <- sprintf("%.0f", 7415*1000000)             #7415000000            9
space[["7714 km2"]] <- sprintf("%.0f", 7714*1000000)              #7714000000            9
space[["8842 km2"]] <- sprintf("%.0f", 8842*1000000)              #8842000000            9
space[["9500 km2"]] <- sprintf("%.0f", 9500*1000000)              #9500000000            9
space["461.786 ha"] <- 461786*10000                               #4617860000            9
space["680,000 ha"] <- 680000*10000                               #6800000000            9
space[["10.452 km²"]] <- sprintf("%.0f", 10452*1000000)           #10452000000          10
space[["11.571 km²"]] <- sprintf("%.0f", 11571*1000000)           #11571000000          10
space[["13.931 km²"]] <- sprintf("%.0f", 13931*1000000)           #13931000000          10
space["14000 km2"] <- sprintf("%.1f", 14000*1000000)              #14000000000          10
space[["23,000 km2"]] <- sprintf("%.1f", 23000*1000000)           #23000000000          10 
space[["23.200 km²"]] <- sprintf("%.1f", 23200*1000000)           #23200000000          10
space[["40,000 km2 "]] <- sprintf("%.0f", 40000*1000000)          #40000000000          10
space[["41.285 km²"]] <- sprintf("%.1f", 41285*1000000)           #41285000000          10 
space[["41.543 km²"]] <- sprintf("%.1f", 41543*1000000)           #41543000000          10 
space["46,717.48 km2"] <- sprintf("%.1f", 46717.48*1000000)       #46717480000          10
space[["56.785 km²"]] <- sprintf("%.0f", 56785*1000000)           #56785000000
space[["89.342 km²"]] <- sprintf("%.0f", 89342*1000000)           #89342000000          10
space["91.646 km?"] <- sprintf("%.1f", 91646*1000000)             #91646000000          10
space[["93.030 km²"]] <- sprintf("%.0f", 93030*1000000)           #93030000000          10
space[["100.210 km²"]] <- sprintf("%.0f", 100210*1000000)         #100210000000
space[["114.763 km²"]] <- sprintf("%.0f", 114763*1000000)         #114763000000
space[["118.484 km²"]] <- sprintf("%.0f", 118484*1000000)         #118484000000
space[["148.460 km²"]] <- sprintf("%.0f",148460*1000000)          #148460000000         11
space[["163.610 km²"]] <- sprintf("%.0f", 163610*1000000)         #163610000000
space[["185.180 km²"]] <- sprintf("%.0f",185180*1000000)          #185180000000         11
space["238.535 km?"] <- sprintf("%.1f",238535*1000000)            #238535000000         11
space["268303 km2"]  <- sprintf("%.1f", 268303*1000000)           #268303000000         11
space[["274.200 km²"]] <- sprintf("%.0f", 274200*1000000)         #274200000000
space["300000 km2"] <- sprintf("%.1f", 300000*1000000)            #300000000000         11
space[["329.847 km²"]] <- sprintf("%.1f", 329847*1000000)         #329847000000         11
space["331.212 km²"] <- sprintf("%.1f", 331212*1000000)           #331212000000         11
space[["357.386 km²"]] <- sprintf("%.0f", 357386*1000000)         #357386000000         11
space[["377.975 km²"]] <- sprintf("%.0f", 377975*1000000)         #377975000000         11
space[["380454.36 km2"]] <- sprintf("%.0f", 380454.36*1000000)    #380454360000         11
space[["385.207 km²"]] <- sprintf("%.1f", 385207*1000000)         #385207000000         11
space[["390.757 km²"]] <- sprintf("%.0f", 390757*1000000)         #390757000000         11
space[["400,000 km2  "]] <- sprintf("%.0f", 400000*1000000)
space[["438.317 km²"]] <- sprintf("%.0f", 438317*1000000)         #438317000000         11
space[["446.550 km²"]] <- sprintf("%.0f", 446550*1000000)         #446550000000         11
space["4.73 * 10^7 hm2"] <- sprintf("%.1f",4.73*10^7*10000)       #473000000000         11
space[["505.990 km²"]] <- sprintf("%.0f", 505990*1000000)         #505990000000         11
space[["513.120 km²"]] <- sprintf("%.0f", 513120*1000000)         #513120000000         11
space[["537,023 sq km"]] <- sprintf("%.1f", 537023*1000000)       #537023000000         11
space[["580.367 km²"]] <- sprintf("%.0f", 580367*1000000)         #580367000000         11
space[["637.657 km²"]] <- sprintf("%.0f", 637657*1000000)         #637657000000         11
space[["643.801 km²"]] <- sprintf("%.0f", 643801*1000000)         #643801000000         11
space[["783.562 km²"]] <- sprintf("%.0f", 783562*1000000)         #783562000000         11
space["881,913 km2"] <- sprintf("%.1f", 881913*1000000)           #881913000000         11
space[["923.768 km²"]] <- sprintf("%.0f", 923768*1000000)         #923768000000
space[["1.010.000 km²"]] <- sprintf("%.0f", 1010000*1000000)      #1010000000000        12
space[["1.030.000 km²"]] <- sprintf("%.0f", 1030000*1000000)      #1030000000000        12
space["1 100 000 km2"] <- sprintf("%.1f", 1100000*1000000)        #1100000000000        12
space[["1.104.000 km²"]] <- sprintf("%.0f", 1040000*1000000)      #1040000000000        12
space[["1.220.000 km²"]] <- sprintf("%.0f", 1220000*1000000)      #1220000000000        12
space[["1.240.000 km²"]] <- sprintf("%.0f", 1240000*1000000)      #1240000000000        12
space["1.267.000 km?"] <- sprintf("%.1f", 1267000*1000000)        #1267000000000        12
space[["1.648.000 km²"]] <- sprintf("%.0f", 1648000*1000000)      #1648000000000        12
space[["1.760.000 km²"]] <- sprintf("%.0f", 1760000*1000000)      #1760000000000        12
space[["1.886.000 km²"]] <- sprintf("%.0f", 1886000*1000000)      #1886000000000        12
space[["1.905.000 km?"]] <- sprintf("%.1f", 1905000*1000000)      #1905000000000        12
space[["1.973.000 km²"]] <- sprintf("%.0f", 1973000*1000000)      #1973000000000        12
space[["2.382.000 km²"]] <- sprintf("%.0f", 2382000*1000000)      #2382000000000        12
space[["3.287.000 km²"]] <- sprintf("%.0f", 3287000*1000000)      #3287000000000        12
space[["4.476.000 km²"]] <- sprintf("%.1f", 4476000*1000000)      #4476000000000        12
space[["4 986 515 km2"]] <- sprintf("%.0f", 4986515*1000000)      #4986515000000
space["7,682,300 km2"] <- sprintf("%.1f", 7682300*1000000)        #7682300000000        12 
space["8.516.000 km?"] <- sprintf("%.1f", 8516000*1000000)        #8516000000000        12
space[["9.597.000 km²"]] <- sprintf("%.1f", 9597000*1000000)      #9597000000000        12
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
                             "increase production of X",
                             "protect farm land resources",
                             "secure food supply",
                             "increase yield of X",
                             "sustain food production",
                             "water sufficiency")
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
                       "reduce domestic prices of X",
                       "increase household incomes",
                       "improve market access",
                       "improve rural livelihoods",
                       "reduce disparities in food security",
                       "reestablish an equitable balance of land ownership",
                       "increase household incomes",
                       "improve market access",
                       "improve rural livelihoods",
                       "rural development",
                       "reduce disparities in food security")
goals_class[["utilisation"]] <- c("increase consumption of fruits and vegetables",
                            "improve diets (more healthy)",
                            "stimulate consumption",
                            "increase nutritional status",
                            "increase food consumption")
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
goals_class[["environmental/climate"]] <- c("maximise delivery of ecosystem services",
                              "digest organic wastes",
                              "regulate pollution",
                              "sustain ecosystems",
                              "maintain wetlands",
                              "recharge aquifers",
                              "store run-off water",
                              "reduce soil erosion",
                              "improve soil fertility",
                              "maintain land in good environmental condition",
                              "reduce greenhouse gas emissions",
                              "reduce agrochemical use",
                              "avoid agricultural expansion",
                              "improve environmental sustainability",
                              "reduce carbon footprint from food production",
                              "reduce ecological footprint from food production",
                              "reduce land use intensity",
                              "reduce water footprint from food production",
                            "reduce agrochemical use")
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
                                   "improve welfare of low-income households",
                                   "improve human health")
goals_class[["other"]] <- c("reduce population growth",
                            "stimulate employment of less skilled manpower")

# governance measure dictionary
measure_class <- list()
measure_class[["education & training"]] <- c("promotion of improved seed",
                                                                           "improving female workers' education levels",
                                                                           "training and scholarship scheme",
                                                                           "development of farm schools",
                                                                           "education & training",
                                             "trust-building between extension services and farmers")

measure_class[["campaigns"]] <- c("awareness campaign on water conservation",
                                  "introduce additional legume cultivation schemes",
                                  "family planning programme",
                                  "improving willingness to walk",
                                  "perfect information communication through agricultural extension services",
                                  "promotion of improved seed",
                                  "promotion of agricultural machinery & equipment",
                                  "share seasonal forecasts with farmers",
                                  "dietary guidelines",
                                  "encourage environmentally friendly farm inputs")

measure_class[["incentives for certain production systems/land uses"]] <- c("abolish quantitative and area-based cotton production restrictions",
                                                                            "land protection policy",
                                                                            "quantity-based cotton production requirements",
                                                                            "relocation of water-demanding crops",
                                                                            "quantity & area-based cotton production requirements") 

measure_class[["store policies"]] <- c("creating new stores",
                                       "subsidies on farm inputs",
                                       "increase availability of (healthy) foods in stores")

measure_class[["infrastructure projects"]] <- c("improving public transport",
                                                "construction of hydro-electric dams",
                                                "sewage treatment",
                                                "improve market infrastructure for fertiliser",
                                                "construction of water storage infrastructure",
                                                "improve irrigation facilities")

measure_class[["affordability & pricing policies"]] <- c("cheaper healthy food/relative pricing",
                                                         "food stamps to low-income households with incentives for fruit and vegetable",
                                                         "cheaper food products that will soon expire",
                                                         "cash transfers to low-income households")

measure_class[["credit policies"]] <- c("credit provision for agricultural production-related expenses",
                                        "reduced interest rates",
                                        "credit programme")

measure_class[["loan policies"]] <- c("adjusted interest rates",
                              "increased interest rates",
                              "decreased interest rate",
                              "increased interest rate")

measure_class[["logistics and management policies"]] <- c("dynamic shelf life policy")

measure_class[["tax policies"]] <- c("decreased value added tax on hotels and restaurants",
                                     "increased value added tax",
                                     "increased value added tax with exceptions for agricultural items",
                                     "increased value added tax with lower rates for agricultural goods",
                                     "reduced value added tax for poor households",
                                     "tax rebate",
                                     "increased value added tax with exemptions for agricultural and food processing goods",
                                     "abolish producer tax on X",
                                     "tax exemption to fertiliser")

measure_class[["domestic subsidies"]] <- c("subsidies on fertilizer",
                                           "elimination of irrigation subsidies",
                                           "subsidies for establishment of perennials",
                                           "subsidies for primary processing and marketing activities",
                                           "subsidies for productive infrastructure",
                                           "subsidies for pure breed livestock acquisition",
                                           "subsidies on agricultural labour",
                                           "subsidies on land rent",
                                           "subsidies on machinery and equipment",
                                           "subsidies on irrigation and drainage projects",
                                           "subsidies on farm inputs",
                                           "subsidies on fertiliser",
                                           "subsidies on fuel",
                                           "subsidies on (hybrid/improved) seeds",
                                           "reduce subsidies on agrochemicals",
                                           "short-term subsidies on (hybrid/improved) seeds",
                                           "short-term subsidies on fertiliser",
                                           "abolish subsidies on fertiliser")

measure_class[["international trade"]] <- c("trade liberalisation",
                                            "trade deliberalisation",
                                            "accession into trade partnership",
                                            "open for trade with neighbouring countries")

measure_class[["other"]] <- c("rice self-sufficiency program", "two-child policy", "rural settlement consolidation", "allow land rental", 
                              "land market closure", "land reform/redistribution")

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

measure_class[["import policies"]] <- c("import tariffs on rice",
                                      "reduce import tariffs on fertilizer",
                                      "abolish quantitative rice import quotas",
                                      "reduced import tariffs on rice",
                                      "reduce import tariffs on fertiliser",
                                      "import tax on X",
                                      "reduce import tax on X",
                                      "reduce/remove import tariffs on X")

measure_class[["export policies"]] <- c("export ban on wheat and coarse grains",
                                        "increased export tax on cereals",
                                        "export quotas for wheat & coarse grains",
                                        "export tariffs on wheat & coarse grains",
                                        "abolish quantiative export quotas of rice",
                                        "export tax on X",
                                        "reduce export tax on X",
                                        "abolish export subsidies for X",
                                        "abolish export taxes for X")

measure_class[["direct support for food producers"]] <- c("minimum support price for rice",
                                                          "minimum support price for milk",
                                                          "abolish production subsidies for X")

measure_class[["nature-centered measures"]] <- c("soil erosion control",
                                                 "afforestation (also agro-forestry)",
                                                 "disaster control")

# commodities
comm_classes <- list()
comm_classes[["cereals & cereal products"]] <- c("rice", "maize", "wheat", "sorghum", "millet", "grain", "coarse grains", "barley", "bread", "maize flour", "oats", "other cereals", "pasta", 
                                                 "refined grains", "teff", "whole grains", "cereals", "maize hybrid/improved", "maize local")
comm_classes[["fruits and derived products"]] <- c("fruit", "plantain", "avocado", "banana", "mango")
comm_classes[["vegetables and derived products"]] <- c("vegetable", "onion", "tomato", "broccoli", "cabbage", "garlic")
comm_classes[["sugar crops and sweeteners, and derived products"]] <- c("sugar", "sugarcane", "candy")
comm_classes[["fibres of vegetal and animal origin"]] <- c("cotton", "plant-based fibers", "raw cotton")
comm_classes[["compisite 'foods'*"]] <- c("food", "other food", "food at home", "food away from home", "processed food",
                                        "healthy food", "unhealthy food", "food of plant origin", "own-produced food", "livestock products")
comm_classes[["composite crops*"]] <- c("crops","trees", "other crops", "cash crops", "horticultural crops", "industrial crops", "non-food crops", 
                                                  "other starchy staples")
comm_classes[["seeds for planting*"]] <- c("hybrid/improved maize seed", "seeds", "local maize seed")
comm_classes[["non-food tree crops and derived products*"]] <- c("wood", "eucalyptus", "wood products")
comm_classes[["products from live animals"]] <- c("milk", "dairy", "egg")
comm_classes[["oil-bearing crops and derived products"]] <- c("groundnut", "soybean", "coconut", "jatropha", "linseed", "oil palm", "oil seeds", "oilseed", "palm oil", "rape seed", "sesame", 
                                                              "peanut")
comm_classes[["products from slaughtered animals"]] <- c("meat", "fish", "beef", "other meat")
comm_classes[["livestock"]] <- c("cattle", "goat", "poultry", "sheep", "livestock", "broiler chicken", "buffalo", "camel", "chicken", "cow", 
                                 "crossbred cattle", "laying hen", "male calf")
comm_classes[["stimulant crops and derived products"]] <- c("tobacco", "coffee", "cocoa", "chat", "cocoa beans", "tea")
comm_classes[["beverages"]] <- c("beverages", "alcoholic beverages", "carbonated soft drinks")
comm_classes[["roots and tubers, and derived products"]] <- c("cassava", "potato", "roots", "sweet potato", "tubers", "yam", "cocoyam")
comm_classes[["pulses and derived products"]] <- c("cowpea", "pulses", "beans", "chickpea", "dry beans", "faba bean", "haricot bean", "legumes", "lentil", "vetch")
comm_classes[["fodder crops and products"]] <- c("animal feed", "feedgrain", "fodder crop", "fonio")
comm_classes[["nuts and derived products"]] <- c("cashews", "nuts")
comm_classes[["beverage crops and spices"]] <- c("hop")
comm_classes[["vegetable and animal oils and fats"]] <- c("edible oil", "biodiesel", "vegetable oil")
comm_classes[["hides and skins"]] <- c("leather")
comm_classes[["other commodities*"]] <- c("enset", "salt")

# FS indicators
FSi_classes <- list()
FSi_classes[["availability"]] <- c("number of (active/passive) farms", "area used for X", "supply of water for irrigation", "area irrigated", "production of X", "productivity of X", 
                                   "yield of X", "yield gap of X", "availability of X", "area left fallow/unused", "supply of X", "consumption of agricultural water", "fertiliser use for X",
                                   )
FSi_classes[["access"]] <- c("profitability of land", "profitability of water use", "profitability of labour", "farm profit", "marginal cost of producing X", "marginal cost of shipping X",
                             "farm gate/producer price of X", "import of X", "export of X", "price of X", "import price of X", "price of irrigation water", "added value of X", 
                             "contribution of X to GDP", "income/wage/salary", "discretionary income", "income from X", "income from agriculture", "income inequality", "poverty incidence/rate",
                             "poverty severity", "poverty depth/gap", "people walking to store as a last resort", "capital assets", "demand for X", "demand for farm land", "production costs",
                             "dietary income differential", "economic benefit/increase in output value of additional production of X", "inflation", "poverty rate/incidence/headcount ratio", 
                             "sales of X", "selling time of X", "unemployed", "demand for agrochemicals", "demand for capital", "demand for farm labour", "demand for agricultural water", 
                             "price of agrochemicals", "price of labour", "price of land", "land use inequality")
FSi_classes[["utilisation"]] <- c("consumption of X", "Healthy Eating Index", "share of X in diet", "quality adjusted life years", "dietary income inequality", "food losses", "purchase of X")
FSi_classes[["stability"]] <- c("resilience to water scarcity", "water security", "food shortage/deficit", "stock/surplus of X", "incidence of higher average income with higher variance (%)",
                                "incidence of higher average income with lower variance (%)", "incidence of identical average income and variance (%)", 
                                "incidence of ïdentical average income with lower variance (%)", "incidence of lower average income with higher variance", 
                                "incidence of lower average income with lower variance [%]", "variance in income", "self-sufficiency of X", "food security risk", 
                                "incidence of identical average income with lower variance (%)", "incidence of lower income with higher variance")
FSi_classes[["unclear"]] <- "unclear"

timpl_classes <- list()
timpl_classes[["governmental entities"]] <- "government"
timpl_classes[["food producing entities"]] <- c("farmer")
timpl_classes[["unclear"]] <- c("unclear")
timpl_classes[["knowledge broker entities"]] <- c("agricultural extension services", "educators")
timpl_classes[["trade entities"]] <- c("trade partnership", "farmer cooperative")
timpl_classes[["non-food manufacturing/processing entities"]] <- c("agricultural machinery manufacturing industry")
timpl_classes[["food consuming entities"]] <- c("consumer", "resident/consumer")
timpl_classes[["citizen entities"]] <- c("citizen", "volunteers", "women")
timpl_classes[["food retail entities"]] <- c("retailer", "stores")
timpl_classes[["knowledge producing entities"]] <- c("agricultural researchers")
timpl_classes[["banks/credit suppliers"]] <- c("credit suppliers")
timpl_classes[["non-food retail entities"]] <- c("input supplier")