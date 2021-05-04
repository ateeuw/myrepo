# dictionary grouping governance objectives into the categories: 
# availability, accessibility, utilisation, stability, environmental/climate, macro-economic, macro-logistics, infrastructure & technology, health & wellbeing, & other

goals_class <- list()
goals_class[["availability"]] <- c("increase productivity of X",
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
                                   "increase/secure food supply",
                                   "increase yield of X",
                                   "sustain food production",
                                   "water sufficiency",
                                   "food sufficiency",
                                   "increase fertiliser production")

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
                                    "reduce disparities in food security",
                                    "increase household expenditure",
                                    "reduce price of fertilisers",
                                    "reduce trade margins of X",
                                    "reduce food losses")

goals_class[["utilisation"]] <- c("increase consumption of fruits and vegetables",
                                  "improve diets (more healthy)",
                                  "stimulate consumption",
                                  "increase nutritional status",
                                  "increase consumption of X")

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

goals_class[["macro-economic"]] <- c("economic growth",
                               "improve market",
                               "increase profitability of businesses",
                               "reduce cost of governance measure",
                               "increase regional income",
                               "equitable rural development",
                               "ensure stability of national export revenues",
                               "increase economic efficiency",
                               "increase public revenues")

goals_class[["macro-logistics"]] <- c("increase supply chain efficiency")

goals_class[["infrastructure & technology"]] <- c("speed up technology diffusion",
                                                  "finance infrastructure",
                                                  "energy sufficiency")

goals_class[["health & wellbeing"]] <- c("improve public health",
                                         "improve living standards",
                                         "improve welfare of low-income households",
                                         "improve welfare",
                                         "improve human health")

goals_class[["other"]] <- c("reduce population growth",
                            "stimulate employment of less skilled manpower")
