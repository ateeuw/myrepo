
# First all nodal measures per sub-class
NOTA_subclass <- list()
NOTA_subclass[["bespoke messages"]] <- c("Investment in and communication through extension services",
                                    "family planning programme (volunteers at door)")

NOTA_subclass[["group targeted messages"]] <- c("encourage environmentally friendly farm inputs",
                                           "scholarship scheme",
                                           "promotion of improved seed",
                                           "promotion of agricultural machinery and equipment",
                                           "farm schools",
                                           "trust-building between extension services and farmers",
                                           "education in ...",
                                           "trainings in waste separation",
                                           "trainings in pesticide use",
                                           "trainings in fertiliser use",
                                           "trainings in ...",
                                           "introduce new techniques to farmers",
                                           "education & training (generic)",
                                           "improving female workers' education levels",
                                           "agricultural extension")

NOTA_subclass[["packaged self-serve messages"]] <- c("share seasonal weather forcasts with farmers")

NOTA_subclass[["propaganda"]] <- c("campaign to promote shopping in supermarkets",
                              "dietary guidelines",
                              "improving willingness to walk",
                              "awareness campaign on water conservation")

# Then all organisational measures per sub-class
NOTA_subclass[["transportation and redistribution"]] <- c("relocation of water demanding crops",
                                                         "crowd-shipping food rescue programme",
                                                         "improve transport and distribution of food in public distribution programme",
                                                         "improv public transport",
                                                         "set up a distribution network for a commodity X",
                                                         "improve public transport")

NOTA_subclass[["at large treatment"]] <- c("construction of irrigation infrastructure",
                                          "construction of market facilities",
                                          "creating new stores",
                                          "construction of market infrastructure",
                                          "sewage treatment",
                                          "disaster control",
                                          "soil erosion control",
                                          "construction of electric grids",
                                          "afforestation",
                                          "construction of dams",
                                          "construction of water storage infrastructure",
                                          "creating new stores")

NOTA_subclass[["unclear"]] <- c("unclear")

NOTA_subclass[["storage and custody"]] <- c("strategic storage reserves of a commodity X") 

NOTA_subclass[["group treatment"]] <- c("increase availability of a commodity X in stores")

NOTA_subclass[["processing"]] <- c("start processing factory for a commodity X", "dynamic shelf life policy", 
                                  "increase nutrient content of a commodity X through processing",
                                  "livestock vaccination programme")


# Then all treasure measures per sub-class
NOTA_subclass[["transfers"]] <- c("food stamps to low-income households",
                             "cash transfers to ...",
                             "cash transfers to low-income households",
                             "food stamps to ...",
                             "compensation for resource dispossession due to infrastructure construction")

NOTA_subclass[["bearer-directed payments"]] <- c("minimum support price for a commodity X",
                                            "subsidies on farm inputs",
                                            "subsidies on energy consumption",
                                            "cheaper healthy food (relative pricing)",
                                            "subsidies on houseing loans",
                                            "subsidies on a commodity X",
                                            "subsidies on land rent")

NOTA_subclass[["contracts"]] <- c("credit provision for agricultural production-related expenses",
                             "create off-farm off-season work opportunities",
                             "credit programmes (generic)",
                             "change interest rate on loans",
                             "willing-seller willing-buyer land reform stimulated by government grants",
                             "forward contracts with producers of a commodity X")

NOTA_subclass[["bounties"]] <- c("direct payments to farmers in exchange for food production, land management or delivery of other ecosystem services",
                            "subsidies on irrigation and drainage projects",
                            "export subsidies for a commodity X",
                            "export tariffs on a commodity X",
                            "import tariffs on a commodity X",
                            "subsidies for agricultural production, processing and marketing activities", #to do: split it up
                            "import tariffs on farm inputs",
                            "support fertiliser application through price incentives",
                            "redistributive direct payments",
                            "subsidies on agricultural activities",
                            "subsidies for primary processing",
                            "subsidies for marketing activities",
                            "incentivise GM food production through redused compliance cost")

NOTA_subclass[["conduits"]] <- c("investment in ...",
                            "investment in fertiliser industry",
                            "investment in road infrastructure",
                            "investment in market infrastructure",
                            "investment in infrastructure (generic)",
                            "investment in irrigation infrastructure",
                            "investment in agricultural technology development",
                            "investment in agricultural extension",
                            "investment in agricultural research",
                            "investment (generic)",
                            "investment in agricultural sector (generic)")

# Then lastly all authority measures per sub-class
NOTA_subclass[["directed constraints"]] <- c("value added tax on a commodity X for ...", 
                                         "land reform/redistribution")

NOTA_subclass[["open permits"]] <- c("export quotas for a commodity X",
                                 "import quotas for a commodity X")

NOTA_subclass[["standard constraints"]] <- c("ban on land sales and/or rental",
                                         "export ban for a commodity X",
                                         "export tax on a commodity X",
                                         "producer tax on a commodity X",
                                         "value added tax on a commodity X",
                                         "import tax on a commodity X",
                                         "land protection policies",
                                         "non-tariff trade barriers",
                                         "quantitative restrictions for production of a commodity X",
                                         "two-child policy",
                                         "tax on farm inputs",
                                         "quantity-based production requirements for a commodity X",
                                         "rural settlement consolidation",
                                         "ban on international trade of a commodity X",
                                         "area-based restrictions for production of a commodity X",
                                         "area-based production requirements for a commodity X",
                                         "value added tax (generic)",
                                         "quantity-based export restrictions for a commodity X")

NOTA_subclass[["open compacts"]] <- c("allow cultivation of bio-fuel in marginal grasslands",
                                  "allow farmer to colonise new land for agriculture",
                                  "allow farmers to perform agroforestry in timber production areas",
                                  "value added tax exemptions for a commodity X")
