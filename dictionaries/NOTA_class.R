# combine tools that are the same but used differently ################################################################
undir_gov <- list()

# nodality
# group targeted messages
undir_gov[["trainings in waste separation"]] <- "provide trainings in waste separation"
undir_gov[["trainings in pesticide use"]] <- "provide trainings in pesticide use"
undir_gov[["trainings in fertiliser use"]] <- "provide trainings in fertiliser use"
undir_gov[["trust-building between extension services and farmers"]] <- c("trust-building between extension services and farmers")
undir_gov[["introduce new techniques to farmers"]] <- c("introduce new techniques to farmers")

# organisation
## transportation and redistribution
undir_gov[["improve transport and distribution of food in public distribution programme"]] <- c("improve transport and distribution of food in food distribution programme")
undir_gov[["crowd-shipping food rescue programme"]] <- "crowd-shipping food rescue programme"

## at large treatment
undir_gov[["construction of irrigation infrastructure"]] <- c("construction of irrigation infrastructure", "construction of water canals", "construction of irrigation network")
undir_gov[["construction of electric grids"]] <- "construction of electric grids"
undir_gov[["construction of road infrastructure"]] <- c("improve road networks", "construction of road networks")
undir_gov[["construction of market infrastructure"]] <- c("improve market infrastructure", "construction of market facilities")

## storage and custody
undir_gov[["strategic storage reserves of a commodity X"]] <- c("strategic storage reserves of X")

## processing
undir_gov[["increased shelf life for a commodity X"]] <- c("increased shelf life for X")

# treasure
## bounties
undir_gov[["import tariffs on a commodity X"]] <- c("eliminate import tariffs on X", "reduce import tariffs on X", "remove import tariffs")
undir_gov[["support fertiliser application through price incentives"]] <- "support fertiliser application through price incentives"
undir_gov[["direct payments to farmers in exchange for food production, land management or delivery of other ecosystem services"]] <- c("remove direct payments", "abolish production subsidies for X")
undir_gov[["export subsidies for a commodity X"]] <- c("abolish export subsidies for X")

## contracts
undir_gov[["willing-buyer willing-seller land reform stimulated by grants"]] <- "willing-buyer willing-seller land reform stimulated by grants"
undir_gov[["forward contracts with producers of a commodity X"]] <- c("forward constracts with X producers")
undir_gov[["generic credit programmes"]] <- "improve access to credit"
undir_gov[["create off-farm off-season work opportunities"]] <- "create off-farm off-season work opportunities"

## bearer-directed payments
undir_gov[["subsidies on housing loans"]] <- "subsidies on housing loans"
undir_gov[["subsidies on energy consumption"]] <- "subsidies on energy consumption"
undir_gov[["subsidies on farm inputs"]] <- c("reduce subsidies on fertilisers", "abolish subsidies on fertiliser", "short-term subsidies on fertiliser", "short-term subsidies on (hybrid/improved) seeds")

## transfers
undir_gov[["food stamps to low-income households"]] <- c("food stamps to low-income households during price shocks")
undir_gov[["compensation for resource dispossession due to infrastructure construction"]] <- "compensation for resource dispossession due to infrastructure construction"

## conduits
undir_gov[["investment in road infrastructure"]] <- "investment in road infrastructure"
undir_gov[["investment in fertiliser industry"]] <- "investment in fertiliser industry"
undir_gov[["investment in market infrastructure"]] <- "investment in market infrastructure"

# flagged
## check articles again
undir_gov[["not sure how to classify check article again"]] <-  c("remove conditional agricultural production subsidies based on target prices of X", "conditional agricultural production subsidies based on target prices of X")

## compounded and political
undir_gov[["compounded and political"]] <- "liberalisation of fertiliser prices"

# authority
## standard constraints
undir_gov[["penalties for unregistered businesses"]] <- "increased penalties for unregistered businesses"
undir_gov[["non-tariff trade barriers"]] <- "reduce non-tariff trade barriers"
undir_gov[["tax on farm inputs"]] <- c("increase tax on biocides", "tax exemption to fertiliser")
undir_gov[["ban on land sales and/or rental"]] <- "allow land rental"
undir_gov[["ban on international trade of a commodity X"]] <- "land market closure"
undir_gov[["export tax on a commodity X"]] <- c("abolish export taxes for X")
undir_gov[["two-child policy"]] <- c("two-child policy")

## open compacts
undir_gov[["allow the cultivation of bio-fuel in marginal grasslands"]] <- c("allow the cultivation of bio-fuel in marginal grasslands")

## directed constraints
undir_gov[["land reform/redistribution"]] <- c("land reform/redistribution")

# combine tools that are the same but used differently ################################################################


NOTA_class <- list()

# First all nodal measures per sub-class
nodality <- list()
nodality[["bespoke messages"]] <- c("Investment in and communication through extension services",
                                    "family planning programme (volunteers at door)")

nodality[["group targeted messages"]] <- c("encourage environmentally friendly farm inputs",
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
                                           "introduce new techniques to farmers")

nodality[["packaged self-serve messages"]] <- c("share seasonal weather forcasts with farmers")

nodality[["propaganda"]] <- c("capaign to promote shopping in supermarkets",
                              "dietary guidelines",
                              "improving willingness to walk",
                              "awareness campaign on water conservation")

nodality_rev <- reverse_dict(dict = nodality)
NOTA_class[["Nodality"]] <- nodality_rev

# Then all organisational measures per sub-class
organisation <- list()
organisation[["transportation and redistribution"]] <- c("relocation of water demanding crops",
                                                         "crowd-shipping food rescue programme",
                                                         "improve transport and distribution of food in public distribution programme",
                                                         "improve public transport")

organisation[["at large treatment"]] <- c("construction of irrigation infrastructure",
                                          "construction of market facilities",
                                          "creating new stores",
                                          "improve market infrastructure for farm inputs",
                                          "sewage treatment",
                                          "disaster control",
                                          "soil erosion control",
                                          "construction of electric grids",
                                          "afforestation",
                                          "construction of hydro-electric dams",
                                          "construction of water storage infrastructure")

organisation[["storage and custody"]] <- c("strategic storage reserves of a commodity X") 

organisation_rev <- reverse_dict(dict = organisation)
NOTA_class[["Organisation"]] <- organisation

# Then all treasure measures per sub-class
treasure <- list()
treasure[["transfers"]] <- c("food stamps to low-income households",
                             "cash transfers to ...",
                             "food stamps to ...",
                             "compensation for resource dispossession due to infrastructure construction")

treasure[["bearer-directed payments"]] <- c("minimum support price for a commodity X",
                                            "subsidies on farm inputs",
                                            "subsidies on energy consumption",
                                            "cheaper healthy food (relative pricing)",
                                            "subsidies on houseing loans",
                                            "subsidies on healthy food")

treasure[["contracts"]] <- c("credit provision for agricultural production-related expenses",
                             "create off-farm off-season work opportunities",
                             "generic credit programmes",
                             "change interest rate on loans",
                             "willing-seller willing-buyer land reform stimulated by government grants",
                             "forward contracts with producers of a commodity X")

treasure[["bounties"]] <- c("direct payments to farmers in exchange for food production, land management or delivery of other ecosystem services",
                            "subsidies on irrigation and drainage projects",
                            "export subsidies for a commodity X",
                            "export tariffs on a commodity X",
                            "import tariffs on a commodity X",
                            "subsidies for agricultural production, processing and marketing activities", #to do: split it up
                            "import tariffs on farm inputs",
                            "support fertiliser application through price incentives")

treasure[["conduits"]] <- c("investment in ...",
                            "investment in fertiliser industry",
                            "investment in road infrastructure",
                            "investment in market infrastructure")

treasure_rev <- reverse_dict(dict = treasure)
NOTA_class[["Treasure"]] <- organisation

# Then lastly all authority measures per sub-class
authority <- list()
authority[["directed constraints"]] <- c("value added tax on a commodity X for ...", 
                                         "land reform/redistribution")

authority[["open permits"]] <- c("export quotas for a commodity X",
                                 "import quotas for a commodity X")

authority[["standard constraints"]] <- c("ban on land sales and/or rental",
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
                                         "area-based production requirements for a commodity X")

authority[["open compacts"]] <- c("allow cultivation of bio-fuel in marginal grasslands",
                                  "allow farmers to colonise new land for agriculture",
                                  "allow farmers to perform agroforestry in timber production areas")

authority_rev <- reverse_dict(dict = authority)
NOTA_class[["Authority"]] <- authority

#
NOTA_simpl <- list()
NOTA_simpl <- nodality_rev
NOTA_simpl <- append(NOTA_simpl, organisation_rev)
NOTA_simpl <- append(NOTA_simpl, treasure_rev)
NOTA_simpl <- append(NOTA_simpl, authority_rev)
