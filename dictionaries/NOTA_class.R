# combine tools that are the same but used differently ################################################################
undir_gov <- list()

# nodality
## group targeted messages
undir_gov[["trainings in waste separation"]] <- "provide trainings in waste separation"
undir_gov[["trainings in pesticide use"]] <- "provide trainings in pesticide use"
undir_gov[["trainings in fertiliser use"]] <- "provide trainings in fertiliser use"
undir_gov[["trust-building between extension services and farmers"]] <- c("trust-building between extension services and farmers")
undir_gov[["introduce new techniques to farmers"]] <- c("introduce new techniques to farmers")
undir_gov[["encourage environmentally friendly farm inputs"]] <- "encourage environmentally friendly farm inputs"
undir_gov[["education & training (generic)"]] <- c("education & training")
undir_gov[["farm schools"]] <- c("development of farm schools")
undir_gov[["scholarship scheme"]] <- c("training and scholarship scheme")
undir_gov[["promotion of agricultural machinery and equipment"]] <- c("promotion of agricultural machinery & equipment")
undir_gov[["promotion of improved seed"]] <- c("promotion of improved seed")
undir_gov[["improving female workers' education levels"]] <- c("improving female workers' education levels") #unsure
undir_gov[["agricultural extension"]] <- c("(expand) agricultural extension services")

## propaganda
undir_gov[["dietary guidelines"]] <- "dietary guidelines"
undir_gov[["awareness campaign on water conservation"]] <- c("awareness campaign on water conservation")
undir_gov[["campaign to promote shopping in supermarkets"]] <- c("campaign to promote shopping in supermarkets (versus convenience stores)")
undir_gov[["improving willingness to walk"]] <- c("improving willingness to walk")

## packaged self-serve messages
undir_gov[["share seasonal forecasts with farmers"]] <- c("share seasonal forecasts with farmers")

## bespoke messages
undir_gov[["family planning programme (volunteers at door)"]] <- c("family planning programme")

# organisation
## transportation and redistribution
undir_gov[["improve transport and distribution of food in public distribution programme"]] <- c("improve transport and distribution of food in food distribution programme")
undir_gov[["crowd-shipping food rescue programme"]] <- "crowd-shipping food rescue programme"
undir_gov[["improve public transport"]] <- c("improving public transport")
undir_gov[["set up a distribution network for a commodity X"]] <- c("setup local X distribution network")

## at large treatment
undir_gov[["construction of irrigation infrastructure"]] <- c("improve irrigation facilities", "construction of irrigation infrastructure", "construction of water canals", "construction of irrigation network")
undir_gov[["construction of electric grids"]] <- "construction of electric grids"
undir_gov[["construction of road infrastructure"]] <- c("improve road networks", "construction of road networks")
undir_gov[["construction of market infrastructure"]] <- c("improve market infrastructure", "construction of market facilities", "improve market infrastructure for fertiliser")
undir_gov[["construction of water storage infrastructure"]] <- c("construction of water storage infrastructure")
undir_gov[["disaster control"]] <- c("disaster control")
undir_gov[["afforestation"]] <- c("afforestation (also agro-forestry)")
undir_gov[["sewage treatment"]] <- c("sewage treatment")
undir_gov[["soil erosion control"]] <- c("soil erosion control")
undir_gov[["construction of hydro-electric dams"]] <- c("construction of hydro-electric dams")
undir_gov[["relocation of water demanding crops"]] <- c("relocation of water-demanding crops")
undir_gov[["creating new stores"]] <- c("creating new stores")

## storage and custody
undir_gov[["strategic storage reserves of a commodity X"]] <- c("strategic storage reserves of X")

## processing
undir_gov[["increased shelf life for a commodity X"]] <- c("increased shelf life for X")
undir_gov[["cheaper food products that will soon expire"]] <- c("cheaper food products that will soon expire")
undir_gov[["dynamic shelf life policy"]] <- c("dynamic shelf life policy")
undir_gov[["start processing factory for a commodity X"]] <- c("start X processing factory")
undir_gov[["increase nutrient content of a commodity X through processing"]] <- c("increase nutrient content of X through processing")
undir_gov[["livestock vaccination programme"]] <- c("livestock vaccination campaign")

## group treatment
undir_gov[["increase availability of a commodity X in stores"]] <- c("increase availability of (healthy) foods in stores")

# treasure
## bounties
undir_gov[["import tariffs on a commodity X"]] <- c("remove import tariffs for X", "import tariffs on X", "reduce/remove import tariffs on X", "reduce import tariffs on X", "remove import tariffs")
undir_gov[["support fertiliser application through price incentives"]] <- "support fertiliser application through price incentives"
undir_gov[["direct payments to farmers in exchange for food production, land management or delivery of other ecosystem services"]] <- c("(increase) direct payments", "(increase) direct payments for X production", "reduce direct payments", "reduce direct payments for X production", "remove direct payments", "abolish production subsidies for X")
undir_gov[["export subsidies for a commodity X"]] <- c("abolish export subsidies for X", "abolish export subsidies")
undir_gov[["variable cost subsidies for production of a commodity X"]] <- "variable cost subsidies for X production" 
undir_gov[["export tariffs on a commodity X"]] <- "export tariffs on X"
undir_gov[["import tariffs on farm inputs"]] <- c("reduce import tariffs on fertiliser")
undir_gov[["redistributive direct payments"]] <- c("redistributive direct payments")
undir_gov[["subsidies on irrigation and drainage projects"]] <- c("subsidies on irrigation and drainage projects")
undir_gov[["subsidies on agricultural activities"]] <- c("subsidies on agricultural labour", "subsidies for establishment of perennials")
undir_gov[["subsidies for primary processing"]] <- c("subsidies for primary processing")
undir_gov[["subsidies for marketing activities"]] <- c("subsidies for marketing activities")
undir_gov[["incentivise GM food production through redused compliance cost"]] <- c("incentivise GM food production through redused compliance cost")

## contracts
undir_gov[["willing-buyer willing-seller land reform stimulated by grants"]] <- "willing-buyer willing-seller land reform stimulated by grants"
undir_gov[["forward contracts with producers of a commodity X"]] <- c("forward constracts with X producers")
undir_gov[["credit programmes (generic)"]] <- c("improve access to credit", "credit programme")
undir_gov[["create off-farm off-season work opportunities"]] <- "create off-farm off-season work opportunities"
undir_gov[["change interest rate on loans"]] <- c("reduce interest rate on loans", "increased interest rate", "adjusted interest rates")
undir_gov[["credit provision for agricultural production-related expenses"]] <- c("credit provision for agricultural production-related expenses")

## bearer-directed payments
undir_gov[["subsidies on housing loans"]] <- "subsidies on housing loans"
undir_gov[["subsidies on energy consumption"]] <- "subsidies on energy consumption"
undir_gov[["subsidies on farm inputs"]] <- c("subsidies on fertiliser", "elimination of irrigation subsidies", "(increase) subsidies on agrochemicals", "(increase) subsidies on machinery and equipment", "subsidies for productive infrastructure", "subsidies for pure breed livestock acquisition", "(increase) subsidies on (hybrid/improved/high quality) seeds", "(increase) subsidies on farm inputs", "reduce subsidies on agrochemicals", "reduce subsidies on fertilisers", "abolish subsidies on fertiliser", "short-term subsidies on fertiliser", "short-term subsidies on (hybrid/improved) seeds", "subsidies on fuel", "subsidies on supplementary feed during forage shortage", "subsidies on supplementary feed during drought", "subsidies on supplementary feed during and after drought")
undir_gov[["minimum support price for a commodity X"]] <- c("(increase) minimum support price for X", "reduce minimum support price for X")
undir_gov[["subsidies on land rent"]] <- c("subsidies on land rent")
undir_gov[["cheaper healthy food (relative pricing)"]] <- c("cheaper healthy food/relative pricing")
undir_gov[["subsidies on a commodity X"]] <- c("subsidies on X")

## transfers
undir_gov[["food stamps to low-income households"]] <- c("food stamps to low-income households", "food stamps to low-income households with incentives for fruit and vegetable", "food stamps to low-income households during price shocks")
undir_gov[["compensation for resource dispossession due to infrastructure construction"]] <- "compensation for resource dispossession due to infrastructure construction"
undir_gov[["tax rebate"]] <- c("tax rebate")
undir_gov[["cash transfers to low-income households"]] <- c("cash transfers to low-income households")

## conduits
undir_gov[["investment in road infrastructure"]] <- "investment in road infrastructure"
undir_gov[["investment in fertiliser industry"]] <- c("investment in fertiliser industry", "(increase) investment in farm inputs")
undir_gov[["investment in market infrastructure"]] <- "investment in market infrastructure"
undir_gov[["investment in infrastructure (generic)"]] <- c("(increase) investment in infrastructure")
undir_gov[["investment in irrigation infrastructure"]] <- c("(increase) investment in irrigation")
undir_gov[["investment in agricultural technology development"]] <- c("investment in agricultural machinery & equipment", "(increase) investment in agricultural technology development")
undir_gov[["investment in agricultural extension"]] <- c("(increase) investment in extension services", "perfect information communication through agricultural extension services")
undir_gov[["investment in agricultural research"]] <- c("(increase) investment in agricultural research")
undir_gov[["investment (generic)"]] <- c("increased investment general")
undir_gov[["investment in agricultural sector (generic)"]] <- c("(increase) investment in agricultural sector")

# flagged
## check articles again
undir_gov[["not sure how to classify check article again"]] <-  c("remove conditional agricultural production subsidies based on target prices of X", "conditional agricultural production subsidies based on target prices of X")

## compounded and political
undir_gov[["compounded and political"]] <- c("liberalisation of fertiliser prices", "rice self-sufficiency programme")

## unclear
undir_gov[["unclear"]] <- "unclear"

# authority
## standard constraints
undir_gov[["penalties for unregistered businesses"]] <- "increased penalties for unregistered businesses"
undir_gov[["non-tariff trade barriers"]] <- c("remove non-tariff trade barriers", "introduce non-tariff trade barriers", "reduce non-tariff trade barriers")
undir_gov[["tax on farm inputs"]] <- c("increase tax on biocides", "tax exemption to fertiliser")
undir_gov[["ban on land sales and/or rental"]] <- "allow land rental"
undir_gov[["ban on international trade of a commodity X"]] <- c("land market closure", "open for trade with neighbouring countries")
undir_gov[["export tax on a commodity X"]] <- c("reduce export tax on X", "reduce export tax on X", "abolish export taxes for X", "export tax on X", "increased export tax on X")
undir_gov[["two-child policy"]] <- c("two-child policy")
undir_gov[["import tax on a commodity X"]] <- c("import tax on X", "import tax on X", "reduce import tax on X")
undir_gov[["rural settlement consolidation"]] <- "rural settlement consolidation"
undir_gov[["producer tax on a commodity X"]] <- "abolish producer tax on X"
undir_gov[["land protection policy"]] <- "land protection policy"
undir_gov[["value added tax on a commodity X"]] <- c("reduce value added tax on hotels and restaurants", "reduce value added tax for X")
undir_gov[["export ban for a commodity X"]] <- c("export ban on X")
undir_gov[["reduce value added taxes for poor households"]] <- c("reduce value added tax for poor households") #unsure
undir_gov[["quantity-based production requirements for a commodity X"]] <- c("quantity-based production requirements for X")
undir_gov[["value added tax (generic)"]] <- c("increased value added tax")
undir_gov[["quantitative restrictions for production of a commodity X"]] <- c("abolish quantitative X production restrictions")
undir_gov[["area-based restrictions for production of a commodity X"]] <- c("abolish area-based X production restrictions")
undir_gov[["area-based production requirements for a commodity X"]] <- c("area-based production requirements for X")
undir_gov[["quantity-based export restrictions for a commodity X"]] <- c("quantity-based X export restrictions", "remove quantity-based X export restrictions")

## open permits
undir_gov[["export quotas for a commodity X"]] <- c("abolish quantiative export quotas for X", "export quotas for X")
undir_gov[["import quotas for a commodity X"]] <- c("abolish quantitative X import quotas")

## open compacts
undir_gov[["allow the cultivation of bio-fuel in marginal grasslands"]] <- c("allow the cultivation of bio-fuel in marginal grasslands")
undir_gov[["value added tax exemptions for a commodity X"]] <- c("value added tax exemptions for X")
undir_gov[["allow farmer to colonise new land for agriculture"]] <- c("allow farmer to colonise new (natural) land for agriculture")
undir_gov[["allow farmers to perform agroforestry in timber production areas"]] <- c("allow agroforestry between trees in timber production areas")

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
                                           "introduce new techniques to farmers",
                                           "education & training (generic)",
                                           "improving female workers' education levels",
                                           "agricultural extension")

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
                                                         "improv public transport",
                                                         "set up a distribution network for a commodity X")

organisation[["at large treatment"]] <- c("construction of irrigation infrastructure",
                                          "construction of market facilities",
                                          "creating new stores",
                                          "construction of market infrastructure",
                                          "sewage treatment",
                                          "disaster control",
                                          "soil erosion control",
                                          "construction of electric grids",
                                          "afforestation",
                                          "construction of hydro-electric dams",
                                          "construction of water storage infrastructure")

organisation[["storage and custody"]] <- c("strategic storage reserves of a commodity X") 

organisation[["group treatment"]] <- c("increase availability of a commodity X in stores")

organisation[["processing"]] <- c("start processing factory for a commodity X", "dynamic shelf life policy", 
                                  "increase nutrient content of a commodity X through processing",
                                  "livestock vaccination programme")

organisation_rev <- reverse_dict(dict = organisation)
NOTA_class[["Organisation"]] <- organisation

# Then all treasure measures per sub-class
treasure <- list()
treasure[["transfers"]] <- c("food stamps to low-income households",
                             "cash transfers to ...",
                             "cash transfers to low-income households",
                             "food stamps to ...",
                             "compensation for resource dispossession due to infrastructure construction")

treasure[["bearer-directed payments"]] <- c("minimum support price for a commodity X",
                                            "subsidies on farm inputs",
                                            "subsidies on energy consumption",
                                            "cheaper healthy food (relative pricing)",
                                            "subsidies on houseing loans",
                                            "subsidies on a commodity X",
                                            "subsidies on land rent")

treasure[["contracts"]] <- c("credit provision for agricultural production-related expenses",
                             "create off-farm off-season work opportunities",
                             "credit programmes (generic)",
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
                            "support fertiliser application through price incentives",
                            "redistributive direct payments",
                            "subsidies on agricultural activities",
                            "subsidies for primary processing",
                            "subsidies for marketing activities",
                            "incentivise GM food production through redused compliance cost")

treasure[["conduits"]] <- c("investment in ...",
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
                                         "area-based production requirements for a commodity X",
                                         "value added tax (generic)",
                                         "quantity-based export restrictions for a commodity X")

authority[["open compacts"]] <- c("allow cultivation of bio-fuel in marginal grasslands",
                                  "allow farmers to colonise new land for agriculture",
                                  "allow farmers to perform agroforestry in timber production areas",
                                  "value added tax exemptions for a commodity X")

authority_rev <- reverse_dict(dict = authority)
NOTA_class[["Authority"]] <- authority

#
NOTA_simpl <- list()
NOTA_simpl <- nodality_rev
NOTA_simpl <- append(NOTA_simpl, organisation_rev)
NOTA_simpl <- append(NOTA_simpl, treasure_rev)
NOTA_simpl <- append(NOTA_simpl, authority_rev)
