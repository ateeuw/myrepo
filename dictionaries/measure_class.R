# dictionary grouping governance measures into the categories: 
# education & training, campaigns, incentives for certain production systems/land uses, store policies, infrastructure projects, affordability & pricing policies, credit policies, loan policies, 
# logistics & management policies, tax policies, domestic subsidies, international trade agreements, activity- or sector-based investments, import policies, export policies, 
# direct support for food producers, nature-centered measures, other, unclear

measure_class <- list()

measure_class[["penalties"]] <- c("increased penalties for unregistered businesses")

measure_class[["education & training"]] <- c("promotion of improved seed",
                                             "improving female workers' education levels",
                                             "training and scholarship scheme",
                                             "development of farm schools",
                                             "education & training",
                                             "trust-building between extension services and farmers",
                                             "provide trainings in pesticide use",
                                             "provide trainings in fertiliser use",
                                             "provide trainings in waste separation")

measure_class[["campaigns"]] <- c("awareness campaign on water conservation",
                                  "introduce additional legume cultivation schemes",
                                  "family planning programme",
                                  "improving willingness to walk",
                                  "perfect information communication through agricultural extension services",
                                  "promotion of improved seed",
                                  "promotion of agricultural machinery & equipment",
                                  "share seasonal forecasts with farmers",
                                  "dietary guidelines",
                                  "encourage environmentally friendly farm inputs",
                                  "campaign to promote shopping in supermarkets (versus convenience stores)",
                                  "(expand) agricultural extension services")

measure_class[["land use policies"]] <- c("abolish quantitative and area-based cotton production restrictions",
                                                                            "land protection policy",
                                                                            "relocation of water-demanding crops",
                                                                            "allow the cultivation of bio-fuel in marginal grasslands",
                                                                            "allow agroforestry between trees in timber production areas",
                                          "allow farmer to colonise new (natural) land for agriculture") 

measure_class[["agricultural production policies"]] <- c("quantity-based cotton production requirements", "quantity & area-based cotton production requirements")


measure_class[["store policies"]] <- c("creating new stores",
                                       "subsidies on farm inputs",
                                       "increase availability of (healthy) foods in stores",
                                       "increased shelf life for X")

measure_class[["infrastructure projects"]] <- c("improving public transport",
                                                "construction of hydro-electric dams",
                                                "sewage treatment",
                                                "improve market infrastructure for fertiliser",
                                                "construction of water storage infrastructure",
                                                "improve irrigation facilities",
                                                "construction of electric grids",
                                                "construction of irrigation network",
                                                "construction of water canals",
                                                "construction of market facilities",
                                                "construction of road networks",
                                                "improve market infrastructure",
                                                "improve road networks")

measure_class[["pricing policies"]] <- c("cheaper healthy food/relative pricing",
                                                         "cheaper food products that will soon expire",
                                                         "strategic storage reserves of X")

measure_class[["direct support for consumers"]] <- c("crowd-shipping food rescue programme",
                                                     "food stamps to low-income households with incentives for fruit and vegetable",
                                                     "cash transfers to low-income households",
                                                     "food stamps to low-income households during price shocks",
                                                     "food stamps to low-income households")

measure_class[["credit policies"]] <- c("credit provision for agricultural production-related expenses",
                                        "reduced interest rates",
                                        "credit programme",
                                        "improve access to credit")

measure_class[["loan policies"]] <- c("adjusted interest rates",
                                      "increased interest rates",
                                      "decreased interest rate",
                                      "increased interest rate",
                                      "reduce interest rate on loans")

measure_class[["logistics and management policies"]] <- c("dynamic shelf life policy", "improve transport and distribution of food in food distribution programme")

measure_class[["tax policies"]] <- c("increased value added tax",
                                     "increased value added tax with exceptions for agricultural items",
                                     "increased value added tax with lower rates for agricultural goods",
                                     "tax rebate",
                                     "increased value added tax with exemptions for agricultural and food processing goods",
                                     "abolish producer tax on X",
                                     "tax exemption to fertiliser",
                                     "reduce value added tax for poor households",
                                     "reduce value added tax on hotels and restaurants",
                                     "increase tax on biocides")

measure_class[["subsidies"]] <- c("subsidies on fertilizer",
                                           "elimination of irrigation subsidies",
                                           "subsidies for establishment of perennials",
                                           "subsidies for primary processing and marketing activities",
                                           "subsidies for productive infrastructure",
                                           "subsidies for pure breed livestock acquisition",
                                           "subsidies on agricultural labour",
                                           "subsidies on land rent",
                                           "subsidies on machinery and equipment",
                                           "subsidies on irrigation and drainage projects",
                                           "subsidies on fertiliser",
                                           "subsidies on fuel",
                                           "subsidies on (hybrid/improved) seeds",
                                           "reduce subsidies on agrochemicals",
                                           "short-term subsidies on (hybrid/improved) seeds",
                                           "short-term subsidies on fertiliser",
                                           "abolish subsidies on fertiliser",
                                           "reduce subsidies on fertilisers",
                                           "subsidies on energy consumption",
                                           "subsidies on housing loans",
                                           "(increase) subsidies on agrochemicals",
                                           "(increase) subsidies on farm inputs")

measure_class[["international trade agreements"]] <- c("trade liberalisation",
                                            "trade deliberalisation",
                                            "accession into trade partnership",
                                            "open for trade with neighbouring countries",
                                            "reduce non-tariff trade barriers")

measure_class[["other"]] <- c("rice self-sufficiency programme", "two-child policy", "rural settlement consolidation", "allow land rental", 
                              "land market closure", "land reform/redistribution", "liberalisation of fertiliser prices")

measure_class[["activity- or sector-based investments"]] <- c("(increased) investment in agricultural sector",
                                            "increased investment general",
                                            "investment in agricultural machinery & equipment",
                                            "investment in fertiliser industry",
                                            "investment in road infrastructure",
                                            "(increase) investment in agricultural sector",
                                            "(increase) investment in agricultural research",
                                            "(increase) investment in extension services",
                                            "(increase) investment in agricultural technology development",
                                            "(increase) investment in farm inputs",
                                            "(increase) investment in irrigation",
                                            "(increase) investment in infrastructure",
                                            "investment in market infrastructure")

measure_class[["unclear"]] <- "unclear"

measure_class[["import policies"]] <- c("import tariffs on rice",
                                        "reduce import tariffs on fertilizer",
                                        "abolish quantitative rice import quotas",
                                        "reduced import tariffs on rice",
                                        "reduce import tariffs on fertiliser",
                                        "import tax on X",
                                        "reduce import tax on X",
                                        "reduce/remove import tariffs on X",
                                        "remove import tariffs",
                                        "reduce import tariffs on X",
                                        "eliminate import tariffs on X")

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
                                                          "abolish production subsidies for X",
                                                          "create off-farm off-season work opportunities",
                                                          "minimum support price for X",
                                                          "direct payments",
                                                          "conditional agricultural production subsidies based on target prices of X",
                                                          "remove conditional agricultural production subsidies based on target prices of X",
                                                          "remove direct payments",
                                                          "(increase) direct payments",
                                                          "compensation for resource dispossession due to infrastructure construction")

measure_class[["nature-centered measures"]] <- c("soil erosion control",
                                                 "afforestation (also agro-forestry)",
                                                 "disaster control")
