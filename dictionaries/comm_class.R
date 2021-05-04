# dictionary grouping food system commodities into the categories, using FAOs commodity list: http://www.fao.org/waicent/faoinfo/economic/faodef/annexe.htm 
# cereals & cereal products, fruits and derived products, vegetables and derived products, sugar crops and sweeteners and derived products, fibres of vegetal and animal origin, composite foods, 
# seeds for planting, non-food tree crops and derived products, products from live animals, oil-bearing crops and derived products, products from slaughtered animals, livestock, 
# stimulant crops and derived products, beverages, roots and tubers and derived products, pulses and derived products, fodder crops and derived products, nuts and derived products, 
# beverage crops and spices, vegetal and animal oils and fats, hides and skins, other commodities. * indicate that commodity group is not part of the FAO commodity list.



comm_class <- list()
comm_class[["cereals & cereal products"]] <- c("rice", "maize", "wheat", "sorghum", "millet", "grain", "coarse grains", "barley", "bread", "maize flour", "oats", "other cereals", "pasta", 
                                                 "refined grains", "teff", "whole grains", "cereals", "maize hybrid/improved", "maize local")
comm_class[["fruits and derived products"]] <- c("fruit", "plantain", "avocado", "banana", "mango")
comm_class[["vegetables and derived products"]] <- c("vegetable", "onion", "tomato", "broccoli", "cabbage", "garlic")
comm_class[["sugar crops and sweeteners, and derived products"]] <- c("sugar", "sugarcane", "candy")
comm_class[["fibres of vegetal and animal origin"]] <- c("cotton", "plant-based fibers", "raw cotton")
comm_class[["compisite foods*"]] <- c("food", "other food", "food at home", "food away from home", "processed food", "staple food",
                                          "healthy food", "unhealthy food", "food of plant origin", "own-produced food", "livestock products")
comm_class[["composite crops*"]] <- c("crops","trees", "other crops", "cash crops", "horticultural crops", "industrial crops", "non-food crops", 
                                        "other starchy staples", "export crops", "food crops")
comm_class[["seeds for planting*"]] <- c("hybrid/improved maize seed", "seeds", "local maize seed")
comm_class[["non-food tree crops, and derived products*"]] <- c("wood", "eucalyptus", "wood products")
comm_class[["products from live animals"]] <- c("milk", "dairy", "egg")
comm_class[["oil-bearing crops and derived products"]] <- c("groundnut", "soybean", "coconut", "jatropha", "linseed", "oil palm", "oil seeds", "oilseed", "palm oil", "rape seed", "sesame", 
                                                              "peanut")
comm_class[["products from slaughtered animals"]] <- c("meat", "fish", "beef", "other meat")
comm_class[["livestock"]] <- c("cattle", "goat", "poultry", "sheep", "livestock", "broiler chicken", "buffalo", "camel", "chicken", "cow", "other livestock", "oxen",
                                 "crossbred cattle", "laying hen", "male calf")
comm_class[["stimulant crops and derived products"]] <- c("tobacco", "coffee", "cocoa", "chat", "cocoa beans", "tea")
comm_class[["beverages"]] <- c("beverages", "alcoholic beverages", "carbonated soft drinks")
comm_class[["roots and tubers, and derived products"]] <- c("cassava", "potato", "roots", "sweet potato", "tubers", "yam", "cocoyam")
comm_class[["pulses and derived products"]] <- c("cowpea", "pulses", "beans", "chickpea", "dry beans", "faba bean", "haricot bean", "legumes", "lentil", "vetch")
comm_class[["fodder crops and derived products"]] <- c("animal feed", "feedgrain", "fodder crop", "fonio", "grass")
comm_class[["nuts and derived products"]] <- c("cashews", "nuts")
comm_class[["beverage crops and spices"]] <- c("hop")
comm_class[["vegetable and animal oils and fats"]] <- c("edible oil", "biodiesel", "vegetable oil")
comm_class[["hides and skins"]] <- c("leather")
comm_class[["other commodities*"]] <- c("enset", "salt")