# count different codes
ids <- which(quotes_long$code_group == "per model - type")
unique(quotes_long$name[ids])
length(unique(quotes_long$name[ids]))

ids <- which(quotes_long$code_group == "per model - domain")
unique(quotes_long$name[ids])
length(unique(quotes_long$name[ids]))

ids <- which(quotes_long$code_group == "per interaction - exchange")
unique(quotes_long$name[ids])
length(unique(quotes_long$name[ids]))

ids <- which(quotes_long$code_group == "spatial & temporal - representation")
unique(quotes_long$name[ids])
length(unique(quotes_long$name[ids]))

ids <- which(quotes_long$code_group == "modelling - data")
sort(unique(quotes_long$name[ids]))
length(unique(quotes_long$name[ids]))

ids <- which(quotes_long$code_group == "agent - representation")
sort(unique(quotes_long$name[ids]))
length(unique(quotes_long$name[ids]))

ids <- which(quotes_long$code_group == "food system - echelon")
sort(unique(quotes_long$name[ids]))
length(unique(quotes_long$name[ids]))

ids <- which(quotes_long$code_group == "per agent - source heterogeneity")
sort(unique(quotes_long$name[ids]))
length(unique(quotes_long$name[ids]))

ids <- which(quotes_long$code_group == "agent - method")
sort(unique(quotes_long$name[ids]))
length(unique(quotes_long$name[ids]))

ids <- which(quotes_long$code_group == "agent - paradigm")
sort(unique(quotes_long$name[ids]))
length(unique(quotes_long$name[ids]))

ids <- which(quotes_long$code_group == "agent - theory")
sort(unique(quotes_long$name[ids]))
length(unique(quotes_long$name[ids]))

ids <- which(quotes_long$code_group == "per measure - formulation")
sort(unique(quotes_long$name[ids]))
length(unique(quotes_long$name[ids]))

ids <- which(quotes_long$code_group == "per measure - type 2")
sort(unique(quotes_long$name[ids]))
length(unique(quotes_long$name[ids]))

ids <- which(quotes_long$code_group == "per effect - type other")
sort(unique(quotes_long$name[ids]))
length(unique(quotes_long$name[ids]))

ids <- which(quotes_long$code_group == "modelling - aim")
sort(unique(quotes_long$name[ids]))
length(unique(quotes_long$name[ids]))
