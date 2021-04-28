LoughranMcDonald_MasterDictionary_2016 <- read.csv("~/Downloads/LoughranMcDonald_MasterDictionary_2016.csv")

nrow(LoughranMcDonald_MasterDictionary_2016)

summary(LoughranMcDonald_MasterDictionary_2016$Sequence.Number)

table(LoughranMcDonald_MasterDictionary_2016$Negative,
      LoughranMcDonald_MasterDictionary_2016$Positive)

LoughranMcDonald_MasterDictionary_2016$kevid <- NA

kev_disjoint_id <- function(currcol) {
  if (currcol*LoughranMcDonald_MasterDictionary_2016$Positive != 0) {
    LoughranMcDonald_MasterDictionary_2016$kevid <- 1
  } else if (currcol*LoughranMcDonald_MasterDictionary_2016$Negative != 0) {
    LoughranMcDonald_MasterDictionary_2016$kevid <- 1
  } else if (currcol*LoughranMcDonald_MasterDictionary_2016$Litigious != 0) {
    LoughranMcDonald_MasterDictionary_2016$kevid <- 1
  } else if (currcol*LoughranMcDonald_MasterDictionary_2016$Modal != 0) {
    LoughranMcDonald_MasterDictionary_2016$kevid <- 1
  } else if (currcol*LoughranMcDonald_MasterDictionary_2016$Uncertainty != 0) {
    LoughranMcDonald_MasterDictionary_2016$kevid <- 1
  } else if (currcol*LoughranMcDonald_MasterDictionary_2016$Constraining != 0) {
    LoughranMcDonald_MasterDictionary_2016$kevid <- 1
  } else {
    LoughranMcDonald_MasterDictionary_2016$kevid <- 0
  }
}

kev_disjoint_id(LoughranMcDonald_MasterDictionary_2016$Positive)

summary(LoughranMcDonald_MasterDictionary_2016$kevid)

LoughranMcDonald_MasterDictionary_2016$Positive * LoughranMcDonald_MasterDictionary_2016$Negative != 0
