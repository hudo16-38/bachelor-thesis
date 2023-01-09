library(foreign)
library(psych)
library(mokken)
library(mirt)
library(lme4)
library(robustlmm)

load("robust.RData")
load("robustA.RData")


rf1 = ranef(l1rA)
rf2 = ranef(l2rA)
rf3 = ranef(l3rA)
rf4 = ranef(l4rA)
rf5 = ranef(l5rA)
rf6 = ranef(l6rA)
rf7 = ranef(l7rA)
rf8 = ranef(l8rA)

odbor.flex = rf1$`odbor:(fakulta:VS)`
dim(odbor.flex)

