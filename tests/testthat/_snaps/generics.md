# generics, ranger

    Code
      print(fc.ranger)
    Output
      faircause object:
      
      Attribute:        x 
      Outcome:          y 
      Confounders:      z 
      Mediators:        w 

---

    Code
      summary(fc.ranger)
    Output
      faircause object summary: 
      
      Call:
      fairness_cookbook(data = data, X = "x", W = "w", Z = "z", Y = "y", 
          x0 = 0, x1 = 1)
      
      Protected attribute:                 x
      Protected attribute levels:          0, 1
      Total Variation (TV): 0.282415 
      TV decomposition(s) :
      
      TV_x0x1(y) (0.282415) = CtfDE_x0x1(y | x0) (0.3226957) - CtfIE_x1x0(y | x0) (-0.02969135) - CtfSE_x1x0(y | x0) (0.03633649)

# generics, linear

    Code
      print(fc.lin)
    Output
      faircause object:
      
      Attribute:        x 
      Outcome:          y 
      Confounders:      z 
      Mediators:        w 

---

    Code
      summary(fc.lin)
    Output
      faircause object summary: 
      
      Call:
      fairness_cookbook(data = data, X = "x", W = "w", Z = "z", Y = "y", 
          x0 = 0, x1 = 1, model = "linear")
      
      Protected attribute:                 x
      Protected attribute levels:          0, 1
      Total Variation (TV): 0.282415 
      TV decomposition(s) :
      
      TV_x0x1(y) (0.282415) = CtfDE_x0x1(y | x0) (0.2510988) - CtfIE_x1x0(y | x0) (-0.08145256) - CtfSE_x1x0(y | x0) (0.03633649)

