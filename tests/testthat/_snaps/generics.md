# test generics for combinations of method, model

    Code
      print(fc)
    Output
      faircause object:
      
      Attribute:        x 
      Outcome:          y 
      Confounders:      z 
      Mediators:        w 

---

    Code
      summary(fc)
    Output
      faircause object summary: 
      
      Call:
      fairness_cookbook(data = data, X = "x", Z = "z", W = "w", Y = "y", 
          x0 = 0, x1 = 1, method = method_inner, model = model_inner)
      
      Protected attribute:                 x
      Protected attribute levels:          0, 1
      Total Variation (TV): 0.282415 
      TV decomposition(s) :
      
      TV_x0x1(y) (0.282415) = CtfDE_x0x1(y | x0) (0.2465689) - CtfIE_x1x0(y | x0) (-0.1156382) - CtfSE_x1x0(y | x0) (0.07979211)

---

    Code
      summary(fc, decompose = "general")
    Output
      faircause object summary: 
      
      Call:
      fairness_cookbook(data = data, X = "x", Z = "z", W = "w", Y = "y", 
          x0 = 0, x1 = 1, method = method_inner, model = model_inner)
      
      Protected attribute:                 x
      Protected attribute levels:          0, 1
      Total Variation (TV): 0.282415 
      TV decomposition(s) :
      
      TV_x0x1(y) (0.282415) = NDE_x0x1(y) (-0.06347852) - NIE_x1x0(y) (-0.1992033) + ExpSE_x0(y) (-0.1841516) - ExpSE_x1(y) (-0.0374613)

---

    Code
      print(fc)
    Output
      faircause object:
      
      Attribute:        x 
      Outcome:          y 
      Confounders:      z 
      Mediators:        w 

---

    Code
      summary(fc)
    Output
      faircause object summary: 
      
      Call:
      fairness_cookbook(data = data, X = "x", Z = "z", W = "w", Y = "y", 
          x0 = 0, x1 = 1, method = method_inner, model = model_inner)
      
      Protected attribute:                 x
      Protected attribute levels:          0, 1
      Total Variation (TV): 0.282415 
      TV decomposition(s) :
      
      TV_x0x1(y) (0.282415) = CtfDE_x0x1(y | x0) (0.3521946) - CtfIE_x1x0(y | x0) (0.01974941) - CtfSE_x1x0(y | x0) (0.05003016)

---

    Code
      summary(fc, decompose = "general")
    Output
      faircause object summary: 
      
      Call:
      fairness_cookbook(data = data, X = "x", Z = "z", W = "w", Y = "y", 
          x0 = 0, x1 = 1, method = method_inner, model = model_inner)
      
      Protected attribute:                 x
      Protected attribute levels:          0, 1
      Total Variation (TV): 0.282415 
      TV decomposition(s) :
      
      TV_x0x1(y) (0.282415) = NDE_x0x1(y) (0.318212) - NIE_x1x0(y) (-0.01434307) + ExpSE_x0(y) (0.02247164) - ExpSE_x1(y) (-0.02766845)

---

    Code
      print(fc)
    Output
      faircause object:
      
      Attribute:        x 
      Outcome:          y 
      Confounders:      z 
      Mediators:        w 

---

    Code
      summary(fc)
    Output
      faircause object summary: 
      
      Call:
      fairness_cookbook(data = data, X = "x", Z = "z", W = "w", Y = "y", 
          x0 = 0, x1 = 1, method = method_inner, model = model_inner)
      
      Protected attribute:                 x
      Protected attribute levels:          0, 1
      Total Variation (TV): 0.282415 
      TV decomposition(s) :
      
      TV_x0x1(y) (0.282415) = CtfDE_x0x1(y | x0) (0.2890577) - CtfIE_x1x0(y | x0) (-0.03976944) - CtfSE_x1x0(y | x0) (0.04641213)

---

    Code
      summary(fc, decompose = "general")
    Output
      faircause object summary: 
      
      Call:
      fairness_cookbook(data = data, X = "x", Z = "z", W = "w", Y = "y", 
          x0 = 0, x1 = 1, method = method_inner, model = model_inner)
      
      Protected attribute:                 x
      Protected attribute levels:          0, 1
      Total Variation (TV): 0.282415 
      TV decomposition(s) :
      
      TV_x0x1(y) (0.282415) = NDE_x0x1(y) (0.2838739) - NIE_x1x0(y) (-0.0397003) + ExpSE_x0(y) (NA) - ExpSE_x1(y) (NA)

---

    Code
      print(fc)
    Output
      faircause object:
      
      Attribute:        x 
      Outcome:          y 
      Confounders:      z 
      Mediators:        w 

---

    Code
      summary(fc)
    Output
      faircause object summary: 
      
      Call:
      fairness_cookbook(data = data, X = "x", Z = "z", W = "w", Y = "y", 
          x0 = 0, x1 = 1, method = method_inner, model = model_inner)
      
      Protected attribute:                 x
      Protected attribute levels:          0, 1
      Total Variation (TV): 0.2809984 
      TV decomposition(s) :
      
      TV_x0x1(y) (0.2809984) = CtfDE_x0x1(y | x0) (0.2973412) - CtfIE_x1x0(y | x0) (-0.08442102) - CtfSE_x1x0(y | x0) (0.1007638)

---

    Code
      summary(fc, decompose = "general")
    Output
      faircause object summary: 
      
      Call:
      fairness_cookbook(data = data, X = "x", Z = "z", W = "w", Y = "y", 
          x0 = 0, x1 = 1, method = method_inner, model = model_inner)
      
      Protected attribute:                 x
      Protected attribute levels:          0, 1
      Total Variation (TV): 0.2809984 
      TV decomposition(s) :
      
      TV_x0x1(y) (0.2809984) = NDE_x0x1(y) () - NIE_x1x0(y) () + ExpSE_x0(y) () - ExpSE_x1(y) ()

