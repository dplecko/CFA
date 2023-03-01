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
      fairness_cookbook(data = data, X = "x", Z = "z", W = "w", Y = "y", 
          x0 = 0, x1 = 1)
      
      Protected attribute:                 x
      Protected attribute levels:          0, 1
      Total Variation (TV): 0.282415 
      TV decomposition(s) :
      
      TV_x0x1(y) (0.282415) = CtfDE_x0x1(y | x0) (0.1428479) - CtfIE_x1x0(y | x0) (-0.2193592) - CtfSE_x1x0(y | x0) (0.07979211)

---

    Code
      summary(fc.ranger, decompose = "general")
    Output
      faircause object summary: 
      
      Call:
      fairness_cookbook(data = data, X = "x", Z = "z", W = "w", Y = "y", 
          x0 = 0, x1 = 1)
      
      Protected attribute:                 x
      Protected attribute levels:          0, 1
      Total Variation (TV): 0.282415 
      TV decomposition(s) :
      
      TV_x0x1(y) (0.282415) = NDE_x0x1(y) (-0.1107859) - NIE_x1x0(y) (-0.2465106) + ExpSE_x0(y) (-0.1841516) - ExpSE_x1(y) (-0.0374613)

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
      fairness_cookbook(data = data, X = "x", Z = "z", W = "w", Y = "y", 
          x0 = 0, x1 = 1, model = "linear")
      
      Protected attribute:                 x
      Protected attribute levels:          0, 1
      Total Variation (TV): 0.282415 
      TV decomposition(s) :
      
      TV_x0x1(y) (0.282415) = CtfDE_x0x1(y | x0) (0.347797) - CtfIE_x1x0(y | x0) (0.01535178) - CtfSE_x1x0(y | x0) (0.05003016)

