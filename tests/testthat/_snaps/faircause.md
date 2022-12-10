# fairness_cookbook_eo works

    Code
      summary(ran)
    Output
      faircause object summary: 
      
      Call:
      fairness_cookbook(data = data[y_idx, ], X = X, W = W, Z = Z, 
          Y = Yhat, x0 = x0, x1 = x1, eo = TRUE)
      
      Protected attribute:                 x
      Protected attribute levels:          0, 1
      Error Rate (ER): 0.05257591 
      ER decomposition :
      
      ER_x0x1(yhat | y) (0.05257591) = ERde_x0x1(yhat | x0, y) (0.06545695) - ERie_x1x0(yhat | x0, y) (-0.01773636) - ERse_x1x0(yhat | y) (0.007642455)

