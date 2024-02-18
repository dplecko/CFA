
import pandas as pd
import torch
import torch.nn as nn
import torch.nn.functional as F
import copy
import numpy as np
from sklearn.linear_model import LogisticRegression

class TwoLayerArchitecture(nn.Module):
    def __init__(self, input_size, hidden_size, output_size):
        super(TwoLayerArchitecture, self).__init__()
        # Define the architecture here.
        self.layer1 = nn.Linear(input_size, hidden_size)
        self.layer2 = nn.Linear(hidden_size, hidden_size)
        self.output_layer = nn.Linear(hidden_size, output_size)
    
    def forward(self, x):
        # Define the forward pass here.
        x = F.relu(self.layer1(x))
        x = F.relu(self.layer2(x))
        x = self.output_layer(x)
        return x

# Custom loss for direct effect (!)
def causal_loss(pred, pred0, pred1, X, Z, W, Y, px_z, eta_de, eta_ie, eta_se_x1, 
                eta_se_x0, relu_eps, eps, task_type):

    if task_type == 'classification':
        pred_prob = torch.sigmoid(pred).squeeze()
        pred_prob1 = torch.sigmoid(pred1).squeeze()
        pred_prob0 = torch.sigmoid(pred0).squeeze()
    else:
        pred_prob = pred.squeeze()
        pred_prob1 = pred1.squeeze()
        pred_prob0 = pred0.squeeze()
    X_sq = X.squeeze()
    
    # get P(x | z) model
    px = X.mean()
    
    # get f_{x_1, W_{x_0}}
    wgh0 = (1 - px) / (1 - px_z)
    fx1_wx0 = (pred_prob1[X_sq == 0] * wgh0[X_sq == 0]).sum() / (wgh0[X_sq == 0].sum())

    # get f_{x_0, W_{x_0}}
    fx0_wx0 = (pred_prob0[X_sq == 0] * wgh0[X_sq == 0]).sum() / (wgh0[X_sq == 0].sum())
    
    # get f_{x_1, W_{x_1}}
    wgh1 = px / (px_z)
    fx1_wx1 = (pred_prob1[X_sq == 1] * wgh1[X_sq == 1]).sum() / (wgh1[X_sq == 1].sum())
    
    # get f | x0
    f_x0 = pred_prob[X_sq == 0].mean()
    
    # get f | x1
    f_x1 = pred_prob[X_sq == 1].mean()
    
    # \sum_i=1^n [f(x1, w) - f(x0, w)] * 1 / n (direct effect)
    nde_loss = torch.abs(fx1_wx0 - fx0_wx0 - eta_de)
    nie_loss = torch.abs(fx1_wx0 - fx1_wx1 - eta_ie)
    nse_loss_x1 = torch.abs(f_x1 - fx1_wx1 - eta_se_x1) 
    nse_loss_x0 = torch.abs(f_x0 - fx0_wx0 - eta_se_x0)
    
    # in ReLU style, penalize only larger deviations (and use larger \lambda)
    if relu_eps:
      nde_loss = torch.relu(nde_loss - eps)
      nie_loss = torch.relu(nie_loss - eps)
      nse_loss_x1 = torch.relu(nse_loss_x1 - eps)
      nse_loss_x0 = torch.relu(nse_loss_x0 - eps)
    
    custom_loss = nde_loss + nie_loss + nse_loss_x1 + nse_loss_x0
    return custom_loss

# Training with Early Stopping
def train_w_es(train_data, eval_data, x_col, z_cols, w_cols, y_col, lmbd, lr=0.001, 
               epochs=500, patience=20, max_restarts=5, eval_size=1000, 
               nde=True, nie=True, nse=True, eta_de=0, eta_ie=0, eta_se_x1=0, eta_se_x0=0,
               verbose=False, loss=False, relu_eps=False, eps = 0.005, batch_size=512):

    # decide which effects are nullified based on the BN set
    if nde:
      eta_de = 0
    if nie:
      eta_ie = 0
    if nse:
      eta_se_x0 = eta_se_x1 = 0

    # Initial partition of data
    X_train = train_data[x_col].values.reshape(-1, 1)
    Z_train = train_data[z_cols].values.reshape(-1, len(z_cols))
    W_train = train_data[w_cols].values.reshape(-1, len(w_cols))
    Y_train = train_data[y_col].values.reshape(-1, 1).squeeze()

    # Determine whether the task is classification or regression
    task_type = 'regression' if len(np.unique(Y_train)) > 2 else 'classification'

    # Split data into training and evaluation sets
    X_eval = eval_data[x_col].values.reshape(-1, 1)
    Z_eval = eval_data[z_cols].values.reshape(-1, len(z_cols))
    W_eval = eval_data[w_cols].values.reshape(-1, len(w_cols))
    Y_eval = eval_data[y_col].values.reshape(-1, 1).squeeze()

    # Now you can fit the logistic regression or any other model on the training data
    log_reg = LogisticRegression()
    log_reg.fit(Z_train, X_train.ravel())
    
    # Fit also on evaluation set
    eval_log_reg = LogisticRegression()
    eval_log_reg.fit(Z_eval, X_eval.ravel())

    # Assuming you want to compute px_z_t for training set to use it later in your model
    px_z_t = torch.tensor(log_reg.predict_proba(Z_train)[:, 1], dtype=torch.float)
    eval_px_z_t = torch.tensor(eval_log_reg.predict_proba(Z_eval)[:, 1], 
                               dtype=torch.float)

    # Prepare tensors for PyTorch model training
    X_train_t = torch.tensor(X_train, dtype=torch.float)
    Z_train_t = torch.tensor(Z_train, dtype=torch.float)
    W_train_t = torch.tensor(W_train, dtype=torch.float)
    Y_train_t = torch.tensor(Y_train, dtype=torch.float)
    
    fts_train = np.hstack([X_train, Z_train, W_train])
    
    # Also prepare evaluation data as tensors if needed for validation
    X_eval_t = torch.tensor(X_eval, dtype=torch.float)
    Z_eval_t = torch.tensor(Z_eval, dtype=torch.float)
    W_eval_t = torch.tensor(W_eval, dtype=torch.float)
    Y_eval_t = torch.tensor(Y_eval, dtype=torch.float)
    
    fts_eval = np.hstack([X_eval, Z_eval, W_eval])

    column_names = (
            [x_col] +
            (list(z_cols) if isinstance(z_cols, (list, np.ndarray)) else [z_cols]) +
            (list(w_cols) if isinstance(w_cols, (list, np.ndarray)) else [w_cols])
    )
    x_idx = column_names.index(x_col)

    best_global_loss = float('inf')
    best_model_global = None
    restarts = 0

    while restarts <= max_restarts:
        model = TwoLayerArchitecture(input_size=fts_train.shape[1], hidden_size=16,
                                     output_size=1)

        loss_fn = nn.MSELoss() if task_type == 'regression' else nn.BCEWithLogitsLoss()
        optim = torch.optim.Adam(model.parameters(), lr=lr)

        fts_train_t = torch.tensor(fts_train, dtype=torch.float)
        lbl_train_t = torch.tensor(Y_train, dtype=torch.float).unsqueeze(1)
        fts_eval_t = torch.tensor(fts_eval, dtype=torch.float)
        lbl_eval_t = torch.tensor(Y_eval, dtype=torch.float).unsqueeze(1)
        
        fts0 = fts_train_t.clone()
        fts1 = fts_train_t.clone()
        fts0[:, x_idx] = 0
        fts1[:, x_idx] = 1
        eval_fts0 = fts_eval_t.clone()
        eval_fts1 = fts_eval_t.clone()
        eval_fts0[:, x_idx] = 0
        eval_fts1[:, x_idx] = 1
        
        best_loss = float('inf')
        counter = 0

        model.train()
        for epoch in range(epochs):
            model.train()
            # Implement mini-batch training
            permutation = torch.randperm(fts_train_t.size()[0])
            for i in range(0, fts_train_t.size()[0], batch_size):

                indices = permutation[i:i+batch_size]
                batch_fts, batch_lbl = fts_train_t[indices], lbl_train_t[indices]

                # Adjustments for intervened features for the custom loss
                batch_fts0 = batch_fts.clone()
                batch_fts1 = batch_fts.clone()
                batch_fts0[:, x_idx] = 0
                batch_fts1[:, x_idx] = 1
                
                pred = model(batch_fts)
                pred0 = model(batch_fts0)
                pred1 = model(batch_fts1)

                bce_loss = loss_fn(pred, batch_lbl)
                custom_loss = causal_loss(pred, pred0, pred1,
                                          batch_fts[:, 0:1], 
                                          batch_fts[:, 1:len(z_cols)+1], 
                                          batch_fts[:, len(z_cols)+1:], batch_lbl, 
                                          px_z_t[indices], eta_de, eta_ie, 
                                          eta_se_x1, eta_se_x0,
                                          relu_eps, eps, task_type)
                
                total_loss = bce_loss + lmbd * custom_loss

                optim.zero_grad()
                total_loss.backward()
                optim.step()

            model.eval()
            with torch.no_grad():

                eval_pred = model(fts_eval_t)
                eval_pred0 = model(eval_fts0)
                eval_pred1 = model(eval_fts1)
                
                eval_bce_loss = loss_fn(eval_pred, lbl_eval_t)
                eval_causal_loss = causal_loss(
                  eval_pred, eval_pred0, eval_pred1,
                  X_eval_t, Z_eval_t, W_eval_t, Y_eval_t, 
                  eval_px_z_t, eta_de, eta_ie, eta_se_x1, eta_se_x0,
                  relu_eps, eps, task_type
                )
                eval_tot_loss = eval_bce_loss + lmbd * eval_causal_loss
                
            model.train()

            if eval_tot_loss < best_loss:
                best_loss = eval_tot_loss
                counter = 0
                best_model = copy.deepcopy(model)
            else:
                counter += 1

            if verbose and (epoch + 1) % 50 == 0:
                print(f'E {epoch+1}, '
                      f'Train BCE: {bce_loss.item():.4}, '
                      f'Causal Loss: {custom_loss.item():.4}, '
                      f'Eval BCE: {eval_bce_loss.item():.4}, ',
                      f'Eval Causal: {eval_causal_loss.item():.4}, ')

            if counter >= patience:
                if verbose:
                    print(f'Restarting at E {epoch+1}, Restart {restarts+1}/{max_restarts}')
                break

        if best_loss < best_global_loss:
            best_global_loss = best_loss
            best_model_global = best_model

        if restarts == max_restarts or counter < patience:
            if verbose:
                print("Max restarts reached or no early stop. Stopping.")
            break

        restarts += 1

    # best_model_global.eval()
    # with torch.no_grad():
    #     test_preds = torch.sigmoid(best_model_global(fts_test_t))
    # 
    # return test_preds.numpy()
    return best_model_global

def pred_nn_proba(model, test_data, task_type):
    """
    Generate predictions using a trained model and test features.

    Parameters:
    - model: Trained PyTorch model.
    - test_features: Test features as a PyTorch tensor.

    Returns:
    - Numpy array of predictions.
    """
    
    # convert test data to numpy
    test_arr = test_data.values
    
    # create the test data tensor
    fts_test_t = torch.tensor(test_arr, dtype=torch.float)
    
    model.eval()  # Set the model to evaluation mode
    with torch.no_grad():  # Ensure gradients are not computed for inference
        predictions = model(fts_test_t)
        if task_type == 'classification':
            predictions = torch.sigmoid(predictions)  # Apply sigmoid to get probabilities
    return predictions.numpy()
  
# learning_rates = [0.1, 0.01, 0.001, 0.0001]
# best_lr = None
# lowest_loss = float('inf')
# 
# for lr in learning_rates:
#     print(f'Training with lr={lr}')
#     eval_loss = train_w_es(compas_data, lmbd=0.1, lr=lr, epochs=1000, patience=5,
#                            loss=True) 
#     if eval_loss < lowest_loss:
#         lowest_loss = eval_loss
#         best_lr = lr
# 
# print(f'Best learning rate: {best_lr} with loss: {lowest_loss}')

# train_w_es(compas_data, test_data = compas_data,
#            x_col = ['race'], y_col = ['two_year_recid'],
#            w_cols = ['juv_fel', 'juv_misd', 'juv_other', 'priors', 'charge'],
#            z_cols = ['sex', 'age'],
#            lmbd=1, lr=0.001, epochs=1000, patience=100, verbose=True, batch_size=256)
