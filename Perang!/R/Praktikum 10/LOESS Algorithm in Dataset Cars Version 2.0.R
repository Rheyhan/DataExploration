x = c(0.5578,2.0217,2.5773,3.4140,4.3014,4.7448,5.1074,6.5412,6.7216,7.2601)
y = c(17.7780,108.6017,142.2226,166.9969,205.8401,217.6891,224.9667,232.0901,231.1128,226.4877)
data_xy = data.frame(cbind(x,y))

# =============================
loess_basic = function(data,degree,q,col){
  loessmod = loess(data[,2]~data[,1], data = data, span = q, degree = degree)
  smoothed = predict(loessmod)
  lines(smoothed, x = data[,1], col = col, lwd = 1)
}

# =============================
loess_audhi = function(data, degree, q, col){
  data = as.data.frame(data)
  length_data = dim(data)[1]
  nq = length_data*q
  sort(data[,1], decreasing = FALSE)
  difference_x = matrix(0, ncol = length_data, nrow = length_data)
  difference_x_abs = matrix(0, ncol = length_data, nrow = length_data)
  value_h = c(0, ncol = length_data)
  sort_data = matrix(0, ncol = length_data, nrow = length_data)
  value_z = matrix(0, ncol = length_data, nrow = length_data)
  value_w = matrix(0, ncol = length_data, nrow = length_data)
  value_b = matrix(0, ncol = length_data, nrow = 2)
  predicted_y = matrix(0, ncol = length_data, nrow = 1)
  
  if (((degree + 1)/length_data) <= q & q <= 1) {
    for (i in 1:length_data) {
      focal_point = data[i,1]
      difference_x[,i] = data[,1] - focal_point
      difference_x_abs[,i] = abs(data[,1] - focal_point)
      sort_data[,i] = sort(difference_x_abs[,i])
      value_h[i] = sort_data[nq,i]
      value_z[,i] = difference_x_abs[,i]/value_h[i]
      
      for (j in 1:length_data) {
        if (abs(value_z[j,i]) < 1) {
          value_w[j,i] = (1-(abs(value_z[j,i]))^3)^3
        } else {
          value_w[j,i] = 0 
        }
      }
      
      data_x = as.vector(data[,1])
      rep_1 = c(rep(1,length_data))
      x = cbind(rep_1,data_x)
      value_b[,i] = solve(t(x) %*% diag(value_w[,i], ncol = length_data, nrow = length_data) %*% x) %*% t(x) %*% diag(value_w[,i], ncol = length_data, nrow = length_data) %*% as.vector(data[,2])
      
      predicted_y[i] = value_b[1,i] + value_b[2,i]*data[i,1]
    }
  } else {
    warning('Value q (Smoothing Parameter) is out of bound)')
  }
    
  list_answer = list(Difference_X =difference_x,Value_h = value_h, Value_Z = value_z, Value_W = value_w, Value_b = value_b, Predicted_y = predicted_y)
  lines(predicted_y, x = data[,1], col = col)
  #return(list_answer)
}

# EXECUTE THE FUNCTION
plot(x = data_xy$x,
     y = data_xy$y)
loess_audhi(data = data_xy,degree = 2,q = 0.7,col = 'green')

loess_basic(data = data_xy,degree = 2,q = 0.7,col = 'blue')
