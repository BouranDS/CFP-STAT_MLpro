##############################################################################################
# Regression methods------->                                                                 #
#                 regression ridge with exact and gradient descent on batch approaches       #
##############################################################################################
# require package R6 for the use of R6Class in order to get the similarity with python object#
#                                                                                            #
RidgeRegression <- R6Class(classname = "RidgeRegression", 
                           public = list(
                             coef = NULL,
                             my_mean = NULL,
                             my_sd = NULL,
                             lambda = NULL,
                             initialize = function(lambda){
                               self$lambda <- lambda
                             },
                             scaler = function(x){
                               self$my_mean <- base::apply(X = x, MARGIN = 2, 
                                                           FUN = mean)
                               self$my_sd   <- base::apply(X = x, MARGIN = 2, 
                                                           FUN = function(e){
                                                             sd(e)*(sqrt((length(e)-1)/length(e)))})
                             },
                             transformer = function(x){
                               transformation <- t((t(x)-self$my_mean)/self$my_sd)
                               
                             },
                             fit = function(x, y, solver){
                               self$scaler(x) # to compute the scale parameters
                               x_transformed <- self$transformer(x)
                               ncol_x <-  base::ncol(x)
                               nrow_x <-  base::nrow(x)
                               x_transformed <-  cbind(1,x_transformed)
                               sqrt_lambda   <-  base::sqrt(self$lambda)
                               x_only_lambda <-  cbind(0,base::diag(sqrt_lambda, 
                                                                    nrow = ncol_x,
                                                                    ncol = ncol_x)
                               )
                               x_transformed_lambda <- rbind(x_transformed,
                                                             x_only_lambda)
                               y <- base::matrix(y, nrow = length(y) , ncol = 1)
                               zero_y <- base::matrix(rep(0,ncol_x), 
                                                      nrow = ncol_x,
                                                      ncol = 1)
                               y_lambda <- rbind(y,zero_y)
                               if(solver == 'qr'){
                                 self$qr_solver(x_transformed_lambda,y_lambda)
                               }else if(solver=='sdv'){
                                 self$sdv_solver(x_transformed_lambda,y_lambda)
                               }
                               
                             },
                             qr_solver = function(x,y){
                               self$coef <- base::qr.coef(qr(x),y)
                             },
                             sdv_solver = function(x,y){
                               sdv <- base::svd(x)
                               inter_mat <- ((sdv$v %*% (base::diag(1/sdv$d))) %*% t(sdv$u)) 
                               self$coef <- inter_mat %*% y
                             },
                             predict = function(x_new){
                               test_dim <- base::dim(x_new)
                               if(is.null(test_dim)){
                                 x_new <- base::matrix(x_new, ncol = base::length(self$coef))
                               }
                               x_new_transformed <- self$transformer(x_new)
                               prediction <- cbind(
                                 rep(1,nrow(x_new)),x_new_transformed) %*% self$coef
                               return(prediction)
                             }
                           )
)