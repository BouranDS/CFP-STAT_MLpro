###########################################################
# linear regression with exact approach                   #
# this is based on R6 class from R6 package               #
###########################################################
# To make this work R6 is required                        #
#                                                         #
LinearRegression <- R6::R6Class(
  classname = "LinearRegression", 
  public = list(
    coef = NULL,
    initialize = function(){
      
    },
    fit = function(x, y, solver = 'qr'){
      if(solver == 'svd'){
        self$sdv_solver(cbind(1,x),y)
      }else{
        self$qr_solver(cbind(1,x),y)
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
    predict = function(x){
      test_dim <- base::dim(x)
      if(is.null(test_dim)){
        x <- base::matrix(x, ncol = length(self$coef))
      }
      base::cbind(1,x) %*% self$coef
    },
    afficher = function(){
      resultat <- 'y = '
      if (base::is.null(self$coef)){
        resultat <- base::cat(resultat,'...', sep ='')
      }else{
        n_size = base::length(self$coef)
        reg_info = base::matrix(data=NA, nrow = n_size, ncol = 3) 
        reg_info = base::data.frame(reg_info)
        base::colnames(reg_info) = c( "signe","coefs","names")
        kl = 1
        other <- ""
        for(ikl in self$coef){
          reg_info[kl,'coefs'] = base::abs(base::round(ikl, digits = 2))
          reg_info[kl,'signe'] = base::ifelse(ikl>=0,base::ifelse(kl == 1,'','+'),'-')
          my_name = base::paste('X',kl-1, sep = "_")
          #print(my_name)
          reg_info[kl,'names']  = base::ifelse(kl == 1,"", my_name)
          inter  <- base::paste(reg_info[kl,'signe'], reg_info[kl,'coefs'], 
                             reg_info[kl,'names'], collapse = " ")
          other <- base::paste(other,inter, sep = " ")
         
          kl <- kl+1
        }
        #base::apply(self$coef, 1, function(x) round(x,digits = 2))
        #reg_info[,'signe'] = base::apply(reg_info[,'coefs'],2,function(x) base::sign(x))
        #list_name = base::paste("X",1:(n_size-1) , sep = "_")
        #list_name = base::c("",list_name)
        #reg_info[,'names'] = list_name
        resultat <- base::paste(resultat, other, sep = "") 
        #print(reg_info)
      }
 
      #resultat <- base::cat(resultat)
      #resultat <- base::paste('y ', resultat, collapse = "=")
      #base::print(resultat)
      return(base::cat(resultat))
    }
  )
  )