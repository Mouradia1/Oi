###########################################################################
##INICIO
###########################################################################


library(tensorflow)
library(magrittr)
setwd("~/Clientes/Oi/Round3/")

###########################################################################
##MUESTRAS ENTRENAMIENTO - VALIDACION
###########################################################################


# Lectura de datos
Mnist_train <- read.csv("train.csv")

## Separa en entrenamiento y validación
set.seed(2018)
n <- nrow(Mnist_train)
### 70% entrenamiento
n_train <- round(0.7*n) 
### muestra entrenamiento
index_train <- sample(seq_len(n), size = n_train)
### datos entrenamiento
Mnist_train_70PR <- Mnist_train[index_train,]
### datos validación
Mnist_train_30PR <- Mnist_train[-index_train,]

# Separación del target
Mnist_label_70PR <- Mnist_train_70PR[,1] # for 70PR train data
Mnist_label_30PR <- Mnist_train_30PR[,1] # for 30PR train data
# Separacion predictores
Mnist_train_image_70PR <- Mnist_train_70PR[,-1]/256
Mnist_train_image_30PR <- Mnist_train_30PR[,-1]/256

###########################################################################
##MANIPULACION
###########################################################################

# Transformacion en matriz de los predictores
Matrix_Mnist_train_image_70PR <- as.matrix(Mnist_train_image_70PR)
Matrix_Mnist_train_image_30PR <- as.matrix(Mnist_train_image_30PR)


#Transformacion de los data frame del target en una matriz
##Reconstruccion para target entrenamiento
MnistLabel_70PR <- data.frame(Mnist_label_70PR)
for(i in c(0:9)){
  newCol <- ifelse(MnistLabel_70PR$Mnist_label == i,
                   1,
                   0)
  MnistLabel_70PR <- cbind(MnistLabel_70PR, newCol)
}
names(MnistLabel_70PR)[2:11] <- c(0:9)
Matrix_MNIST_label_70PR <- as.matrix(MnistLabel_70PR[,-1])

##Reconstruccion datos validacion
MnistLabel_30PR <- data.frame(Mnist_label_30PR)
for(i in c(0:9)){
  newCol <- ifelse(MnistLabel_30PR$Mnist_label == i,
                   1,
                   0)
  MnistLabel_30PR <- cbind(MnistLabel_30PR, newCol)
}
names(MnistLabel_30PR)[2:11] <- c(0:9)
Matrix_MNIST_label_30PR <- as.matrix(MnistLabel_30PR[,-1])

###########################################################################
##CONSTRUCCION MODELO - DEFINICION FUNCIONES
###########################################################################


# Define function for filter
# [filter_height, filter_width, in_channels, out_channels]
add_conv_filter <- function(filterShape){ # random filter as Variable
  filterForConvLayer <- tf$truncated_normal(filterShape, stddev = 0.1) %>% tf$Variable() 
  return(filterForConvLayer)
}
# Define function for bias
add_bias <- function(BiasShape){
  bias <- tf$constant(0.1, shape = BiasShape) %>% tf$Variable()
  return(bias)
}
# Define function for convolution layer
add_convolutionLayer <- function(inputData, filter_weight, activation_function = "None"){
  conv2dLayer <- tf$nn$conv2d(input = inputData, # input data
                              # filter should be shape(filter_height, filter_width, in_channels, out_channels)
                              filter = filter_weight, 
                              strides = shape(1L, 2L, 2L, 1L), # strides = [1, x_strides, y_strides, 1]
                              padding = 'SAME'
  )
  if(activation_function == "None"){
    output_result <- conv2dLayer %>% tf$nn$dropout(., keep_prob = keep_prob_s)
  }else{
    output_result <- conv2dLayer %>% tf$nn$dropout(., keep_prob = keep_prob_s) %>% activation_function()
  }
  return(output_result)
}
# Define function for Max pooling layer
add_maxpoolingLayer <- function(inputData){
  MaxPooling <- tf$nn$max_pool(inputData, # input data should be [batch, height, width, channels]
                               ksize = shape(1L, 2L, 2L, 1L), # 2*2 pixels for max pooling
                               strides = shape(1L, 2L, 2L, 1L), # strides =  [1, x_strides, y_strides, 1]
                               padding = 'SAME')
  return(MaxPooling)
}
# Define function for flatten layer
add_flattenLayer <- function(inputData, numberOfFactors){
  flatten_layer <- tf$reshape(inputData, shape(-1, numberOfFactors))
  return(flatten_layer)
}
# Define function for fully connected layer
add_fullyConnectedLayer <- function(inputData, Weight_FCLayer, bias_FCLayer, activation_function = "None"){
  Wx_plus_b <- tf$matmul(inputData, Weight_FCLayer)+bias_FCLayer
  if(activation_function == "None"){
    FC_output_result <- Wx_plus_b %>% tf$nn$dropout(., keep_prob = keep_prob_s)
  }else{
    FC_output_result <- Wx_plus_b %>% tf$nn$dropout(., keep_prob = keep_prob_s) %>% activation_function()
  }
  return(FC_output_result)
}
```

Building a function to evaluate the performance of training.

```{r}

# Define compute_accuracy function
compute_accuracy <- function(model_result, v_xs, v_ys){
  y_pre <- sess$run(model_result, feed_dict = dict(xs = v_xs, keep_prob_s= 1))
  correct_prediction <- tf$equal(tf$argmax(y_pre, 1L), tf$argmax(v_ys, 1L))
  accuracy <- tf$cast(correct_prediction, tf$float32) %>% tf$reduce_mean(.)
  result <- sess$run(accuracy, feed_dict = dict(xs = v_xs, ys = v_ys, keep_prob_s= 1))
  return(result)
}
```

                                                                                                                          