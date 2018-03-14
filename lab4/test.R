
habilitar_paquetes<-function(){
  library(keras)
  library (corrplot)	
  library(ngram)
}

instalar_prerrequisitos<-function(){
  install_tensorflow()	
  #	install.packages("ngram") 
}

cargar_datos<-function(dibujar){
  # Read in `iris` data
  iris <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"), header = FALSE) 
  
  # Return the first part of `iris`
  #	head(iris)
  
  # Inspect the structure
  #	str(iris)
  
  # Obtain the dimensions
  #	dim(iris)
  
  names(iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  if(dibujar==TRUE){
    plot(iris$Petal.Length, 
         iris$Petal.Width, 
         pch=21, bg=c("red","green3","blue")[unclass(iris$Species)], 
         xlab="Petal Length", 
         ylab="Petal Width")
  }
  
  return(iris)
}

correlacion<-function(iris){
  # Overall correlation between `Petal.Length` and `Petal.Width` 
  cor(iris$Petal.Length, iris$Petal.Width)
  
  # Store the overall correlation in `M`
  M <- cor(iris[,1:4])
  
  # Plot the correlation plot with `M`
  #corrplot(M, method="circle")
}

conjunto_entrenamiento<-function(iris){
  
  # Determine sample size
  ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))
  
  # Split the `iris` data
  iris.training <- iris[ind==1, 1:4]
  iris.test <- iris[ind==2, 1:4]
  
  # Split the class attribute
  iris.trainingtarget <- iris[ind==1, 5]
  str(iris.trainingtarget)
  iris.testtarget <- iris[ind==2, 5]
  
  # One hot encode training target values
  iris.trainLabels <- to_categorical(iris.trainingtarget)
  
  # One hot encode test target values
  iris.testLabels <- to_categorical(iris.testtarget)
  
  # Print out the iris.testLabels to double check the result
  #print(iris.testLabels)
  return(new("DataSet",
             data=as.matrix(iris),
             entrenamiento=as.matrix(iris.training),
             prueba=as.matrix(iris.test),
             clase_entrenamiento=iris.trainingtarget,
             clase_prueba=iris.testtarget,
             etiquetas_entrenamiento=iris.trainLabels,
             etiquetas_prueba=iris.testLabels))
}	


modelo1<-function(){
  
  # Initialize a sequential model
  modelo <- keras_model_sequential() 
  
  # Add layers to the model
  modelo %>% 
    layer_dense(units = 8, activation = 'relu', input_shape = c(4)) %>% 
    layer_dense(units = 3, activation = 'softmax')
  
  return(modelo)
  
}

describir_modelo<-function(model){
  
  # Print a summary of a model
  #summary(model)
  
  # Get model configuration
  get_config(model)
  
  # Get layer configuration
  get_layer(model, index = 1)
  
  # List the model's layers
  #model$layers
  
  # List the input tensors
  #model$inputs
  
  # List the output tensors
  #model$outputs	
}

compilar_modelo<-function (model){
  # Compile the model
  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = 'adam',
    metrics = 'accuracy'
  )
  
  return (model)
  
}

ajustar_modelo<-function(model, datos){
  # Fit the model
  
  history<-fit(    model,
                   datos@entrenamiento, 
                   datos@etiquetas_entrenamiento, 
                   epochs = 200, 
                   batch_size = 5, 
                   validation_split = 0.2
  )
  
  return (history)
}


perdida_modelo<-function(history){
  # Plot the model loss of the training data
  plot(history$metrics$loss, main="Model Loss", xlab = "epoch", ylab="loss", col="blue", type="l")
  
  # Plot the model loss of the test data
  lines(history$metrics$val_loss, col="green")
  
  # Add legend
  legend("topright", c("train","test"), col=c("blue", "green"), lty=c(1,1))
  
  
}

exactitud_modelo<-function(history){
  
  # Plot the accuracy of the training data 
  plot(history$metrics$acc, main="Model Accuracy", xlab = "epoch", ylab="accuracy", col="blue", type="l")
  
  # Plot the accuracy of the validation data
  lines(history$metrics$val_acc, col="green")
  
  # Add Legend
  legend("bottomright", c("train","test"), col=c("blue", "green"), lty=c(1,1))
  
}

predictor<-function(data, model){
  # Predict the classes for the test data
  classes <- model %>% predict_classes(data@prueba, batch_size = 128)
  
  # Confusion matrix
  table(data@clase_prueba, classes)
  
}

evaluar_modelo<-function(datos, model){
  
  # Evaluate on test data and labels
  score <- model %>% evaluate(datos@prueba, datos@etiquetas_prueba, batch_size = 128)
  
  # Print the score
  print(score)
  
}

guardar_modelo<-function(nombre_archivo, modelo){
  
  #save_model_hdf5(modelo, nombre_archivo)
  #save_model_weights_hdf5(modelo, concatenate(nombre_archivo,".h5"))
  
}

####################

setClass(Class="DataSet",
         representation(
           data="matrix",
           entrenamiento="matrix",
           prueba="matrix",
           clase_entrenamiento="factor",
           clase_prueba="factor",
           etiquetas_entrenamiento="matrix",
           etiquetas_prueba="matrix"
         )
)

habilitar_paquetes()

iris<-cargar_datos(FALSE)

correlacion(iris)

#summary(iris)

datos<-conjunto_entrenamiento(iris)

modelo<-modelo1()

describir_modelo(modelo)

modelo<-compilar_modelo(modelo)

#ajuste<-ajustar_modelo(modelo, datos)

#plot(ajuste)

predictor(datos, modelo)

historia<-ajustar_modelo(modelo, datos)

#perdida_modelo(historia)

#exactitud_modelo(historia)

evaluar_modelo(datos, modelo)

guardar_modelo(modelo, "modelo")