import pandas as pd
import numpy as np
from lightgbm import LGBMRegressor, Dataset
import matplotlib.pyplot as plt
from sklearn.metrics import mean_squared_error
import matplotlib.pyplot as plt

dataset = pd.read_excel("PowerBi.xlsx")

#filtrar datos seleccionando solo lo numerico
dataset = dataset.select_dtypes(include=[np.number]).copy()

dataset = dataset.drop(['DURACION_CARRERA', 'ID'], axis=1)

#almacena el valor de la prediccion
predictions = []

#almacena los modelos
importances_list = []

#algoritmo LOOCV, sacar 1 dato, hacer un modelo con los restantes y calcular la prediccion para el dato faltante
for i in range(len(dataset)):
 #dividir df
 train_data = pd.concat([dataset.iloc[:i], dataset.iloc[i+1:]])
 valid_data = dataset.iloc[i]
 
 #eliminar la variable dependiente de los datos de entrenamiento
 X_train = train_data.drop('Semestres', axis=1)
 y_train = train_data['Semestres']
 X_valid = valid_data.drop('Semestres')
 
 lgb_train = Dataset(X_train, y_train)
 
 params = {
  'objective': 'regression',
  'metric': 'l2',
  'num_leaves': 31,
  'learning_rate': 0.05,
  'n_estimators': 100,
  'min_data_in_bin': 1,
  'min_data_in_leaf': 5,
  'verbose': -1 #quita warnings
 }
 
 #entrenar modelo
 lgb_model = LGBMRegressor(**params, importance_type='gain') #gain/split
 lgb_model.fit(X_train, y_train)
 
 #almacenar modelo/importancias
 importances_list.append(lgb_model.feature_importances_)
 
 #aplicar modelo
 prediction = lgb_model.predict(X_valid.to_frame().transpose())
 predictions.append(prediction)
 
 #progreso de la operacion
 if (i % 20) == 0:
    print(f"{int(100 * (i / 200))}% completado")

#promedio de las importancias
importances_avg = np.mean(importances_list, axis=0)

#crear el grafico con el top5 mayores importancias Gain/Split
feature_names = X_train.columns
importances_avg_df = pd.DataFrame({'Feature': feature_names, 'Importance': importances_avg})

ax = importances_avg_df.sort_values(by='Importance', ascending=False).head(5).plot(kind='bar', x='Feature', y='Importance', rot=90, figsize=(10, 5))
ax.set_xticklabels(ax.get_xticklabels(), rotation=45, ha="right")
plt.subplots_adjust(bottom=0.3)
plt.title("Promedio de Importancia de Características (Gain)")
plt.show()

#error cuadratico medio
rmse = np.sqrt(mean_squared_error(dataset['Semestres'].values, predictions))