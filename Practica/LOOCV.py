import pandas as pd
import numpy as np
from lightgbm import LGBMRegressor, Dataset
import matplotlib.pyplot as plt
from sklearn.metrics import mean_squared_error
import matplotlib.pyplot as plt
from matplotlib.patches import Patch

dataset = pd.read_excel("PowerBi.xlsx")

#filtrar datos seleccionando solo lo numerico
dataset = dataset.select_dtypes(include=[np.number]).copy()

#dataset = dataset.drop(['DURACION_CARRERA', 'ID', 'ANO_TITULACION', 'TOTAL_CREDITOS_PRIMER_ANO', 'PROMEDIO_PONDERADO', 'PROMEDIO_PSU_LM'], axis=1)
dataset = dataset[['NOMBRE_PROGRAMA', 'PUEBLO_ORIGINARIO', 'Semestres']]

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
 lgb_model = LGBMRegressor(**params, importance_type='split') #gain/split
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
total_importance = np.sum(importances_avg)
importances_norm = importances_avg / total_importance

#error cuadratico medio
mse = round(mean_squared_error(dataset['Semestres'].values, predictions), 4)
mse_patch = Patch(color='white', label=f'MSE: {mse}')
num_filas = len(dataset.index)

#crear el grafico con el top5 mayores importancias Gain/Split
feature_names = X_train.columns
importances_avg_df = pd.DataFrame({'Caracteristica': feature_names, 'Importancia': importances_norm})

ax = importances_avg_df.sort_values(by='Importancia', ascending=False).plot(kind='bar', x='Caracteristica', y='Importancia', rot=90, figsize=(10, 6))
ax.set_xticklabels(ax.get_xticklabels(), rotation=30, ha="right")
ax.set_ylabel('Importancia', fontsize='large',labelpad=20)
ax.set_xlabel('Caracteristica', fontsize='large')
ax.legend(handles=[mse_patch],fontsize='xx-large')
ax.tick_params(axis='both', pad=10)
plt.subplots_adjust(bottom=0.27)
plt.title("Promedio de Importancias Normalizadas (Split)",fontsize='xx-large', y=1.05)
plt.show()
