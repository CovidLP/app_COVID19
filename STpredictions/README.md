### Resultados do modelo de Previsão de Curto Prazo

#### Formato dos arquivos para upload:

**Arquivo**: `<País>_n.rds` contém resultados do modelo para número acumulado de casos. O nome preenchido em <País> deve ser idêntico ao nome do país preenchido no campo "Country/Region" da base de dados.

O objeto contido neste arquivo, deve ser do tipo "list", contendo o(s) seguinte(s) elemento(s):

- `df_predict`: objeto data.frame de dimensão (nº dias previsão x 4), com as variáveis:
  * `date` (datas futuras), formato "yyyy-mm-dd";
  * `q25` (quantil 2.5%);
  * `med` (mediana);
  * `q975` (quantil 97.5%).
  Contém os resultados da previsão de curto prazo para o número acumulado de casos (mediana, e quantis de 2.5% e 97.5%) para as datas futuras listadas.
