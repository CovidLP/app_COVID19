### Resultados do modelo de Previsão de Curto Prazo

#### Formato dos arquivos para upload:

**Arquivo**: `<País>_n.rds` contém resultados do modelo para número acumulado de casos.

O objeto contido neste arquivo, deve ser uma lista com os seguintes elementos:

- `df_predict`: objeto data.frame de dimensão (nº dias previsão X 4), com as variáveis:
  * `date` (datas futuras);
  * `q25` (quantil 2.5%);
  * `med` (mediana);
  * `q975` (quantil 97.5%).
  Contém os resultados da previsão de curto prazo para o número acumulado de casos (mediana, e quantis de 2.5% e 97.5%) para as datas futuras listadas.

- `a`: vetor de tamanho 3, contendo os resultados da previsão para os parâmetros do modelo.

- `b`: vetor de tamanho 3, contendo os resultados da previsão para os parâmetros do modelo.

- `c`: vetor de tamanho 3, contendo os resultados da previsão para os parâmetros do modelo.

- `assint`: vetor de tamanho 3, contendo os resultados da previsão para os parâmetros do modelo.

(Para mais detalhes sobre os parâmetros acima, ver o arquivo de [fundamentação teórica](../www/Covid19UFMG.pdf)).

