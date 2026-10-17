(ns consultas.core
  (:require [consultas.csv :as csv]
            [consultas.motor :as motor]
            [consultas.parser :as parser]))

(defn csv->dados
  "Converte conteudo textual de um CSV delimitado por ponto e virgula em
   {:esquema [[campo tipo] ...] :registros [mapa ...]}."
  [conteudo]
  (csv/csv->dados conteudo))

(defn consulta
  "Cria uma consulta nova, sem nenhum elemento: devolve todos os registros."
  []
  [])

(defn onde
  "Deriva uma consulta acrescentando um filtro; expressao e um vetor."
  [consulta expressao]
  (conj consulta [:onde expressao]))

(defn selecionar
  "Deriva uma consulta acrescentando a projecao final de campos."
  [consulta campos]
  (conj consulta [:selecionar campos]))

(defn ordenar-por
  "Deriva uma consulta acrescentando uma ordenacao :asc ou :desc."
  [consulta campo ordem]
  (conj consulta [:ordenar-por campo ordem]))

(defn limitar
  "Deriva uma consulta acrescentando um limite de registros."
  [consulta quantidade]
  (conj consulta [:limitar quantidade]))

(defn analisar
  "Transforma o texto de uma consulta, sem a palavra QUERY, em AST de dados
   puros; erro de sintaxe e sinalizado com ex-info."
  [texto]
  (parser/analisar texto))

(defn executar
  "Executa a consulta sobre {:esquema [...] :registros seq}: devolve a sequencia
   de registros, o valor do elemento final ou {:erros [...]}."
  [consulta dados]
  (let [{:keys [funcao erros]} (motor/compilar consulta (:esquema dados))]
    (if (seq erros)
      {:erros erros}
      (funcao (:registros dados)))))
