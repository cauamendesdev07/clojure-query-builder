(ns consultas.csv
  (:require [clojure.string :as str]
            [consultas.lexico :as lexico]))

(def ^:private tipos
  {"inteiro" :inteiro "decimal" :decimal "texto" :texto})

(defn- erro-csv [linha mensagem]
  (throw (ex-info (str "CSV invalido na linha " linha ": " mensagem)
                  {:linha linha})))

(defn- partes [linha]
  (vec (.split ^String linha ";" -1)))

(defn- declaracao->campo [declaracao numero-campo]
  (let [partes (str/split declaracao #":" -1)]
    (when-not (= 2 (count partes))
      (erro-csv 1 (str "cabecalho malformado no campo " numero-campo)))
    (let [nome (str/trim (first partes))
          tipo-texto (str/trim (second partes))
          tipo (get tipos tipo-texto)]
      (when-not (and (seq nome) (re-matches lexico/padrao-identificador nome))
        (erro-csv 1 (str "nome de campo invalido no campo " numero-campo)))
      (when-not tipo
        (erro-csv 1 (str "tipo desconhecido no campo " nome ": " tipo-texto)))
      [(keyword nome) tipo])))

(defn- cabecalho->esquema [linha]
  (when (str/blank? linha)
    (erro-csv 1 "cabecalho vazio"))
  (let [declaracoes (partes linha)
        esquema (mapv (fn [indice declaracao]
                        (declaracao->campo declaracao (inc indice)))
                      (range)
                      declaracoes)
        nomes (mapv first esquema)]
    (when-not (= (count nomes) (count (distinct nomes)))
      (erro-csv 1 "nomes de campo duplicados"))
    esquema))

(defn- valor-inteiro [texto campo linha]
  (when-not (re-matches lexico/padrao-inteiro texto)
    (erro-csv linha (str "inteiro invalido no campo " campo)))
  (try
    (Long/parseLong texto)
    (catch NumberFormatException _
      (try
        (java.math.BigInteger. texto)
        (catch NumberFormatException _
          (erro-csv linha (str "inteiro invalido no campo " campo)))))))

(defn- valor-decimal [texto campo linha]
  (when-not (re-matches lexico/padrao-decimal texto)
    (erro-csv linha (str "decimal invalido no campo " campo)))
  (try
    (Double/parseDouble texto)
    (catch NumberFormatException _
      (erro-csv linha (str "decimal invalido no campo " campo)))))

(defn- valor-campo [valor tipo campo linha]
  (when (empty? valor)
    (erro-csv linha (str "valor ausente no campo " campo)))
  (case tipo
    :texto valor
    :inteiro (valor-inteiro (str/trim valor) campo linha)
    :decimal (valor-decimal (str/trim valor) campo linha)
    (erro-csv linha (str "tipo desconhecido no campo " campo))))

(defn- linha->registro [esquema linha numero-linha]
  (when (empty? linha)
    (erro-csv numero-linha "linha vazia entre registros"))
  (let [valores (partes linha)]
    (when-not (= (count esquema) (count valores))
      (erro-csv numero-linha
                (str "quantidade de campos incorreta: esperado "
                     (count esquema) ", recebido " (count valores))))
    (reduce (fn [registro [[campo tipo] valor]]
              (assoc registro campo
                     (valor-campo valor tipo campo numero-linha)))
            {}
            (map vector esquema valores))))

(defn- separar-linhas [texto]
  (let [linhas (str/split texto #"\r\n|\n|\r" -1)]
    (if (and (> (count linhas) 1)
             (re-find #"(?:\r\n|\n|\r)$" texto))
      (pop linhas)
      linhas)))

(defn csv->dados
  "Converte o conteudo textual de um CSV delimitado por ponto e virgula em
   esquema ordenado e mapas de registros, sem realizar I/O."
  [conteudo]
  (let [linhas (separar-linhas (or conteudo ""))]
    (when (empty? linhas)
      (erro-csv 1 "arquivo vazio"))
    (let [esquema (cabecalho->esquema (first linhas))
          registros (mapv (fn [indice linha]
                            (linha->registro esquema linha (+ indice 2)))
                          (range)
                          (rest linhas))]
      {:esquema esquema :registros registros})))
