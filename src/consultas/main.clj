(ns consultas.main
  (:require [clojure.string :as str]
            [consultas.core :as core]))

(defn- formatar-decimal [valor]
  (.toPlainString
   (.setScale (bigdec valor) 2 java.math.RoundingMode/HALF_UP)))

(def ^:private formatadores
  {:decimal formatar-decimal
   :inteiro str
   :texto str})

(defn- formatar-campo [valor tipo]
  ((get formatadores tipo str) valor))

(defn- imprimir-linha [valores]
  (println (str/join "; " valores)))

(defn- imprimir-registros [registros campos esquema]
  (if-let [registros (seq registros)]
    (let [tipos (into {} esquema)]
      (doseq [registro registros]
        (imprimir-linha (map (fn [campo]
                               (formatar-campo (get registro campo)
                                               (get tipos campo)))
                             campos))))
    (println "(vazio)")))

(defn- tipo-resultado [agregacao esquema]
  (case (first agregacao)
    :contar :inteiro
    :soma (get (into {} esquema) (second agregacao))
    :media :decimal))

(defn- imprimir-agrupamentos [grupos campo agregacao esquema]
  (if (empty? grupos)
    (println "(vazio)")
    (doseq [[chave valor] grupos]
      (imprimir-linha [(formatar-campo chave (get (into {} esquema) campo))
                       (formatar-campo valor
                                       (tipo-resultado agregacao esquema))]))))

(defn- imprimir-escalar [valor tipo]
  (println (formatar-campo valor tipo)))

(def ^:private impressoras
  {:contar (fn [resultado _ _]
             (imprimir-escalar resultado :inteiro))
   :soma (fn [resultado [_ campo] esquema]
           (imprimir-escalar resultado (get (into {} esquema) campo)))
   :media (fn [resultado _ _]
            (imprimir-escalar resultado :decimal))
   :agrupar-por (fn [resultado [_ campo agregacao] esquema]
                  (imprimir-agrupamentos resultado campo agregacao esquema))
   :selecionar (fn [resultado [_ campos] esquema]
                 (imprimir-registros resultado campos esquema))})

(defn- imprimir-resultado [resultado consulta esquema]
  (let [final (peek consulta)
        imprimir-final (get impressoras (first final)
                            (fn [resultado _ esquema]
                              (imprimir-registros resultado
                                                  (mapv first esquema)
                                                  esquema)))]
    (imprimir-final resultado final esquema)))

(defn- comando-query [texto dados]
  (let [consulta (core/analisar texto)
        resultado (core/executar consulta dados)]
    (if (and (map? resultado) (contains? resultado :erros))
      (throw (ex-info (str/join "; " (:erros resultado)) {:tipo :validacao}))
      (imprimir-resultado resultado consulta (:esquema dados)))))

(defn- responder! [linha dados]
  (cond
    (str/blank? linha)
    (throw (ex-info "linha em branco" {:tipo :comando}))

    :else
    (let [[verbo & argumentos] (str/split (str/triml linha) #"[ \t]+" 2)]
      (case verbo
        "EXIT" :sair
        "QUERY" (comando-query (or (first argumentos) "") dados)
        (throw (ex-info "comando desconhecido" {:tipo :comando}))))))

(defn- ler-linha []
  (print "> ")
  (flush)
  (read-line))

(defn- sessao! [dados]
  (loop [linha (ler-linha)]
    (when linha
      (let [resultado (try
                        (responder! linha dados)
                        (catch Exception erro
                          (println (str "ERRO: " (.getMessage erro)))
                          :continuar))]
        (when-not (= :sair resultado)
          (recur (ler-linha)))))))

(defn- ler-csv [caminho]
  (try
    (core/csv->dados (slurp caminho))
    (catch clojure.lang.ExceptionInfo erro
      {:erros [(.getMessage erro)]})
    (catch java.io.IOException erro
      {:erros [(.getMessage erro)]})))

(defn- falha! [mensagem]
  (binding [*out* *err*]
    (println (str "ERRO: " mensagem)))
  (System/exit 1))

(defn -main [& argumentos]
  (when-not (= 1 (count argumentos))
    (falha! (if (empty? argumentos)
              "informe o caminho do CSV"
              "informe somente um caminho de CSV")))
  (let [dados (ler-csv (first argumentos))]
    (if (contains? dados :erros)
      (falha! (first (:erros dados)))
      (sessao! dados))))
