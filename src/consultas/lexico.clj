(ns consultas.lexico
  (:require [clojure.string :as str]))

(def padrao-identificador #"[\p{L}\p{N}_-]+")
(def padrao-inteiro #"[+-]?\d+")
(def padrao-decimal #"[+-]?(?:\d+\.\d*|\.\d+)(?:[eE][+-]?\d+)?")

(def ^:private padrao-token
  #"\"[^\"]*\"|<=|>=|!=|[=<>|(),]|[^|(),\"=!<>\s]+")

(def ^:private operadores
  {"=" := "!=" :!= "<" :< "<=" :<= ">" :> ">=" :>=})

(def ^:private pontuacoes
  {"|" :barra "(" :abre-paren ")" :fecha-paren "," :virgula})

(defn erro-sintaxe [mensagem]
  (throw (ex-info mensagem {:tipo :sintaxe})))

(defn- classificar [texto]
  (cond
    (contains? pontuacoes texto) {:tipo (get pontuacoes texto)}
    (contains? operadores texto) {:tipo :operador :valor (get operadores texto)}
    (str/starts-with? texto "\"")
    {:tipo :texto :valor (subs texto 1 (dec (count texto)))}
    :else {:tipo :palavra :valor texto}))

(defn tokenizar
  "Divide o texto da consulta em tokens; nao guarda posicao de leitura."
  [texto]
  (let [texto (or texto "")
        sobra (str/replace texto padrao-token "")]
    (when-not (str/blank? sobra)
      (erro-sintaxe (str "caractere inesperado: " (str/trim sobra))))
    (mapv classificar (re-seq padrao-token texto))))
