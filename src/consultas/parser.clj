(ns consultas.parser
  (:require [consultas.lexico :as lexico]))

(def ^:private precedencias {"ou" 1 "e" 2})
(def ^:private palavras-agregacao #{"contar" "soma" "media"})
(def ^:private tipos-finais #{:selecionar :contar :soma :media :agrupar-por})

(defn- valor-token [token]
  (:valor token))

(defn- palavra? [token valor]
  (and (= :palavra (:tipo token)) (= valor (valor-token token))))

(defn- campo? [token]
  (and (= :palavra (:tipo token))
       (re-matches lexico/padrao-identificador (valor-token token))))

(defn- texto->numero [texto]
  (cond
    (re-matches lexico/padrao-inteiro texto)
    (try
      (Long/parseLong texto)
      (catch NumberFormatException _
        (java.math.BigInteger. texto)))

    (re-matches lexico/padrao-decimal texto)
    (Double/parseDouble texto)

    :else nil))

(defn- campo-esperado [tokens contexto]
  (let [token (first tokens)]
    (if (campo? token)
      [(keyword (valor-token token)) (next tokens)]
      (lexico/erro-sintaxe (str "campo esperado" (when contexto (str " " contexto)))))))

(defn- exigir-palavra [tokens valor contexto]
  (if (palavra? (first tokens) valor)
    (next tokens)
    (lexico/erro-sintaxe (str "esperado '" valor "' " contexto))))

(defn- literal [tokens]
  (let [token (first tokens)]
    (case (:tipo token)
      :texto [(valor-token token) (next tokens)]
      :palavra (if-let [numero (texto->numero (valor-token token))]
                 [numero (next tokens)]
                 (lexico/erro-sintaxe "literal esperado: numero ou texto entre aspas"))
      (lexico/erro-sintaxe "literal esperado: numero ou texto entre aspas"))))

(defn- comparacao [tokens]
  (let [token-campo (first tokens)]
    (when-not (campo? token-campo)
      (lexico/erro-sintaxe "campo esperado antes do operador"))
    (let [depois-campo (next tokens)
          token-operador (first depois-campo)]
      (when-not (= :operador (:tipo token-operador))
        (lexico/erro-sintaxe
         (str "operador de comparacao esperado"
              (when-let [valor (valor-token token-operador)]
                (str " no lugar de " valor)))))
      (let [[valor restantes] (literal (next depois-campo))]
        [[(valor-token token-operador) (keyword (valor-token token-campo)) valor]
         restantes]))))

(declare expressao-binaria)

(defn- expressao-unaria [tokens]
  (cond
    (palavra? (first tokens) "nao")
    (let [[expressao restantes] (expressao-unaria (next tokens))]
      [[:nao expressao] restantes])

    (= :abre-paren (:tipo (first tokens)))
    (let [[expressao restantes] (expressao-binaria (next tokens) 1)]
      (when-not (= :fecha-paren (:tipo (first restantes)))
        (lexico/erro-sintaxe "parentese sem fechamento"))
      [expressao (next restantes)])

    :else
    (comparacao tokens)))

(defn- precedencia-logica [token]
  (when (= :palavra (:tipo token))
    (get precedencias (valor-token token))))

(defn- expressao-binaria [tokens precedencia-minima]
  (loop [par (expressao-unaria tokens)]
    (let [[esquerda restantes] par
          token (first restantes)
          precedencia (precedencia-logica token)]
      (if (and precedencia (>= precedencia precedencia-minima))
        (let [[direita depois] (expressao-binaria (next restantes) (inc precedencia))]
          (recur [[(keyword (valor-token token)) esquerda direita] depois]))
        [esquerda restantes]))))

(defn- elemento-ordenar [tokens]
  (let [depois-de-por (exigir-palavra (next tokens) "por" "apos ordenar")
        [campo depois-campo] (campo-esperado depois-de-por "apos ordenar por")
        descendente? (palavra? (first depois-campo) "desc")]
    [[:ordenar-por campo (if descendente? :desc :asc)]
     (if descendente? (next depois-campo) depois-campo)]))

(defn- elemento-limitar [tokens]
  (let [token (first (next tokens))
        numero (when (= :palavra (:tipo token))
                 (texto->numero (valor-token token)))]
    (when-not (and numero (integer? numero))
      (lexico/erro-sintaxe "numero esperado apos limitar"))
    [[:limitar numero] (nnext tokens)]))

(defn- campos-selecionados [tokens]
  (let [[campo restantes] (campo-esperado tokens "apos selecionar")]
    (loop [campos [campo]
           restantes restantes]
      (if (= :virgula (:tipo (first restantes)))
        (let [[proximo depois] (campo-esperado (next restantes) "apos virgula")]
          (recur (conj campos proximo) depois))
        [campos restantes]))))

(defn- elemento-selecionar [tokens]
  (let [[campos restantes] (campos-selecionados (next tokens))]
    [[:selecionar campos] restantes]))

(defn- agregacao [tokens]
  (cond
    (palavra? (first tokens) "contar")
    [[:contar] (next tokens)]

    (palavra? (first tokens) "soma")
    (let [[campo restantes] (campo-esperado (next tokens) "apos soma")]
      [[:soma campo] restantes])

    (palavra? (first tokens) "media")
    (let [[campo restantes] (campo-esperado (next tokens) "apos media")]
      [[:media campo] restantes])

    :else
    (lexico/erro-sintaxe "agregacao esperada: contar, soma ou media")))

(defn- elemento-agrupar [tokens]
  (let [depois-de-por (exigir-palavra (next tokens) "por" "apos agrupar")
        [campo depois-campo] (campo-esperado depois-de-por "apos agrupar por")
        depois-de-com (exigir-palavra depois-campo "com" "apos o campo de agrupamento")
        [agregacao restantes] (agregacao depois-de-com)]
    [[:agrupar-por campo agregacao] restantes]))

(defn- elemento [tokens]
  (let [token (first tokens)]
    (cond
      (= :barra (:tipo token))
      (lexico/erro-sintaxe "estagio vazio")

      (palavra? token "onde")
      (let [[expressao restantes] (expressao-binaria (next tokens) 1)]
        [[:onde expressao] restantes])

      (palavra? token "ordenar") (elemento-ordenar tokens)
      (palavra? token "limitar") (elemento-limitar tokens)
      (palavra? token "selecionar") (elemento-selecionar tokens)
      (contains? palavras-agregacao (valor-token token)) (agregacao tokens)
      (palavra? token "agrupar") (elemento-agrupar tokens)

      :else
      (lexico/erro-sintaxe (str "estagio desconhecido: " (valor-token token))))))

(defn- verificar-final [elementos]
  (let [final (first (keep-indexed (fn [indice elemento]
                                     (when (contains? tipos-finais (first elemento))
                                       indice))
                                   elementos))]
    (when (and (some? final) (< final (dec (count elementos))))
      (lexico/erro-sintaxe
       (str (name (first (nth elementos final))) " tem que ser o ultimo elemento")))))

(defn- analisar-elementos [tokens]
  (loop [elementos []
         restantes tokens]
    (if (empty? restantes)
      elementos
      (let [[elemento depois] (elemento restantes)]
        (cond
          (empty? depois)
          (conj elementos elemento)

          (= :barra (:tipo (first depois)))
          (if (seq (next depois))
            (recur (conj elementos elemento) (next depois))
            (lexico/erro-sintaxe "estagio vazio apos barra vertical"))

          :else
          (lexico/erro-sintaxe "tokens excedentes apos o elemento"))))))

(defn analisar
  "Analisa o texto de uma consulta, sem a palavra QUERY, e devolve a AST."
  [texto]
  (let [tokens (lexico/tokenizar texto)]
    (if (empty? tokens)
      []
      (let [elementos (analisar-elementos tokens)]
        (verificar-final elementos)
        elementos))))
