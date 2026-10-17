(ns consultas.motor)

(def ^:private tipos-numericos #{:inteiro :decimal})
(def ^:private tipos-agregacao #{:contar :soma :media})
(def ^:private tipos-finais #{:selecionar :contar :soma :media :agrupar-por})

(defn- esquema->mapa [esquema]
  (into {} esquema))

(defn- tipo-campo [esquema campo]
  (get esquema campo ::inexistente))

(defn- nome-campo [campo]
  (if (keyword? campo) (name campo) (str campo)))

(defn- erro-campo [esquema campo]
  (when (= ::inexistente (tipo-campo esquema campo))
    [(str "campo inexistente: " (nome-campo campo))]))

(defn- tipo-literal [literal]
  (cond
    (string? literal) :texto
    (integer? literal) :inteiro
    (number? literal) :decimal
    :else :invalido))

(defn- igual? [esquerda direita]
  (if (and (number? esquerda) (number? direita))
    (== esquerda direita)
    (= esquerda direita)))

(defn- comparar [esquerda direita]
  (compare esquerda direita))

(defn- operador-comparacao [operador]
  (case operador
    := (fn [esquerda direita] (igual? esquerda direita))
    :!= (fn [esquerda direita] (not (igual? esquerda direita)))
    :< (fn [esquerda direita] (neg? (comparar esquerda direita)))
    :<= (fn [esquerda direita] (not (pos? (comparar esquerda direita))))
    :> (fn [esquerda direita] (pos? (comparar esquerda direita)))
    :>= (fn [esquerda direita] (not (neg? (comparar esquerda direita))))))

(defn- erros-comparacao [esquema campo literal]
  (let [tipo (tipo-campo esquema campo)
        literal-tipo (tipo-literal literal)]
    (concat
     (erro-campo esquema campo)
     (when (and (not= ::inexistente tipo)
                (not (and (contains? tipos-numericos tipo)
                          (contains? tipos-numericos literal-tipo)))
                (not= tipo literal-tipo))
       [(str "comparacao entre " (name tipo) " e " (name literal-tipo)
             " no campo " (nome-campo campo))]))))

(defn- compilar-comparacao [operador argumentos esquema]
  (let [[campo literal] argumentos
        forma-ok? (and (= 2 (count argumentos))
                       (keyword? campo)
                       (or (string? literal) (number? literal)))]
    (if-not forma-ok?
      {:erros [(str "forma invalida para comparacao " (name operador))]}
      (let [erros (vec (erros-comparacao esquema campo literal))]
        (if (seq erros)
          {:erros erros}
          (let [comparacao (operador-comparacao operador)]
            {:funcao (fn [registro]
                       (comparacao (get registro campo) literal))
             :erros []}))))))

(defn- compilar-expressao [expressao esquema]
  (let [[tipo & argumentos] expressao]
    (case tipo
      :e
      (if (and (= 2 (count argumentos)) (every? vector? argumentos))
        (let [[a b] argumentos
              esquerda (compilar-expressao a esquema)
              direita (compilar-expressao b esquema)
              erros (vec (concat (:erros esquerda) (:erros direita)))]
          (if (seq erros)
            {:erros erros}
            {:funcao (fn [registro]
                       (and ((:funcao esquerda) registro)
                            ((:funcao direita) registro)))
             :erros []}))
        {:erros ["forma invalida para e"]})

      :ou
      (if (and (= 2 (count argumentos)) (every? vector? argumentos))
        (let [[a b] argumentos
              esquerda (compilar-expressao a esquema)
              direita (compilar-expressao b esquema)
              erros (vec (concat (:erros esquerda) (:erros direita)))]
          (if (seq erros)
            {:erros erros}
            {:funcao (fn [registro]
                       (or ((:funcao esquerda) registro)
                           ((:funcao direita) registro)))
             :erros []}))
        {:erros ["forma invalida para ou"]})

      :nao
      (if (and (= 1 (count argumentos)) (vector? (first argumentos)))
        (let [compilada (compilar-expressao (first argumentos) esquema)]
          (if (seq (:erros compilada))
            {:erros (vec (:erros compilada))}
            {:funcao (complement (:funcao compilada)) :erros []}))
        {:erros ["forma invalida para nao"]})

      (:= :!= :< :<= :> :>=)
      (compilar-comparacao tipo argumentos esquema))))

(defn- compilar-onde [argumentos esquema]
  (if (and (= 1 (count argumentos)) (vector? (first argumentos)))
    (let [compilada (compilar-expressao (first argumentos) esquema)]
      (if (seq (:erros compilada))
        {:erros (vec (:erros compilada))}
        {:funcao (partial filter (:funcao compilada)) :erros []}))
    {:erros ["forma invalida para onde"]}))

(defn- ordenar [campo comparador registros]
  (sort-by #(get % campo) comparador registros))

(defn- compilar-ordenar [argumentos esquema]
  (let [[campo ordem] argumentos
        comparador (case ordem
                     :asc comparar
                     :desc (fn [esquerda direita] (comparar direita esquerda))
                     nil)
        erros (vec (concat (when-not (and (= 2 (count argumentos)) (keyword? campo))
                             ["forma invalida para ordenar-por"])
                           (erro-campo esquema campo)))]
    (cond
      (seq erros) {:erros erros}
      comparador {:funcao (partial ordenar campo comparador) :erros []}
      :else {:erros [(str "ordem desconhecida: " ordem)]})))

(defn- compilar-limitar [argumentos]
  (let [quantidade (first argumentos)]
    (cond
      (not= 1 (count argumentos)) {:erros ["forma invalida para limitar"]}
      (and (integer? quantidade) (not (neg? quantidade)))
      {:funcao (partial take quantidade) :erros []}
      :else {:erros ["limite deve ser um inteiro nao negativo"]})))

(defn- compilar-selecionar [argumentos esquema]
  (let [campos (first argumentos)
        forma-ok? (and (= 1 (count argumentos))
                       (vector? campos)
                       (seq campos)
                       (every? keyword? campos))]
    (if-not forma-ok?
      {:erros ["forma invalida para selecionar"]}
      (let [erros (vec (mapcat #(erro-campo esquema %) campos))]
        (if (seq erros)
          {:erros erros}
          {:funcao (partial map #(select-keys % campos)) :erros []})))))

(defn- dobrar [redutor registros]
  ((:fim redutor) (reduce (:passo redutor) (:zero redutor) registros)))

(defn- redutor-contagem []
  {:zero 0
   :passo (fn [total _] (inc total))
   :fim identity})

(defn- redutor-soma [campo]
  {:zero 0
   :passo (fn [total registro] (+ total (get registro campo)))
   :fim identity})

(defn- redutor-media [campo]
  {:zero [0 0]
   :passo (fn [[total quantidade] registro]
            [(+ total (get registro campo)) (inc quantidade)])
   :fim (fn [[total quantidade]]
          (if (zero? quantidade)
            0.0
            (/ (double total) quantidade)))})

(defn- agrupar [campo redutor registros]
  (let [{:keys [zero passo fim]} redutor
        grupos (reduce (fn [acumulado registro]
                         (update acumulado (get registro campo)
                                 (fn [atual]
                                   (passo (or atual zero) registro))))
                       (sorted-map-by comparar)
                       registros)]
    (mapv (fn [[chave valor]] [chave (fim valor)]) grupos)))

(defn- erros-numericos [esquema campo]
  (let [tipo (tipo-campo esquema campo)]
    (concat
     (erro-campo esquema campo)
     (when (and (keyword? campo)
                (not= ::inexistente tipo)
                (not (contains? tipos-numericos tipo)))
       [(str "agregacao numerica exige numero no campo " (nome-campo campo))]))))

(defn- compilar-agregacao-numerica [tipo argumentos esquema construtor]
  (if (and (= 1 (count argumentos)) (keyword? (first argumentos)))
    (let [campo (first argumentos)
          erros (vec (erros-numericos esquema campo))]
      (if (seq erros)
        {:erros erros}
        {:redutor (construtor campo) :erros []}))
    {:erros [(str "forma invalida para " (name tipo))]}))

(defn- compilar-agregacao [agregacao esquema]
  (let [[tipo & argumentos] agregacao]
    (when-not (contains? tipos-agregacao tipo)
      (throw (IllegalArgumentException.
              (str "agregacao desconhecida: " tipo))))
    (case tipo
      :contar (if (empty? argumentos)
                {:redutor (redutor-contagem) :erros []}
                {:erros ["contar nao recebe campos"]})
      :soma (compilar-agregacao-numerica tipo argumentos esquema redutor-soma)
      :media (compilar-agregacao-numerica tipo argumentos esquema redutor-media))))

(defn- compilar-agregacao-terminal [agregacao esquema]
  (let [compilada (compilar-agregacao agregacao esquema)]
    (if (seq (:erros compilada))
      compilada
      {:funcao (partial dobrar (:redutor compilada)) :erros []})))

(defn- compilar-agrupamento [campo agregacao esquema]
  (let [compilada (compilar-agregacao agregacao esquema)
        erros (vec (concat (erro-campo esquema campo)
                           (:erros compilada)))]
    (if (seq erros)
      {:erros erros}
      {:funcao (partial agrupar campo (:redutor compilada)) :erros []})))

(defn- compilar-elemento [elemento esquema]
  (let [[tipo & argumentos] elemento]
    (case tipo
      :onde (compilar-onde argumentos esquema)
      :ordenar-por (compilar-ordenar argumentos esquema)
      :limitar (compilar-limitar argumentos)
      :selecionar (compilar-selecionar argumentos esquema)
      :contar (compilar-agregacao-terminal elemento esquema)
      :soma (compilar-agregacao-terminal elemento esquema)
      :media (compilar-agregacao-terminal elemento esquema)
      :agrupar-por (if (and (= 2 (count argumentos))
                            (keyword? (first argumentos))
                            (vector? (second argumentos)))
                     (compilar-agrupamento (first argumentos) (second argumentos) esquema)
                     {:erros ["forma invalida para agrupar-por"]}))))

(defn- compor [funcoes]
  (apply comp (reverse funcoes)))

(defn- indice-final [elementos]
  (first (keep-indexed (fn [indice elemento]
                         (when (contains? tipos-finais (first elemento))
                           indice))
                       elementos)))

(defn compilar
  "Confere a consulta contra o esquema e devolve {:funcao f} ou {:erros [...]}.
   A funcao composta recebe a sequencia de registros e devolve os registros
   resultantes ou o valor do elemento final."
  [consulta esquema]
  (if-not (and (vector? consulta) (every? vector? consulta))
    {:erros ["consulta invalida: esperado um vetor de elementos"]}
    (let [elementos (vec consulta)
          campos (esquema->mapa esquema)
          final (indice-final elementos)
          erros-finalizacao (when (and (some? final)
                                       (< final (dec (count elementos))))
                              [(str (name (first (nth elementos final)))
                                    " tem que ser o ultimo elemento")])
          compilados (mapv #(compilar-elemento % campos) elementos)
          erros (vec (concat erros-finalizacao (mapcat :erros compilados)))
          terminal? (and (seq elementos)
                         (contains? tipos-finais (first (peek elementos))))]
      (if (seq erros)
        {:erros erros}
        (let [funcoes (mapv :funcao (if terminal?
                                      (subvec compilados 0 (dec (count compilados)))
                                      compilados))
              pipeline (compor funcoes)]
          {:funcao (if terminal?
                     (fn [registros]
                       ((:funcao (peek compilados)) (pipeline registros)))
                     pipeline)})))))
