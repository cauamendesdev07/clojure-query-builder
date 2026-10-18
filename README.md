# Integrantes

- Cauã Mendes
- Stephany França

# EP2 - Query builder funcional em Clojure

## Como rodar

```sh
clojure -M:run dados/filmes.csv
```

Digite `QUERY` seguido de uma consulta por linha. `QUERY` sem consulta imprime
os registros; `EXIT` ou o fim da entrada encerra a sessão. Uma linha inválida
mostra `ERRO:` e não encerra uma sessão já iniciada.

## AST

A consulta é um **vetor plano de elementos**, na ordem de leitura. Cada elemento
é um vetor cujo primeiro item é o tipo do elemento. O último elemento pode ser
um *final* — `:selecionar` ou um terminal —, e nada vem depois dele:

```clojure
[[:onde [:e [:= :genero "drama"] [:>= :nota 8]]]
 [:ordenar-por :nota :desc]
 [:selecionar [:titulo :nota]]]
```

A consulta vazia é `[]` e devolve todos os registros na ordem do arquivo.

Elementos de estágio:

```clojure
[:onde expressao]
[:ordenar-por campo :asc] ; ou :desc
[:limitar inteiro-nao-negativo]
```

Elementos finais:

```clojure
[:selecionar [campo ...]]
[:contar]
[:soma campo]
[:media campo]
[:agrupar-por campo agregacao]
```

`agregacao` tem a mesma forma dos terminais escalares: `[:contar]`,
`[:soma campo]` ou `[:media campo]`.

As expressões são vetores recursivos: comparações `[:= campo literal]`,
`[:!= campo literal]`, `[:< campo literal]`, `[:<= campo literal]`,
`[:> campo literal]` e `[:>= campo literal]`; conjunção `[:e esquerda direita]`,
disjunção `[:ou esquerda direita]` e negação `[:nao expressao]`. O literal é um
número (inteiro ou decimal) ou um texto entre aspas.

O analisador produz exatamente o mesmo dado que o builder:

```clojure
(= (analisar "onde genero = \"drama\" | limitar 2")
   (-> (consulta)
       (onde [:= :genero "drama"])
       (limitar 2)))
```

O builder apenas acrescenta elementos à copia: continuar uma consulta já
finalizada torna a consulta inválida, e `executar` devolve `{:erros [...]}` sem
tocar nos registros. Uma AST montada à mão com um nó que não existe na
linguagem, como `[[:onde [:= :ano 2000]] [:voar 3]]`, falha nomeando `:voar`.

## Namespaces

- `consultas.main`: terminal e leitura do arquivo; concentra os efeitos de I/O.
- `consultas.lexico`: tokenização do texto por expressão regular.
- `consultas.parser`: análise recursiva e sem estado; devolve a AST vetorial.
- `consultas.motor`: conferência da AST contra o esquema e compilação em
  funções, com `case` sem ramo padrão por tipo de nó; agregações com `reduce`.
- `consultas.csv`: leitura do CSV para `{:esquema ... :registros ...}`.
- `consultas.core`: API pública do builder, `analisar`, `executar` e
  `csv->dados`.

`consultas.core/csv->dados` aceita conteúdo textual CSV e devolve
`{:esquema [[campo tipo] ...] :registros [...]}`. As funções públicas exigidas
são `consulta`, `onde`, `selecionar`, `ordenar-por`, `limitar`, `analisar` e
`executar`.
