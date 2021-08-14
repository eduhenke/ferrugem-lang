# ferrugem-lang

Este projeto trata do desenvolvimento de um compilador didático para a disciplina de Construção de Compiladores (INE5426) do Curso de Bacharelado em Ciências da Computação da Universidade Federal de Santa Catarina (UFSC), e foi desenvolvido pelos alunos Nelson Luiz Joppi Filho e Eduardo Henke.

Para o desenvolvimento desse compilador, também é utilizada a ferramenta Logos, para o analisador léxico; e Lalrpop como analisador sintático.

Nas próximas subseções, daremos uma breve descrição a respeito da compilação e da execução do presente projeto.

## instruções

É necessário ter um compilador de Rust, daí no terminal:

```bash
make
```

Que vai realizar a analise léxica do arquivo-fonte `examples/simple.lcc`.
Veja o `Makefile` e a pasta `examples`, para outros arquivos.

## Executando

Para fazer o processo de compilação do arquivo feira.lcc, disponibilizado como um dos três programas para teste: cargo run -q examples/feira.lcc
Vale lembrar que o caminho do arquivo de teste passado como argumento deve ser relativo à raiz do projeto. Os programas teste disponibilizados para a linguagem Conv_CC estão disponíveis na pasta examples.

## Arquivos de teste LCC

Como apresentado na seção anterior, os programas teste para a linguagem ConvCC encontram-se no diretório examples, e consiste nos arquivos feira.lcc, vet.lcc, matriz.lcc e def_functions.lcc