# PickTok

A parser combinator like nom but specialized in parsing &[Token].

It has similar combinators as [nom](https://github.com/rust-bakery/nom), but also provides convenient parser generator macro for each Token variants.

If your parser system has `tokenize` or `lex` stage, then it will be happy to code with `picktok` to make your Syntax Tree.

## Example

Here is an example of parsing integer list from Token with variant of `LParen`, `RParent`, `Integer`.

```rust
// mod token

mod token {
    use token_combinator::TokenParser;

    #[derive(Clone, TokenParser)]
    pub enum Token {
        LParen,       // (
        RParen,       // )
        Integer(i64), // 10, 0xFF, 0b01, 0o70...
    }
}

mod ast {
    pub enum AST {
        IntegerLiteral,
        List(Vec<AST>),
    }
}

use token::parser::*;

pub fn parse_list(tokens: &[token::Token]) -> ast::AST {
    map(
        delimited(
            l_paren,
            many0(map(integer, |i| AST::IntegerLiteral(i))),
            r_paren,
        ),
        |integers| AST::List(integers),
    )(tokens)
}
```

```rust
// mod parser
use token::parser::*

fn parse_list(tokens: &[Token]) -> AST {
  
}
```
