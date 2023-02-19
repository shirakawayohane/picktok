use seq_macro::seq;

use crate::*;

pub trait Alt<'a, T, O> {
    fn alt(&mut self, tokens: &'a [T]) -> ParseResult<'a, T, O>;
}

pub fn alt<'a, T, O, List: Alt<'a, T, O>>(
    mut l: List,
) -> impl FnMut(&'a [T]) -> ParseResult<'a, T, O> {
    move |tokens: &'a [T]| l.alt(tokens)
}

macro_rules! alt_trait_impl {
    ($n:expr) => {
      seq!(N in 0..$n {
        impl<'a, O, #(P~N,)* T> Alt<'a, T, O> for (#(P~N,)*)
        where
          #(
            P~N: TokenParser<'a, T, O>,
          )*
        {
          fn alt(&mut self, _tokens: &'a [T]) -> ParseResult<'a, T, O> {
            let mut _max_consumed_tokens_len = 0;
            let mut _max_token_consumed_error: Option<ParseError<T>> = None;
            #(
              match self.N.parse(_tokens) {
                Err(err) => {
                  if err.tokens_consumed >= _max_consumed_tokens_len {
                    _max_consumed_tokens_len = err.tokens_consumed;
                    _max_token_consumed_error = Some(err);
                  }
                },
                result => return result
              }
            )*

            Err(_max_token_consumed_error.unwrap())
          }
      }
      });
    };
}

macro_rules! alt_trait {
  ($n: expr) => {
    seq!(N in 0..$n {
        alt_trait_impl!(N);
    });
  }
}

alt_trait!(24);
