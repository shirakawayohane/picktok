use crate::*;
use seq_macro::seq;

pub trait Tuple<'a, W, O> {
    /// Tries to apply all parsers in the tuple in various orders until all of them succeed
    fn tuple(&mut self, tokens: &'a [W]) -> ParseResult<'a, W, O>;
}

pub fn tuple<'a, W: Clone, O, List: Tuple<'a, W, O>>(
    mut l: List,
) -> impl FnMut(&'a [W]) -> ParseResult<'a, W, O> {
    move |tokens: &'a [W]| l.tuple(tokens)
}

macro_rules! alt_trait_impl {
    ($n:expr) => {
      seq!(N in 0..$n {
        impl<'a, W, #(O~N,)* #(P~N,)*> Tuple<'a, W, (#(O~N,)*)> for (#(P~N,)*)
          where
          W: 'a,
          #(
            P~N: TokenParser<'a, W, O~N>,
          )*
        {
          fn tuple(&mut self, tokens: &'a [W]) -> ParseResult<'a, W, (#(O~N,)*)> {
            let _num_tokens = tokens.len();
            #(
                let (tokens, result~N) = match self.N.parse(tokens) {
                    Ok((rest, result)) => (rest, result),
                    Err(err) => return Err(err.with_tokens_consumed(_num_tokens - tokens.len()))
                };
            )*
            Ok((tokens, (#(result~N,)*)))
          }
      }
      });
    };
}

macro_rules! tuple_trait {
  ($n: expr) => {
    seq!(N in 0..$n {
        alt_trait_impl!(N);
    });
  }
}

tuple_trait!(16);
