use crate::*;
use seq_macro::seq;

pub trait Permutation<'a, T, O> {
    /// Tries to apply all parsers in the permutation in various orders until all of them succeed
    fn permutation(&mut self, tokens: &'a [T]) -> ParseResult<'a, T, O>;
}

pub fn permutation<'a, T: Clone, O, List: Permutation<'a, T, O>>(
    mut l: List,
) -> impl FnMut(&'a [T]) -> ParseResult<'a, T, O> {
    move |tokens: &'a [T]| l.permutation(tokens)
}

macro_rules! alt_trait_impl {
    ($n:expr) => {
      seq!(N in 0..$n {
        impl<'a, T, #(O~N,)* #(P~N,)*> Permutation<'a,T, (#(O~N,)*)> for (#(P~N,)*)
          where
          #(
            P~N: TokenParser<'a, T, O~N>,
          )*
        {
          fn permutation(&mut self, tokens: &'a [T]) -> ParseResult<'a, T, (#(O~N,)*)> {
            let _num_tokens = tokens.len();
            let mut _rest = tokens;
            #(let mut _succeeded_~N = false;)*
            #(let mut _error_of_parser~N: Option<ParseError<T>> = None;)*
            #(let mut _result_of_parser~N: Option<O~N> = None;)*
            for _ in 0..$n {
                #(
                    if !_succeeded_~N {
                        match self.N.parse(_rest) {
                            Ok((rest_tokens, result)) => {
                                _rest = rest_tokens;
                                _result_of_parser~N = Some(result);
                                _succeeded_~N = true;
                            }
                            Err(err) => {
                                _error_of_parser~N = Some(err);
                            }
                        };
                    }
                )*
            }
            #(
                if !_succeeded_~N {
                    return Err(_error_of_parser~N.unwrap());
                }
            )*
            #(
                let result~N = _result_of_parser~N.unwrap();
            )*
            Ok((_rest, (#(result~N,)*)))
          }
      }
      });
    };
}

macro_rules! permutation_trait {
  ($n: expr) => {
    seq!(N in 0..$n {
        alt_trait_impl!(N);
    });
  }
}

permutation_trait!(16);
