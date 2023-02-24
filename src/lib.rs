mod alt;
mod permutation;
mod tuple;

pub use alt::alt;
pub use permutation::permutation;
pub use picktok_proc_macros::TokenParser;
pub use tuple::tuple;

// T stands for Token
// O stands for Output

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ParseErrorKind<T> {
    Expects { expects: &'static str, found: T },
    NotEnoughToken,
    Fail,
    InfiniteLoop,
    Context(&'static str),
    Other(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ParseError<T> {
    pub errors: Vec<ParseErrorKind<T>>,
    pub tokens_consumed: usize,
}

impl<T> ParseError<T> {
    pub fn from_error_kind(kind: ParseErrorKind<T>) -> Self {
        ParseError {
            errors: vec![kind],
            tokens_consumed: 0,
        }
    }
    pub fn with_tokens_consumed(self, tokens_consumed: usize) -> Self {
        ParseError {
            errors: self.errors,
            tokens_consumed,
        }
    }
    pub fn with_error_appended(self, kind: ParseErrorKind<T>) -> Self {
        let mut errors = self.errors;
        errors.push(kind);
        ParseError {
            errors,
            tokens_consumed: self.tokens_consumed,
        }
    }
}

pub type ParseResult<'a, T, O> = Result<(&'a [T], O), ParseError<T>>;

pub trait TokenParser<'a, T, O> {
    fn parse(&mut self, tokens: &'a [T]) -> Result<(&'a [T], O), ParseError<T>>;
}

impl<'a, T, O, F> TokenParser<'a, T, O> for F
where
    T: 'a,
    F: FnMut(&'a [T]) -> Result<(&'a [T], O), ParseError<T>>,
{
    fn parse(&mut self, tokens: &'a [T]) -> Result<(&'a [T], O), ParseError<T>> {
        self(tokens)
    }
}

pub trait UnwrapToken<T> {
    fn unwrap_token(&self) -> &T;
}

impl<T> UnwrapToken<T> for T {
    fn unwrap_token(&self) -> &T {
        self
    }
}

pub fn context<'a, T: 'a, O>(
    context: &'static str,
    mut parser: impl FnMut(&'a [T]) -> ParseResult<'a, T, O>,
) -> impl FnMut(&'a [T]) -> ParseResult<'a, T, O> {
    move |tokens: &'a [T]| match parser(tokens) {
        Err(err) => Err(err.with_error_appended(ParseErrorKind::Context(context))),
        ok => ok,
    }
}

pub fn many1<'a, T, O>(
    mut parser: impl TokenParser<'a, T, O>,
) -> impl FnMut(&'a [T]) -> ParseResult<'a, T, Vec<O>>
where
    T: 'a,
{
    move |tokens: &'a [T]| {
        let mut vec = Vec::new();
        let mut rest = tokens;
        let mut last_len = rest.len();
        let mut succeeded_at_least_once = false;
        while rest.len() > 0 {
            match parser.parse(rest) {
                Ok((rest_tokens, item)) => {
                    if rest_tokens.len() == last_len {
                        return Err(ParseError::from_error_kind(ParseErrorKind::InfiniteLoop));
                    }
                    last_len = rest_tokens.len();
                    rest = rest_tokens;
                    succeeded_at_least_once = true;
                    vec.push(item);
                    continue;
                }
                Err(err) => {
                    if succeeded_at_least_once {
                        break;
                    } else {
                        return Err(err);
                    }
                }
            }
        }
        Ok((rest, vec))
    }
}

pub fn many0<'a, T, O>(
    mut parser: impl TokenParser<'a, T, O>,
) -> impl FnMut(&'a [T]) -> ParseResult<'a, T, Vec<O>>
where
    T: 'a,
{
    move |tokens: &'a [T]| {
        let mut vec = Vec::new();
        let mut rest = tokens;
        let mut last_len = rest.len();
        while rest.len() > 0 {
            match parser.parse(rest) {
                Ok((rest_tokens, item)) => {
                    if rest_tokens.len() == last_len {
                        return Err(ParseError::from_error_kind(ParseErrorKind::InfiniteLoop));
                    }
                    last_len = rest_tokens.len();
                    rest = rest_tokens;
                    vec.push(item);
                    continue;
                }
                _ => break,
            }
        }
        Ok((rest, vec))
    }
}

pub fn many0_until_end<'a, T, O>(
    mut parser: impl TokenParser<'a, T, O>,
) -> impl FnMut(&'a [T]) -> ParseResult<'a, T, Vec<O>>
where
    T: 'a + std::fmt::Debug,
{
    move |tokens: &'a [T]| {
        let mut vec = Vec::new();
        let mut rest = tokens;
        let mut last_len = rest.len();
        while rest.len() > 0 {
            match parser.parse(rest) {
                Ok((rest_tokens, item)) => {
                    if rest_tokens.len() == last_len {
                        return Err(ParseError::from_error_kind(ParseErrorKind::InfiniteLoop));
                    }
                    last_len = rest_tokens.len();
                    rest = rest_tokens;
                    vec.push(item);

                    if rest_tokens.is_empty() {
                        return Ok((rest, vec));
                    }

                    continue;
                }
                Err(err) => return Err(err),
            }
        }
        Ok((rest, vec))
    }
}

pub fn opt<'a, T, O>(
    mut parser: impl TokenParser<'a, T, O>,
) -> impl FnMut(&'a [T]) -> ParseResult<'a, T, Option<O>> {
    move |tokens: &'a [T]| match parser.parse(tokens) {
        Ok((rest, output)) => Ok((rest, Some(output))),
        Err(_) => Ok((tokens, None)),
    }
}

pub fn delimited<'a, T: 'a, O1, O2, O3>(
    mut l: impl FnMut(&'a [T]) -> ParseResult<'a, T, O1>,
    mut main: impl FnMut(&'a [T]) -> ParseResult<'a, T, O2>,
    mut r: impl FnMut(&'a [T]) -> ParseResult<'a, T, O3>,
) -> impl FnMut(&'a [T]) -> ParseResult<'a, T, O2> {
    move |tokens: &'a [T]| {
        let (rest, _) = l(tokens)?;
        let (rest, result) = main(rest)?;
        let (rest, _) = r(rest)?;

        Ok((rest, result))
    }
}

pub fn preceded<'a, O1, T: 'a, O2>(
    mut first: impl FnMut(&'a [T]) -> ParseResult<'a, T, O1>,
    mut second: impl FnMut(&'a [T]) -> ParseResult<'a, T, O2>,
) -> impl FnMut(&'a [T]) -> ParseResult<'a, T, O2> {
    move |tokens: &'a [T]| {
        let (rest, _) = first(tokens)?;
        let (rest, result) = second(rest)?;

        Ok((rest, result))
    }
}

pub fn terminated<'a, T: 'a, O1, O2>(
    mut first: impl FnMut(&'a [T]) -> ParseResult<'a, T, O1>,
    mut second: impl FnMut(&'a [T]) -> ParseResult<'a, T, O2>,
) -> impl FnMut(&'a [T]) -> ParseResult<'a, T, O1> {
    move |tokens: &'a [T]| {
        let (rest, result) = first(tokens)?;
        let (rest, _) = second(rest)?;

        Ok((rest, result))
    }
}

pub fn separated_list0<'a, T: 'a, O, OSep>(
    mut separator_parser: impl FnMut(&'a [T]) -> ParseResult<'a, T, OSep>,
    mut item_parser: impl FnMut(&'a [T]) -> ParseResult<'a, T, O>,
) -> impl FnMut(&'a [T]) -> ParseResult<'a, T, Vec<O>> {
    move |tokens: &'a [T]| {
        let mut items = Vec::new();
        let mut rest = tokens;
        let mut last_len = rest.len();
        while !tokens.is_empty() {
            match item_parser(rest) {
                Ok((rest_tokens, item)) => {
                    if rest_tokens.len() == last_len {
                        return Err(ParseError::from_error_kind(ParseErrorKind::InfiniteLoop));
                    }
                    last_len = rest_tokens.len();
                    rest = rest_tokens;
                    items.push(item);
                }
                Err(_) => return Ok((rest, items)),
            }
            if rest.is_empty() {
                return Ok((rest, items));
            }
            match separator_parser(rest) {
                Ok((rest_tokens, _)) => {
                    rest = rest_tokens;
                }
                Err(_) => return Ok((rest, items)),
            }
        }
        Ok((&[], Vec::new()))
    }
}

pub fn separated_list1<'a, T: 'a, O, OSep>(
    mut separator_parser: impl FnMut(&'a [T]) -> ParseResult<'a, T, OSep>,
    mut item_parser: impl FnMut(&'a [T]) -> ParseResult<'a, T, O>,
) -> impl FnMut(&'a [T]) -> ParseResult<'a, T, Vec<O>> {
    move |tokens: &'a [T]| {
        let num_tokens = tokens.len();
        let mut items = Vec::new();
        let mut rest = tokens;
        let mut last_len = rest.len();
        while !tokens.is_empty() {
            match item_parser(rest) {
                Ok((rest_tokens, item)) => {
                    if rest_tokens.len() == last_len {
                        return Err(ParseError::from_error_kind(ParseErrorKind::InfiniteLoop));
                    }
                    last_len = rest_tokens.len();
                    rest = rest_tokens;
                    items.push(item);
                }
                Err(err) => {
                    if items.len() > 0 {
                        return Ok((rest, items));
                    } else {
                        return Err(err.with_tokens_consumed(num_tokens - rest.len()));
                    }
                }
            }
            if rest.is_empty() {
                return Ok((rest, items));
            }
            match separator_parser(rest) {
                Ok((rest_tokens, _)) => {
                    rest = rest_tokens;
                }
                Err(_) => return Ok((rest, items)),
            }
        }
        // If tokens is empty, returns error.
        return Err(ParseError {
            errors: vec![ParseErrorKind::NotEnoughToken],
            tokens_consumed: 0,
        });
    }
}

pub fn map<'a, T: 'a, OParser, O>(
    mut parser: impl FnMut(&'a [T]) -> ParseResult<'a, T, OParser>,
    mut mapper: impl FnMut(OParser) -> O,
) -> impl FnMut(&'a [T]) -> ParseResult<'a, T, O> {
    move |tokens: &'a [T]| {
        let (rest, result) = parser(tokens)?;
        Ok((rest, mapper(result)))
    }
}

pub fn map_res<'a, T: 'a, O1, O2>(
    mut parser: impl FnMut(&'a [T]) -> ParseResult<'a, T, O1>,
    mut mapper: impl FnMut(ParseResult<'a, T, O1>) -> ParseResult<'a, T, O2>,
) -> impl FnMut(&'a [T]) -> ParseResult<'a, T, O2> {
    move |tokens: &'a [T]| mapper(parser(tokens))
}

pub fn success<'a, T: 'a>(tokens: &'a [T]) -> ParseResult<'a, T, &T> {
    if tokens.is_empty() {
        return Err(ParseError {
            errors: vec![ParseErrorKind::NotEnoughToken],
            tokens_consumed: 0,
        });
    }
    Ok((&tokens[1..], &tokens[0]))
}

pub fn fail<'a, T: 'a>(tokens: &'a [T]) -> ParseResult<'a, &T, T> {
    if tokens.is_empty() {
        return Err(ParseError {
            errors: vec![ParseErrorKind::NotEnoughToken],
            tokens_consumed: 0,
        });
    }
    Err(ParseError {
        errors: vec![ParseErrorKind::Fail],
        tokens_consumed: 0,
    })
}

pub fn many0_count<'a, T: 'a, O>(
    mut parser: impl FnMut(&'a [T]) -> ParseResult<'a, T, O>,
) -> impl FnMut(&'a [T]) -> ParseResult<'a, T, usize> {
    move |tokens: &'a [T]| {
        let mut rest = tokens;
        let mut count = 0;
        loop {
            let len = rest.len();
            match parser(rest) {
                Ok((i, _)) => {
                    if i.len() == len {
                        return Err(ParseError {
                            errors: vec![ParseErrorKind::InfiniteLoop],
                            tokens_consumed: 0,
                        });
                    }

                    rest = i;
                    count += 1;
                }
                Err(_) => return Ok((rest, count)),
            }
        }
    }
}
