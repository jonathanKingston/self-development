//use std::borrow::Cow;

const TOKEN: &'static str = ";";
const SPACE: &'static str = " ";

#[derive(Debug, PartialEq)]
pub struct TokenizerToken<'h> {
    token_str: &'h str
}

impl<'a> TokenizerToken <'a> {
    fn new(token_str: &'a str) -> Self {
        TokenizerToken { token_str: token_str }
    }
}

#[derive(Debug, PartialEq)]
pub struct TokenizerDirectiveRow<'z> {
    directive_row: Vec<TokenizerToken<'z>>
}

impl<'zz> TokenizerDirectiveRow<'zz> {
    fn new() -> Self {
        TokenizerDirectiveRow { directive_row: Vec::new() }
    }
    fn push(&mut self, token: TokenizerToken<'zz>) {
        self.directive_row.push(token)
    }
}

#[derive(Debug, PartialEq)]
pub struct TokenizerDirectives<'xx> {
    directives: Vec<TokenizerDirectiveRow<'xx>>
}

impl<'xxz> TokenizerDirectives<'xxz> {
    fn new() -> Self {
        TokenizerDirectives { directives: Vec::new() }
    }
    fn push(&mut self, directive_row: TokenizerDirectiveRow<'xxz>) {
        self.directives.push(directive_row)
    }
}

pub struct Tokenizer;

impl Tokenizer {
    fn get_tokens(policy: &str) -> TokenizerDirectives {
        let mut output: TokenizerDirectives = TokenizerDirectives::new();
        let policy_tokens: Vec<&str> = policy.split(TOKEN).collect(); 
        for part in policy_tokens {
            let mut p = TokenizerDirectiveRow::new();
            for pa in part.split(SPACE).filter(|&z| z != "").map(|x| TokenizerToken::new(x)) {
              p.push(pa);
            }
            output.push(p);
        }
        return output;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn vec_to_directives(vecs: Vec<Vec<&str>>) -> TokenizerDirectives {
        let mut directives = Vec::new();
        for strings in vecs {
            let row = vec_to_directive_row(strings);
            directives.push(row);
        }
        TokenizerDirectives { directives }
    }

    fn vec_to_directive_row(strings: Vec<&str>) -> TokenizerDirectiveRow {
        let mut directive_row = Vec::new();
        for s in strings.iter() {
            directive_row.push(TokenizerToken::new(s));
        }
        TokenizerDirectiveRow { directive_row }
    }

    #[test]
    fn get_token_output() {
        assert_eq!(Tokenizer::get_tokens("script-src x y z; other-thing b f g"),
            vec_to_directives(vec![
                vec!["script-src", "x", "y", "z"],
                vec!["other-thing", "b", "f", "g"]
            ])
        );
        assert_ne!(Tokenizer::get_tokens("script-src x y v; other-thing b f d"),
            vec_to_directives(vec![
                vec!["script-src", "x", "y", "z"],
                vec!["other-thing", "b", "f", "g"]
            ])
        );
    }
}
