use std::{io::BufRead, iter::Peekable};

use serde::{Deserialize, Serialize};

pub struct Tokenizer<R>
where
    R: BufRead,
{
    read_char: Peekable<ReadChar<R>>,
}

impl<R> Tokenizer<R>
where
    R: BufRead,
{
    pub fn new(read: R) -> Self {
        Self {
            read_char: ReadChar::new(read).peekable(),
        }
    }
}

struct ReadChar<R> {
    reader: R,
    line: Option<String>,
    pos: usize,
}

#[derive(PartialEq, Eq, Debug, Hash, Clone, Serialize, Deserialize)]
pub struct Term(String);

impl Term {
    #[cfg(test)]
    pub fn new(term: String) -> Self {
        Term(term)
    }
}

impl<R> ReadChar<R> {
    pub fn new(reader: R) -> Self {
        Self {
            reader,
            line: None,
            pos: 0,
        }
    }
}

impl<R> Iterator for ReadChar<R>
where
    R: BufRead,
{
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        match self.line.as_mut() {
            Some(line) => match line.chars().nth(self.pos) {
                Some(character) => {
                    self.pos += 1;
                    Some(character)
                }
                None => {
                    self.line = None;
                    self.pos = 0;
                    self.next()
                }
            },
            None => {
                let mut line = String::new();
                if let Ok(n) = self.reader.read_line(&mut line) {
                    if n == 0 {
                        return None;
                    }
                    self.line = Some(line);
                    self.next()
                } else {
                    None
                }
            }
        }
    }
}

impl<R> Iterator for Tokenizer<R>
where
    R: BufRead,
{
    type Item = Term;
    fn next(&mut self) -> Option<Self::Item> {
        match self.read_char.next() {
            Some(ch) if ch.is_alphabetic() => {
                let mut token = String::new();
                token.extend(ch.to_uppercase());
                let mut old_lower_case = false;
                while let Some(ch) = self.read_char.next_if(|ch| {
                    ch.is_alphanumeric() && !(old_lower_case == true && ch.is_uppercase())
                }) {
                    old_lower_case = ch.is_lowercase();
                    token.extend(ch.to_uppercase());
                }
                Some(Term(token))
            }
            Some(ch) if ch.is_numeric() => {
                let mut token = String::new();
                token.push(ch);
                while let Some(ch) = self
                    .read_char
                    .next_if(|ch| ch.is_numeric() || *ch == '.' || *ch == ',')
                {
                    token.push(ch);
                }
                Some(Term(token))
            }
            Some(x) if x.is_whitespace() => self.next(),
            Some('.') | Some(',') | Some(';') | Some(':') => self.next(),
            Some(x) => Some(Term(String::from(x))),
            None => None,
        }
    }
}

#[cfg(test)]
mod test {
    use std::io::Cursor;

    use super::*;
    #[test]
    fn tokenize_text() {
        let text = Cursor::new("Some text, Example123. 32.12 - camelCase CamelCase UPPERCASE");
        let tokens = Tokenizer::new(text).collect::<Vec<Term>>();
        assert_eq!(
            vec![
                Term("SOME".into()),
                Term("TEXT".into()),
                Term("EXAMPLE123".into()),
                Term("32.12".into()),
                Term("-".into()),
                Term("CAMEL".into()),
                Term("CASE".into()),
                Term("CAMEL".into()),
                Term("CASE".into()),
                Term("UPPERCASE".into())
            ],
            tokens
        );
    }
}
