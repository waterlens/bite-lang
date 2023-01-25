use smartstring::alias::String;
use thiserror::Error;

#[derive(Debug, Error, PartialEq)]
pub enum UnescapeError {
    #[error("invalid escape {escape}")]
    InvalidEscape { escape: char },
    #[error("unexpected end of escape sequence")]
    UnexpectedEndOfString,
}

pub fn unescape(s: &str) -> Result<String, UnescapeError> {
    enum State {
        Normal,
        Escape,
    }
    use State::*;
    let mut iter = s.chars();
    let mut r = String::new();
    let mut st = Normal;
    loop {
        match st {
            Normal => {
                if let Some(x) = iter.next() {
                    if x == '\\' {
                        st = Escape;
                    } else {
                        r.push(x);
                        st = Normal;
                    }
                } else {
                    break;
                }
            }
            Escape => {
                if let Some(x) = iter.next() {
                    let ch = match x {
                        'a' => '\u{07}',
                        'b' => '\u{08}',
                        't' => '\u{09}',
                        'n' => '\u{0A}',
                        'v' => '\u{0B}',
                        'f' => '\u{0C}',
                        'r' => '\u{0D}',
                        'e' => '\u{1B}',
                        '\\' | '\'' | '\"' => x,
                        _ => {
                            return Err(UnescapeError::InvalidEscape { escape: x });
                        }
                    };
                    r.push(ch);
                    st = Normal;
                } else {
                    return Err(UnescapeError::UnexpectedEndOfString);
                }
            }
        }
    }
    Ok(r)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unescape_test() {
        assert_eq!(unescape(r#"\""#), Ok("\"".into()));
        assert_eq!(
            unescape(r#"\x"#),
            Err(UnescapeError::InvalidEscape { escape: 'x' })
        );
        assert_eq!(unescape(r#"\"#), Err(UnescapeError::UnexpectedEndOfString));
        assert_eq!(unescape(r#"\\\""#), Ok("\\\"".into()));
        assert_eq!(
            unescape(r#"\a\b\v\f\n\r\t\e\\\'\""#),
            Ok("\u{07}\u{08}\u{0b}\u{0c}\u{0a}\u{0d}\u{09}\u{1b}\u{5c}\u{27}\u{22}".into())
        )
    }
}
