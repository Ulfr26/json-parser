use crate::JsonValue;
use nom::branch::alt;
use nom::character::complete::multispace0;
use nom::combinator::{eof, map_res, opt};
use nom::error::ParseError;
use nom::multi::{fold_many0, many_m_n, separated_list1};
use nom::sequence::terminated;
use nom::sequence::{delimited, pair, preceded};
use nom::AsChar;
use nom::{
    bytes::complete::tag, character::complete::satisfy, combinator::recognize, multi::many1_count,
    sequence::tuple, IResult,
};
use nom::{InputIter, Parser, Slice};
use std::collections::HashMap;
use std::ops::RangeFrom;

#[derive(Debug)]
pub struct ParseUnicodeCharError {
    num: u32,
}

impl std::fmt::Display for ParseUnicodeCharError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!(
            "error while parsing a unicode character from u32: {}",
            self.num
        ))
    }
}

impl std::error::Error for ParseUnicodeCharError {}

fn sign(input: &str) -> IResult<&str, &str> {
    recognize(opt(alt((tag("+"), tag("-")))))(input)
}

fn onenine(input: &str) -> IResult<&str, char> {
    satisfy(|c| ('1'..='9').contains(&c))(input)
}

fn digit(input: &str) -> IResult<&str, char> {
    alt((satisfy(|c| c == '0'), onenine))(input)
}

fn digits(input: &str) -> IResult<&str, &str> {
    recognize(many1_count(digit))(input)
}

fn fraction(input: &str) -> IResult<&str, &str> {
    recognize(opt(preceded(tag("."), digits)))(input)
}

fn exponent(input: &str) -> IResult<&str, &str> {
    recognize(opt(tuple((
        satisfy(|c| c == 'e' || c == 'E'),
        sign,
        digits,
    ))))(input)
}

fn integer(input: &str) -> IResult<&str, &str> {
    recognize(tuple((
        sign,
        alt((recognize(pair(onenine, digits)), recognize(digit))),
    )))(input)
}

fn number(input: &str) -> IResult<&str, f32> {
    recognize(tuple((integer, fraction, exponent)))
        .map(|num| num.parse::<f32>().unwrap())
        .parse(input)
}

fn hex(input: &str) -> IResult<&str, char> {
    alt((
        digit,
        satisfy(|c| ('A'..='F').contains(&c)),
        satisfy(|c| ('a'..='f').contains(&c)),
    ))(input)
}

fn unicode_char(input: &str) -> IResult<&str, char> {
    map_res(
        preceded(tag("u"), many_m_n(4, 4, hex)),
        |digits| -> Result<char, Box<dyn std::error::Error>> {
            let num = u32::from_str_radix(&String::from_iter(digits.into_iter()), 16)?;
            let res = char::from_u32(num).ok_or(ParseUnicodeCharError { num })?;
            Ok(res)
        },
    )(input)
}

fn map_to_output<I, F, O, R: Clone, E: ParseError<I>>(
    mut inner: F,
    output: R,
) -> impl FnMut(I) -> IResult<I, R, E>
where
    F: FnMut(I) -> IResult<I, O, E>,
{
    move |i| {
        let (input, _) = inner(i)?;
        Ok((input, output.clone()))
    }
}

fn match_char<I: Slice<RangeFrom<usize>> + InputIter>(c: char) -> impl FnMut(I) -> IResult<I, char>
where
    <I as InputIter>::Item: AsChar,
{
    satisfy(move |i| i == c)
}

fn special_character(input: &str) -> IResult<&str, char> {
    alt((
        map_to_output(match_char('b'), '\u{8}'),
        map_to_output(match_char('f'), '\u{C}'),
        map_to_output(match_char('n'), '\n'),
        map_to_output(match_char('r'), '\r'),
        map_to_output(match_char('t'), '\t'),
    ))(input)
}

fn escape(input: &str) -> IResult<&str, char> {
    alt((
        satisfy(|c| ['"', '\\', '/'].contains(&c)),
        special_character,
        unicode_char,
    ))(input)
}

fn character(input: &str) -> IResult<&str, char> {
    alt((
        satisfy(|c| c != '"' && c != '\\' && ('\u{0020}'..'\u{10FFFF}').contains(&c)),
        preceded(tag("\\"), escape),
    ))(input)
}

fn characters(input: &str) -> IResult<&str, String> {
    fold_many0(character, String::new, |mut s: String, c| {
        s.push(c);
        s
    })(input)
}

fn string(input: &str) -> IResult<&str, String> {
    delimited(tag("\""), characters, tag("\""))(input)
}

fn element(input: &str) -> IResult<&str, JsonValue> {
    delimited(multispace0, value, multispace0)(input)
}

fn elements(input: &str) -> IResult<&str, Vec<JsonValue>> {
    separated_list1(tag(","), element)(input)
}

fn array(input: &str) -> IResult<&str, Vec<JsonValue>> {
    delimited(
        tag("["),
        alt((elements, multispace0.map(|_| Vec::new()))),
        tag("]"),
    )(input)
}

fn member(input: &str) -> IResult<&str, (String, JsonValue)> {
    let (input, (tag, _, value)) = tuple((
        delimited(multispace0, string, multispace0),
        tag(":"),
        element,
    ))(input)?;

    Ok((input, (tag, value)))
}

fn members(input: &str) -> IResult<&str, HashMap<String, JsonValue>> {
    separated_list1(tag(","), member)
        .map(|v| HashMap::from_iter(v.into_iter()))
        .parse(input)
}

fn object(input: &str) -> IResult<&str, HashMap<String, JsonValue>> {
    delimited(
        tag("{"),
        alt((members, multispace0.map(|_| HashMap::new()))),
        tag("}"),
    )(input)
}

fn value(input: &str) -> IResult<&str, JsonValue> {
    alt((
        object.map(JsonValue::Object),
        array.map(JsonValue::Array),
        string.map(JsonValue::String),
        number.map(JsonValue::Number),
        map_to_output(tag("true"), JsonValue::Bool(true)),
        map_to_output(tag("false"), JsonValue::Bool(false)),
        map_to_output(tag("null"), JsonValue::Null),
    ))(input)
}

pub fn json(input: &str) -> Result<JsonValue, nom::Err<nom::error::Error<&str>>> {
    let (_, value) = terminated(element, eof)(input)?;
    Ok(value)
}

#[cfg(test)]
mod tests {
    use super::*;

    // https://stackoverflow.com/a/28392068
    macro_rules! hashmap {
        ($( $key: expr => $val: expr ),*) => {{
             let mut map = ::std::collections::HashMap::new();
             $( map.insert($key, $val); )*
             map
        }}
    }

    #[test]
    fn test_sign() {
        assert_eq!(sign("hi"), Ok(("hi", "")));
        assert_eq!(sign("+hi"), Ok(("hi", "+")));
        assert_eq!(sign("-hi"), Ok(("hi", "-")));
    }

    #[test]
    fn test_digits() {
        assert_eq!(digits("12345"), Ok(("", "12345")));
        assert_eq!(digits("0000beans"), Ok(("beans", "0000")));
    }

    #[test]
    fn test_integer() {
        assert_eq!(integer("12345"), Ok(("", "12345")));
        assert_eq!(integer("-134"), Ok(("", "-134")));
        assert_eq!(integer("0123"), Ok(("123", "0")));
    }

    #[test]
    fn test_num() {
        assert_eq!(number("12345"), Ok(("", 12345.0)));
        assert_eq!(number("2345.0"), Ok(("", 2345.0)));
        assert_eq!(number("345.0beans"), Ok(("beans", 345.0)));
        assert_eq!(number("12.0e23"), Ok(("", 12.0e23)));
        assert_eq!(number("15.0e23e23"), Ok(("e23", 15.0e23)));
    }

    #[test]
    fn test_escaped_char() {
        assert_eq!(escape("\\"), Ok(("", '\\')));
        assert_eq!(escape("/"), Ok(("", '/')));
        assert_eq!(escape("\""), Ok(("", '"')));
        assert_eq!(escape("n"), Ok(("", '\n')));

        assert_eq!(escape("u3090"), Ok(("", 'ゐ')));
        assert_eq!(escape("u2230"), Ok(("", '∰')));
    }

    #[test]
    fn test_char() {
        assert_eq!(character("abcd"), Ok(("bcd", 'a')));
        assert_eq!(character(r"\nhello"), Ok(("hello", '\n')));
        assert_eq!(
            character(r"\u30eB\u30FC\30d3\u30fc"),
            Ok((r"\u30FC\30d3\u30fc", 'ル'))
        );
    }

    #[test]
    fn test_string() {
        assert_eq!(string("\"abcd\""), Ok(("", "abcd".to_string())));
        assert_eq!(string("\"\\nhello\""), Ok(("", "\nhello".to_string())));
        assert_eq!(
            string("\"\\u30eB\\u30FC\\u30d3\\u30fc\""),
            Ok(("", "ルービー".to_string()))
        );
    }

    #[test]
    fn test_json() {
        let example1 = "{
    \"fruit\": \"Apple\",
    \"size\": \"Large\",
    \"colour\": \"Red\"
}";

        let value1 = JsonValue::Object(hashmap![
            "fruit".to_string() => JsonValue::String("Apple".to_string()),
            "size".to_string() => JsonValue::String("Large".to_string()),
            "colour".to_string() => JsonValue::String("Red".to_string())
        ]);

        assert_eq!(json(example1), Ok(value1));
    }
}
