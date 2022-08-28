//! Trying to express the same idea, but in rust which is more strict about types

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

trait Cursor<T> {
    fn peek(&self, length: i32) -> Option<T>;
    fn take(&mut self, length: i32) -> Option<T>;
    fn has(&self, length: i32) -> bool;
    fn clone(&self) -> Self;
}

#[derive(Debug)]
struct Token<T> {
    content: Vec<T>,
    children: Vec<Token<T>>,
}

#[derive(Debug)]
struct Error<E> {
    message: String,
    error: Option<E>,
}

impl<E> Error<E> {
    fn new(message: String, error: Option<E>) -> Error<E> {
        Error {
            message: message,
            error: error,
        }
    }
}

struct Parsed<InputToken, OutputToken, E, C: Cursor<InputToken>> {
    token: Token<OutputToken>,
    errors: Vec<Error<E>>,
    cursor: C,
    is_error: bool,
    is_commited: bool,
    phantom: std::marker::PhantomData<InputToken>,
}

impl<IT, OT, E, C: Cursor<IT>> Parsed<IT, OT, E, C> {
    fn new(
        token: Token<OT>,
        errors: Vec<Error<E>>,
        cursor: C,
        is_error: bool,
        is_commited: bool,
    ) -> Self {
        Parsed {
            token,
            errors,
            cursor,
            is_error,
            is_commited: is_commited,
            phantom: std::marker::PhantomData,
        }
    }

    fn ok(token: Token<OT>, errors: Vec<Error<E>>, cursor: C) -> Self {
        Parsed {
            token,
            errors,
            cursor,
            is_error: false,
            is_commited: false,
            phantom: std::marker::PhantomData,
        }
    }

    fn err(token: Token<OT>, errors: Vec<Error<E>>, cursor: C) -> Self {
        Parsed {
            token,
            errors,
            cursor,
            is_error: true,
            is_commited: false,
            phantom: std::marker::PhantomData,
        }
    }

    fn is_err(&self) -> bool {
        return self.is_error && !self.is_commited;
    }
}

trait Parser<InputToken, OutputToken, Error, C: Cursor<InputToken>> {
    fn parse(&self, input: C) -> Parsed<InputToken, OutputToken, Error, C>;
}

struct Sequence<InputToken, OutputToken, Error, C: Cursor<InputToken>> {
    parsers: Vec<Box<dyn Parser<InputToken, OutputToken, Error, C>>>,
}

impl<IT, OT, E, C: Cursor<IT>> Sequence<IT, OT, E, C> {
    fn new(parsers: Vec<Box<dyn Parser<IT, OT, E, C>>>) -> Self {
        Sequence { parsers }
    }
}

impl<IT, OT, E, C: Cursor<IT>> Parser<IT, OT, E, C> for Sequence<IT, OT, E, C> {
    fn parse(&self, cursor: C) -> Parsed<IT, OT, E, C> {
        let mut cursor = cursor;
        let mut errors = Vec::new();
        let mut tokens = Vec::new();
        let mut is_error = false;
        let mut is_commited = false;
        for parser in self.parsers.iter() {
            let parsed = parser.parse(cursor);

            if parsed.is_err() {
                is_error = true;
            }
            if parsed.is_commited {
                is_commited = true;
            }

            cursor = parsed.cursor;

            errors.extend(parsed.errors);
            tokens.push(parsed.token);

            if is_error {
                break;
            }
        }
        is_error = is_error && !is_commited;
        let token = Token {
            content: Vec::new(),
            children: tokens,
        };
        return Parsed::new(token, errors, cursor, is_error, false);
    }
}

/// Parses using given parser and flattens one layer of children
/// i.e. returns token with concatenated content of token and its children, and with grandchildren
struct Flatten<IT, OT, E, C: Cursor<IT>> {
    parser: Box<dyn Parser<IT, OT, E, C>>,
}

impl<IT, OT, E, C: Cursor<IT>> Flatten<IT, OT, E, C> {
    fn new(parser: Box<dyn Parser<IT, OT, E, C>>) -> Self {
        Flatten { parser }
    }
}

impl<IT, OT, E, C: Cursor<IT>> Parser<IT, OT, E, C> for Flatten<IT, OT, E, C> {
    fn parse(&self, cursor: C) -> Parsed<IT, OT, E, C> {
        let parsed = self.parser.parse(cursor);
        let mut content = parsed.token.content;
        let mut children = Vec::new();
        for child in parsed.token.children.into_iter() {
            content.extend(child.content);
            children.extend(child.children);
        }

        let token = Token { content, children };
        return Parsed::new(
            token,
            parsed.errors,
            parsed.cursor,
            parsed.is_error,
            parsed.is_commited,
        );
    }
}

struct Repeat<IT, OT, E, C: Cursor<IT>> {
    min: u32,
    parser: Box<dyn Parser<IT, OT, E, C>>,
}

impl<IT, OT, E, C: Cursor<IT>> Repeat<IT, OT, E, C> {
    fn new(min: u32, parser: Box<dyn Parser<IT, OT, E, C>>) -> Self {
        Repeat { min, parser }
    }
}

impl<IT, OT, E, C: Cursor<IT>> Parser<IT, OT, E, C> for Repeat<IT, OT, E, C> {
    fn parse(&self, cursor: C) -> Parsed<IT, OT, E, C> {
        let mut cursor = cursor;
        let mut token = Token {
            content: Vec::new(),
            children: Vec::new(),
        };
        let mut errors = Vec::new();
        let mut count = 0;
        loop {
            let parsed = self.parser.parse(cursor);
            let err = parsed.is_err();
            cursor = parsed.cursor;
            token.content.extend(parsed.token.content);
            token.children.extend(parsed.token.children);
            if err {
                if count < self.min {
                    errors.extend(parsed.errors);
                    return Parsed::err(token, errors, cursor);
                } else {
                    return Parsed::ok(token, errors, cursor);
                }
            }
            errors.extend(parsed.errors);
            count += 1;
        }
    }
}

struct Commit<IT, OT, E, C: Cursor<IT>> {
    parser: Box<dyn Parser<IT, OT, E, C>>,
}

impl<IT, OT, E, C: Cursor<IT>> Commit<IT, OT, E, C> {
    fn new(parser: Box<dyn Parser<IT, OT, E, C>>) -> Self {
        Commit { parser }
    }
}

impl<IT, OT, E, C: Cursor<IT>> Parser<IT, OT, E, C> for Commit<IT, OT, E, C> {
    fn parse(&self, cursor: C) -> Parsed<IT, OT, E, C> {
        let mut parsed = self.parser.parse(cursor);
        if parsed.is_err() {
            return parsed;
        } else {
            parsed.is_commited = true;
            return parsed;
        }
    }
}

struct AnyOf<IT, OT, E, C: Cursor<IT>> {
    parsers: Vec<Box<dyn Parser<IT, OT, E, C>>>,
}

impl<IT, OT, E, C: Cursor<IT>> AnyOf<IT, OT, E, C> {
    fn new(parsers: Vec<Box<dyn Parser<IT, OT, E, C>>>) -> Self {
        AnyOf { parsers }
    }
}

impl<IT, OT, E, C: Cursor<IT>> Parser<IT, OT, E, C> for AnyOf<IT, OT, E, C> {
    fn parse(&self, cursor: C) -> Parsed<IT, OT, E, C> {
        for parser in self.parsers.iter() {
            let parsed = parser.parse(cursor.clone());
            if !parsed.is_err() {
                return parsed;
            }
        }

        let token = Token {
            content: Vec::new(),
            children: Vec::new(),
        };
        return Parsed::err(
            token,
            vec![Error::new("No viable alternative".to_owned(), None)],
            cursor,
        );
    }
}

struct StringCursor {
    text: Rc<String>,
    index: usize,
}

impl StringCursor {
    fn new(text: Rc<String>) -> Self {
        StringCursor { text, index: 0 }
    }
}

impl Cursor<String> for StringCursor {
    fn has(&self, length: i32) -> bool {
        self.index + length as usize <= self.text.len()
    }

    fn peek(&self, length: i32) -> Option<String> {
        if self.has(length) {
            Some(self.text[self.index..self.index + length as usize].to_string())
        } else {
            None
        }
    }

    fn take(&mut self, length: i32) -> Option<String> {
        if self.has(length) {
            let result = self.peek(length);
            self.index += length as usize;
            result
        } else {
            None
        }
    }

    fn clone(&self) -> Self {
        StringCursor {
            text: self.text.clone(),
            index: self.index,
        }
    }
}

struct ExpectString {
    text: String,
}

impl Parser<String, String, String, StringCursor> for ExpectString {
    fn parse(&self, cursor: StringCursor) -> Parsed<String, String, String, StringCursor> {
        let mut cursor = cursor;
        let text = cursor.take(self.text.len() as i32);
        if text == Some(self.text.clone()) {
            return Parsed::ok(
                Token {
                    content: vec![self.text.clone()],
                    children: Vec::new(),
                },
                Vec::new(),
                cursor,
            );
        } else {
            return Parsed::err(
                Token {
                    content: vec![],
                    children: Vec::new(),
                },
                vec![Error::new(format!("Expected '{}'", self.text), None)],
                cursor,
            );
        }
    }
}

type Rules<IT, OT, E, C> = HashMap<String, Box<dyn Parser<IT, OT, E, C>>>;

struct Rule<IT, OT, E, C: Cursor<IT>> {
    name: String,
    rules: Rc<RefCell<Rules<IT, OT, E, C>>>,
}

impl<IT, OT, E, C: Cursor<IT>> Rule<IT, OT, E, C> {
    fn new(name: String, rules: Rc<RefCell<Rules<IT, OT, E, C>>>) -> Self {
        Rule { name, rules }
    }
}

impl<IT, OT, E, C: Cursor<IT>> Parser<IT, OT, E, C> for Rule<IT, OT, E, C> {
    fn parse(&self, cursor: C) -> Parsed<IT, OT, E, C> {
        let rule = self.rules.borrow();
        return rule.get(&self.name).unwrap().parse(cursor);
    }
}

macro_rules! expect {
    ($text:expr) => {
        Box::new(ExpectString {
            text: $text.to_string(),
        })
    };
}

macro_rules! sequence {
    ($($parser:expr),*) => {
        Box::new(Sequence::new(vec![$($parser),*]))
    };
}
macro_rules! flatten {
    ($parser:expr) => {
        Box::new(Flatten::new($parser))
    };
}

macro_rules! any_of {
    ($($parser:expr),*) => {
        Box::new(AnyOf::new(vec![$($parser),*]))
    };
}

macro_rules! repeat {
    ($min:expr, $parser:expr) => {
        Box::new(Repeat::new($min, $parser))
    };
}

macro_rules! commit {
    ($parser:expr) => {
        Box::new(Commit::new($parser))
    };
}

macro_rules! flat {
    ($($parser:expr),*) => {
        flatten!(sequence!($($parser),*))
    };
}

fn main() {
    let parser = any_of!(
        flat!(commit!(flat!(expect!("a"), expect!("b"))), expect!("c")),
        expect!("abx")
    );

    let text = "abxdef".to_owned();
    let cursor = StringCursor::new(Rc::new(text));
    let parsed = parser.parse(cursor);
    println!("{:?}", parsed.token);
    println!("{:?}", parsed.errors);
}
