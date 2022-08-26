'''
Lexer made from parser combinators.
Additionally it should be able to handle erroneous data
'''


from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import *
import re


@dataclass
class Source:
    filename: str
    text: str


@dataclass
class Location:
    source: Source
    begin: int
    end: int

    def __str__(self):
        return f"{self.source.filename}:{self.begin}..{self.end}"

    def length(self):
        return self.end - self.begin

    def join(locations):
        locations = list(locations)
        return Location(
            source=locations[0].source,
            begin=min(loc.begin for loc in locations),
            end=max(loc.end for loc in locations),
        )


@dataclass
class Token:
    kind: str
    text: str
    location: Location

    def __str__(self):
        return f"{self.kind}({repr(self.text)})@{self.location}"


class CharCursor:
    def __init__(self, source: Source, start: int = 0):
        self.source = source
        self.pos = start

    def has(self, length: int = 1) -> bool:
        return self.pos + length <= len(self.source.text)

    def current_location(self, length: int) -> Location:
        return Location(self.source, self.pos, self.pos + length)

    def peek(self, length: int) -> Tuple[str, Location]:
        if self.has(length):
            return self.source.text[self.pos:self.pos + length], self.current_location(length)
        else:
            return "", self.current_location(0)

    def take(self, length: int) -> Tuple[str, Location]:
        if self.has(length):
            result = self.peek(length)
            self.pos += length
            return result
        else:
            return "", self.current_location(0)

    def clone(self) -> "CharCursor":
        return CharCursor(self.source, self.pos)

    def remaining(self) -> str:
        return self.source.text[self.pos:]


@dataclass
class Error:
    message: str
    location: Location

    def __str__(self):
        return f"Error: {self.message} at {self.location}"


@dataclass
class Lexed:
    tokens: List[Token]
    errors: List[Error]
    cursor: CharCursor
    commited: bool = False
    is_error: bool = False

    def length(self):
        return sum(token.location.length() for token in self.tokens)

    def is_err(self):
        return self.is_error and not self.commited


class Lexer(ABC):
    def __init__(self, name):
        self.name = name

    @abstractmethod
    def lex(self, cursor: CharCursor) -> Lexed:
        pass


class Regex(Lexer):
    def __init__(self, name, regex):
        super().__init__(name)
        self.regex = regex
        self.pattern = re.compile(f"^({regex})")

    def lex(self, cursor: CharCursor) -> Lexed:
        matched = self.pattern.search(cursor.remaining())
        if matched:
            matched = matched.group(0)
            text, location = cursor.take(len(matched))
            return Lexed([Token(self.name, text, location)], [], cursor)
        else:
            error = Error(f"{self.name} doesn't match",
                          cursor.current_location(0))
            return Lexed([], [error], cursor, is_error=True)


class AnyOf(Lexer):
    def __init__(self, name, lexers):
        super().__init__(name)
        self.lexers = lexers

    def lex(self, cursor: CharCursor) -> Lexed:
        results = [lexer.lex(cursor.clone()) for lexer in self.lexers]
        ok_results = [result for result in results if not result.is_err()]
        if ok_results:
            return max(ok_results, key=lambda result: result.length())
        else:
            error_message = f"Expected one of: {', '.join(lexer.name for lexer in self.lexers)}"
            location = cursor.current_location(1)
            cursor.take(1)
            return Lexed([], [Error(error_message, location)], cursor, is_error=True)


class Repeat(Lexer):
    def __init__(self, name, lexer):
        super().__init__(name)
        self.lexer = lexer

    def lex(self, cursor: CharCursor) -> Lexed:
        results = []
        while cursor.has():
            result = self.lexer.lex(cursor.clone())
            if result.is_err():
                if len(results) == 0:
                    return result
                break
            results.append(result)
            cursor = result.cursor
        return Lexed(sum([res.tokens for res in results], []), [], cursor)


class Maybe(Lexer):
    def __init__(self, name, lexer):
        super().__init__(name)
        self.lexer = lexer

    def lex(self, cursor: CharCursor) -> Lexed:
        result = self.lexer.lex(cursor.clone())
        if result.is_err():
            return Lexed([], [], cursor)
        else:
            return result


class Sequence(Lexer):
    def __init__(self, name, lexers):
        super().__init__(name)
        self.lexers = lexers

    def lex(self, cursor: CharCursor) -> Lexed:
        result = Lexed([], [], cursor)
        for lexer in self.lexers:
            lexed = lexer.lex(result.cursor)
            result.is_error = result.is_error or lexed.is_error
            result.commited = result.commited or lexed.commited
            result.tokens.extend(lexed.tokens)
            result.errors.extend(lexed.errors)
            result.cursor = lexed.cursor
            if lexed.is_err():
                break
        return result


class Commit(Lexer):
    def __init__(self, name, lexer):
        super().__init__(name)
        self.lexer = lexer

    def lex(self, cursor: CharCursor) -> Lexed:
        result = self.lexer.lex(cursor.clone())
        if not result.is_err():
            result.commited = True
        return result


class Exactly(Lexer):
    def __init__(self, name, text):
        super().__init__(name)
        self.text = text

    def lex(self, cursor: CharCursor) -> Lexed:
        text, location = cursor.peek(len(self.text))
        if text == self.text:
            cursor.take(len(self.text))
            return Lexed([Token(self.name, text, location)], [], cursor)
        else:
            error = Error(f"Expected {self.text}, got {text}", location)
            return Lexed([], [error], cursor, is_error=True)


class Join(Lexer):
    def __init__(self, name, lexer):
        super().__init__(name)
        self.lexer = lexer

    def lex(self, cursor: CharCursor) -> Lexed:
        result = self.lexer.lex(cursor)
        if result.is_err():
            return result
        content = "".join(token.text for token in result.tokens)
        location = Location.join(token.location for token in result.tokens)
        token = Token(self.name, content, location)
        return Lexed([token], result.errors, result.cursor)


class StringLiteral(Lexer):
    def __init__(self):
        super().__init__('string')

        self.lexer = Sequence("string",
                              [
                                  Commit("string", Exactly(
                                      "string start", "\"")),
                                  Repeat(
                                      "string content",
                                      AnyOf("string content", [
                                          Exactly("escaped quote", '\\"'),
                                          Regex("non-quote", r"[^\"]"),
                                      ])
                                  ),
                                  Exactly("string end", "\"")
                              ]
                              )

    def lex(self, cursor: CharCursor) -> Lexed:
        result = self.lexer.lex(cursor)
        if result.is_err():
            return result
        content = "".join(token.text for token in result.tokens)
        location = Location.join(token.location for token in result.tokens)
        token = Token("string", content, location)
        if result.tokens[-1].kind == "string end":
            return Lexed([token], [], result.cursor)
        else:
            token.kind = "unterminated_string"
            error = Error("Unterminated string", location)
            return Lexed([token], [error], result.cursor)


class Synchronized(Lexer):
    def __init__(self, name, lexer_pairs):
        super().__init__(name)
        self.lexer_pairs = lexer_pairs

    def lex(self, cursor: CharCursor) -> Lexed:
        result = Lexed([], [], cursor)
        skipped = Token("skipped", "", cursor.current_location(0))
        while cursor.has():
            for (sync_lexer, token_lexer) in self.lexer_pairs:
                attempt = sync_lexer.lex(cursor.clone())
                if not attempt.is_err():
                    lexed = token_lexer.lex(cursor)
                    tokens = lexed.tokens
                    if skipped.text:
                        tokens = [skipped] + lexed.tokens
                    return Lexed(tokens, lexed.errors, lexed.cursor)

            taken, location = cursor.take(1)
            skipped.text += taken
            skipped.location = Location.join((skipped.location, location))

        return result


binary_number = Regex("binary_number", r"0b[01]+")
decimal_number = Regex("decimal_number", r"\d+")
hex_number = Regex("hex_number", r"0x[0-9a-fA-F]+")
number = AnyOf("number", [binary_number, decimal_number, hex_number])

symbols = "()[]{}<>=!+-*/%&|^~.,;:"
symbol = Regex("symbol", '|'.join(f"\\{symbol}" for symbol in symbols))
operator = Join('operator', Repeat("operator_part", symbol))

identifier = Regex("identifier", r"[a-zA-Z_][a-zA-Z0-9_]*")
string = StringLiteral()
whitespace = Regex("whitespace", r"[ \t\n\r]+")
unknown = Regex("unknown", r".")

fn = Join("function", Sequence("fn",
                               [
                                   Commit("fn start", Sequence("fn start", [
                                       Exactly("fn", "fn"), whitespace, identifier])),
                                   Exactly("(", "("),
                                   Exactly(")", ")"),
                               ]
                               ))

comment = Regex("comment", r"#[^\n]*\n?")

items = [fn, number, identifier, string,
         comment, operator, whitespace, unknown]
synced_items = Synchronized("items", [(item, item) for item in items])
main = Repeat("main", synced_items)

cursor = CharCursor(Source("test.txt", 'fn x(+ '))
lexed = main.lex(cursor)
for token in lexed.tokens:
    print(token)

for error in lexed.errors:
    print(error)
