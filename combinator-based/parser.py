from abc import ABC, abstractmethod
from dataclasses import field
from typing import *
from lang import *


class TokenCursor:
    def __init__(self, tokens, start: int = 0):
        self.tokens = tokens
        self.pos = start

    def has(self, length: int = 1) -> bool:
        return self.pos + length <= len(self.tokens)

    def peek(self, length: int) -> List[Token]:
        if self.has(length):
            return self.tokens[self.pos:self.pos + length]
        else:
            return []

    def take(self, length: int) -> Tuple[str, Location]:
        if self.has(length):
            result = self.peek(length)
            self.pos += length
            return result
        else:
            return []

    def clone(self) -> "TokenCursor":
        return TokenCursor(self.tokens, self.pos)

    def remaining(self) -> List[Token]:
        return self.tokens[self.pos:]


@dataclass
class Error:
    message: str
    location: Location

    def __str__(self):
        return f"Error: {self.message} at {self.location}"


@dataclass
class Node:
    kind: str
    children: List["Node"]
    tokens: List[Token] = field(default_factory=list)

    def __str__(self):
        return f"{self.kind}({self.content()})@{self.location()}"

    def content(self):
        if self.children:
            return ", ".join(str(child) for child in self.children)
        else:
            return "".join(token.text for token in self.tokens)

    def location(self) -> Location:
        if self.children:
            return Location.join(child.location() for child in self.children)
        else:
            return Location.join(token.location for token in self.tokens)


@dataclass
class Parsed:
    nodes: List[Node]
    errors: List[Error]
    cursor: TokenCursor
    commited: bool = False
    is_error: bool = False

    def length(self):
        return sum(node.location().length() for node in self.nodes)

    def is_err(self):
        return self.is_error and not self.commited


class Parser(ABC):
    def __init__(self, name):
        self.name = name

    @abstractmethod
    def parse(self, cursor: TokenCursor) -> Parsed:
        pass


class Repeat(Parser):
    def __init__(self, name, parser):
        super().__init__(name)
        self.parser = parser

    def parse(self, cursor: TokenCursor) -> Parsed:
        results = []
        while cursor.has():
            result = self.parser.parse(cursor.clone())
            if result.is_err():
                if len(results) == 0:
                    return result
                break
            results.append(result)
            cursor = result.cursor
        return Parsed(sum([res.nodes for res in results], []), sum([res.errors for res in results], []), cursor)


class Maybe(Parser):
    def __init__(self, name, parser):
        super().__init__(name)
        self.parser = parser

    def parse(self, cursor: TokenCursor) -> Parsed:
        result = self.parser.parse(cursor.clone())
        if result.is_err():
            return Parsed([], [], cursor)
        else:
            return result


class Sequence(Parser):
    def __init__(self, name, parsers):
        super().__init__(name)
        self.parsers = parsers

    def parse(self, cursor: TokenCursor) -> Parsed:
        result = Parsed([], [], cursor)
        for parser in self.parsers:
            parsed = parser.parse(result.cursor)
            result.is_error = result.is_error or parsed.is_error
            result.commited = result.commited or parsed.commited
            result.nodes.extend(parsed.nodes)
            result.errors.extend(parsed.errors)
            result.cursor = parsed.cursor
            if parsed.is_err():
                break
        result.is_error = result.is_err()
        result.commited = False
        return result


class Commit(Parser):
    def __init__(self, name, parser):
        super().__init__(name)
        self.parser = parser

    def parse(self, cursor: TokenCursor) -> Parsed:
        result = self.parser.parse(cursor.clone())
        if not result.is_err():
            result.commited = True
        return result


class OfKind(Parser):
    def __init__(self, name, kind):
        super().__init__(name)
        self.kind = kind

    def parse(self, cursor: TokenCursor) -> Parsed:
        if cursor.has() and cursor.peek(1)[0].kind == self.kind:
            node = Node(self.name, [], cursor.take(1))
            return Parsed([node], [], cursor)
        else:
            message = f"Expected {self.kind} got {cursor.peek(1)[0].kind}"
            return Parsed([], [Error(message, cursor.peek(1)[0].location)], cursor, is_error=True)


class OfContent(Parser):
    def __init__(self, name, content):
        super().__init__(name)
        self.content = content

    def parse(self, cursor: TokenCursor) -> Parsed:
        if cursor.has() and cursor.peek(1)[0].text == self.content:
            node = Node(self.name, [], cursor.take(1))
            return Parsed([node], [], cursor)
        else:
            message = f"Expected {self.content} got {cursor.peek(1)[0].text}"
            return Parsed([], [Error(message, cursor.peek(1)[0].location)], cursor, is_error=True)


class Skip(Parser):
    '''
    Parse a sequence of tokens, but does not emit any node
    '''

    def __init__(self, name, parser):
        super().__init__(name)
        self.parser = parser

    def parse(self, cursor: TokenCursor) -> Parsed:
        result = self.parser.parse(cursor)
        result.nodes = []
        return result


class AnyOf(Parser):
    def __init__(self, name, parsers):
        super().__init__(name)
        self.parsers = parsers

    def parse(self, cursor: TokenCursor) -> Parsed:
        results = [parser.parse(cursor.clone()) for parser in self.parsers]
        ok_results = [result for result in results if not result.is_err()]
        if ok_results:
            return max(ok_results, key=lambda result: result.length())
        else:
            error_message = f"Expected one of: {', '.join(parser.name for parser in self.parsers)} got {cursor.peek(1)[0].text}"
            return Parsed([], [Error(error_message, cursor.peek(1)[0].location)], cursor, is_error=True)


class Synchronized(Parser):
    def __init__(self, name, parser_pairs):
        super().__init__(name)
        self.parser_pairs = parser_pairs

    def parse(self, cursor: TokenCursor) -> Parsed:
        result = Parsed([], [], cursor)
        skipped = Node("skipped", [])
        while cursor.has():
            for (sync_parser, parser) in self.parser_pairs:
                attempt = sync_parser.parse(cursor.clone())
                if not attempt.is_err():
                    parsed = parser.parse(cursor)
                    nodes = parsed.nodes
                    if skipped.tokens:
                        nodes = [skipped] + parsed.nodes
                    return Parsed(nodes, result.errors + parsed.errors, parsed.cursor)
            token = cursor.take(1)[0]
            skipped.tokens.append(token)
            if not result.errors:
                result.errors.append(
                    Error(f"Unexpected token: `{token.text}`", token.location))
            result.cursor = cursor
        result.nodes = [skipped]
        return result


class Collect(Parser):
    def __init__(self, name, parser):
        super().__init__(name)
        self.parser = parser

    def parse(self, cursor: TokenCursor) -> Parsed:
        result = self.parser.parse(cursor)
        node = Node(self.name, result.nodes)
        return Parsed([node], result.errors, result.cursor)


WS = Skip("skip_whitespace", Maybe("maybe whitespace",
                                   Repeat("skip_whitespace", OfKind("skip_whitespace", "whitespace"))))


expression = AnyOf("expr", [OfKind("idenfitier", "identifier"), OfKind(
    "decimal_number", "decimal_number")])

let = Collect("let", Sequence("let",
                              [
                                  Skip("let", OfKind("let", "let")),
                                  WS,
                                  OfKind("name", "identifier"),
                                  WS,
                                  Skip("=", OfContent("=", "=")),
                                  WS,
                                  expression
                              ]
                              ))

item = AnyOf("item", [let])

let_pair = (OfContent("let", "let"), let)
synced_items = Synchronized("items", [let_pair])
lang_parser = Repeat("items", synced_items)
