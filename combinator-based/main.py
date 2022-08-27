from lang import *
from lexer import CharCursor, lang_lexer
from parser import TokenCursor, lang_parser


char_cursor = CharCursor(Source("test.txt", "let x = ? + 2; let x = 2;"))
lexed = lang_lexer.lex(char_cursor)
print("Lexed")
for token in lexed.tokens:
    print(token)

print("Lexing errors:")
for error in lexed.errors:
    print(error)

token_cursor = TokenCursor(lexed.tokens)
parsed = lang_parser.parse(token_cursor)

print("Parsed")
for node in parsed.nodes:
    print(node)

print("Parsing errors:")
for error in parsed.errors:
    print(error)
