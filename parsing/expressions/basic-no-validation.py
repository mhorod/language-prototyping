'''
Parsing expression consisted of some binary operators
with their associativity and precedence.
The parser does not validate data and will produce runtime error
on an invalid input.
'''

from dataclasses import dataclass
from enum import Enum


class Associativity(Enum):
    '''
    Associativity of a binary operator.
    NONE indicates that operator is not associative and has to be parenthesised
    '''
    NONE = 0
    LEFT = 1
    RIGHT = 2


@dataclass
class Precedence:
    '''
    Precedence of an operator.
    Higher values of binding are executed before lower values
    '''
    associativity: Associativity
    binding: int


class Binary:
    def __init__(self, value, *, left=None, right=None):
        self.value = value
        self.left = left
        self.right = right

    def __str__(self):
        if self.left is None and self.right is None:
            return str(self.value)
        else:
            return f"({self.left} {self.value} {self.right})"

class Unary:
    def __init__(self, op, value):
        self.op = op
        self.value = value

    def __str__(self):
        return f"({self.op}{self.value})"


class Function:
    def __init__(self, name, args):
        self.name = name
        self.args = args

    def __str__(self):
        return f"{self.name}({', '.join(str(arg) for arg in self.args)})"


class InvalidExpression(Exception):
    pass


class ExpressionParser:
    def __init__(self, binary_operators, unary_operators):
        self.binary = binary_operators
        self.unary = unary_operators
        self.operators = [*binary_operators.keys(), *unary_operators]

    def parse_expr(self, tokens):
        return self._parse_expr(tokens, None)[0]
    
    
    def parse_simple_term(self, tokens):
        '''
        Parse a term that is not a function application i.e.
        - expression in parentheses
        - unary operation
        - a value
        '''
        if tokens[0] == '(':
            return self.parse_parens(tokens)
        elif tokens[0] in ['!', '~', '-']:
            op, value, tokens = tokens[0], tokens[1], tokens[2:]
            return Unary(op, value), tokens
        else:
            return tokens[0], tokens[1:]

    def parse_term(self, tokens):
        if (
                len(tokens) > 1 and 
                self.is_value(tokens[0]) and 
                (tokens[1] not in self.operators and tokens[1] != ')')
                ):
            name, tokens = tokens[0], tokens[1:]
            args = []
            while tokens and tokens[0] not in self.operators:
                if tokens[0] == '(':
                    arg, tokens = self.parse_parens(tokens)
                else:
                    arg, tokens = tokens[0], tokens[1:]
                args.append(arg)
            return Function(name, args), tokens
        else:
            return self.parse_simple_term(tokens)

    def is_value(self, token):
        return token not in self.operators and token not in '()'

    def _parse_expr(self, tokens, last_operator):
        # Parse right-hand side of {last_operator}
        # i.e. expression that uses only tigher binding operators
        node, tokens = self.parse_term(tokens)
        previous_operator = last_operator
        while (
            tokens and
            tokens[0] != ')' and
            self.precedes_at_rhs(last_operator, tokens[0])
        ):
            op, tokens = tokens[0], tokens[1:]

            if not self.is_known_operator(op):
                raise InvalidExpression(f'Unknown operator: {op}')
            elif op == previous_operator and self.is_not_associative(op):
                raise InvalidExpression(f'{op} is not associative, use parens')

            previous_operator = op
            parsed_node, tokens = self._parse_expr(tokens, op)
            node = Binary(op, left=node, right=parsed_node)

        return node, tokens

    def is_known_operator(self, op):
        return op in self.operators

    def is_not_associative(self, op):
        return self.binary[op].associativity == Associativity.NONE

    def precedes_at_rhs(self, left, right):
        '''
        Does expression of form a {left} b {right} c resolve as
        False) (a {left} b) {right} c
        True) a {left} (b {right} c}
        '''
        return (
            left is None or
            self.precedes(right, left) or
            self.associates_to_right_of(left, right)
        )

    def associates_to_right_of(self, op1, op2):
        return op1 == op2 and self.is_right_associative(op1)

    def is_right_associative(self, op):
        return self.binary[op].associativity == Associativity.RIGHT

    def precedes(self, op1, op2):
        return self.binary[op1].binding > self.binary[op2].binding

    def parse_parens(self, tokens):
        assert tokens[0] == '('
        expr, tokens = self._parse_expr(tokens[1:], None)
        assert tokens[0] == ')'
        return expr, tokens[1:]


def infix(op, binding):
    return (op, Precedence(Associativity.NONE, binding))


def infixl(op, binding):
    return (op, Precedence(Associativity.LEFT, binding))


def infixr(op, binding):
    return (op, Precedence(Associativity.RIGHT, binding))


def lex(text):
    '''
    Splits text on sequences of symbols
    '''
    symbols = '!@$%^&*-+<=>?/~|'
    result = []
    token = ''
    for i, char in enumerate(text):
        if char == ' ':
            if token:
                result.append(token)
                token = ''
            continue
        if char in '()':
            if token:
                result.append(token)
                token = ''
            result.append(char)
        elif len(token) == 0 or (token[-1] in symbols) == (char in symbols):
            token += char
        else:
            result.append(token)
            token = char

    if token:
        result.append(token)

    return result

def show_example(parser, text):
    lexed = lex(text)
    parsed = parser.parse_expr(lexed)
    print("", text)
    print("", parsed)

binary_operators = dict([
    infixr('$', 0),
    infixl('+', 4),
    infixl('-', 4),
    infixl('*', 5),
    infixr('^', 6),
    infix('==', 1),
    infixr('||', 9),
    infixr('&&', 10),
])
unary_operators = ['!', '-', '~']

examples = {
    "addition and multiplication" : "a + b * c + d",
    "addition and subtraction" : "a + b - c - d + e",
    "exponentation" : "a ^ b ^ c ^ d",
    "equality" : "2 + 2 == 2 * 2" ,
    "boolean operators" : "a || b || c && d && e",
    "unary operations" : "-a + !True * ~123",
    "function application" : "2 + f x y z == 3",
    "function application on expressions": "f (x + y) (x + z)",
    "dollar operator" : "f $ (x + 2) $ 3",
}
parser = ExpressionParser(binary_operators, unary_operators)

for name, expr in examples.items():
    print(name)
    show_example(parser, expr)
    print()
