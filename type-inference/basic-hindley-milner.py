class Type:
    def __repr__(self):
        return str(self)

# Types
class TypeVariable(Type):
    def __init__(self, name: str):
        self.name = name

    def __eq__(self, other):
        if type(other) == type(self):
            return self.name == other.name
        else:
            return False

    def __repr__(self):
        return f"TypeVar({self.name})"

    def __str__(self):
        return self.name


class TypeConstructor(Type):
    def __init__(self, name, args = None):
        self.name = name
        self.args = args or []

    def __str__(self):
        args = ' '.join(str(arg) for arg in self.args)
        if args:
            return f"{self.name} {args}"
        else:
            return str(self.name)


class Function(TypeConstructor):
    def __init__(self, arg_type: Type, result_type: Type):
        super().__init__('->', [arg_type, result_type])
        self.arg_type = arg_type
        self.result_type = result_type


    def str(self, use_parens=False):
        if isinstance(self.arg_type, Function):
            left = self.arg_type.str(True)
        else:
            left = str(self.arg_type)
        right= str(self.result_type)

        result =  f"{left} -> {right}"
        if use_parens:
            result = f"({result})"
        return result

    def __str__(self):
        return self.str()

class Scheme:
    def __init__(self, params, type: Type):
        self.params = params
        self.type = type

    def __str__(self):
        return f"forall {' '.join(self.params)}. {self.type}"

    def __repr__(self):
        return str(self)

# Expressions
class Expression:
    def __repr__(self):
        return str(self)

class Literal(Expression):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return str(self.value)

class Variable(Expression):
    def __init__(self, name):
        self.name = name

    def __str__(self):
        return self.name

class FunctionApplication(Expression):
    def __init__(self, function, arg):
        self.function = function
        self.arg = arg


    def __str__(self):
        return f"({self.function} {self.arg})"

class Lambda(Expression):
    def __init__(self, arg_name, result):
        self.arg_name = arg_name
        self.result = result
    
    def str(self, fn=True):
        pre = "(fn " if fn else ""
        suf = ")" if fn else ""
        if type(self.result) == Lambda:
            return f"{pre}{self.arg_name} {self.result.str(False)}{suf}"
        else:
            return f"{pre}{self.arg_name} => {self.result}{suf}"

    def __str__(self):
        return self.str()


class Where(Expression):
    def __init__(self, expression, mapping):
        self.expression = expression
        self.mapping = mapping

    def __str__(self):
        mapping = ", ".join(f"{var} = {value}" for var, value in self.mapping.items())
        return f"{self.expression} where {mapping}"

class TypeVariables:
    def __init__(self):
        self.variables_made = 0

    def new(self):
        name = f"t{self.variables_made}"
        self.variables_made += 1
        return TypeVariable(name)

class Substitution:
    def __init__(self, mapping=None):
        self.mapping = mapping or {}

    def apply_to_type(self, type):
        if isinstance(type, TypeVariable):
            if type.name in self.mapping:
                return self.mapping[type.name]
            else:
                return type
        elif isinstance(type, Function):
            return Function(
                    self.apply_to_type(type.arg_type), 
                    self.apply_to_type(type.result_type)
            )
        elif isinstance(type, TypeConstructor):
            substituted_args = [self.apply_to_type(arg) for arg in type.args]
            return TypeConstructor(type.name, substituted_args)
        

    def apply_to_scheme(self, scheme: Scheme):
        # Remove all variables quantified in the scheme
        pruned_mapping = { **self.mapping }
        for param in scheme.params:
            if param in pruned_mapping:
                pruned_mapping.pop(param)

        substituted_type = Substitution(pruned_mapping).apply_to_type(scheme.type)
        return Scheme(scheme.params, substituted_type)

    def apply_to_context(self, ctx):
        return {
                name : self.apply_to_scheme(scheme)
                for name, scheme in ctx.items()
        }

    def after(self, substitution):
        new_mapping = {
            **self.mapping,
            **{
                name : self.apply_to_type(type)
                for name, type in substitution.items()
            }
        }
        return Substitution(new_mapping)

    def items(self):
        return self.mapping.items()
    
    def add(self, name, type):
        self.mapping[name] = type

    def __str__(self):
        mapping_strs = [f"{name} => {type}" for name, type in self.mapping.items()]
        return f"[{', '.join(mapping_strs)}]"

    def __repr__(self):
        return str(self)

class UnboundVariable(Exception):
    def __init__(self, name):
        super().__init__(f'Unbound variable: {name}')

class OccursCheckFail(Exception):
    def __init__(self, name, type):
        super().__init__(f'Occurs check failed while unifying {name} with {type}')

class InvalidUnification(Exception):
    def __init__(self, t1, t2):
        super().__init__(f'Cannot unify {t1} with {t2}')

class Inference:
    def __init__(self, type_variables = None):
        self.type_variables = type_variables or TypeVariables()

    def infer(self, expression: Expression, ctx = None):
        ctx = ctx or {}
        infer_expr_type = {
            Literal: self.infer_literal,
            Variable : self.infer_variable,
            FunctionApplication : self.infer_function_application,
            Lambda : self.infer_lambda,
            Where : self.infer_where,
        }
        for expr_type, infer_expr in infer_expr_type.items():
            if isinstance(expression, expr_type):
                return infer_expr(expression, ctx)

    def infer_literal(self, literal, ctx):
        return TypeConstructor('Lit'), Substitution()

    def infer_variable(self, variable, ctx):
        name = variable.name
        if name in ctx:
            return self.instantiate(ctx[name]), Substitution()
        else:
            raise UnboundVariable(name)

    def infer_function_application(self, expression, ctx):
        result_type = self.type_variables.new()

        function_type, s1 = self.infer(expression.function, ctx)
        arg_type, s2 = self.infer(expression.arg, s1.apply_to_context(ctx))
        s3 = unify(s2.apply_to_type(function_type), Function(arg_type, result_type))
        return s3.apply_to_type(result_type), s3.after(s2.after(s1))

    def infer_lambda(self, expression, ctx):
        arg_type = self.type_variables.new()
        result_ctx = { **ctx, expression.arg_name : Scheme([], arg_type)}
        result_type, s1 = self.infer(expression.result, result_ctx)
        arg_type = s1.apply_to_type(arg_type)
        return Function(arg_type, result_type), s1
    
    def infer_where(self, where, ctx):
        inferred_mapping = {
            name : self.infer(expr)
            for name, expr in where.mapping.items()
        }
        result_ctx = { **ctx }
        for name, (type, _) in inferred_mapping.items():
            result_ctx[name] = Scheme([], type)

        for _, substitution in inferred_mapping.values():
            result_ctx = substitution.apply_to_context(result_ctx)
        
        type, substitution = self.infer(where.expression, result_ctx)
        for _, s in inferred_mapping.values():
            substitution = s.after(substitution)

        return type, substitution
        
    def instantiate(self, scheme):
        substitution = Substitution({
            param : self.type_variables.new()
            for param in scheme.params
        })
        return substitution.apply_to_type(scheme.type)
    
def unify(t1, t2):
    if isinstance(t1, TypeConstructor) and isinstance(t2, TypeConstructor):
        if t1.name == t2.name:
            substitution = Substitution()
            for a1, a2 in zip(t1.args, t2.args):
                s = unify(substitution.apply_to_type(a1), substitution.apply_to_type(a2))
                substitution = s.after(substitution)
            result = substitution
            return result
        else:
            raise InvalidUnification(t1, t2)
    elif isinstance(t2, TypeVariable):
        return var_bind(t2.name, t1)
    elif isinstance(t1, TypeVariable):
        return var_bind(t1.name, t2)
    else:
        raise InvalidUnification(t1, t2)

def var_bind(name, type):
    if type == TypeVariable(name):
        return Substitution()
    elif name in free_variables_of_type(type):
        raise OccursCheckFail(name, type)
    else:
        return Substitution({name : type})

def free_variables_of_type(type):
    if isinstance(type, TypeVariable):
        return { type.name }
    elif isinstance(type, TypeConstructor):
        if len(type.args) > 0:
            return set.union(*[free_variables_of_type(arg) for arg in type.args])
        else:
            return set()
    else:
        print('dupa', type)

def free_variables_of_scheme(scheme):
    return set.difference(
            free_variables_of_type(scheme.type),
            set(scheme.params)
    )

def free_variables_of_context(context):
    if context:
        return set.union(*[free_variables_of_scheme(scheme) for scheme in context.values()])
    else:
        return set()


def generalize(type, ctx):
    variables = set.difference(
            free_variables_of_type(type),
            free_variables_of_context(ctx)
    )
    if variables:
        return Scheme(list(variables), type)
    else:
        return type

class AlphabetTypeVariables:
    def __init__(self):
        self.variables_made = 0
    
    def new(self):
        var =  chr(ord('a') + self.variables_made)
        self.variables_made += 1
        return var

def prettify(scheme):
    '''
    Replace bound type variables generated during inference with
    consecutive letters of the alphabet
    '''
    if type(scheme) == Scheme:
        type_variables = AlphabetTypeVariables()
        mapping = { param : type_variables.new() for param in scheme.params }
        scheme.params = list(mapping.values())
        return Substitution(mapping).apply_to_scheme(scheme)
    else:
        return scheme


def infer(expr, ctx):
    '''
    Infers generalized and prettified type of expr, as well as
    all variables in context 
    '''
    inference = Inference()
    type, substitution = inference.infer(expr, ctx)
    pretty = prettify(generalize(type, ctx))
    print(pretty)
    for var, scheme in ctx.items():
        type = Inference().instantiate(scheme)
        type = substitution.apply_to_type(type)
        pretty = prettify(generalize(type, {}))
        print(f"{var}: {pretty}")



def s_combinator():
    inference = Inference()

    ctx = {
            'x' : Scheme([], TypeVariable('x')),
            'y' : Scheme([], TypeVariable('y')),
            'z' : Scheme([], TypeVariable('z')),
    }

    expr = FunctionApplication(
        FunctionApplication(Variable('x'), Variable('y')),
        FunctionApplication(Variable('y'), Variable('z'))
    )

    type, s = inference.infer(expr, ctx)

    print(expr)
    for var, scheme in ctx.items():
        type = Inference().instantiate(scheme)
        type = s.apply_to_type(type)
        scheme = generalize(type, {})
        pretty = prettify(scheme)
        print(f"{var}: {pretty}")


def if_expression():
    infer(if_expr, {})

def literal():
    expr = Lambda('x', Literal(123))
    infer(expr, {})

def if_with_literal():
    expr = FunctionApplication(
            FunctionApplication(
                if_expr,
                true,
            ),
            Literal(123)
        )
    infer(expr, {})

if_expr = Lambda(
    'cond', 
    Lambda(
        'true', 
        Lambda(
            'false',
            FunctionApplication(
                FunctionApplication(
                    Variable('cond'), 
                    Variable('true')),
                    Variable('false')
            )
        )
    )
)

true = Lambda('a', Lambda('b', Variable('a')))
false = Lambda('a', Lambda('b', Variable('b')))


s_combinator()
