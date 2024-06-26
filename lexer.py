import ply.lex as lex

# List of token names
tokens = (
    'HASH',
    'DOT',
    'LPAREN',
    'RPAREN',
    'VAR'
)

# Regular expressions for simple tokens
t_HASH = r'\#'
t_DOT = r'\.'
t_LPAREN = r'\('
t_RPAREN = r'\)'

# Regular expression rule for variables
def t_VAR(t):
    r'[a-z]'
    t.value = str(t.value)
    return t

# Ignored characters
t_ignore = ' \t'

# Error handling rule
def t_error(t):
    if t is None:
        raise SyntaxError("Illegal character at end of input")
    else:
        raise SyntaxError(f"Illegal character '{t.value[0]}'")

# Build the lexer
lexer = lex.lex()

# Test the lexer
if __name__ == "__main__":
    data = '#x. (#y. y) x'
    lexer.input(data)
    for tok in lexer:
        print(tok)
