import ply.yacc as yacc
from lexer import tokens

# Abstract Syntax Tree (AST) node types
class Var:
    def __init__(self, name):
        self.name = name  # Variable name

    def __repr__(self):
        return self.name  # Return variable name as string

class Lambda:
    def __init__(self, var, body):
        self.var = var  # Bound variable
        self.body = body  # Body of the lambda expression

    def __repr__(self):
        return f"(λ{self.var}.{self.body})"  # Return lambda expression as string

class Application:
    def __init__(self, func, arg):
        self.func = func  # Function part of application
        self.arg = arg  # Argument part of application

    def __repr__(self):
        return f"({self.func} {self.arg})"  # Return application as string

# Parsing rules

# expr: An expression can be a variable, a function application, or a lambda abstraction
def p_expr_var(p):
    'expr : VAR'
    p[0] = Var(p[1])  # Create a Var node

def p_expr_func_arg(p):
    'expr : func arg'
    p[0] = Application(p[1], p[2])  # Create an Application node

def p_expr_lambda(p):
    'expr : HASH VAR DOT expr'
    p[0] = Lambda(p[2], p[4])  # Create a Lambda node

# func: A function can be a variable, a lambda abstraction within parentheses, or a function application
def p_func_var(p):
    'func : VAR'
    p[0] = Var(p[1])  # Create a Var node

def p_func_lambda(p):
    'func : LPAREN HASH VAR DOT expr RPAREN'
    p[0] = Lambda(p[3], p[5])  # Create a Lambda node

def p_func_application(p):
    'func : func arg'
    p[0] = Application(p[1], p[2])  # Create an Application node

# arg: An argument can be a variable, a lambda abstraction within parentheses, or a function application within parentheses
def p_arg_var(p):
    'arg : VAR'
    p[0] = Var(p[1])  # Create a Var node

def p_arg_lambda(p):
    'arg : LPAREN HASH VAR DOT expr RPAREN'
    p[0] = Lambda(p[3], p[5])  # Create a Lambda node

def p_arg_application(p):
    'arg : LPAREN func arg RPAREN'
    p[0] = Application(p[2], p[3])  # Create an Application node

def p_error(p):
    print("Syntax error at '%s'" % p.value if p else "Syntax error at EOF")  # Handle syntax error

# Build the parser
parser = yacc.yacc()

# Semantic analysis: alpha conversion to avoid variable capture
def alpha_conversion(expr, old_var, new_var):
    """
    Performs alpha conversion to avoid variable capture.

    Args:
        expr (Expr): The expression to perform alpha conversion on.
        old_var (str): The old variable to replace.
        new_var (str): The new variable to replace with.

    Returns:
        Expr: The converted expression.
    """
    if isinstance(expr, Var):
        # If the variable matches the old variable, replace it with the new variable
        if expr.name == old_var:
            replacement_var = 'j'
            new_var = replacement_var
            return Var(new_var)
        else:
            return expr
    elif isinstance(expr, Lambda):
        # If the lambda variable matches the old variable, replace it with the new variable in the lambda body
        if expr.var == old_var:
            return Lambda(new_var, alpha_conversion(expr.body, old_var, new_var))
        else:
            # Otherwise, recursively perform alpha conversion on the lambda body
            return Lambda(expr.var, alpha_conversion(expr.body, old_var, new_var))
    elif isinstance(expr, Application):
        # Recursively perform alpha conversion on the function and argument parts of the application
        return Application(alpha_conversion(expr.func, old_var, new_var),
                           alpha_conversion(expr.arg, old_var, new_var))

# Beta reduction
def beta_reduction(expr):
    if isinstance(expr, Application):
        if isinstance(expr.func, Lambda):
            return True, substitute(expr.func.body, expr.func.var, expr.arg)  # Apply beta reduction
        else:
            reduced, new_func = beta_reduction(expr.func)
            if reduced:
                return True, Application(new_func, expr.arg)  # Reduce function part
            else:
                reduced, new_arg = beta_reduction(expr.arg)
                if reduced:
                    return True, Application(expr.func, new_arg)  # Reduce argument part
    return False, expr

# Substitute variable with value in an expression
def substitute(expr, var, value):
    if isinstance(expr, Var):
        if expr.name == var:
            return value  # Replace variable with value
        else:
            return expr
    elif isinstance(expr, Lambda):
        if expr.var == var:
            return expr
        else:
            return Lambda(expr.var, substitute(expr.body, var, value))  # Substitute in lambda body
    elif isinstance(expr, Application):
        return Application(substitute(expr.func, var, value), substitute(expr.arg, var, value))  # Substitute in application

# Eta reduction
def eta_reduction(expr):
    if isinstance(expr, Lambda):
        if isinstance(expr.body, Application):
            if isinstance(expr.body.arg, Var):
                if expr.var == expr.body.arg.name and expr.var not in free_vars(expr.body.func):
                    return True, expr.body.func  # Apply eta reduction
    return False, expr

# Find free variables in an expression
def free_vars(expr):
    if isinstance(expr, Var):
        return {expr.name}  # Return variable as a set
    elif isinstance(expr, Lambda):
        return free_vars(expr.body) - {expr.var}  # Remove bound variable
    elif isinstance(expr, Application):
        return free_vars(expr.func) | free_vars(expr.arg)  # Union of free variables

# Reduce expression to normal form
def reduce_expression(expr):
    """
    Reduce an expression to normal form using beta reduction and eta reduction.

    Args:
        expr (Expression): The expression to reduce.

    Returns:
        Expression: The reduced expression.
    """
    # Repeat beta and eta reduction until the expression cannot be reduced further
    while True:
        # Perform beta reduction
        reduced, new_expr = beta_reduction(expr)
        if reduced:
            # Display beta reduction
            print("Beta reduction:", expr, "=>", new_expr)
            expr = new_expr
            continue

        # Perform eta reduction
        reduced, new_expr = eta_reduction(expr)
        if reduced:
            # Display eta reduction
            print("Eta reduction:", expr, "=>", new_expr)
            expr = new_expr
            continue

        # Exit the loop if the expression cannot be reduced further
        break

    # Return the reduced expression
    return expr

# Main function to display results
def main():
    while True:
        try:
            s = input('lambda > ')
        except EOFError:
            break
        if not s:
            continue
        result = parser.parse(s)
        print("Parsed expression:", result)
        print("Reduced expression:", reduce_expression(result))

# Run main function
if __name__ == "__main__":
    main()
