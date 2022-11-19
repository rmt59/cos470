# [[file:python_symbol.org::*Description][Description:1]]
class SymbolGenerator():
    default_prefix = 'S'

    def __init__(self,prefix=None):
        self.prefix = prefix if prefix else SymbolGenerator.default_prefix
        self.num = 0

    def new_symbol(self,prefix=None):
        if prefix is None:
            prefix = self.prefix

        self.num += 1
        return f'{prefix}{self.num}'
# Description:1 ends here
