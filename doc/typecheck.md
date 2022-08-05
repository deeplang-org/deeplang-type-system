1. AST
2. Walker
input: node, visit_func
DFS
return type: VISIT_CHILDREN, SKIP_CHILDREN, STOP
3. symbol table
// scope_level 0
func a() { // scope_gate
   var a1 = 123 // scope_level 1
   func b() {
      a1 = 123 // 2, aaa
   } // Scope symbol_table decl  scope_name: aa
}

func b() {
//scope_name: ab
}

scope  level name kind
a      0     a    decl
a      0     b    decl
aa     1     a1   decl
aa     1     b    decl
aaa    2     a1   expr

lookup
a.b field_lookup

resolve name
reference expr -> target

types
AST type, ty sema type
equal subtype construct
types ADT typecompare

- primitive type
- user defined type

func a(a:I32, b:I32): I32

type interning
functy
paramlist ty return_ty
(I32, I32) -> I32


ast context

1. walker the whole ast, collect decls to symboltable, resolve name, check redefinition
2. walker the ast, ast type -> ty
3. walker the ast, for all exprs, do typecheck according typing rules

for some other checker:
- prepare check
  - collect symbol
  - resolve name
- precheck
  - ast type -> ty
- typecheck
  - typing rule

- postcheck
  - using ty info here
  optimize:
  for all funcbodys: parallel check



| 模块                            | 负责人         |
| ------------------------------- | -------------- |
| walker                          | 练琪灏         |
| typecheck                       | 陈楷骐         |
| symboltable                     | 魏韧韬，练琪灏 |
| types/typeconstruct/typecompare | 陈煜杰         |

