semantics error: [Type Error] Type Named Int Not Found
in [file "examples/example.dp", row 11, col 25 to row 11, col 28]

semantics error: [Error]  function bar not found 

semantics error: [Pattern Error]  declared type doesn't match with the given expr
in [file "examples/example.dp", row 23, col 13 to row 23, col 20]

semantics error: [Error] Type Foo Not Found

semantics error: [Error] function name foo has been used

semantics error: [Type Error] Type Named String Not Found
in [file "examples/example.dp", row 40, col 18 to row 40, col 24]

semantics error: [Error] The same ADT Name

semantics error: [Error] The same Struct Name

semantics error: [Type Error] Type Named String Not Found
in [file "examples/example.dp", row 70, col 20 to row 70, col 26]

semantics error: [Type Error] Type Named String Not Found
in [file "examples/example.dp", row 81, col 20 to row 81, col 26]

semantics error: [Error]  function times not found 

semantics error: [Error] varaible main Not Found

=== ANF for examples/example.dp ===
fun foo() -> #1 -- "examples/example.dp":10,0-10,13 =
  jump #1 () -- "examples/example.dp":10,0-10,13

fun bar($1) -> #2 -- "examples/example.dp":11,0-11,31 =
  jump #2 () -- "examples/example.dp":11,0-11,31

fun main() -> #3 -- "examples/example.dp":29,0-33,1 =
  $2 = mk((ANF.Struct "Foo"))() -- "examples/example.dp":31,20-31,31
  $3 = $2.fib -- "examples/example.dp":32,4-32,15
  $4 = $3($2, 10) -- "examples/example.dp":32,4-32,15
  jump #3 () -- "examples/example.dp":29,0-33,1

fun foo($5) -> #4 -- "examples/example.dp":35,0-35,25 =
  jump #4 () -- "examples/example.dp":35,0-35,25

fun bar($6) -> #5 -- "examples/example.dp":40,0-42,1 =
  $7 = 0 -- "examples/example.dp":41,4-41,25
  jump #5 () -- "examples/example.dp":40,0-42,1

=== WAT for examples/example.dp ===
Fatal error: exception Failure("struct Foo not found")
