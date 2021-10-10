bnfc --ocaml -m -o ./output deeplang.cf 
make -C ./output
./output/TestDeeplang ./example.dp
