.DEFAULT_GOAL := run_simple

run_simple:
	@cargo run -q examples/simple.lcc

run_lex_error:
	@cargo run -q examples/lexical_error.lcc

run_imc:
	@cargo run -q examples/imc.lcc

run_feira:
	@cargo run -q examples/feira.lcc

run_matriz:
	@cargo run -q examples/matriz.lcc

test:
	@cargo test -- --show-output
