build:
	dune build --profile=release

clean:
	dune clean

perf:
	perf record -g --call-graph=dwarf _build/default/bin/main_day6.exe

report:
	perf report

#flame: build
#	perf record -g --call-graph=dwarf $(EXE)
#	perf script > out.perf
#	stackcollapse-perf.pl out.perf > folded.txt
#	flamegraph.pl folded.txt > flame.svg
#	@echo "Flamegraph generated: flame.svg"
