
%.run: %.o main.o
	@echo "final >"
	gcc $+ -o $@

main.o: main.c
	@echo "main.o >"
	gcc -c $<

%.o: %.c
	@echo ".o: $(%.c) >"
	gcc -c $<
