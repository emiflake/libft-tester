LIBFT = libft.so

all: $(LIBFT)

test: re exec

exec:
	stack build --ghc-options=-O2 --exec libft-tester-exe

$(LIBFT):
	make -C ../libft/ so
	cp ../libft/libft.so .

clean:
	rm libft.so

re: clean $(LIBFT)
