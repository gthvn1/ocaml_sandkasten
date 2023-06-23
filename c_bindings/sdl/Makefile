.PHONY: build

SDL_TUTO=sdl_tuto

run: $(SDL_TUTO)
	./$(SDL_TUTO)

$(SDL_TUTO): $(SDL_TUTO).c
	clang -o $@ $< $(shell sdl2-config --cflags --libs)

clean:
	rm -f $(SDL_TUTO)
