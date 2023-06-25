#include <SDL.h>
#include <stdbool.h>
#include <stdlib.h>
#include <unistd.h>

int clear_screen(SDL_Renderer * r)
{
    // Need to set color before clearing
    if (SDL_SetRenderDrawColor(r, 0, 0, 0, SDL_ALPHA_OPAQUE) <0) {
        printf("[ERROR] draw color error: %s\n", SDL_GetError());
        return -1;
    }

    if (SDL_RenderClear(r) < 0) {
        printf("[ERROR] render clear: %s\n", SDL_GetError());
        return -1;
    } 

    return 0;
}

int draw_rectangle(SDL_Renderer *r, SDL_Rect *rect)
{
    // Draw things in red
    if (SDL_SetRenderDrawColor(r, 255, 0, 0, SDL_ALPHA_OPAQUE) <0) {
        printf("[ERROR] draw color error: %s\n", SDL_GetError());
        return -1;
    }

    // Draw a red rectangle
    if (SDL_RenderDrawRect(r, rect) < 0) {
        printf("[ERROR] draw rect error: %s\n", SDL_GetError());
        return -1;
    }

    if (SDL_RenderFillRect(r, rect) < 0) {
        printf("[ERROR] draw rect error: %s\n", SDL_GetError());
        return -1;
    }

    return 0;
}

int main(void)
{
    bool game_is_running = true;
    int win_width = 800;
    int win_height = 600;
    int ret = 0;
    uint64_t sticks, eticks, delay = 0;
    uint64_t fps = 60;
    int dx = 1;
    int dy = 1;
    SDL_Window *window;
    SDL_Renderer *renderer;
    SDL_Event event;
    SDL_Rect rect = { .x = win_width/2, .y = win_height/2, .w = 50, .h = 30 };

    if (SDL_Init(SDL_INIT_VIDEO) < 0) {
        printf("[ERROR] init video: %s\n", SDL_GetError());
        return 1;
    }

    // Create window
    window = SDL_CreateWindow(
            "tuto",
            SDL_WINDOWPOS_UNDEFINED,
            SDL_WINDOWPOS_UNDEFINED,
            win_width,
            win_height,
            SDL_WINDOW_RESIZABLE);
    if (window == NULL) {
        printf("[ERROR] create window: %s\n", SDL_GetError());
        return 1;
    }

    // Create renderer
    renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_SOFTWARE);
    if (renderer == NULL) {
        printf("[ERROR] create renderer: %s\n", SDL_GetError());
        ret = 1;
        goto destroy_window;
    }

    // event loop
    while (SDL_PollEvent(&event) || game_is_running) {
        sticks = SDL_GetTicks64();

        // What to do with the event?
        switch (event.type) {
            case SDL_QUIT:
                game_is_running = false;
                break;
            case SDL_KEYDOWN:
                switch (event.key.keysym.scancode) {
                    case SDL_SCANCODE_Q:
                        printf("Key Q pressed\n");
                        game_is_running = false;
                        break;
                    case SDL_SCANCODE_LEFT:
                        if (dx > 0) dx *= -1; // dx must be negative
                        break;
                    case SDL_SCANCODE_RIGHT:
                        if (dx < 0) dx *= -1; // dx must be positive
                        break;
                    case SDL_SCANCODE_UP:
                        if (dy > 0) dy *= -1; // dy must be negative
                        break;
                    case SDL_SCANCODE_DOWN:
                        if (dy < 0) dy *= -1; // dy must be positive
                        break;
                    default:
                        // nothing to do
                        break;
                }
        }

        // update the state
        rect.x += dx;
        rect.y += dy;

        // Draw new frame
        if (clear_screen(renderer) < 0) {
            ret = 1;
            goto destroy_renderer;
        }

        if (draw_rectangle(renderer, &rect) < 0) {
            ret = 1;
            goto destroy_renderer;
        };

        SDL_RenderPresent(renderer);

        // Add delay to reach 60 FPS if needed
        eticks = SDL_GetTicks64();
        delay = 1000/fps - (eticks - sticks);
        if (delay > 0)
            SDL_Delay(delay);
    }

destroy_renderer:
    SDL_DestroyRenderer(renderer);

destroy_window:
    SDL_DestroyWindow(window);
    SDL_Quit();
    return ret;
}
