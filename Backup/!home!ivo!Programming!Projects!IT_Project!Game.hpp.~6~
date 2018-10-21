#ifndef GAME_H
#define GAME_H

#include <vector>
#include <stdio.h>
#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>

class AssetManager;
class ColliderComponent;

class Game
{
private:
    SDL_Window* window;
    int counter;

public:

    Game();
    ~Game();


    void init( const char* title, int windWidth, int windoHeight, bool fullscreen );
    void handleEvents();
    void update();
    void render();
    void clean();
    bool isRunning();


    static bool isActive;
    static SDL_Rect camera;
    static SDL_Event event;
    static SDL_Renderer* renderer;
    static AssetManager* assets;


    enum groupLabels : std::size_t {
	groupMap,
	groupPlayer,
	groupObjects,
	groupColliders,
	groupProjectiles
    };
};

#endif
