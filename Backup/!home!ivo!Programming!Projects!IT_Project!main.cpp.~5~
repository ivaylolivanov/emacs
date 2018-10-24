#include "Game.hpp"
/*
  game loop:
  - handle any user input
  - update all object eg. positions and other
  - render changes to the display


 */

int main( int argc, char* argv[] )
{
    const int FPS = 60;
    const int frameDelay = 1000 / FPS;

    Uint32 frameStart;
    int frameTime;


    Game* game = new Game();

    game->init( "IT Project - Physics recreation", 800, 640, false );
    while ( game->isRunning() )
    {
	frameStart = SDL_GetTicks();

	game->handleEvents();
	game->update();
	game->render();

	frameTime = SDL_GetTicks() - frameStart;

	if ( frameDelay > frameTime )
	    SDL_Delay( frameDelay -  frameTime);
    }


    game->clean();

    return 0;
}
