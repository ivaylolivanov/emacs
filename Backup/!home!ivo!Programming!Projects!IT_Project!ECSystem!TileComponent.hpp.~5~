#ifndef TILE_COMPONENT
#define TILE_COMPONENT

#ifdef _WIN32

#include "SDL2\SDL.h"
#include "..\TextureManager.hpp"
#include "..\Vector2D.hpp"
#include "..\AssetManager.hpp"

#endif



#ifdef _WIN64

#include "SDL2\SDL.h"
#include "..\TextureManager.hpp"
#include "..\Vector2D.hpp"
#include "..\AssetManager.hpp"

#endif



#ifdef __linux__

#include <SDL2/SDL.h>
#include "../TextureManager.hpp"
#include "../Vector2D.hpp"
#include "../AssetManager.hpp"

#endif

#include "EntityComponentSystem.hpp"


class TileComponent : public Component
{
private:
    SDL_Rect srcRect;
    SDL_Rect destRect;
    Vector2D position;
    SDL_Texture* texture;

public:

    TileComponent() = default;
    TileComponent( int srcX, int srcY, int x, int y, int tileSize, int tileScale, std::string texID ) {
	texture = Game::assets->getTexture( texID );

	position.setX( x );
	position.setY( y );

	srcRect.x = srcX;
	srcRect.y = srcY;
	srcRect.w = srcRect.h = tileSize;

	destRect.x = x;
	destRect.y = y;
	destRect.w = destRect.h = tileSize * tileScale;

    }

    ~TileComponent() { SDL_DestroyTexture( this-> texture ); }

    void setSrcRect  ( SDL_Rect srcR )    { this->srcRect = srcR; }
    void setDestRect ( SDL_Rect destR )   { this->destRect = destR; }
    void setTexture  ( SDL_Texture* tex ) { this->texture = tex; }

    SDL_Rect getSrcRect()     { return this->srcRect; }
    SDL_Rect getDestRect()    { return this->destRect; }
    SDL_Texture* getTexture() { return this->texture; }
    Vector2D getPosition()    { return this->position; }

    void draw() override { TextureManager::Draw( this->texture, srcRect, destRect, SDL_FLIP_NONE ); }

    void update() override {
	destRect.x = position.getX() - Game::camera.x;
	destRect.y = position.getY() - Game::camera.y;
    }
};

#endif
