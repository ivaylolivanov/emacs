#ifndef COLLISION
#define COLLISION

#include <stdio.h>
#include <SDL2/SDL.h>
#include "ECSystem/ColliderComponent.hpp"

class ColliderComponent;

class Collision
{
public:
    static bool AABB( const SDL_Rect& rec1, const SDL_Rect& rec2 );
    static bool AABB( const ColliderComponent& col1, const ColliderComponent& col2 );
};

#endif
