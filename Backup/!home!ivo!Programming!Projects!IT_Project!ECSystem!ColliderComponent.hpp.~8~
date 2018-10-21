#ifndef COLLIDER_COMPONENT
#define COLLIDER_COMPONENT

#include <string>


#ifdef _WIN32

#include "SDL2\SDL.h"

#endif



#ifdef _WIN64

#include "SDL2\SDL.h"

#endif



#ifdef __linux__

#include <SDL2/SDL.h>

#endif



#include "Components.hpp"

class ColliderComponent : public Component
{
private:
    std::string tag;
    SDL_Rect collider;
    TransformComponent* transform;

public:

    ColliderComponent ( std::string tag ){
	this->tag = tag;
    }

    ColliderComponent ( std::string tag, int x, int y, int size ){
	this->tag = tag;
	collider.x = x;
	collider.y = y;
	collider.h = collider.w = size;
    }


    void setTag( std::string tag)      { this->tag = tag; }
    void setCollider ( SDL_Rect coll ) { this->collider = coll; }
    // void setTransformComp ( TransformComponent* transf ) { this->transform = transf; }


    std::string getTag()   const                { return this->tag; }
    SDL_Rect getCollider() const                { return this->collider; }
    TransformComponent* getTransformComponent() { return this->transform; }


    void init() override {

	if ( !entity->hasComponent< TransformComponent >() ) {
	    entity->addComponent< TransformComponent >();
	}

	transform = &entity->getComponent< TransformComponent >();
    }

    void update() override {
	if ( tag != "terrain" )
	{
	    collider.x = static_cast< int >( transform->getXPos() );
	    collider.y = static_cast< int >( transform->getYPos() );

	    collider.w = transform->getWidth() * transform->getScale();
	    collider.h = transform->getHeight() * transform->getScale();
	}
    }
};

#endif
