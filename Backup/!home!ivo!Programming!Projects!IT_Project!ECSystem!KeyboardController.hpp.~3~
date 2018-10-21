#ifndef KEYBOARD_CONTROLLER
#define KEYBOARD_CONTROLLER

#include "../Game.hpp"
#include "EntityComponentSystem.hpp"
#include "Components.hpp"
#include "SpriteComponent.hpp"

class KeyboardConstroller : public Component
{
public:
    TransformComponent* transform;
    SpriteComponent* sprite;

    void init() override {
	transform = &entity->getComponent< TransformComponent >();
	sprite = &entity->getComponent< SpriteComponent >();
    }
    void update() override {
	if( Game::event.type == SDL_KEYDOWN )
	{
	    switch ( Game::event.key.keysym.sym ) {
		case SDLK_w:
		    transform->getVelocity().setY( -1 );
		    sprite->play( "walkUp" );
		    break;
		case SDLK_a:
		    transform->getVelocity().setX( -1 );
		    sprite->play( "walkSideway" );
		    sprite->spriteFlip = SDL_FLIP_HORIZONTAL;
		    break;
		case SDLK_d:
		    transform->getVelocity().setX( 1 );
		    sprite->play( "walkSideway" );
		    break;
		case SDLK_s:
		    transform->getVelocity().setY( 1 );
		    sprite->play( "walkDown" );
		default:
		    break;
	    }
	}

	if ( Game::event.type == SDL_KEYUP )
	{
	    switch ( Game::event.key.keysym.sym ) {
		case SDLK_w:
		    transform->getVelocity().setY( 0 );
		    sprite->play( "idle" );
		    break;
		case SDLK_a:
		    transform->getVelocity().setX( 0 );
		    sprite->play( "idle" );
		    sprite->spriteFlip = SDL_FLIP_NONE;
		    break;
		case SDLK_d:
		    transform->getVelocity().setX( 0 );
		    sprite->play( "idle" );
		    break;
		case SDLK_s:
		    transform->getVelocity().setY( 0 );
		    sprite->play( "idle" );
		    break;
		case SDLK_ESCAPE:
		    Game::isActive = false;
		    break;
		default:
		    break;
	    }
	}
    }
};

#endif
