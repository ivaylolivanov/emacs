#ifndef KEYBOARD_CONTROLLER
#define KEYBOARD_CONTROLLER

#include "../Game.hpp"
#include "EntityComponentSystem.hpp"
#include "Components.hpp"
#include "SpriteComponent.hpp"

class KeyboardConstroller : public Component
{
private:
    bool isFired = false;

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
	    transform->getVelocity().setX( 0 );
	    transform->getVelocity().setY( 0 );

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
		    break;
		case SDLK_j:
		    if( !isFired )
		    {
			Game::assets->createProjectile( Vector2D( transform->getXPos() + 15,
								  transform->getYPos() + 70 ),
							Vector2D( 0, 1 ),
							200,
							1,
							"projectile" );
			isFired = true;
		    }

		    break;
		case SDLK_k:
		    if( !isFired )
		    {
			Game::assets->createProjectile( Vector2D( transform->getXPos() + 15,
								  transform->getYPos() - 70 ),
							Vector2D( 0, -1 ),
							200,
							1,
							"projectile" );
			isFired = true;
		    }
		    break;
		case SDLK_h:
		    if( !isFired )
		    {
			Game::assets->createProjectile( Vector2D( transform->getXPos() - 40,
								  transform->getYPos() ),
							Vector2D( -1, 0 ),
							200,
							1,
							"projectile" );
			isFired = true;
		    }
		    break;
		case SDLK_l:
		    if( !isFired )
		    {
			Game::assets->createProjectile( Vector2D( transform->getXPos() + 70,
								  transform->getYPos() ),
							Vector2D( 1, 0 ),
							200,
							1,
							"projectile" );
			isFired = true;
		    }
		    break;
		default:
		    break;
	    }
	}

	if ( Game::event.type == SDL_KEYUP )
	{
	    switch ( Game::event.key.keysym.sym ) {
		case SDLK_w:
		    transform->decAcceleration( 0.3 );
		    sprite->play( "idle" );
		    break;
		case SDLK_a:
		    transform->decAcceleration( 0.3 );
		    sprite->play( "idle" );
		    sprite->spriteFlip = SDL_FLIP_NONE;
		    break;
		case SDLK_d:
		    transform->decAcceleration( 0.3 );
		    sprite->play( "idle" );
		    break;
		case SDLK_s:
		    transform->decAcceleration( 0.3 );
		    sprite->play( "idle" );
		    break;
		case SDLK_j:
		case SDLK_k:
		case SDLK_h:
		case SDLK_l:
		    isFired = false;
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
