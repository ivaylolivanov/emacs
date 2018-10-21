#ifndef POSITION_COMPONENT
#define POSITION_COMPONENT

#include "Components.hpp"
#include "../Vector2D.hpp"

class TransformComponent : public Component
{
private:
    Vector2D position;
    Vector2D velocity;

    int speed  = 3;
    int width  = 32;
    int height = 32;
    int scale  = 1; // also used as mass

    float friction = 0.02;
    float acceleration;


public:

    TransformComponent() {
	position.zero();
    }

    TransformComponent( int scl ) {
	position.zero();
	position.setX( 800 );
	position.setY( 640 );

	scale = scl;
    }

    TransformComponent( float x, float y ) {
	position.setX( x );
	position.setY( y );
    }

    TransformComponent( float x, float y, int scl ){
	position.zero();
	position.setX( x );
	position.setY( y );

	scale = scl;
    }

    TransformComponent( float x, float y, int h, int w, int scl ) {
	position.setX( x );
	position.setY( y );
	height = h;
	width = w;
	scale = scl;
    }



    void setXPos         ( int x )   { position.setX( x ); }
    void setYPos         ( int y )   { position.setY( y ); }
    void setSpeed        ( int spd ) { this->speed = spd; }
    void setWidth        ( int w )   { this->width = w; }
    void setHeight       ( int h )   { this->height = h; }
    void setScale        ( int scl ) { this->scale = scl; }
    void setPosition     ( Vector2D& pos ) { this->position = pos; }
    void setVelocity     ( Vector2D& vel ) { this->velocity = vel; }
    void setFriction     ( float fric ) { this->friction = fric; }
    void setAcceleration ( float accel ) { this->acceleration = accel; }


    int getXPos           () { return position.getX(); }
    int getYPos           () { return position.getY(); }
    int getSpeed          () { return this->speed; }
    int getWidth          () { return this->width; }
    int getHeight         () { return this->height; }
    int getScale          () { return this->scale; }
    float getFriction     () { return this->friction; }
    float getAcceleration () { return this->acceleration; }
    Vector2D& getPosition () { return this->position; }
    Vector2D& getVelocity () { return this->velocity; }



    void init() override {
	velocity.zero();
    }

    void update() override {

	float tempX = position.getX();
	float tempY = position.getY();

	position.setX( tempX + velocity.getX() * this->speed );
	position.setY( tempY + velocity.getY() * this->speed );
    }

};

#endif
