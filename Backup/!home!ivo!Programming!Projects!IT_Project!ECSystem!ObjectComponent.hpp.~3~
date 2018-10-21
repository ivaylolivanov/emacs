#ifndef OBJECT_COMPONENT
#define OBJECT_COMPONENT

#include "../Vector2D.hpp"
#include "Components.hpp"
#include "EntityComponentSystem.hpp"

class ObjectComponent : public Component
{
private:
    TransformComponent* transform;
    int durability = 1;
    bool isStatic = true;
    int mass = 1;


public:
    ObjectComponent  (){}
    ObjectComponent  ( int dur, bool stat ) : durability( dur ), isStatic( stat ){}
    ObjectComponent  ( int dur, bool stat, int m ) : durability( dur ), isStatic( stat ), mass( m ){}
    ObjectComponent  ( bool stat ) : isStatic( stat ){}
    ~ObjectComponent (){}


    void setDurability ( int dur )  { this->durability = dur; }
    void decDurability ( int decr ) { this->durability -= decr; }
    void setIsStatic   ( bool stat) { this->isStatic = stat; }
    void setMaxx       ( int m )    { this->mass = m; }

    int getMass       () { return this->mass; }
    bool getIsStatic  () { return this->isStatic; }
    int getDurability () { return this->durability; }


    void init () override { transform = &entity->getComponent< TransformComponent >(); }

    void update () override {
	if( durability <= 0 )
	{
	    printf( "The object durability fell under 0. The Object was destroyed\n" );
	    entity->destroy();
	}

	if( transform->getAcceleration() > 0 )
	    transform->decAcceleration( transform->getStep() * mass );

	if( transform->getAcceleration() <= 0 )
	{
	    transform->getVelocity().setX( 0 );
	    transform->getVelocity().setY( 0 );
	    transform->setStep( 0 );
	}
    }

};

#endif
