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


public:
    ObjectComponent  (){}
    ObjectComponent  ( int dur, bool stat ) : durability( dur ), isStatic( stat ){}
    ObjectComponent  ( bool stat ) : isStatic( stat ){}
    ~ObjectComponent (){}


    void setDurability ( int dur )  { this->durability = dur; }
    void setIsStatic   ( bool stat) { this->isStatic = stat; }


    int getDurability () { return this->durability; }
    bool getIsStatic  () { return this->isStatic; }


    void init () override { transform = &entity->getComponent< TransformComponent >(); }

    void update () override {
	if( durability <= 0 )
	{
	    printf( "The object durability fell under 0. The Object was destroyed\n" );
	    entity->destroy();
	}
    }

};

#endif
