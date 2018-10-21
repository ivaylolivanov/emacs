#ifndef COMPONENTS
#define COMPONENTS

#include "EntityComponentSystem.hpp"

class PositionComponent : public Component
{
private:
    int xPos = 0;
    int yPos = 0;

public:

    int getXPos () { return this->xPos; }
    int getYPos () { return this->yPos; }

    void init() override {
	this->xPos = 0;
	this->yPos = 0;
    }

    void update() override {
	this->xPos++;
	this->yPos++;
    }

    void setPosition( int x, int y ) {
	this->xPos = x;
	this->yPos = y;
    }

};

#endif
