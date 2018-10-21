#ifndef POSITION_COMPONENT
#define POSITION_COMPONENT

#include "Components.hpp"

class PositionComponent : public Component
{
private:
    int xPos;
    int yPos;

public:

    // Constructors:
    PositionComponent() {
	this->xPos = 0;
	this->yPos = 0;
    }

    PositionComponent( int x, int y ) {
	this->xPos = x;
	this->yPos = y;
    }

    // Setters
    void setXPos( int x ) { this->xPos = x; }
    void setYPos( int y ) { this->yPos = y; }

    // Getters
    int getXPos () { return this->xPos; }
    int getYPos () { return this->yPos; }


    // Methods
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
