#ifndef ECS
#define ECS

#include <iostream>
#include <vector>
#include <memory>
#include <algorithm>
#include <bitset>
#include <array>

class Component;
class Entity;

using ComponentID = std::size_t;
constexpr std::size_t maxComponents = 32;
using ComponentBitSet = std::bitset< maxComponents >;
using ComponentArray = std::array< Component* , maxComponents >;



inline ComponentID getComponentID()
{
    static ComponentID lastID = 0;
    return lastID++;
}

template < typename T > inline ComponentID getComponentID() noexcept
{
    static ComponentID typeID = getComponentID();
    return typeID;
}

class Component
{
public:

    Entity* entity;
    virtual void init()   { };
    virtual void update() { };
    virtual void draw()   { };

    virtual ~Component()  { };
};

class Entity
{
private:
    bool active = true;
    std::vector< std::unique_ptr< Component > > components;

    ComponentArray componentArray;
    ComponentBitSet componentBitSet;

public:
    void update() {
	for( auto& comp : components ) comp->update();
	for( auto& comp : components ) comp->draw();
    }

    void draw()     {}
    bool isActive() { return this->active; }
    void destroy()  { this->active = false; }

    template < typename T > bool hasComponent() const {
	return componentBitSet[getComponentID< T >];
    }

};



#endif
