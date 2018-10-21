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



inline ComponentID getComponentTypeID()
{
    static ComponentID lastID = 0;
    return lastID++;
}

template < typename T > inline ComponentID getComponentTypeID() noexcept
{
    static ComponentID typeID = getComponentTypeID();
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
    }


    void draw()     { for( auto& comp : components ) comp->draw(); }
    bool isActive() { return this->active; }
    void destroy()  { this->active = false; }


    template < typename T > bool hasComponent() const {
	return componentBitSet[ getComponentTypeID< T > ];
    }


    template < typename T, typename... TArgs >
    T& addComponent( TArgs&&... mArgs ) {

	T* comp ( new T( std::forward< TArgs >( mArgs )... ));
	comp->entity = this;

	std::unique_ptr< Component > uniqPtr ( comp );
	components.emplace_back( std::move ( uniqPtr ) );

	componentArray  [ getComponentTypeID< T >() ] = comp;
	componentBitSet [ getComponentTypeID< T >() ] = true;

	comp->init();

	return *comp;
    }


    template < typename T > T& getComponent() const {
	auto ptr( componentArray [ getComponentTypeID< T >() ]);
	return *static_cast< T* >( ptr );
    }

};


class Manager
{

private:
    std::vector< std::unique_ptr< Entity >> entities;

public:
    void update()  { for ( auto& ent : entities ) ent->update(); }
    void draw()    { for ( auto& ent : entities ) ent->draw();   }

    void refresh() {
	entities.erase(
	    std::remove_if(
		std::begin( entities ),
		std::end( entities ),
		[](const std::unique_ptr<Entity> &mEntity) { return !mEntity->isActive(); }
		),
	    std::end( entities ));
    }

    Entity& addEntity() {
	Entity* ent = new Entity;
	std::unique_ptr< Entity > uniqPtr { ent };
	entities.emplace_back( std::move ( uniqPtr ) );
	return *ent;
    }

};


#endif
