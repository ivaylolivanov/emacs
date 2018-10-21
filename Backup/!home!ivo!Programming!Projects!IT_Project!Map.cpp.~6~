#include "Map.hpp"
#include "Game.hpp"
#include <fstream>
#include "ECSystem/EntityComponentSystem.hpp"
#include "ECSystem/Components.hpp"

extern Manager manager;

Map::Map( std::string texID, int mapScl, int tileSz )
    : textureID( texID ), mapScale( mapScl ), tileSize( tileSz )
{
    this->scaledSize = mapScl * tileSz;
}

Map::~Map()
{}

void Map::loadMap( std::string path, int sizeX, int sizeY )
{
    char tile;
    int srcX, srcY;
    std::fstream mapFile;

    mapFile.open( path );

    for (int y = 0; y < sizeY; ++y)
    {
	for (int x = 0 ; x < sizeX; ++x)
	{

	    mapFile.get( tile );
	    srcY = atoi( &tile ) * tileSize;

	    mapFile.get( tile );
	    srcX = atoi( &tile ) * tileSize;


	    addTile( srcX, srcY, x*scaledSize, y*scaledSize );
	    mapFile.ignore();
	}
    }

    mapFile.ignore();

    for( int y = 0; y < sizeY; ++y )
    {
	for (int x = 0 ; x < sizeX; ++x)
	{
	    mapFile.get( tile );
	    if( tile == '1' )
	    {
		auto& tileCollider( manager.addEntity() );
		tileCollider.addComponent< ColliderComponent >
		    ( "terrain", x * scaledSize, y * scaledSize , scaledSize );
		tileCollider.addGroup( Game::groupColliders );
	    }
	    mapFile.ignore();
	}
    }

    mapFile.close();
}


void Map::addTile( int srcX, int srcY, int x, int y )
{
    auto& tile( manager.addEntity() );
    tile.addComponent< TileComponent > ( srcX, srcY, x, y, tileSize, mapScale, textureID );
    tile.addGroup( Game::groupMap );
}
