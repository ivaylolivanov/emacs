#include "AssetManager.hpp"

AssetManager::AssetManager( Manager* mngr ) : manager( manager )
{}

AssetManager::~AssetManager()
{}

void AssetManager::addTexture( std::string id, const char *path )
{
    textures.emplace( id, TextureManager::LoadTexture( path ) );
}

SDL_Texture* AssetManager::getTexture( std::string id )
{
    return textures[id];
}
