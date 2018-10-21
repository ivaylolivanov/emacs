#include "Game.hpp"
#include "TextureManager.hpp"
#include "Map.hpp"
#include "ECSystem/Components.hpp"
#include "Vector2D.hpp"
#include "Collision.hpp"
#include "AssetManager.hpp"


Map* arenaMap;
Manager manager;

SDL_Renderer* Game::renderer = nullptr;
SDL_Event Game::event;

auto& player( manager.addEntity() );

bool Game::isActive = false;
SDL_Rect Game::camera = { 0, 0, 800, 600 };
AssetManager* Game::assets = new AssetManager( &manager );

Game::Game()
{}

Game::~Game()
{}

void Game::init( const char* title, int windowWidth, int windowHeight, bool fullscreen )
{
    int flags = 0;
    if ( fullscreen )
	flags = SDL_WINDOW_FULLSCREEN;



    if ( SDL_Init( SDL_INIT_EVERYTHING ) == 0 )
    {
	printf( "SDL initialized successfully!\n" );
	window = SDL_CreateWindow( title, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, windowWidth, windowHeight, flags );

	if ( window )
	    printf( "Window successfully created!\n" );

	renderer = SDL_CreateRenderer( window, -1, 0 );
	if ( renderer )
	{
	    printf( "Renderer initialized successfully!\n" );
	    SDL_SetRenderDrawColor( renderer, 200, 200, 200, 255 );
	}

	isActive = true;
    }
    else
	isActive = false;


    assets->addTexture( "terrain", "Assets/MapSpriteSheet.png" );
    assets->addTexture( "player" , "Assets/SpriteSheet.png" );
    assets->addTexture( "projectile", "Assets/frostbolt.png" );

    arenaMap = new Map( "terrain", 2, 32 );

    arenaMap->loadMap( "Assets/Warmage_arena.map", 25, 20 );

    player.addComponent< TransformComponent >( 2 );
    player.getComponent< TransformComponent >().setStep( 0.15 );
    player.addComponent< SpriteComponent >( "player", true );
    player.addComponent< KeyboardConstroller >( );
    player.addComponent< ColliderComponent >( "player" );
    player.addGroup( groupPlayer );

}

auto& tiles( manager.getGroup( Game::groupMap ) );
auto& players( manager.getGroup( Game::groupPlayer ) );
auto& colliders( manager.getGroup( Game::groupColliders ) );
auto& projectiles( manager.getGroup( Game::groupProjectiles ) );

void Game::handleEvents()
{

    SDL_PollEvent( &event );
    switch ( event.type)
    {
	case SDL_QUIT:
	    isActive = false;
	    break;

	default:
	    break;
    }
}

void Game::update()
{

    SDL_Rect playerColl = player.getComponent< ColliderComponent >().getCollider();
    Vector2D playerPos  = player.getComponent< TransformComponent >().getPosition();

    manager.refresh();
    manager.update();

    for( auto& coll : colliders )
    {
	SDL_Rect tempColl = coll->getComponent< ColliderComponent >().getCollider();
	if( Collision::AABB( tempColl,playerColl ) )
	    player.getComponent< TransformComponent >().setPosition( playerPos );
    }

    for( auto& proj : projectiles )
    {
	if( Collision::AABB(
		player.getComponent< ColliderComponent >().getCollider(),
		proj->getComponent< ColliderComponent >().getCollider()
		) )
	{
	    printf( "Player hit!\n" );
	    proj->destroy();
	}

    }

    camera.x = player.getComponent< TransformComponent >().getXPos() - 400;
    camera.y = player.getComponent< TransformComponent >().getYPos() - 320;

    if( camera.x < 0 ) camera.x = 0;
    if( camera.y < 0 ) camera.y = 0;

    if( camera.x > camera.w ) camera.x = camera.w;
    if( camera.y > camera.h ) camera.y = camera.h;

}

void Game::render()
{
    SDL_RenderClear( renderer );

    for ( auto& tile : tiles )       { tile->draw(); }
    for ( auto& plyr : players )     { plyr->draw(); }
    for ( auto& proj : projectiles ) { proj->draw(); }

    SDL_RenderPresent( renderer );
}

void Game::clean()
{
    SDL_DestroyWindow( window );
    SDL_DestroyRenderer( renderer );
    SDL_Quit();

    printf( "SDL cleaned!\n" );
}

bool Game::isRunning() { return this->isActive; }
