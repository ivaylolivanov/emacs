#ifndef VECTOR2D
#define VECTOR2D

#include <iostream>

class Vector2D
{
private:
    float x;
    float y;

public:
    Vector2D();
    Vector2D( float x, float y );

    void  setX( float x );
    void  setY( float y );
    float getX();
    float getY();

    Vector2D& add( const Vector2D vec );
    Vector2D& subtract( const Vector2D vec );
    Vector2D& multiply( const Vector2D vec );
    Vector2D& divide( const Vector2D vec );

    Vector2D& operator+=( Vector2D& vec );
    Vector2D& operator-=( Vector2D& vec );
    Vector2D& operator*=( Vector2D& vec );
    Vector2D& operator/=( Vector2D& vec );

    Vector2D& operator* (const int& num);
    Vector2D& zero();

    friend Vector2D& operator+( Vector2D& vec1, Vector2D& vec2 );
    friend Vector2D& operator-( Vector2D& vec1, Vector2D& vec2 );
    friend Vector2D& operator*( Vector2D& vec1, Vector2D& vec2 );
    friend Vector2D& operator/( Vector2D& vec1, Vector2D& vec2 );
    friend std::ostream& operator<<( std::ostream& stream, const Vector2D& vec );

};

#endif
