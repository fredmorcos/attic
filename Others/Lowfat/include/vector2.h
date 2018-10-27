////////////////////////////////////////////////////////////////////////////////
//3456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 
//
//  lowfat - an "engine" for natural document viewing for free desktop-systems
//
//  copyright (c) 2007 Mirco Müller
//
//  lowfat is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  lowfat is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with Foobar; if not, write to the Free Software
//  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
//
////////////////////////////////////////////////////////////////////////////////

#ifndef _VECTOR2_H
#define _VECTOR2_H

#include <math.h>
#include <iostream>

template<typename T>
struct Vector2
{
	T x; 
	T y;

	Vector2 () :
		x(0),
		y(0)
	{
	}

	Vector2 (T nx, T ny) :
		x (nx),
		y (ny)
	{
	}

	~Vector2 ()
	{
	}

	Vector2& operator+=(const Vector2& a)
	{
		x += a.x;
		y += a.y;

		return *this;
	}

	Vector2& operator*=(T s)
	{
		x *= s;
		y *= s;

		return *this;
	}

	Vector2& operator+() const
	{
		return *this;
	}

	Vector2 operator-() const
	{
		return Vector2 (-x, -y);
	}

	Vector2& operator-=(const Vector2& a)
	{
		return operator+=(-a);
	}

	Vector2& operator/=(T s)
	{
		return operator*=(1 / s);
	}

	Vector2 operator+(const Vector2& a) const
	{
		return Vector2(*this) += a;
	}

	Vector2 operator-(const Vector2& a) const
	{
		return Vector2(*this) -= a;
	}

	Vector2 operator*(T s) const
	{
		return Vector2(*this) *= s;
	}

	Vector2 operator/(T s) const
	{
		return Vector2(*this) /= s;
	}

	friend Vector2 operator*(T s, const Vector2& a)
	{
		return Vector2(a) *= s;
	}

	Vector2& rotate(float angle)
	{
		const double radians (-angle * 3.141592f / 180.0f);
		const double nx = x * cos (radians) + y * sin (radians);
		const double ny = y * cos (radians) - x * sin (radians);
		x = (T) nx;
		y = (T) ny;

		return *this;
	}

	Vector2& normalize ()
	{
		if ((*this).length () != 0)
			return operator/=((*this).length ());
		else
			return *this;
	}

	static T
	dot (const Vector2<T>& a,
	     const Vector2<T>& b)
	{
		return T (a.x * b.x + a.y * b.y);
	}

	friend std::ostream&
	operator<<(std::ostream& s,
		   const Vector2& a)
	{
		return (s << "(" << a.x << ", " << a.y << ")");
	}

	T
	length ()
	{
		return (T) sqrt ((x*x) + (y*y));
	}

};

template<typename T>
T dot (const Vector2<T>& a,
       const Vector2<T>& b)
{
	return T (a.x * b.x + a.y * b.y);
}

template<typename T>
T length(const Vector2<T>& a)
{
	return (T) sqrt (dot (a, a));
}

template<typename T>
T angle (const Vector2<T>& a,
	 const Vector2<T>& b)
{
	double c (dot(a, b) / (length (a) * length (b)));

	if (c > 1)
		c = 1;

    return (T) acos (c);
}

#endif // _VECTOR2_H
