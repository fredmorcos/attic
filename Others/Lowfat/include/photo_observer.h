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

#ifndef _PHOTOOBSERVER_H
#define _PHOTOOBSERVER_H

#include "include/image.h"
#include "include/gui_observer.h"
#include "include/forward.h"

class Lowfat;

class PhotoObserver;
typedef SharedPtr<PhotoObserver> PhotoObserverPtr;

class PhotoObserver  : public GuiObserver
{
public:
    static PhotoObserverPtr make(Lowfat* lowfat);

	virtual bool onMousePress(const MouseEvent& e);
	virtual bool onMouseRelease(const MouseEvent& e);
	virtual bool onMouseMove(const MouseEvent& e);

	virtual bool onKeyPress(const KeyEvent& e);
	virtual bool onKeyRelease(const KeyEvent& e);

	virtual bool onTime( const TimeEvent& e );
	virtual void clearSelectedList();
	virtual void setSelection( ImagePtr );

	virtual void setDynamic(float d);
	virtual std::list<ImagePtr>& getSelection();
	virtual void selectionToFront();
	virtual void deleteImage(ImagePtr image);
	int max (int a, int b);
	int min (int a, int b);
	float max (float a, float b);
	float min (float a, float b);
	float clamp(float x);

private:
	void handleTrashcanSaug(ImagePtr image, const Vec2f& mouse);
	PhotoObserver(Lowfat* lowfat);

	void zoomSoft(ImagePtr image, Vec2f dragPoint, float zoomFactor);
	void onMouseDrag( const MouseEvent& e);
	void RotateSelection( float angle );
	void pullTogether();
	void setRestoreList();

	float dynamic_;
	bool	shift_;
	bool	buttonLeft_;
	bool	buttonRight_;
	bool	buttonMid_;
	bool	noRotate_;
	int		lastX_;
	int		lastY_;
	int		lastXDiff_;
	int		lastYDiff_;
	unsigned int lastLocationTime_;
	unsigned int lastDoubleClickStart_;

	bool	zoomImages_;
	Vec2f	zoomLocation_;

	bool	releaseSelection_;

	std::list<ImagePtr> selectedList_;
	std::list<ImagePtr> deletedList_;
	std::list<ImagePtr> selectedPreBox_;
	std::list<ImagePtr> selectedRestore_;

	Lowfat* parent_;
};

#endif // _PHOTOOBSERVER_H
