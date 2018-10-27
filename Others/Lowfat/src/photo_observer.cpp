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

#include <algorithm>
#include <list>

#include "SDL.h"
#include "include/photo_observer.h"
#include "include/selection.h"
#include "include/lowfat.h"

bool g_b_funMode;

using std::for_each;
using std::list;
using std::swap;

static const float dynamicThreshold (0.5f);
static const float outerTrashcanSaugDist (140.0f);
static const float innerTrashcanSaugDist (30.0f);

PhotoObserverPtr
PhotoObserver::make (Lowfat* lowfat)
{
	return PhotoObserverPtr (new PhotoObserver (lowfat));
}

PhotoObserver::PhotoObserver(Lowfat* lowfat) :
	dynamic_(1),
	shift_(false),
	buttonLeft_(false),
	buttonRight_(false),
	buttonMid_(false),
	noRotate_(false),
	lastX_(0),
	lastY_(0),
	lastXDiff_(0),
	lastYDiff_(0),
	lastLocationTime_(0),
	lastDoubleClickStart_(0),
	zoomImages_(false),
	zoomLocation_(Vec2f(0,0)),
	releaseSelection_(false),
	parent_(lowfat)
{
}

void PhotoObserver::clearSelectedList()
{
	for (std::list<ImagePtr>::iterator it = selectedList_.begin();
		it != selectedList_.end(); ++it)
	{
		(*it)->setHighlight( true, SDL_GetTicks(), outFade );
	}

	bool isDifferentList = false;
	if(selectedList_.size() != selectedRestore_.size())
	{
		isDifferentList = true;
	}
	else
	{
        std::list<ImagePtr>::iterator it1 = selectedList_.begin();
        std::list<ImagePtr>::iterator it2 = selectedRestore_.begin();
		if (it1 != it2)
		{
			isDifferentList = true;
		}
	}

	if((selectedList_.size() > 0) && (isDifferentList))
	{
		selectedRestore_ = selectedList_;
	}
	selectedList_.clear();
}

void PhotoObserver::setSelection( ImagePtr image )
{
	selectedList_.push_back( image );
	image->setHighlight( true, SDL_GetTicks(), inFade );
}

void PhotoObserver::zoomSoft(ImagePtr image, Vec2f dragPoint, float zoomFactor)
{
	for (std::list<ImagePtr>::iterator it = selectedList_.begin();
		it != selectedList_.end(); ++it)
	{
		image =*it;

		const Vec2f oldSize(image->getSize());
		const Vec2f newSize(image->getDestSize() * zoomFactor);
		const float totalZoomFactor(length(newSize) / length(oldSize));
		const Vec2f delta(dragPoint - image->getLocation());
		const Vec2f newPos(image->getLocation() + delta * totalZoomFactor);
		image->setTransition(image->getLocation() - (newPos - dragPoint), 
							image->getDestSize() * zoomFactor,
                  			image->getRotation());
	}
}

static void zoom(ImagePtr image, Vec2f dragPoint, float zoomFactor)
{
	Vec2f oldSize = image->getSize();
    image->setSize(oldSize * zoomFactor);
	zoomFactor = image->getSize().x > image->getSize().y ?
		image->getSize().x / oldSize.x : image->getSize().y / oldSize.y;

    const Vec2f delta(dragPoint- image->getLocation());
	const Vec2f newPos(image->getLocation() + delta * zoomFactor);
    image->setLocation(image->getLocation() - (newPos - dragPoint));
}

void PhotoObserver::pullTogether() {
	const int selectedCount(selectedList_.size());
	if (selectedCount == 0) return;
	float degrees(90);
	float angle(-degrees / 2);
	if (selectedCount == 1) angle = 0;
	const float angleDelta(degrees / selectedCount);

	for (std::list<ImagePtr>::iterator it = selectedList_.begin();
	     it != selectedList_.end();
		 ++it) {
		ImagePtr img(*it);
		const float maxSize(120);
		img->setTransition (Vec2f (lastX_, lastY_),
				    img->getMaximizedSize ((int) maxSize,
							   (int) maxSize),
				    angle);
		angle += angleDelta;
	}
}

bool PhotoObserver::onMousePress(const MouseEvent& e)
{
	bool process = true;
	ImagePtr image = parent_->getImage( e.x, e.y );

    switch(e.button)
	{
    case MouseButton::left:
		buttonLeft_ = true;
		break;
    case MouseButton::right:
		buttonRight_ = true;
		zoomImages_ = true;
		zoomLocation_ = Vec2f( e.x, e.y );
		if((selectedList_.size() == 0) && (image.get() != 0))
		{
			selectedList_.push_back( image );
			image->setHighlight( true, SDL_GetTicks(), inFade );
			parent_->toFront(image);
		}
		break;
    case MouseButton::mid:
		buttonMid_ = true;
		break;
    case MouseButton::wheelUp:
		if( image.get() != 0 )
		{
			zoomSoft(image, Vec2f(e.x, e.y), 1.204 );
			process = false;
		}
        break;        
    case MouseButton::wheelDown:
		if( image.get() != 0 )
		{
	        zoomSoft(image, Vec2f(e.x, e.y), 1 / 1.204);
			process = false;
		}
        break;
	}

	if (buttonLeft_)
	{
		if (image.get() != 0)
		{

			unsigned int time = SDL_GetTicks ();
			if (time - this->lastDoubleClickStart_ < 200)
			{
				float zoomFactor = parent_->display_->getWidth () / image->getSize ().x;
				Vec2f targetSize = image->getSize () * zoomFactor;
				if (targetSize.y > parent_->display_->getHeight ())
				{
					zoomFactor = parent_->display_->getHeight() / image->getSize().y;
					targetSize = image->getSize() * zoomFactor;
				}
				zoomFactor *= 0.55;

				Vec2f imageOrigin (parent_->display_->getWidth() / 2,parent_->display_->getHeight() / 2);
				imageOrigin -= image->getLocation();
				Vec2f zoomOrigin( image->getLocation() );
				float rotation = - image->getRotation();
				for (std::list<ImagePtr>::iterator it = selectedList_.begin();
					it != selectedList_.end(); ++it)
				{
					const Vec2f delta(zoomOrigin- (*it)->getLocation());
					Vec2f newPos((*it)->getLocation() + delta * zoomFactor);
					newPos = (*it)->getLocation() - (newPos - zoomOrigin - imageOrigin);
					(*it)->setTransition( newPos, (*it)->getSize() * zoomFactor, (*it)->getRotation() + rotation, false );
				}
			}
			else
			{
				handleTrashcanSaug(image, Vec2f(e.x, e.y));

				if (!shift_)
				{
					image->setDragPoint(SharedPtr<Vec2f>(new Vec2f(e.x, e.y)));

					releaseSelection_ = true;
					bool isSelected = false;
					for (std::list<ImagePtr>::iterator it = selectedList_.begin();
						it != selectedList_.end(); ++it)
					{
						if((*it) == image)
						{
							isSelected = true;
						}
					}

					if(!isSelected)
					{
						clearSelectedList();
					}
					parent_->getSelection()->setStart( Vec2f(e.x,e.y) );
					parent_->getSelection()->setEnd( Vec2f(e.x,e.y) );
				}

				parent_->toFront(image);

				bool notSelected = true;
				for (std::list<ImagePtr>::iterator it = selectedList_.begin();
					it != selectedList_.end(); ++it)
				{
					if (image.get() == (*it).get())
					{
						notSelected = false;
					}
				}

				if (notSelected)
					selectedList_.push_back (image);

				image->setHighlight (true, SDL_GetTicks (), inFade);
				process = false;
			}

			lastDoubleClickStart_ = SDL_GetTicks ();
		}
		else
		{
			if(!shift_)
				clearSelectedList();

			parent_->getSelection()->setStart (Vec2f (e.x,e.y));
			parent_->getSelection()->setEnd (Vec2f (e.x,e.y));
			parent_->enableSelection (true);
			selectedPreBox_ = selectedList_;

			unsigned int time = SDL_GetTicks ();
			if (time - this->lastDoubleClickStart_ < 200)
			{
				parent_->sort (LfWindow::aspectRatioSortMode,
					       parent_->photoObserver_->getSelection ());
			}
			lastDoubleClickStart_ = SDL_GetTicks ();
		}
	}

	return process;
}

bool
PhotoObserver::onMouseRelease (const MouseEvent& e)
{
	if (e.button == MouseButton::left)
	{
		for (std::list<ImagePtr>::iterator it = selectedList_.begin();
						it != selectedList_.end(); ++it)
		{
			ImagePtr image(*it);
			image->setDragPoint(SharedPtr<Vec2f>(0));
		}

		if(((selectedList_.size() > 1) && !g_b_funMode) || noRotate_)
		{
			for (std::list<ImagePtr>::iterator it = selectedList_.begin();
							it != selectedList_.end(); ++it)
			{
				ImagePtr image(*it);
				image->setVelocity ( Application::getAverageMouseMoveVelocity() * dynamic_);
			}
		}

	}

	switch(e.button)
	{
	case MouseButton::left:
		{
			buttonLeft_ = false;

			if(parent_->hasSelection())
			{
				parent_->enableSelection( false );
				for (std::list<ImagePtr>::iterator it = selectedList_.begin();
					it != selectedList_.end(); ++it)
				{
					parent_->toFront(*it);
				}
			}
			else
			{
				ImagePtr image = parent_->getImage( e.x, e.y );

				Vec2f trashcanOrigin(parent_->getTrashCanLocation());
				const float dist(length(Vec2f(e.x, e.y) - trashcanOrigin));

				if (dist < innerTrashcanSaugDist)
				{
					for (std::list<ImagePtr>::iterator it = selectedList_.begin();
						it != selectedList_.end(); ++it)
					{
						ImagePtr image(*it);
						Vec2f origin(image->getLocation());
						origin += (1.0f - image->getCurrentSizeFactor()) * (Vec2f(e.x, e.y) - origin);
						image->setLocation(origin);
						
						image->setSize(image->getSize() * image->getCurrentSizeFactor());

						Vec2f loc(image->getLocation());
						origin += (1.0f - image->getCurrentSizeFactor()) * (Vec2f(e.x, e.y) - loc);
						image->setLocation(loc);
						image->resetSizeFactor();

						image->setTransition(parent_->getTrashCanLocation() - Vec2f(7, 2), Vec2f(10, 10), 0, true);
						deletedList_.push_back( image );
					}
				}
				else
				{
					for (std::list<ImagePtr>::iterator it = selectedList_.begin();
						it != selectedList_.end(); ++it)
					{
						(*it)->setSizeFactor(1);
					}
				}

			}
		}
		break;
	case MouseButton::right:
		buttonRight_ = false;
		zoomImages_ = false;
		break;
	case MouseButton::mid:
		buttonMid_ = false;
		break;
	}
	return true;
}

bool PhotoObserver::onMouseMove(const MouseEvent& e)
{
	bool process = true;
	if(zoomImages_)
	{
		for (std::list<ImagePtr>::iterator it = selectedList_.begin();
			it != selectedList_.end(); ++it)
		{
			float zoomFactor = 1.0 - (0.003 * (lastX_ - e.x));
			zoom( (*it), zoomLocation_, zoomFactor );
			process = false;
		}
	}

	if(buttonLeft_)
	{
		if(parent_->hasSelection())
		{
			parent_->getSelection()->setEnd( Vec2f(e.x,e.y) );
			std::list<ImagePtr> list = parent_->getImages(
				parent_->getSelection()->getStart(), parent_->getSelection()->getEnd(), 0 );

			selectedList_.clear();
			std::list<ImagePtr>::iterator it;
			for (it = selectedPreBox_.begin();
				it != selectedPreBox_.end(); ++it)
			{
				(*it)->setHighlight( true, SDL_GetTicks(), inFade );
				selectedList_.push_back((*it));
			}

			for (it = list.begin();
				it != list.end(); ++it)
			{
				(*it)->setHighlight( true, SDL_GetTicks(), inFade );
				selectedList_.push_back((*it));
			}
			selectedList_.unique();
		}
		else
		{
			onMouseDrag( e );
			releaseSelection_ = false;
		}
	}

	lastXDiff_ = e.x - lastX_;
	lastYDiff_ = e.y - lastY_;
	lastX_ = e.x;
	lastY_ = e.y;
	lastLocationTime_ = SDL_GetTicks();
	return process;
}

bool
PhotoObserver::onKeyPress (const KeyEvent& e)
{
	bool process = true;
	std::list<ImagePtr>::iterator it;

	Vec2f topLeft;
	Vec2f bottomRight;

	switch (e.key)
	{
		case Key::r:
			for (it = selectedList_.begin ();
			     it != selectedList_.end ();
			     ++it)
			{
				ImagePtr image (*it);
				image->setLocation (400, 500);
				image->setRotation (0);
			}
		break;

		case Key::t:
			for (it = selectedList_.begin ();
			     it != selectedList_.end ();
			     ++it)
			{
				ImagePtr image (*it);
				image->setLocation (400, 50);
				image->setRotation (0);
			}
		break;

		case Key::s:
		{
			selectionToFront ();
			parent_->sort (LfWindow::nameSortMode,
				       parent_->photoObserver_->getSelection ());
			break;
		}

		case Key::a:
		{
			selectionToFront ();
			parent_->sort (LfWindow::aspectRatioSortMode,
				       parent_->photoObserver_->getSelection ());
			break;
		}

		case Key::d:
		{
			selectionToFront ();
			parent_->sort (LfWindow::dateSortMode,
				       parent_->photoObserver_->getSelection ());
			break;
		}

		case Key::del:
		break;

		case Key::left:
			for (it = selectedList_.begin ();
			     it != selectedList_.end ();
			     ++it)
			{
				if (shift_)
				{
					int times = (int) (((*it)->getRotation () - 45.1) / 45);
					(*it)->setRotation (times * 45);
				}
				else
					(*it)->setRotation ((*it)->getRotation () - 1);
			}
			process = false;
		break;

		case Key::right:
			for (it = selectedList_.begin ();
			     it != selectedList_.end ();
			     ++it)
			{
				if (shift_)
				{
					int times = (int) (((*it)->getRotation () + 45) / 45);
					(*it)->setRotation (times * 45);
				}
				else
					(*it)->setRotation ((*it)->getRotation () + 1);
			}
			process = false;
		break;

	case Key::up:
		{
			float angle = 1.0;
			if(shift_)
			{
				angle = 5.0;
			}
			RotateSelection( angle );
		}
		process = false;
		break;
	case Key::down:
		{
			float angle = -1.0;
			if(shift_)
			{
				angle = -5.0;
			}
			RotateSelection( angle );
		}
		process = false;
		break;
	case Key::space:
		pullTogether();
		break;
	case Key::shift:
		shift_ = true;
		break;

	case Key::f1:
		g_b_funMode = false;
		noRotate_ = false;
		break;
	case Key::f2:
		g_b_funMode = true;
		noRotate_ = false;
		break;
	case Key::f3:
		g_b_funMode = false;
		noRotate_ = true;
		break;

	case Key::o:
		for (it = selectedList_.begin();
			it != selectedList_.end(); ++it)
		{
			(*it)->setSize( (*it)->getDefaultSize() );
		}
		process = false;
		break;
	case Key::enter:
		for (std::list<ImagePtr>::iterator it = selectedList_.begin();
			it != selectedList_.end(); ++it)
		{
			(*it)->setHighlight( true, SDL_GetTicks(), outFade );
		}
		std::list<ImagePtr> tempList;
		tempList = selectedList_;
		selectedList_ = selectedRestore_;
		selectedRestore_ = tempList;
		for (std::list<ImagePtr>::iterator it = selectedList_.begin();
			it != selectedList_.end(); ++it)
		{
			(*it)->setHighlight( true, SDL_GetTicks(), inFade );
		}
		selectionToFront();
		break;
    };

	return process;
}

void
PhotoObserver::deleteImage (ImagePtr image)
{
	deletedList_.push_back (image);
	image->setHighlight (true, SDL_GetTicks (), outFade);
	image->setFade (true, SDL_GetTicks (), outFade);

	for (std::list<ImagePtr>::iterator it = selectedList_.begin ();
	     it != selectedList_.end();
	     ++it)
		if ((*it) == image)
			selectedList_.erase (it);
}

bool
PhotoObserver::onKeyRelease (const KeyEvent& e)
{
	std::list<ImagePtr>::iterator it;

	switch (e.key)
	{
		case Key::del:
			for (it = selectedList_.begin ();
			     it != selectedList_.end ();
			     ++it)
			{
				ImagePtr image = (*it);
				image->setTransition (parent_->getTrashCanLocation () - Vec2f(7, 2),
						      Vec2f(10, 10),
						      0,
						      true);
				deletedList_.push_back (image);
			}
			selectedList_.clear ();
		break;

		case Key::left:
		break;

		case Key::shift:
			shift_ = false;
		break;
	};

	return true;
}

bool
PhotoObserver::onTime (const TimeEvent& e)
{
	std::list<ImagePtr>::iterator it = deletedList_.begin ();
	while (it != deletedList_.end ())
	{
		if (!(*it)->isVisible ())
		{
			ImagePtr image = (*it);
			deletedList_.remove (image);
			parent_->removeImage (image);
			it = deletedList_.begin ();
		}
		it++;
	}

	return true;
}

void
PhotoObserver::setDynamic (float d)
{
	dynamic_ = d;
}

std::list<ImagePtr>&
PhotoObserver::getSelection ()
{
	return selectedList_;
}

int
PhotoObserver::max (int a,
		    int b)
{
	if (a < b)
		return b;
	else
		return a;
}

int
PhotoObserver::min (int a,
		    int b)
{
	if (a > b)
		return b;
	else
		return a;
}

float
PhotoObserver::max (float a,
		    float b)
{
	if (a < b)
		return b;
	else
		return a;
}

float
PhotoObserver::min (float a,
		    float b)
{
	if (a > b)
		return b;
	else
		return a;
}

float
PhotoObserver::clamp (float x)
{
	return min (max (x, 0.0f), 1.0f);
}

void
PhotoObserver::handleTrashcanSaug (ImagePtr image,
				   const Vec2f& mouse)
{
	Vec2f trashcanOrigin (parent_->getTrashCanLocation ());
	const float dist (length (mouse - trashcanOrigin));

	const float maxDimension (max (image->getWidth (), image->getHeight ()));
	const float minSize (50.0f);
	float minFactor (minSize / max(maxDimension, minSize));

	float n = (dist - innerTrashcanSaugDist) /
			    (outerTrashcanSaugDist - innerTrashcanSaugDist);
	n = clamp (n);
	const float factor (minFactor + (1.0f - minFactor) * n);
	image->setSizeFactor (factor);
}

void
PhotoObserver::onMouseDrag (const MouseEvent& e)
{
	Vec2f src (lastX_, lastY_);
	Vec2f dst (e.x, e.y);

	if (((selectedList_.size () > 1) && !g_b_funMode) || noRotate_)
	{
		for (std::list<ImagePtr>::iterator it = selectedList_.begin ();
			it != selectedList_.end (); ++it)
		{
			ImagePtr image (*it);
			image->setLocation (image->getLocation () + dst - src);
			image->setVelocity (Vec2f ());
			image->setRotationVelocity (0);
			handleTrashcanSaug (image, Vec2f (e.x, e.y));
			image->vr_ = 0;
			image->v_ = Vec2f ();
			image->setDragPoint (SharedPtr<Vec2f>(0));
			image->dragPoint2_ = dst;
		}
	}
	else
	{
		for (std::list<ImagePtr>::iterator it = selectedList_.begin ();
			it != selectedList_.end (); ++it)
		{
			ImagePtr image = (*it);

			SharedPtr<Vec2f> dragPoint (new Vec2f (e.x, e.y));
			image->setDragPoint (dragPoint);
			image->dragPoint2_ = dst;
			handleTrashcanSaug (image, Vec2f(e.x, e.y));
		}
	}
}

void
PhotoObserver::RotateSelection (float angle)
{
	std::list<ImagePtr>::iterator it;
	Vec2f rotationMid (0,0);
	int count = 0;

	for (it = selectedList_.begin ();
	     it != selectedList_.end();
	     ++it)
	{
		rotationMid += (*it)->getLocation ();
		count ++;
	}

	rotationMid /= count;
	for (it = selectedList_.begin ();
		it != selectedList_.end (); ++it)
	{
		Vec2f relativePos = (*it)->getLocation () - rotationMid;
		Vec2f rotatedPos = relativePos;
		rotatedPos.rotate (angle);
		Vec2f diff = relativePos - rotatedPos;
		(*it)->setLocation ((*it)->getLocation () - diff);
		(*it)->setRotation ((*it)->getRotation () + angle);
	}
}

void
PhotoObserver::selectionToFront ()
{
	for (std::list<ImagePtr>::iterator it = selectedList_.begin ();
	     it != selectedList_.end ();
	     ++it)
		parent_->toFront (*it);
}


