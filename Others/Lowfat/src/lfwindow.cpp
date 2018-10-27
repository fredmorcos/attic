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
#include <stdexcept>

#include "SDL.h"
#include "include/lfdisplay.h"
#include "include/image.h"
#include "include/key.h"
#include "include/lfwindow.h"
#include "include/photo_observer.h"

using std::list;

static const float gap (16);
static const float maximal (8);

LfWindow::LfWindow () :
	lastX_ (0),
	lastY_ (0)
{
}

void
LfWindow::addImage (ImagePtr image)
{
    images_.push_back (image);
}

void
LfWindow::toFront (ImagePtr image)
{
	removeImage (image);
	addImage (image);
}

void
LfWindow::removeImage (ImagePtr image)
{
	ImageSeq::iterator it (std::find (images_.begin (),
			       images_.end(),
			       image));

	if (it == images_.end ())
		throw std::runtime_error ("missing image");

	images_.erase (it);
}

ImagePtr
LfWindow::getImage (int x,
		    int y)
{
	ImageSeq::reverse_iterator it;

	for (it = images_.rbegin (); it != images_.rend (); it++)
	{
		ImagePtr image = *it;
		if (image->isInside (x, y))
			return image;
	}

	return ImagePtr (0);
}

LfWindow::~LfWindow ()
{
}

void
LfWindow::sort (SortMode mode,
		std::list<ImagePtr>& selection)
{
	const float border (3 * gap + maximal);
	sort (mode,
	      Vec2f (border, border),
	      getSize() - Vec2f (border, border),
	      selection);
}

void
LfWindow::sort (SortMode mode,
		const Vec2f& topLeft,
		const Vec2f& bottomRight,
		std::list<ImagePtr>& selection)
{
	ImageSeq copyOfImgs (images_);

	if (selection.size () > 0)
	{
		copyOfImgs.clear ();
		for (std::list<ImagePtr>::iterator it = selection.begin();
		     it != selection.end();
		     it++)
			copyOfImgs.push_back (*it);
	}

	realSort (copyOfImgs, mode);

	float curSize (1000);
	float deltaSize (curSize);
	const float maxIteration (10);
	float bestSizeSoFar(0);

	for (int iteration (0); iteration < maxIteration; ++iteration)
	{
		if (sort(copyOfImgs, curSize, topLeft, bottomRight))
		{
		        if (curSize > bestSizeSoFar)
				bestSizeSoFar = curSize;

		        curSize += deltaSize;
		}
		else
		        curSize -= deltaSize;

		deltaSize /= 2;
	}

	sort (copyOfImgs, bestSizeSoFar, topLeft, bottomRight);
}

bool
sortByName (const ImagePtr& a,
	    const ImagePtr& b)
{
	return a->getFilename () < b->getFilename ();
}

static float
aspectRatio (const ImagePtr& a)
{
	return a->getWidth () / a->getHeight ();
}

bool
sortByAspectRatio (const ImagePtr& a,
		   const ImagePtr& b)
{
	return aspectRatio (a) < aspectRatio (b);
}

bool
sortByDate (const ImagePtr& a,
	    const ImagePtr& b)
{
	return a->getDate () < b->getDate ();
}

void
LfWindow::realSort (ImageSeq& images,
		    SortMode mode)
{
	switch (mode)
	{
		case nameSortMode:
			std::sort (images.begin (), images.end (), sortByName);
		break;

		case aspectRatioSortMode:
			std::sort (images.begin (),
				   images.end (),
				   sortByAspectRatio);
		break;

		case dateSortMode:
			std::sort (images.begin (),
				   images.end (),
				   sortByDate);
		break;

		default:
			throw std::runtime_error ("not implemented yet");
		break;
	};
}

// returns true, if all images fit in window
bool
LfWindow::sort (ImageSeq& images,
		const float maxSize,
		const Vec2f& topLeft,
		const Vec2f& bottomRight)
{
	const float hGap (8);
	const float vGap (hGap);

	const float maxW (bottomRight.x);
	const float maxH (bottomRight.y);

	ImageSeq::iterator it;

	const float leftBorder (topLeft.x + hGap);
	const float topBorder (topLeft.y + vGap);
	Vec2f pos (leftBorder, topBorder);
	
	ImageSeq::iterator firstInRow (images.begin ());
	float maxSizeSoFar (0);

	typedef list<ImagePtr> ImageSeq;

	for (it = images.begin (); it != images.end (); ++it)
	{
		ImagePtr img (*it);

		Vec2f defaultSize (img->getTexture()->getWidth (),
				   img->getTexture()->getHeight ());

		const float aspect (defaultSize.x / defaultSize.y);
		Vec2f size (aspect * maxSize, maxSize);

		if (pos.x + size.x + hGap > maxW)
		{
			pos.x = leftBorder;
			pos.y += vGap + maxSize;
		}

		if (size.y > maxSizeSoFar)
			maxSizeSoFar = size.y;

		const float dy (maxSize - size.y);
		Vec2f trans (pos + Vec2f (0, dy) / 2 + size / 2);
        
		img->setTransition (trans, size, 0);

		pos.x += size.x + hGap;
		if ((trans.y + size.y) > maxH)
			return false;
	}

	return true;
}

LfWindowPtr
LfWindow::make()
{
	return LfWindowPtr (new LfWindow ());
}

void
LfWindow::wobbleTrashcan ()
{
	trashcan_->wobble ();
	trashcanFront_->wobble ();
}

void
LfWindow::paint (LfDisplay& display)
{
	trashcan_->paint (display);

	for (ImageSeq::iterator it(images_.begin ());
	     it != images_.end ();
	     ++it)
		if (!(*it)->getStatus () == Image::normalStatus)
			(*it)->paint (display);

	for (ImageSeq::iterator it (images_.begin ());
	     it != images_.end ();)
	{
		if ((*it)->getStatus() == Image::deleteMeStatus)
		{
			images_.erase(it);
			it = images_.begin();
			wobbleTrashcan();
		}
		else
			++it;
	}

	trashcanFront_->paint (display);

	for (ImageSeq::iterator it(images_.begin ());
	     it != images_.end ();
	     ++it)
		if ((*it)->getStatus () == Image::normalStatus)
			(*it)->paint (display);
}

std::list<ImagePtr>
LfWindow::getImages (Vec2f topLeft,
		     Vec2f bottomRight,
		     int intersection) const
{
	std::list<ImagePtr> list;

	for (ImageSeq::const_iterator it(images_.begin ());
	     it != images_.end();
	     ++it)
		if ((*it)->intersectBox (topLeft, bottomRight ) >= intersection)
			list.push_back ((*it));

	return list;
}

void
LfWindow::setImages (const ImageList& list)
{
	images_.clear ();

	for (ImageList::const_iterator it (list.begin ());
	     it != list.end();
	     ++it)
		images_.push_back (*it);
}

LfWindow::ImageList
LfWindow::getImages () const
{
	ImageList list;

	for (ImageSeq::const_iterator it (images_.begin ());
	     it != images_.end();
	     ++it)
		list.push_back (*it);

	return list;
}

void
LfWindow::advance (unsigned int t)
{
	for (ImageSeq::iterator it(images_.begin ());
	     it != images_.end();
	     ++it)
	{
		ImagePtr image (*it);
		image->advance (t);
	}
}
