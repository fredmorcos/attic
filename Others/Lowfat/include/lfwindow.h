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

#ifndef _LFWINDOW_H
#define _LFWINDOW_H

#include <deque>
#include <list>

#include "include/component.h"
#include "include/lfdisplay.h"
#include "include/shared_ptr.h"
#include "include/forward.h"
#include "include/image.h"

class LfDisplay;

class Trashcan : public Image
{
	public:
		Vec2f defaultSize_;

		Trashcan (const std::string& texturename) :
			Image (texturename)
		{
			time_ = 1000;  // no wobble
			defaultSize_ = getSize ();
		}

		Trashcan () :
			Image ("pixmaps/trashcan.png")
		{
			time_ = 1000;  // no wobble
			defaultSize_ = getSize ();
		}

		void wobble ()
		{
			time_ = 0;
		}

		static SharedPtr<Trashcan> make ()
		{
			return SharedPtr<Trashcan>(new Trashcan ());
		}

		static SharedPtr<Trashcan> make (const std::string& texturename)
		{
			return SharedPtr<Trashcan>(new Trashcan (texturename));
		}

		virtual void realAdvance (float delta)
		{
			time_ += delta;	
		}

		virtual void paint (LfDisplay& display)
		{
			Image::paint (display);

			const float wobbleTime (1.0f);
			if (time_ < wobbleTime)
			{

				float a (((wobbleTime - time_) / wobbleTime) * pow (0.92f, time_));
				const float s (0.3f * a * sin (16.0f * time_));
				setSize (Vec2f (defaultSize_.x * (1 + s), defaultSize_.y * (1 - s)));
			}
		}

	private:
		float time_;
};

class LfWindow : public Component
{
	public:
		static LfWindowPtr make ();

		virtual ~LfWindow ();
		virtual void paint (LfDisplay& display);

		void addImage (ImagePtr image);
		void removeImage (ImagePtr image);
		ImagePtr getImage (int x, int y);
		std::list<ImagePtr> getImages (Vec2f topLeft,
					       Vec2f bottomRight,
					       int intersection) const;

		virtual void advance (unsigned int t);
		void wobbleTrashcan ();

		enum SortMode
		{
			aspectRatioSortMode,
			dateSortMode,
			nameSortMode
		};

		void sort (SortMode mode, std::list<ImagePtr>& selection);
		void sort (SortMode mode,
			   const Vec2f& topLeft,
			   const Vec2f& bottomRight,
			   std::list<ImagePtr>& selection);
		void toFront (ImagePtr image);

		typedef std::list<ImagePtr> ImageList;
		ImageList getImages () const;
		void setImages (const ImageList& list);

		PhotoObserverPtr photoObserver_;

	protected:
		LfWindow ();

	private:
		int lastX_;
		int lastY_;

	protected:
		typedef std::deque<ImagePtr> ImageSeq;
		ImageSeq images_;
		SharedPtr<Trashcan> trashcan_;
		SharedPtr<Trashcan> trashcanFront_;

	private:
		bool sort (ImageSeq& images,
			   const float maxSize,
			   const Vec2f& topLeft,
			   const Vec2f& bottomRight);
		void realSort (ImageSeq& images,
			       SortMode mode);
};

#endif // _LFWINDOW_H
