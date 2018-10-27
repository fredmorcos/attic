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

#ifndef _IMAGE_H
#define _IMAGE_H

#include <string>
#include <vector>

#include "include/component.h"
#include "include/texture.h"
#include "include/date.h"

class LfDisplay;

enum FadeType
{
	noFade,
	inFade,
	outFade
};

class Image : public Component
{
	public:
		static ImagePtr make (const std::string& filename,
				      bool filter = true);

		virtual ~Image ();
		virtual void paint (LfDisplay& display);

		void setRotationVelocity (float f)
		{
			vr_ = f;
		}

		virtual bool isInside (int x,
				       int y) const;

		virtual void maximizeSize (int maxWidth,
					   int maxHeight);
		virtual Vec2f getMaximizedSize (int maxWidth,
						int maxHeight) const;
		virtual Vec2f getDefaultSize () const;
		Vec2f getTarget () const;
		void adjustTarget (const Vec2f& delta);
		virtual void setRotation (float angle);
		virtual float getRotation () const;
		virtual void scale (float s);
		virtual void advance (unsigned int t);
		virtual void getBoundingBox (Vec2f& topLeft,
					     Vec2f& bottomRight);

		void move (const Vec2f& delta,
			   float timeDelta);
		virtual Vec2f getDestSize () const;
		virtual Vec2f getDestLocation () const;

		void setAlpha (float f);

		virtual void setShadow (bool enable);
		virtual void setHighlight (bool enable,
					   unsigned int t = 0,
					    FadeType type = noFade);
		virtual void setFade (bool enable,
				      unsigned int t = 0,
				      FadeType type = noFade );
		virtual bool isVisible ();

		virtual int	intersectBox (const Vec2f topLeft,
					      const Vec2f bottomRight) const;
		virtual std::string getFilename() const;

		virtual void setTransition (const Vec2f& dstPos,
					    const Vec2f& dstSize,
					    float dstAngle,
					    bool doTheTrashCan = false);

		virtual void setDate(const Date& d);
		virtual Date getDate() const;
		int max (int a, int b);
		int min (int a, int b);
		float max (float a, float b);
		float min (float a, float b);
		virtual ImagePtr clone() const;

		virtual SharedPtr<Vec2f> getDragPoint() const;
		virtual void setDragPoint (SharedPtr<Vec2f> v);

		bool isInTransition() const;
		void setHeight(float height);
		TexturePtr getTexture() const;
		std::string getPath() const;

		static void	setDynamic(float dynamic);

		enum LfStatus
		{
			normalStatus,
			behindTrashcanStatus,
			deleteMeStatus
		};

		LfStatus getStatus () const { return status_; }
		void setSizeFactor(float s);
		float getCurrentSizeFactor();
		void resetSizeFactor();

		void setVelocity( Vec2f velocity );
		Vec2f dragPoint2_;
		float vr_;
		Vec2f v_;

	protected:
		Image(const Image& other);
		Image(const std::string& filename, bool filter = true);
		Image() {};

	private:
		float sizeFactor_;
		float shouldBeSizeFactor_;

		LfStatus status_;
		void updateInertia();
		bool doTheTrashCan_;
		SharedPtr<Vec2f> dragPoint_;
		Vec2f lastDragPoint_;

		void drawShadow (const Vec2f& origin, LfDisplay& d);
		void drawBorder (const Vec2f& origin, LfDisplay& d);

		unsigned int lastTime_;
		Vec2f defaultSize_;
		TexturePtr texture_;
		float rotation_;

		Vec2f srcTransition_;
		Vec2f dstTransition_;
		Vec2f srcSize_;
		Vec2f dstSize_;
		float srcAngle_;
		float dstAngle_;
		float deltaTransition_;

		Vec2f m_;
		Vec2f m0_;
		Vec2f add_;
		int mode_;
		Vec2f velocity_;

		bool hasShadow_;
		Date date_;
		bool doesFade_;
		FadeType fadeType_;
		unsigned int fadeStart_;
		float fadeAlpha_;

		float mass_;
		float inertia_;

		bool hasHighlight_;
		FadeType highlightType_;
		unsigned int highlightStart_;
		float highlightAlpha_;

		std::string filename_;
		std::string path_;

		float random_;
		int modeTime_;

		static TexturePtr shadowBorder;
		static TexturePtr shadowEdge;

		static float dynamic_;
};

#endif // _IMAGE_H
