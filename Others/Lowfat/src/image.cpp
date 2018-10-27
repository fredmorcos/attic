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

#include <string>
#include <stdexcept>
#include <exception>
#include <assert.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "SDL_image.h"
#include "include/lfmath.h"
#include "include/lfdisplay.h"
#include "include/image.h"
#include "include/application.h"
#include "include/lowfat.h"

using std::string;
using std::runtime_error;

extern bool g_b_funMode;

TexturePtr Image::shadowBorder = TexturePtr (0);
TexturePtr Image::shadowEdge = TexturePtr (0);

float Image::dynamic_ = 1.0f;

void
Image::setDynamic (float dynamic)
{
	Image::dynamic_ = dynamic;
}

ImagePtr
Image::make (const std::string& filename,
	     bool filter)
{
	return ImagePtr (new Image (filename, filter));
}

int
Image::max (int a,
	    int b)
{
	if (a < b)
		return b;
	else
		return a;
}

int
Image::min (int a,
	    int b)
{
	if (a > b)
		return b;
	else
		return a;
}

float
Image::max (float a,
	    float b)
{
	if (a < b)
		return b;
	else
		return a;
}

float
Image::min (float a,
	    float b)
{
	if (a > b)
		return b;
	else
		return a;
}

static Date
getFileDate (const std::string& filename)
{
	int year;
	int month;
	int day;
	int hours;
	int mins;
	int secs;
	struct stat statBuffer;
	struct tm* p_tmBuffer;

	stat (filename.c_str (),  &statBuffer);

	p_tmBuffer = localtime (&statBuffer.st_mtime);

	year  = 1900 + p_tmBuffer->tm_year;
	month = 1 + p_tmBuffer->tm_mon;
	day   = p_tmBuffer->tm_mday;
	hours = p_tmBuffer->tm_hour;
	mins  = p_tmBuffer->tm_min;
	secs  = p_tmBuffer->tm_sec;

	return Date (year, month, day, hours, mins, secs);
}

Vec2f
Image::getDestSize () const
{
	return dstSize_;
}

Vec2f
Image::getDestLocation () const
{
	return dstTransition_;
}

static string
stripPath (const string& filename)
{
	unsigned int idx = filename.find_last_of ('/');

	if (idx == string::npos)
		idx = filename.find_last_of ('\\');

	return (idx != string::npos) ?
		filename.substr (idx + 1, filename.size ()) :
		filename;
}

ImagePtr
Image::clone () const
{
	return ImagePtr (new Image (*this));
}

Image::Image (const Image& other) :
	vr_ (other.vr_),
	v_ (other.v_),
	sizeFactor_ (other.sizeFactor_),
	shouldBeSizeFactor_ (other.shouldBeSizeFactor_),
	status_ (other.status_),
	lastDragPoint_ (other.lastDragPoint_),
	lastTime_ (other.lastTime_),
	texture_ (other.texture_),
	rotation_ (other.rotation_),
	deltaTransition_ (other.deltaTransition_),
	m_ (other.m_),
	m0_ (other.m0_),
	add_ (other.add_),
	mode_ (other.mode_),
	velocity_ (other.v_),
	hasShadow_ (other.hasShadow_),
	date_ (other.date_),
	doesFade_ (other.doesFade_),
	fadeType_ (other.fadeType_),
	fadeStart_ (other.fadeStart_),
	fadeAlpha_ (other.fadeAlpha_),
	mass_ (other.mass_),
	inertia_ (other.inertia_),
	hasHighlight_ (other.hasHighlight_),
	highlightType_ (other.highlightType_),
	highlightStart_ (other.highlightStart_),
	highlightAlpha_ (other.highlightAlpha_),
	filename_ (other.filename_),
	path_ (other.path_),
	modeTime_ (other.modeTime_)
{
	setSize (other.getSize ());
	setLocation (other.getLocation ());
}

Image::Image (const std::string& filename,
	      bool filter) :
	vr_ (0),
	sizeFactor_ (1),
	shouldBeSizeFactor_ (1),
	status_ (normalStatus),
	doTheTrashCan_ (false),
	lastDragPoint_ (0, 0),
	lastTime_ (0),
	texture_ (Texture::make (filename, filter)),
	rotation_ (0),
	deltaTransition_ (1),
	mode_ (0),
	velocity_ (Vec2f (0.0f, 0.0f)),
	hasShadow_ (true),
	date_ (Date (0, 0, 0, 0, 0, 0)),
	doesFade_ (false),
	fadeType_(noFade),
	fadeStart_ (0),
	fadeAlpha_ (1.0f),
	hasHighlight_ (true),
	highlightType_ (noFade),
	highlightStart_ (0),
	highlightAlpha_ (0.0f),
	filename_ (stripPath (filename)),
	path_ (filename),
	modeTime_ (0)
{
	mass_ = 10;
	updateInertia ();

	const float maxWidth (300);
	const float maxHeight (maxWidth);

	maximizeSize ((int) maxWidth, (int) maxHeight);

	if (Image::shadowBorder.get () == 0)
		Image::shadowBorder = Texture::make ("pixmaps/shadow_border.png",
						     true);
	if (Image::shadowEdge.get () == 0)
		Image::shadowEdge = Texture::make ("pixmaps/shadow_edge.png",
						   true);

	setDate (getFileDate (filename));
	dstTransition_ = getLocation ();
	dstSize_ = getSize ();
	dstAngle_ = getRotation ();

	if (texture_->hasAlpha ())
		setShadow (false);
}

std::string
Image::getFilename() const
{
	return filename_;
}

void
Image::updateInertia ()
{
	const float radius (length (getSize ()) / 2);
	inertia_ = mass_ * radius * radius;
}

Vec2f
Image::getMaximizedSize (int maxWidth,
			 int maxHeight) const
{
	Vec2f defaultSize (texture_->getWidth (), texture_->getHeight ());

	const float aspect (defaultSize.x / defaultSize.y);

	if (defaultSize.x > maxWidth)
	{
		defaultSize.x = maxWidth;
		defaultSize.y = maxWidth / aspect;
	}

	if (defaultSize.y > maxHeight)
	{
		defaultSize.y = maxHeight;
		defaultSize.x = maxHeight * aspect;
	}

	return defaultSize;
}

void
Image::maximizeSize (int maxWidth,
		     int maxHeight)
{
	defaultSize_ = Vec2f (texture_->getWidth (), texture_->getHeight ());
 
	Vec2f defaultSize = defaultSize_;

	const float aspect = defaultSize_.x / defaultSize_.y;

	if (defaultSize.x > maxWidth)
	{
		defaultSize.x = maxWidth;
		defaultSize.y = maxWidth / aspect;
	}

	if (defaultSize.y > maxHeight)
	{
		defaultSize.y = maxHeight;
		defaultSize.x = maxHeight * aspect;
	}

	setSize (defaultSize);
}

Image::~Image()
{
}

Vec2f
Image::getDefaultSize() const
{
	return defaultSize_;
}

Vec2f
Image::getTarget() const
{
	return dstTransition_;
}

void
Image::setTransition (const Vec2f& dstPos,
		      const Vec2f& dstSize,
		      float dstAngle,
		      bool doTheTrashCan)
{
	doTheTrashCan_ = doTheTrashCan;

	if (doTheTrashCan_)
		random_ = (float) rand() / RAND_MAX;

	int dir = 360;
	if (dstAngle - getRotation () > 0)
		dir = -360;

	while (fabs (dstAngle - getRotation ()) > 180)
		dstAngle += dir;

	if ((dstAngle - getRotation ()) > 180)
		dstAngle -= 360;
	else if ((getRotation () - dstAngle) > 180)
		dstAngle += 360;

	if (doTheTrashCan_)
		dstAngle += 360.0f * (2.0f + random_ * 2.0f);

	v_ = Vec2f ();
	vr_ = 0;

	srcTransition_ = getLocation ();
	dstTransition_ = dstPos;
	srcSize_ = getSize ();
	dstSize_ = dstSize;
	srcAngle_ = getRotation ();
	dstAngle_ = dstAngle;
	deltaTransition_ = 0;
}

void
Image::adjustTarget (const Vec2f& delta)
{
	dstTransition_ += delta;
}

void
Image::scale (float s)
{
	Vec2f newSize (getDefaultSize () * s);
	setSize (newSize);
}

float
Image::getCurrentSizeFactor ()
{
	return sizeFactor_;
}

void
Image::drawShadow (const Vec2f& origin,
		   LfDisplay& display)
{
	Vec2f size (getSize () * sizeFactor_);

	if (hasShadow_)
	{
		const float shadowOpacity = 0.36f;
		const float shadowSize = 1.0f;
		Vec2f direction (7.0f, 7.0f);

		Vec2f width (size.x / 2.0f, 0.0f);
		Vec2f height (0.0f, size.y / 2.0f);

		float nShadowWidth = Image::shadowBorder->getWidth () < size.x ?
			Image::shadowBorder->getWidth () :
			size.x;

		float nShadowHeight = Image::shadowBorder->getWidth () < size.y ?
			Image::shadowBorder->getWidth () :
			size.y;

		Vec2f shadowWidth (nShadowWidth, 0.0f);
		Vec2f shadowHeight (0.0f, nShadowHeight);

		width.rotate (rotation_);
		height.rotate (rotation_);
		shadowWidth.rotate (rotation_);
		shadowHeight.rotate (rotation_);

		Vec2f location (origin);

		display.setTexture (TexturePtr (0));

		const Color color (0.0f,
				   0.0f,
				   0.0f,
				   shadowOpacity * fadeAlpha_);
		const Color color2 (1.0f,
				    1.0f,
				    1.0f,
				    shadowOpacity * fadeAlpha_);

		Vec2f tl = location +
			   (shadowWidth + shadowHeight) / 2.0 +
			   (- width - height) *
			   shadowSize;

		Vec2f tr = location +
			   (- shadowWidth + shadowHeight) / 2.0 +
			   (width - height) *
			   shadowSize;

		Vec2f br = location +
			    (- shadowWidth - shadowHeight) / 2.0 +
			    (width + height) *
			    shadowSize;

		Vec2f bl = location +
			   (shadowWidth - shadowHeight) / 2.0 +
			   (- width + height) *
			   shadowSize;

		display.drawQuad (tl + direction,
				  color,
				  tr + direction,
				  color,
				  br + direction,
				  color,
				  bl + direction,
				  color);

		display.setTexture (Image::shadowBorder);

		display.drawQuad (br + direction + shadowWidth,
				  color2,
				  br + direction,
				  color2,
				  tr + direction,
				  color2,
				  tr + direction + shadowWidth,
				  color2);

		display.drawQuad (tl + direction - shadowWidth,
				  color2,
				  tl + direction,
				  color2,
				  bl + direction,
				  color2,
				  bl + direction - shadowWidth,
				  color2);

		display.drawQuad (bl + direction + shadowHeight,
				  color2,
				  bl + direction,
				  color2,
				  br + direction,
				  color2,
				  br + direction + shadowHeight,
				  color2);

		display.drawQuad (tl + direction - shadowHeight,
				  color2,
				  tl + direction,
				  color2,
				  tr + direction,
				  color2,
				  tr + direction - shadowHeight,
				  color2);

		display.setTexture (Image::shadowEdge);

		display.drawQuad (bl + direction - shadowWidth,
				  color2,
				  bl + direction,
				  color2,
				  bl + direction + shadowHeight,
				  color2,
				  bl + direction - shadowWidth + shadowHeight,
				  color2);

		display.drawQuad (br + direction + shadowWidth,
				  color2,
				  br + direction,
				  color2,
				  br + direction + shadowHeight,
				  color2,
				  br + direction + shadowWidth + shadowHeight,
				  color2);

		display.drawQuad (tl + direction - shadowWidth,
				  color2,
				  tl + direction,
				  color2,
				  tl + direction - shadowHeight,
				  color2,
				  tl + direction - shadowWidth - shadowHeight,
				  color2);

		display.drawQuad (tr + direction + shadowWidth,
				  color2,
				  tr + direction,
				  color2,
				  tr + direction - shadowHeight,
				  color2,
				  tr + direction + shadowWidth - shadowHeight,
				  color2);
	}
}

void
Image::drawBorder (const Vec2f& origin,
		   LfDisplay& display)
{
	if (hasHighlight_ && (fadeAlpha_ * highlightAlpha_ > 0))
	{
		Vec2f size (getSize () * sizeFactor_);
		Vec2f width (size.x / 2.0f, 0.0f);
		Vec2f height (0.0f, size.y / 2.0f);
		Vec2f depth1 (6.0f, 6.0f);
		Vec2f depth2 (6.0f, -6.0f);
		width.rotate (rotation_);
		height.rotate (rotation_);
		depth1.rotate (rotation_);
		depth2.rotate (rotation_);

		Vec2f location (origin);

		display.setTexture (TexturePtr (0));
		Color in (0.0f, 0.0f, 0.0f, 0.25f * fadeAlpha_ * highlightAlpha_);
		Color out (0.0f, 0.0f, 0.0f, 0.0f * fadeAlpha_ * highlightAlpha_);

		for (int i = 0; i < 2; i++)
		{
			display.drawQuad (location + width - height,
					  in,
					  location + width - height + depth2,
					  out,
					  location + width + height + depth1,
					  out,
					  location + width + height,
					  in);

			display.drawQuad (location - width - height,
					  in,
					  location - width - height - depth1,
					  out,
					  location - width + height - depth2,
					  out,
					  location - width + height,
					  in);

			display.drawQuad (location - width - height,
					  in,
					  location - width - height - depth1,
					  out,
					  location + width - height + depth2,
					  out,
					  location + width - height,
					  in);

			display.drawQuad (location - width + height,
					  in,
					  location - width + height - depth2,
					  out,
					  location + width + height + depth1,
					  out,
					  location + width + height,
					  in);

			display.drawQuad (location + width - height,
					  in,
					  location + width - height - depth2,
					  out,
					  location + width + height - depth1,
					  out,
					  location + width + height,
					  in);

			display.drawQuad (location - width - height,
					  in,
					  location - width - height + depth1,
					  out,
					  location - width + height + depth2,
					  out,
					  location - width + height,
					  in);

			display.drawQuad (location - width - height,
					  in,
					  location - width - height + depth1,
					  out,
					  location + width - height - depth2,
					  out,
					  location + width - height,
					  in);

			display.drawQuad (location - width + height,
					  in,
					  location - width + height + depth2,
					  out,
					  location + width + height - depth1,
					  out,
					  location + width + height,
					  in);

			in = Color (1.0f,
				    1.0f,
				    1.0f,
				    1.0f * fadeAlpha_ * highlightAlpha_);
			out = Color (1.0f,
				     1.0f,
				     1.0f,
				     0.0f * fadeAlpha_ * highlightAlpha_);
		}

		display.setDrawColor (Color (1.0f,
					     1.0f,
					     1.0f,
					     fadeAlpha_ * highlightAlpha_));

		display.drawLine (location - width - height,
				  location + width - height );

		display.drawLine (location + width - height,
				  location + width + height );

		display.drawLine (location + width + height,
				  location - width + height );

		display.drawLine (location - width + height,
				  location - width - height );
	}
}

bool
Image::isInside (int x,
		 int y) const
{
	Vec2f p (x, y);
	p -= getLocation ();
	p.rotate (-rotation_);
	p += getLocation ();

	return p.x >= getX () - getWidth () / 2.0f &&
	       p.x < getX () + getWidth () / 2.0f &&
	       p.y >= getY () - getHeight () / 2.0f &&
	       p.y < getY () + getHeight () / 2.0f;
}

void
Image::getBoundingBox (Vec2f& topLeft,
		       Vec2f& bottomRight)
{
	Vec2f br = getSize() / 2.0f;
	Vec2f bl = getSize() / 2.0f;
	bl.x = - bl.x;

	br.rotate (rotation_);
	bl.rotate (rotation_);

	float minX = -fabs (br.x) < -fabs (bl.x) ? -fabs (br.x) : -fabs (bl.x);
	float maxX = fabs (br.x) > fabs (bl.x) ? fabs (br.x) : fabs (bl.x);
	float minY = -fabs (br.y) < -fabs (bl.y) ? -fabs (br.y) : -fabs (bl.y);
	float maxY = fabs (br.y) > fabs (bl.y) ? fabs (br.y) : fabs (bl.y);

	Vec2f mid = getLocation ();
	topLeft = mid + Vec2f (minX, minY);
	bottomRight = mid + Vec2f (maxX, maxY);
}

SharedPtr<Vec2f>
Image::getDragPoint () const
{
	return dragPoint_;
}

static float
calcTorque (const Vec2f& ld,
	    const Vec2f& f)
{
	Vec2f tmp (-ld.y, ld.x);
	tmp.normalize ();
	return dot (tmp, f) * length (ld);
}

void
Image::advance (unsigned int t)
{
	const int fadeTime = 250;
	const unsigned int highlightFadeTime = 150;

	if (fadeType_ != noFade)
	{
		int timeDiff = t - fadeStart_;

		if (timeDiff >= fadeTime)
			fadeAlpha_ = 1;
		else
			fadeAlpha_ = ((float) timeDiff) / fadeTime;

		if (fadeType_ == outFade)
			fadeAlpha_ = 1.0f - fadeAlpha_;
	}

	if (highlightType_ != noFade)
	{
		unsigned int timeDiff = t - highlightStart_;

		if (timeDiff >= highlightFadeTime)
		{
			if (highlightType_ == outFade)
			{
				highlightType_ = noFade;
				highlightAlpha_ = 0.0f;
			}
			else
			{
				float cosTime = (timeDiff - highlightFadeTime) /
						500.0f;

				if (cosTime > 3.14f)
					cosTime = 3.14f;

				highlightAlpha_ = 0.85f +
						  (cos (cosTime) * 0.15f);
			}
		}
		else
		{
			float newAlpha;

			if (highlightType_ == inFade)
			{
				newAlpha = (((float) timeDiff) /
					   highlightFadeTime);

				highlightAlpha_ = (newAlpha > highlightAlpha_) ?
						  newAlpha :
						  highlightAlpha_;
			}
			else
			{
				newAlpha = 1.0 -
					   (((float) timeDiff) /
					   highlightFadeTime);

				highlightAlpha_ = (newAlpha < highlightAlpha_) ?
						  newAlpha :
						  highlightAlpha_;
			}
		}
	}

	// transition
	const float delta ((t - lastTime_) / 1000.0f); // delta time in seconds

	// make smaller for trashcan pull-in region
	if (sizeFactor_ != shouldBeSizeFactor_)
	{
		const float spd (4.0f);
		const float d ((shouldBeSizeFactor_ > sizeFactor_ ? spd : -spd) * delta);

		sizeFactor_ += d;

		if (d > 0)
			sizeFactor_ = min (sizeFactor_, shouldBeSizeFactor_);
		else if (d < 0)
			sizeFactor_ = max (sizeFactor_, shouldBeSizeFactor_);
	}

	if (deltaTransition_ < 1)
	{
		if (doTheTrashCan_)
		{
			// higher means being faster
			const float transitionSpeed (1.0f + random_ * 0.2f);

			deltaTransition_ = min (deltaTransition_ +
						delta *
						transitionSpeed,
						1.0f);

			const float interpol (sin (deltaTransition_ * 3.141592 - 3.141592 / 2.0f) / 2.0f + 0.5f);
			
			setLocation (srcTransition_ + interpol * (dstTransition_ - srcTransition_));
			const float adjustY ((100.0f + 40.0f * random_)* sin (deltaTransition_ * 3.142592f));
			setLocation (getLocation () - Vec2f (0.0f, adjustY));

			if (deltaTransition_ >= 1.0f)
				status_ = deleteMeStatus;
			else if (deltaTransition_ >= 0.5f)
				status_ = behindTrashcanStatus;

			setSize (srcSize_ + interpol * (dstSize_ - srcSize_));
			const float deltaAngle (interpol * (dstAngle_ - srcAngle_));
			setRotation (srcAngle_ + deltaAngle);
		}
		else
		{
			// higher means being faster
			const float transitionSpeed (1.5f);
			deltaTransition_ = min (deltaTransition_ + delta * transitionSpeed, 1.0f);
			const float interpol (sin(deltaTransition_ * 3.141592f - 3.141592f / 2.0f) / 2.0f + 0.5f);
			setLocation (srcTransition_ + interpol * (dstTransition_ - srcTransition_));
			setSize (srcSize_ + interpol * (dstSize_ - srcSize_));
			const float deltaAngle (interpol * (dstAngle_ - srcAngle_));
			setRotation (srcAngle_ + deltaAngle);
		}
	}

	const float inertia = (getSize().x > getSize().y) ?
			      getSize().x :
			      getSize().y;

	Vec2f dm;
	Vec2f ld;
	float torque = 0;

	float min = 0.0001f;

	if (!dragPoint_.isNull ())
		mode_ = 1;
	else if (mode_ == 1 && dragPoint_.isNull ())
		mode_ = 2;

	const float b (1.04f);

	switch (mode_)
	{
		case 1:
		{
			if (modeTime_ == 1)
			{
				modeTime_ = 0;
				m_ = *dragPoint_;
				m0_ = m_;
				dm = Vec2f ();
			}
			else
			{
				m_ = (*dragPoint_ + m_) / 2.0f;
				dm = m_ - m0_;
				m0_ = m_;
			}

			float M = 200;
			float maxDim = (getSize ().x > getSize ().y) ?
					getSize ().x :
					getSize ().y;

			if (g_b_funMode)
				maxDim = (rand() % 1000) + 100;

			Vec2f f = (dm - v_) * M / maxDim;
			ld = m_ - getLocation ();
			torque = calcTorque (ld, f);
			v_ = dm;
			move (v_, delta);
			vr_ = vr_ - torque / inertia;
			ld = m_ - getLocation ();
			add_ = ld;
			add_.rotate (-vr_);
			add_ -= ld;
			move (-add_,delta);
			break;
		}

		case 2:
			if (modeTime_ == 1)
			{
				modeTime_ = 0;
				v_ -= add_;
			}
			torque = 0;
			add_ = Vec2f ();
			move (v_,delta);

			if (length (v_) < min && abs ((int) vr_) < min)
				mode_ = 0;
		break;

		// nothing to do
		case 0:
		break;

		default:
			assert (false);
		break;
	}

	setRotation (getRotation () - vr_);

	float factor = (1/b);
	factor = pow (factor, delta * 20.f);
	factor *= pow (Image::dynamic_, 0.035f);
	v_ *= factor;
	vr_ *= factor;
	velocity_ *= factor;
	move (velocity_ * delta, delta);
	lastTime_ = t;
}

void
Image::move (const Vec2f& delta,
	     float timeDelta)
{
	setLocation (getLocation () + (delta));
}

bool
Image::isInTransition () const
{
	return deltaTransition_ < 1;
}

TexturePtr
Image::getTexture () const
{
	return texture_;
}

std::string
Image::getPath () const
{
	return path_;
}

void
Image::setHeight (float height)
{
	Vec2f defaultSize (texture_->getWidth (),
			   texture_->getHeight ());

	const float aspect (defaultSize.x / defaultSize.y);
	setSize (Vec2f (aspect * height, height));
}

void
Image::setDragPoint (SharedPtr<Vec2f> v)
{
	if (dragPoint_.isNull () &&
	    !v.isNull ())
		modeTime_ = 1;

	if (!dragPoint_.isNull () &&
	    v.isNull ())
		modeTime_ = 1;

	dragPoint_ = v;

	if (!v.isNull ())
		lastDragPoint_ = *v;
}

void
Image::setRotation (float angle)
{
	rotation_ = angle;
}

float
Image::getRotation () const
{
	return rotation_;
}

void
Image::setDate (const Date& d)
{
	date_ = d;
}

Date
Image::getDate () const
{
	return date_;
}

void
Image::resetSizeFactor ()
{
	shouldBeSizeFactor_ = sizeFactor_ = 1;
}

void
Image::paint (LfDisplay& display)
{
	Vec2f origin (getLocation ());

	origin += (1.0f - sizeFactor_) * (dragPoint2_ - origin);

	drawShadow (origin, display);

	display.setDrawColor (Color (1.0f, 1.0f, 1.0f, fadeAlpha_));
	display.setTexture (texture_);

	Vec2f size (getSize () * sizeFactor_);

	display.drawRectangleMapped (origin.x - size.x / 2.0f,
				     origin.y - size.y / 2.0f,
				     size.x,
				     size.y,
				     rotation_);
	drawBorder (origin, display);
}

void
Image::setSizeFactor(float s)
{
	shouldBeSizeFactor_ = s;
}

void
Image::setShadow (bool enable)
{
	hasShadow_ = enable;
}

void
Image::setHighlight (bool enable,
		     unsigned int t,
		     FadeType type)
{
	hasHighlight_ = enable;
	highlightStart_ = t;
	highlightType_ = type;
}

void Image::setAlpha (float f)
{
	fadeAlpha_ = f;
}

void
Image::setFade (bool enable,
		unsigned int t,
		FadeType type)
{
	doesFade_ = enable;
	fadeStart_ = t;
	fadeType_ = type;
}

bool
Image::isVisible ()
{
	return (fadeAlpha_ != 0);
}

static bool
IntersectLine (Vec2f a1,
	       Vec2f a2,
	       Vec2f b1,
	       Vec2f b2)
{
	bool returnValue = false;

	Vec2f r = a2 - a1;
	Vec2f s = b2 - b1;

	float n = s.y * (a1.x - b1.x) - s.x * (a1.y - b1.y);
	float d = s.x * r.y - s.y * r.x;

	if (fabs (d) >= 0.0001f)
	{
		n /= d;

		float m = r.y * (a1.x - b1.x) - r.x * (a1.y - b1.y);
		d = r.y * s.x - r.x * s.y;
		if (fabs (d) >= 0.0001f)
		{
			m /= d;
			Vec2f intersection1 = a1 + (n * r);
			Vec2f intersection2 = b1 + (m * s);

			if ((n >= 0 ) &&
			    (n <= 1) &&
			    (m >= 0 ) &&
			    (m <= 1))
				returnValue = true;
		}
	}
    
	return returnValue;
}

int
Image::intersectBox (Vec2f topLeft,
		     Vec2f bottomRight ) const
{
	Vec2f start = topLeft;
	Vec2f end = bottomRight;

	if (topLeft.x > bottomRight.x)
	{
		start.x = bottomRight.x;
		end.x = topLeft.x;
	}

	if (topLeft.y > bottomRight.y)
	{
		start.y = bottomRight.y;
		end.y = topLeft.y;
	}

	int inside = 0;

	if (isInside ((int) start.x, (int) start.y))
		inside++;

	if (isInside ((int) start.x, (int) end.y))
		inside++;
	if (isInside ((int) end.x, (int) start.y))
		inside++;

	if (isInside ((int) end.x, (int) end.y))
		inside++;

	Vec2f halfWidth = Vec2f (getWidth () / 2.0f, 0.0f);
	Vec2f halfHeight = Vec2f (0.0f, getHeight () / 2.0f);

	Vec2f tl = - halfWidth - halfHeight;
	Vec2f tr =   halfWidth - halfHeight;
	Vec2f bl = - halfWidth + halfHeight;
	Vec2f br =   halfWidth + halfHeight;

	tl.rotate (rotation_);
	tr.rotate (rotation_);
	bl.rotate (rotation_);
	br.rotate (rotation_);

	tl += getLocation ();
	tr += getLocation ();
	bl += getLocation ();
	br += getLocation ();

	int insideBox = 0;

	if ((tl.x >= start.x) &&
	    (tl.x <= end.x) &&
	    (tl.y >= start.y) &&
	    (tl.y <= end.y))
		insideBox++;

	if ((tr.x >= start.x) &&
	    (tr.x <= end.x) &&
	    (tr.y >= start.y) &&
	    (tr.y <= end.y))
		insideBox++;

	if ((bl.x >= start.x) &&
	    (bl.x <= end.x) &&
	    (bl.y >= start.y) &&
	    (bl.y <= end.y))
		insideBox++;

	if ((br.x >= start.x) &&
	    (br.x <= end.x) &&
	    (br.y >= start.y) &&
	    (br.y <= end.y))
		insideBox++;

	int intersection = -1;

	if ((inside > 0) ||
	    (insideBox > 0))
		intersection = 0;

	if ((inside == 4) ||
	    (insideBox == 4))
		intersection = 1;

	Vec2f boxTL = start;
	Vec2f boxTR = start;
	boxTR.x += end.x - start.x;
	Vec2f boxBR = end;
	Vec2f boxBL = end;
	boxBL.x -= end.x - start.x;

	if (intersection != 0)
		if (IntersectLine(boxTL, boxTR, tl, tr) ||
		    IntersectLine(boxTL, boxTR, tr, br) ||
		    IntersectLine(boxTL, boxTR, br, bl) ||
		    IntersectLine(boxTL, boxTR, bl, tl) ||
		    IntersectLine(boxTR, boxBR, tl, tr) ||
		    IntersectLine(boxTR, boxBR, tr, br) ||
		    IntersectLine(boxTR, boxBR, br, bl) ||
		    IntersectLine(boxTR, boxBR, bl, tl) ||
		    IntersectLine(boxBR, boxBL, tl, tr) ||
		    IntersectLine(boxBR, boxBL, tr, br) ||
		    IntersectLine(boxBR, boxBL, br, bl) ||
		    IntersectLine(boxBR, boxBL, bl, tl) ||
		    IntersectLine(boxBL, boxTL, tl, tr) ||
		    IntersectLine(boxBL, boxTL, tr, br) ||
		    IntersectLine(boxBL, boxTL, br, bl) ||
		    IntersectLine(boxBL, boxTL, bl, tl))
			intersection = 0;

	return intersection;
}

void
Image::setVelocity (Vec2f velocity)
{
	velocity_ = velocity;
}

