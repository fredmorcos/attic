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

#include <list>
#include <strstream>
#include <stdexcept>
#include <vector>
#include <pthread.h>
#include <stdlib.h>
#include <unistd.h>
#include <gtk/gtk.h>

#include "SDL.h"
#include "SDL_syswm.h"
#include "include/action.h"
#include "include/key.h"
#include "include/selection.h"
#include "include/lowfat.h"
#include "include/photo_observer.h"
#include "include/date.h"

using std::cout;
using std::strstream;
using std::list;
using std::string;
using std::vector;
using std::runtime_error;

struct ActionLoadImage : Action {
    ActionLoadImage(Lowfat* lowfat) : lowfat_(lowfat) {}
    virtual void execute() { lowfat_->displayLoadImages(); }
    Lowfat* lowfat_;
};

struct ActionQuit : Action {
    ActionQuit(Lowfat* lowfat) : lowfat_(lowfat) {}
    virtual void execute() { lowfat_->quit(); }
    Lowfat* lowfat_;
};

struct ActionSortName : Action {
    ActionSortName(Lowfat* lowfat) : lowfat_(lowfat) {}
	virtual void execute()
	{
		lowfat_->sort( LfWindow::nameSortMode, lowfat_->photoObserver_->getSelection() );
	}
    Lowfat* lowfat_;
};

struct ActionSortDate : Action {
    ActionSortDate(Lowfat* lowfat) : lowfat_(lowfat) {}
	virtual void execute()
	{
		lowfat_->sort( LfWindow::dateSortMode, lowfat_->photoObserver_->getSelection() );
	}
    Lowfat* lowfat_;
};

struct ActionSortSize : Action {
    ActionSortSize(Lowfat* lowfat) : lowfat_(lowfat) {}
	virtual void execute()
	{
		lowfat_->sort( LfWindow::aspectRatioSortMode, lowfat_->photoObserver_->getSelection() );
	}
    Lowfat* lowfat_;
};

Lowfat::Lowfat (std::string title,
		float width,
		float height,
		bool fullscreenFlag,
		int	argc,
		char** argv) :
	Application (title, width, height, fullscreenFlag),
	mouseX_ (0),
	mouseY_ (0),
	enableSelection_ (0),
	showInfo_ (false),
	infoAlpha_ (0),
	lastSelect_ (0),
	lastTime_ (0),
	argc_ (argc),
	argv_ (argv)
{
	trashcan_ = Trashcan::make();
	trashcanFront_ = Trashcan::make("pixmaps/trashcan_front.png");
	const float vGap(-20);
	const float hGap(-20);
	trashcan_->setLocation(getWidth() - trashcan_->getWidth() / 2 - hGap, getHeight() - trashcan_->getHeight() / 2 - vGap);
	trashcan_->setShadow(false);
	trashcanFront_->setLocation(getWidth() - trashcan_->getWidth() / 2 - hGap, getHeight() - trashcan_->getHeight() / 2 - 1.5*vGap);
	trashcanFront_->setShadow(false);
	photoObserver_ = PhotoObserver::make( this );
	selection_ = Selection::make();
	selection_->setStart( Vec2f( 300, 300) );
	selection_->setEnd( Vec2f( 500, 500) );
	background_ = Background::make( "pixmaps/lowfat_bg.png", "pixmaps/lowfat_light.png");
	addGuiObserver( photoObserver_ );
}

Lowfat::~Lowfat()
{
}


Vec2f
Lowfat::getTrashCanLocation() const
{
	return trashcan_->getLocation();
}

void
dialog_destroy (GtkWidget* widget,
		gpointer data)
{
	gtk_widget_destroy (widget);
	gtk_main_quit ();
}

void
dialog_accept (GtkWidget* widget,
	       gpointer data)
{
	GSList* list;

	list = gtk_file_chooser_get_filenames (GTK_FILE_CHOOSER (widget));

	for (unsigned int i = 0; i < g_slist_length (list); i++)
		((StringList*) data)->push_back ((char*) g_slist_nth_data (list, i));

	for (unsigned int i = 0; i < g_slist_length (list); i++)
		g_free (g_slist_nth_data (list, i));

	g_slist_free (list);
	gtk_widget_destroy (widget);
	gtk_main_quit ();
}

void
dialog_cancel (GtkWidget* widget,
	       gpointer data)
{
	gtk_widget_destroy (widget);
	gtk_main_quit ();
}

void
dialog_response (GtkDialog* dialog,
		 gint arg1,
		 gpointer data)
{
	switch (arg1)
	{
		case GTK_RESPONSE_ACCEPT:
			dialog_accept (GTK_WIDGET (dialog), data);
		break;

		case GTK_RESPONSE_CANCEL:
			dialog_cancel (GTK_WIDGET (dialog), data);
		break;

		default :
			std::cout << "dialog_response () - unhandled action" << std::endl;
		break;
	}
}

void
Lowfat::thread_function (StringList* filenameList)
{
	GtkWidget* fileDialog;

	gtk_init (&argc_, &argv_);
	fileDialog = gtk_file_chooser_dialog_new ("load images",
						  NULL,
						  GTK_FILE_CHOOSER_ACTION_OPEN,
						  GTK_STOCK_CANCEL,
						  GTK_RESPONSE_CANCEL,
						  GTK_STOCK_OPEN,
						  GTK_RESPONSE_ACCEPT,
						  NULL);
	gtk_file_chooser_set_select_multiple (GTK_FILE_CHOOSER (fileDialog), true);
	g_signal_connect (G_OBJECT (fileDialog),
			  "destroy",
			  G_CALLBACK (dialog_destroy),
			  NULL);
	g_signal_connect (G_OBJECT (fileDialog),
			  "response",
			  G_CALLBACK (dialog_response),
			  (gpointer) filenameList);
	gtk_widget_show (fileDialog);
	gdk_threads_enter ();
	gtk_main ();
	gdk_threads_leave ();
}

StringList
Lowfat::dialog ()
{
	StringList vstr_fileNames;
	thread_function (&vstr_fileNames);

	return vstr_fileNames;
}

void
Lowfat::DrawLoadInfo (std::string filename)
{
	// draw quad
	display_->setTexture(TexturePtr(0));
	display_->setDrawColor(0, 0, 0, 0.5f);
	const float quadHeight(80);
	const float quadY(display_->getHeight() / 2 - quadHeight / 2);
	const float quadWidth(display_->getWidth());
	display_->drawRectangleMapped(0, quadY, quadWidth, quadHeight);

	// draw text
	string text("loading \"" + filename + "\"...");
	Vec2f size(display_->getTextSize(text.c_str()));
	const Vec2f gap(10, 10);
	const Vec2f shadowDist(2, 2);
	const float shadowIntensity(0.1f);
	display_->setDrawColor(Color(shadowIntensity, shadowIntensity, shadowIntensity, 1));
	Vec2f origin = Vec2f(quadWidth / 2, quadY + quadHeight / 2) - size / 2;
	display_->drawText(origin, text.c_str());
	display_->setDrawColor(Color(1, 1, 1, 1));
	display_->drawText(origin - shadowDist, text.c_str());
	display_->flip();
}

void Lowfat::displayLoadImages()
{
    StringList filenames(dialog());

	photoObserver_->clearSelectedList();
	if (!filenames.empty())
	{
		list<ImagePtr> tempList;
		int fileCount = filenames.size();
		int count = 0;

		for (StringList::iterator it(filenames.begin()); it != filenames.end(); ++it) {
			try
			{

			paint(*display_.get());
			string filename(*it);

			// draw quad
			display_->setTexture(TexturePtr(0));
			display_->setDrawColor(0, 0, 0, 0.5f);
			const float quadHeight(80);
			const float quadY(display_->getHeight() / 2 - quadHeight / 2);
			const float quadWidth(display_->getWidth());
			display_->drawRectangleMapped(0, quadY, quadWidth, quadHeight);

			// draw text
			string text("loading \"" + filename + "\"...");
			Vec2f size(display_->getTextSize(text.c_str()));
			const Vec2f gap(10, 10);
			const Vec2f shadowDist(2, 2);
			const float shadowIntensity(0.1f);
			display_->setDrawColor(Color(shadowIntensity, shadowIntensity, shadowIntensity, 1));
			Vec2f origin = Vec2f(quadWidth / 2, quadY + quadHeight / 2) - size / 2;
			display_->drawText(origin, text.c_str());
			display_->setDrawColor(Color(1, 1, 1, 1));
			display_->drawText(origin - shadowDist, text.c_str());

			display_->flip();

            ImagePtr image(Image::make(filename));

			Vec2f pos( 0, -100 );
			float rotation = -90 + ((count + 0.5) * 180.0 / fileCount);
			pos.rotate( rotation );
			pos.x += -70 + ((count + 0.5) * 140.0 / fileCount);
			pos.x += display_->getWidth() / 2.0;
			pos.y += display_->getHeight() / 2.0;
			pos.y += 50;
            image->setLocation( pos.x, pos.y );
            image->setRotation( rotation );

			addImage(image);
			tempList.push_back( image );
			image->setFade( true,  SDL_GetTicks(), inFade );
			image->advance(0);
			photoObserver_->setSelection( image );

			count++;
			}
			catch(...)
			{
			}
        }
    }
}

void
Lowfat::paint (LfDisplay& display)
{
	static std::list<unsigned int> fpsCount;

	background_->paint (display);

	Application::paint (display);

	if (enableSelection_)
		selection_->paint (display);

	unsigned int time = SDL_GetTicks ();
	fpsCount.push_back (time);
	std::list<unsigned int> fpsTemp;
	for (std::list<unsigned int>::iterator it = fpsCount.begin ();
		it != fpsCount.end(); ++it)
	{
		if((*it) >= time - 1000 )
		{
			fpsTemp.push_back( *it );
		}
	}
	fpsCount = fpsTemp;

	if (showInfo_)
	{
		Vec2f size(display.getTextSize(info_.c_str()));

		Vec2f gap(20, 20);
		Vec2f shadowDist(2, 2);

		const float shadowIntensity(0.1f);
		display.setDrawColor(Color(shadowIntensity, shadowIntensity, shadowIntensity, infoAlpha_));

		Vec2f origin = Vec2f(gap.x, display.getHeight() - size.y - gap.y);

		display.drawText(origin,
				info_.c_str());
		display.setDrawColor(Color(1, 1, 1, infoAlpha_));
		display.drawText(origin - shadowDist,
				info_.c_str());

		origin = Vec2f(display.getWidth() - 220, gap.y);
		char strFps[200];
		int fps = fpsCount.size();
		sprintf (strFps, "%d", fps);
		std::string stats = "fps: ";
		stats += strFps;
		stats += "\nLoaded Images: ";
		sprintf (strFps, "%d", getImages().size());
		stats += strFps;

		stats += "\nfpsCount length: ";
		sprintf (strFps, "%d", fpsCount.size());
		stats += strFps;

		display.setDrawColor(Color(shadowIntensity, shadowIntensity, shadowIntensity, 1));
		display.drawText(origin,
				stats.c_str());
		display.setDrawColor(Color(1, 1, 1, 1 ));
		display.drawText(origin - shadowDist,
				stats.c_str());
	}

}

void
Lowfat::onMouseButton (int x,
		       int y,
		       int button,
		       bool isDown)
{
}

void
Lowfat::onMouseMove (int x,
		     int y)
{
	mouseX_ = x;
	mouseY_ = y;
}

void
Lowfat::onKey (int key,
	       bool isDown)
{
	if (isDown)
	{
		switch (key)
		{
			case Key::l:
				displayLoadImages ();
			break;

			case Key::i:
				showInfo_ = !showInfo_;
			break;

			case Key::q:
			case Key::escape:
				quit ();
			break;

			default:
			break;
		};
	};
}

BackgroundPtr
Lowfat::getBackground ()
{
	return background_;
}

SelectionPtr
Lowfat::getSelection ()
{
	return selection_;
}

void
Lowfat::enableSelection (bool enable)
{
	enableSelection_ = enable;
}

bool
Lowfat::hasSelection ()
{
	return enableSelection_;
}

void
Lowfat::advance (unsigned int t)
{
	Application::advance (t);

	ImagePtr image (getImage (mouseX_, mouseY_));
	if (!image.isNull ())
	{
		strstream buf;
		buf << "Name:  " << image->getFilename ()
		<< "\n"
		<< "Size:    " << image->getDefaultSize ()
		<< "\n"
		<< "Date:   " << image->getDate ()
		<< '\0';

		lastSelect_ = t;
		info_ = buf.str ();
	}

	const int delta (t - lastSelect_);
	const int fadeTime (500);
	if (delta > fadeTime)
		infoAlpha_ = 0;
	else
		infoAlpha_ = 1.0f - ((float) delta / fadeTime);
 
	trashcan_->realAdvance ((t - lastTime_) / 1000.0f);
	trashcanFront_->realAdvance ((t - lastTime_) / 1000.0f);
	lastTime_ = t;
}

