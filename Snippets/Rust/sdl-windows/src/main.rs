use derive_more::From;
use log::trace;
use sdl2::event::{Event, WindowEvent};
use sdl2::pixels::Color;
use sdl2::render::WindowCanvas;
use sdl2::video::WindowBuildError;
use sdl2::VideoSubsystem;
use std::collections::HashMap;
use std::fmt::{self, Debug};
use std::process;
use std::sync::mpsc;
use thiserror::Error;

#[derive(From, Error)]
enum Error {
    #[error("Cannot initialize logger: {0}")]
    LogInit(log::SetLoggerError),
    #[error("SDL: {0}")]
    SDL(String),
    #[error("SDL-Window: {0}")]
    SDLWindow(WindowBuildError),
    #[error("SDL-Canvas: {0}")]
    SDLCanvas(sdl2::IntegerOrSdlError),
}

impl Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

// A message queue from the perspective of the child window.
struct MessageQ {
    incoming_pair: (mpsc::Sender<Event>, mpsc::Receiver<Event>),
    outgoing_pair: (mpsc::Sender<Event>, mpsc::Receiver<Event>),
}

impl MessageQ {
    fn new() -> Self {
        Self {
            incoming_pair: mpsc::channel(),
            outgoing_pair: mpsc::channel(),
        }
    }
}

fn create_window(
    windows: &mut HashMap<u32, (WindowCanvas, MessageQ)>,
    vid: &VideoSubsystem,
    title: &str,
    width: u32,
    height: u32,
    resizable: bool,
) -> Result<u32, Error> {
    let mut win_builder = vid.window(title, width, height);
    win_builder.allow_highdpi().opengl();
    if resizable {
        win_builder.resizable();
    }
    let win = win_builder.build()?;
    let id = win.id();
    let canvas = win.into_canvas().accelerated().present_vsync().build()?;
    windows.insert(id, (canvas, MessageQ::new()));
    Ok(id)
}

fn main() -> Result<(), Error> {
    env_logger::Builder::new()
        .filter_level(log::LevelFilter::Trace)
        .try_init()?;

    trace!("Process {}", process::id());

    let sdl_ctx = sdl2::init()?;
    let vid = sdl_ctx.video()?;

    let mut windows = HashMap::new();
    let main_win = create_window(&mut windows, &vid, "Rine", 800, 600, true)?;
    windows.get_mut(&main_win).and_then(|(w, _)| {
        w.set_draw_color(Color::RGB(255, 255, 255));
        w.clear();
        w.present();
        Some(())
    });
    let main_win_2 = create_window(&mut windows, &vid, "Rine 2", 800, 600, false)?;
    windows.get_mut(&main_win_2).and_then(|(w, _)| {
        w.set_draw_color(Color::RGB(0, 255, 255));
        w.clear();
        w.present();
        Some(())
    });

    let mut evq = sdl_ctx.event_pump()?;

    'mainloop: loop {
        for ev in evq.wait_iter() {
            match ev {
                Event::Quit { .. } => {
                    trace!("Quit");
                    break 'mainloop;
                }
                Event::Window {
                    window_id,
                    win_event,
                    ..
                } => match win_event {
                    WindowEvent::Exposed | WindowEvent::Resized(_, _) => {
                        trace!("Window {} expose", window_id);
                        windows.get_mut(&window_id).and_then(|(w, _)| {
                            w.clear();
                            w.present();
                            Some(())
                        });
                    }
                    WindowEvent::Close => {
                        trace!("Window {} close", window_id);
                        windows.remove(&window_id);
                    }
                    _ => {}
                },
                _ => {}
            }
        }
    }

    Ok(())
}
