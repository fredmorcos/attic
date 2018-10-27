extern crate sdl2;

use sdl2::pixels::Color;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use std::time::Duration;
use std::thread;
use std::process;

pub fn main() {
  let ctx = sdl2::init().unwrap_or_else(|err| {
    eprintln!("Could not initialize SDL context: {}", err);
    process::exit(1);
  });

  let video = ctx.video().unwrap_or_else(|err| {
    eprintln!("Could not initialize video subsystem: {}", err);
    process::exit(1);
  });

  let window: sdl2::video::Window = video
    .window("Jov", 800, 600)
    // .fullscreen_desktop()
    .build()
    .unwrap_or_else(|err| {
      eprintln!("Could not create window: {}", err);
      process::exit(1);
    });

  let mut canvas = window.into_canvas().build().unwrap_or_else(|err| {
    eprintln!("Could not get canvas from window: {}", err);
    process::exit(1);
  });

  canvas.set_draw_color(Color::RGB(120, 0, 120));
  canvas.clear();
  canvas.present();
  let mut event_pump = ctx.event_pump().unwrap();

  'running: loop {
    for event in event_pump.poll_iter() {
      match event {
        Event::Quit { .. } |
        Event::KeyDown { keycode: Some(Keycode::Escape), .. } => break 'running,
        _ => {}
      }
    }

    canvas.present();

    thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
    // The rest of the game loop goes here...
  }
}
