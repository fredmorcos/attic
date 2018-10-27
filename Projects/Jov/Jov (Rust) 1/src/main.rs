extern crate sdl2;
extern crate chrono;
extern crate humantime;
extern crate humansize;

use humansize::{FileSize, file_size_opts as fsopts};

use sdl2::render::BlendMode;
use sdl2::rect::Rect;
use sdl2::pixels::Color;
use sdl2::event::Event;
use sdl2::event::WindowEvent;
use sdl2::keyboard::Keycode;
use sdl2::image::LoadTexture;

use std::mem;
use std::ffi::CStr;
use std::os::raw::c_char;
use std::time::{Instant, Duration};

mod cec;

fn sys_ram() -> u64 {
  sdl2::cpuinfo::system_ram() as u64 * 1024 * 1024
}

fn sys_ram_hs() -> String {
  sys_ram().file_size(fsopts::CONVENTIONAL).unwrap()
}

fn sdl_ver() -> String {
  format!("{} ({})", sdl2::version::version(), sdl2::version::revision())
}

pub fn main() -> Result<(), String> {
  eprintln!("Welcome to the Jov Media Center");
  eprintln!();

  unsafe {
    let mut buf: [c_char; 256] = mem::uninitialized();

    let mut cec_config: cec::libcec_configuration = mem::zeroed();
    cec::libcec_clear_configuration(&mut cec_config);

    let cec_conn: cec::libcec_connection_t = cec::libcec_initialise(&mut cec_config);

    if cec_conn.is_null() {
      Err("Could not open CEC connection")?;
    }

    cec::libcec_version_to_string(cec_config.clientVersion, buf.as_mut_ptr(), 256);
    let libcec_version: String = CStr::from_ptr(buf.as_ptr()).to_string_lossy().into_owned();
    let libcec_lib_info: String = CStr::from_ptr(cec::libcec_get_lib_info(cec_conn)).to_string_lossy().into_owned();
    eprintln!("libCEC {}:{}", libcec_version, libcec_lib_info);

    let logical_addrs = cec::libcec_get_active_devices(cec_conn);

    cec::libcec_logical_address_to_string(logical_addrs.primary, buf.as_mut_ptr(), 256);
    let prim_logical_addr: String = CStr::from_ptr(buf.as_ptr()).to_string_lossy().into_owned();

    eprintln!(" - Primary: {}", prim_logical_addr);

    for i in logical_addrs.addresses.iter() {
      if *i != 0 {
        cec::libcec_logical_address_to_string(*i, buf.as_mut_ptr(), 256);
        let addr: String = CStr::from_ptr(buf.as_ptr()).to_string_lossy().into_owned();
        eprintln!(" - Address: {}", addr);
      }
    }

    // cec::libcec_cec_version_to_string(cec_config.cecVersion, buf.as_mut_ptr(), 256);
    // let cec_version: String = CStr::from_ptr(buf.as_ptr()).to_string_lossy().into_owned();

    // eprintln!(" - CEC version:  {}", cec_version);

    // let cec_dev_name: String = CStr::from_ptr(cec_config.strDeviceName.as_ptr()).to_string_lossy().into_owned();
    // let cec_dev_lang: String = CStr::from_ptr(cec_config.strDeviceLanguage.as_ptr()).to_string_lossy().into_owned();

    // eprintln!(" - Device name:  {}", cec_dev_name);
    // eprintln!(" - Device lang:  {}", cec_dev_lang);

    // cec::libcec_logical_address_to_string(cec_config.baseDevice, buf.as_mut_ptr(), 256);
    // let cec_base_dev_addr: String = CStr::from_ptr(buf.as_ptr()).to_string_lossy().into_owned();

    // eprintln!(" - Base device:  {}", cec_base_dev_addr);
    // eprintln!(" - HDMI port:    {}", cec_config.iHDMIPort);

    // cec::libcec_vendor_id_to_string(cec_config.tvVendor, buf.as_mut_ptr(), 256);
    // let cec_vendor_id: String = CStr::from_ptr(buf.as_ptr()).to_string_lossy().into_owned();

    // eprintln!(" - TV vendor ID: {}", cec_vendor_id);

    eprintln!();
  }

  eprintln!("SDL Version {}", sdl_ver());
  eprintln!();

  eprintln!("Supported video drivers:");

  for driver in sdl2::video::drivers() {
    eprintln!(" - {}", driver);
  }

  eprintln!();

  eprintln!("System info:");
  eprintln!(" - Number of CPUs:  {}", sdl2::cpuinfo::cpu_count());
  eprintln!(" - Memory:          {}", sys_ram_hs());

  eprintln!(" - Cache line (L1): {} bytes", sdl2::cpuinfo::cpu_cache_line_size());

  eprintln!(" - 3DNow:           {}", sdl2::cpuinfo::has_3d_now());
  eprintln!(" - AltiVec:         {}", sdl2::cpuinfo::has_alti_vec());
  eprintln!(" - AVX:             {}", sdl2::cpuinfo::has_avx());
  eprintln!(" - MMX:             {}", sdl2::cpuinfo::has_mmx());
  eprintln!(" - RDTSC:           {}", sdl2::cpuinfo::has_rdtsc());
  eprintln!(" - SSE:             {}", sdl2::cpuinfo::has_sse());
  eprintln!(" - SSE2:            {}", sdl2::cpuinfo::has_sse2());
  eprintln!(" - SSE3:            {}", sdl2::cpuinfo::has_sse3());
  eprintln!(" - SSE4.1:          {}", sdl2::cpuinfo::has_sse41());
  eprintln!(" - SSE4.2:          {}", sdl2::cpuinfo::has_sse42());
  eprintln!();

  let context = sdl2::init()?;

  let timers = context.timer()?;
  let events = context.event()?;

  let fonts = sdl2::ttf::init().map_err(|e| format!("{}", e))?;

  let font_normal = fonts.load_font("data/DejaVuSans.ttf", 16)?;
  let font_small  = fonts.load_font("data/DejaVuSans.ttf", 10)?;
  let font_bold   = fonts.load_font("data/DejaVuSans-Bold.ttf", 36)?;

  let video = context.video()?;

  eprintln!("Selected video driver: {}", video.current_video_driver());
  eprintln!();

  fn disp_mode(mode: sdl2::video::DisplayMode) -> String {
    format!("{}x{}@{}Hz {:?}", mode.w, mode.h, mode.refresh_rate, mode.format)
  }

  let num_displays = video.num_video_displays()?;

  eprintln!("Displays ({}):", num_displays);

  for i in 0 .. num_displays {
    let name = video.display_name(i).map_err
      (|e| format!("Cannot get display {}'s name: {}", i, e))?;
    let bounds = video.display_bounds(i).map_err
      (|e| format!("Cannot get display {}'s ({}) bounds: {}", i, name, e))?;
    let modes = video.num_display_modes(i).map_err
      (|e| format!("Cannot get display {}'s ({}) modes: {}", i, name, e))?;

    let dpi = video.display_dpi(i).map_err
      (|e| eprintln!("Cannot get display {}'s ({}) DPI info: {}", i, name, e)).ok();

    if let Some((ddpi, hdpi, vdpi)) = dpi {
      eprintln!(" - {}: {}, {} modes, ({},{}) {}x{}, \
                 DDPI: {}, HDPI: {}, VDPI: {}",
                i, name, modes,
                bounds.x(), bounds.y(),
                bounds.width(), bounds.height(),
                ddpi, hdpi, vdpi);
    } else {
      eprintln!(" - {}: {}, {} modes, ({},{}) {}x{}",
                i, name, modes,
                bounds.x(), bounds.y(),
                bounds.width(), bounds.height());
      }

    let current_mode = video.current_display_mode(i)?;
    let desktop_mode = video.desktop_display_mode(i)?;

    eprintln!("  > Display modes:");

    for j in 0 .. modes {
      let mode = video.display_mode(i, j)?;

      eprint!("   - {}: {}", j, disp_mode(mode));

      if mode == current_mode {
        eprint!(" [current]");
      }

      if mode == desktop_mode {
        eprint!(" [desktop]");
      }

      eprintln!();
    }
  }

  eprintln!();

  let window = video.window("Jov", 1280, 720)
                    .position_centered()
                    .resizable()
                    .build()
                    .map_err(|e| format!("{}", e))?;

  let mut canvas = window.into_canvas()
                         .build()
                         .map_err(|e| format!("{}", e))?;

  canvas.set_blend_mode(BlendMode::Blend);
  let canvas_size = canvas.output_size()?;

  let textures = canvas.texture_creator();
  let wall_texture = textures.load_texture("data/wall_c_small.jpg")
                             .map_err(|e| format!("Cannot load wallpaper: {}", e))?;

  let time_start = Instant::now();

  let uptime = Duration::from_secs(time_start.elapsed().as_secs());
  let uptime = humantime::format_duration(uptime);
  let uptime = font_small.render(&format!("Uptime: {}", uptime))
                         .blended(Color::RGB(255, 255, 255))
                         .map_err(|e| format!("{}", e))?;

  let mut uptime_texture = textures.create_texture_from_surface(&uptime)
                                   .map_err(|e| format!("{}", e))?;

  let now = chrono::offset::Local::now();
  let curtime = format!("{}", now.format("%H:%M"));
  let curdate = format!("{}", now.format("%d %B, %Y"));

  let curtime = font_bold.render(&curtime)
                         .blended(Color::RGB(255, 255, 255))
                         .map_err(|e| format!("{}", e))?;
  let curdate = font_normal.render(&curdate)
                           .blended(Color::RGB(255, 255, 255))
                           .map_err(|e| format!("{}", e))?;

  let mut curtime_texture = textures.create_texture_from_surface(&curtime)
                                    .map_err(|e| format!("{}", e))?;

  let mut curdate_texture = textures.create_texture_from_surface(&curdate)
                                    .map_err(|e| format!("{}", e))?;

  struct Event1Sec;
  events.register_custom_event::<Event1Sec>()?;

  let _timer1sec = timers.add_timer(1000, Box::new(|| {
    let _ = events.push_custom_event(Event1Sec)
                  .map_err(|e| eprintln!("Warning: {}", e));
    return 1000;
  }));

  struct Event1Min;
  events.register_custom_event::<Event1Min>()?;

  let _timer1min = timers.add_timer(1000 * 60, Box::new(|| {
    let _ = events.push_custom_event(Event1Min)
                  .map_err(|e| eprintln!("Warning: {}", e));
    return 1000 * 60;
  }));

  let mut ev_queue = context.event_pump()?;
  let mut render = false;

  'mainloop: loop {
    let ev = ev_queue.wait_event();

    match ev {
      Event::Quit { .. } |
      Event::KeyDown { keycode: Some(Keycode::Escape), .. } => break 'mainloop,
      Event::Window { win_event: WindowEvent::Exposed, .. } => {
        render = true;
      },
      _ => if ev.is_user_event() {
        if let Some(Event1Sec) = ev.as_user_event_type() {
          let uptime = Duration::from_secs(time_start.elapsed().as_secs());
          let uptime = humantime::format_duration(uptime);
          let uptime = font_small.render(&format!("Uptime: {}", uptime))
                                 .blended(Color::RGB(255, 255, 255))
                                 .map_err(|e| format!("{}", e))?;

          uptime_texture = textures.create_texture_from_surface(&uptime)
                                   .map_err(|e| format!("{}", e))?;

          render = true;
        } else if let Some(Event1Min) = ev.as_user_event_type() {
          let now = chrono::offset::Local::now();
          let curtime = format!("{}", now.format("%H:%M"));
          let curdate = format!("{}", now.format("%d %B, %Y"));

          let curtime = font_bold.render(&curtime)
                                 .blended(Color::RGB(255, 255, 255))
                                 .map_err(|e| format!("{}", e))?;
          let curdate = font_normal.render(&curdate)
                                   .blended(Color::RGB(255, 255, 255))
                                   .map_err(|e| format!("{}", e))?;

          curtime_texture = textures.create_texture_from_surface(&curtime)
                                    .map_err(|e| format!("{}", e))?;

          curdate_texture = textures.create_texture_from_surface(&curdate)
                                    .map_err(|e| format!("{}", e))?;

          render = true;
        }
      },
    }

    if render {
      canvas.copy(&wall_texture, None, None)?;

      canvas.set_draw_color(Color::RGBA(20, 20, 20, 100));
      canvas.fill_rect(Rect::new(10, 10,
                                 (canvas_size.0 / 2) - 20,
                                 canvas_size.1 - 20))?;

      let curtime_size = curtime_texture.query();
      let curtime_x = canvas_size.0 as i32 - curtime_size.width as i32 - 10;
      let curtime_y = 5;

      canvas.copy(&curtime_texture, None, Rect::new(curtime_x, curtime_y,
                                                    curtime_size.width,
                                                    curtime_size.height))?;

      let curdate_size = curdate_texture.query();
      let curdate_x = canvas_size.0 as i32 - curdate_size.width as i32 - 10;
      let curdate_y = curtime_y + curtime_size.height as i32 - 4;

      canvas.copy(&curdate_texture, None, Rect::new(curdate_x, curdate_y,
                                                    curdate_size.width,
                                                    curdate_size.height))?;

      let uptime_size = uptime_texture.query();
      let uptime_x = canvas_size.0 as i32 - uptime_size.width as i32 - 10;
      let uptime_y = curdate_y + curdate_size.height as i32;

      canvas.copy(&uptime_texture, None, Rect::new(uptime_x, uptime_y,
                                                   uptime_size.width,
                                                   uptime_size.height))?;

      canvas.present();
      render = false;
    }
  }

  Ok(())
}
