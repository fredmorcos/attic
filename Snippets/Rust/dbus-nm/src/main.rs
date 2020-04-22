use dbus::blocking::Connection;
use std::time::Duration;

mod nm;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let conn = Connection::new_system()?;
    let proxy = conn.with_proxy(
        "org.freedesktop.NetworkManager",
        "/org/freedesktop/NetworkManager",
        Duration::new(5, 0),
    );

    use nm::OrgFreedesktopNetworkManager;

    let devices = proxy.get_all_devices()?;

    println!("Found {} devices", devices.len());

    for dev in devices {
        println!("Device: {}", dev);
    }

    Ok(())
}
