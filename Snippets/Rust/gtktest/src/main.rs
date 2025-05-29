use gtk::glib;

mod imp {
    use gtk::glib;
    use gtk::subclass::prelude::*;

    // Object holding the state
    #[derive(Default)]
    pub struct CustomButton;

    // The central trait for subclassing a GObject
    #[glib::object_subclass]
    impl ObjectSubclass for CustomButton {
        const NAME: &'static str = "MyGtkAppCustomButton";
        type Type = super::CustomButton;
        type ParentType = gtk::Button;
    }

    // Trait shared by all GObjects
    impl ObjectImpl for CustomButton {}

    // Trait shared by all widgets
    impl WidgetImpl for CustomButton {}

    // Trait shared by all buttons
    impl ButtonImpl for CustomButton {}
}

use glib::Object;

glib::wrapper! {
    pub struct CustomButton(ObjectSubclass<imp::CustomButton>)
        @extends gtk::Button, gtk::Widget,
        @implements gtk::Accessible, gtk::Actionable, gtk::Buildable, gtk::ConstraintTarget;
}

impl CustomButton {
    pub fn new() -> Self {
        Object::builder().build()
    }

    pub fn with_label(label: &str) -> Self {
        Object::builder().property("label", label).build()
    }
}

fn main() {
    println!("Hello, world!");
}
