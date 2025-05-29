use std::{cell::RefCell, rc::Rc, sync::OnceLock};

use gtk::{
    Align, Application, Button, CustomFilter, CustomSorter, FilterChange, FilterListModel, Label,
    ListItem, ListView, PolicyType, ScrolledWindow, SignalListItemFactory, SingleSelection,
    SortListModel, SorterChange, StringList, StringObject, Switch,
    gio::{self, Settings},
    glib::{self, Object, clone, closure_local},
    prelude::*,
    subclass::prelude::*,
};
use tokio::runtime::Runtime;

mod imp {
    use std::cell::Cell;
    use std::sync::OnceLock;

    use gtk::glib::object::ObjectExt;
    use gtk::glib::subclass::Signal;
    use gtk::glib::{self, Properties};
    use gtk::prelude::*;
    use gtk::subclass::prelude::*;

    #[derive(Properties, Default)]
    #[properties(wrapper_type = super::CustomButton)]
    pub struct CustomButton {
        #[property(get, set)]
        number: Cell<i32>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for CustomButton {
        const NAME: &'static str = "ExifCtlCustomButton";
        type Type = super::CustomButton;
        type ParentType = gtk::Button;
    }

    #[glib::derived_properties]
    impl ObjectImpl for CustomButton {
        fn signals() -> &'static [Signal] {
            static SIGNALS: OnceLock<Vec<Signal>> = OnceLock::new();
            SIGNALS.get_or_init(|| {
                vec![
                    Signal::builder("max-number-reached")
                        .param_types([i32::static_type()])
                        .build(),
                ]
            })
        }

        fn constructed(&self) {
            self.parent_constructed();
            self.obj()
                .bind_property("number", self.obj().as_ref(), "label")
                .sync_create()
                .build();
        }
    }

    impl WidgetImpl for CustomButton {}

    impl ButtonImpl for CustomButton {
        fn clicked(&self) {
            static MAX_NUMBER: i32 = 5;
            let new_number = self.obj().number() + 1;
            let obj = self.obj();
            if new_number == MAX_NUMBER {
                obj.emit_by_name::<()>("max-number-reached", &[&new_number]);
                obj.set_number(0)
            } else {
                obj.set_number(new_number);
            }
        }
    }
}

glib::wrapper! {
    pub struct CustomButton(ObjectSubclass<imp::CustomButton>)
        @extends gtk::Button, gtk::Widget,
        @implements gtk::Accessible, gtk::Actionable, gtk::Buildable, gtk::ConstraintTarget;
}

impl Default for CustomButton {
    fn default() -> Self {
        Object::builder().build()
    }
}

impl CustomButton {
    pub fn with_label(label: &str) -> Self {
        Object::builder().property("label", label).build()
    }
}

#[cfg(target_os = "linux")]
async fn fetch_user_information(button: Button) {
    use ashpd::WindowIdentifier;
    use ashpd::desktop::account::UserInformation;

    // Get native of button for window identifier
    let native = button.native().expect("Need to be able to get native.");
    // Get window identifier so that the dialog will be modal to the main window
    let identifier = WindowIdentifier::from_native(&native).await;
    let request = UserInformation::request()
        .reason("App would like to access user information.")
        .identifier(identifier)
        .send()
        .await;

    if let Ok(response) = request.and_then(|r| r.response()) {
        println!("User name: {}", response.name());
    } else {
        println!("Could not access user information.")
    }
}

#[cfg(not(target_os = "linux"))]
async fn fetch_user_information(_button: Button) {
    println!("fetching user information not available outside target_os = \"linux\"");
}

fn runtime() -> &'static Runtime {
    static RUNTIME: OnceLock<Runtime> = OnceLock::new();
    RUNTIME.get_or_init(|| Runtime::new().expect("Tokio runtime setup"))
}

mod exifctl_window_imp {
    use std::cell::OnceCell;

    use gtk::subclass::prelude::*;
    use gtk::{ApplicationWindow, glib};
    use gtk::{gio::Settings, glib::subclass::types::ObjectSubclass};

    #[derive(Default)]
    pub struct ExifCtlAppWindow {
        pub settings: OnceCell<Settings>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for ExifCtlAppWindow {
        const NAME: &'static str = "ExifCtlAppWindow";
        type Type = super::ExifCtlAppWindow;
        type ParentType = ApplicationWindow;
    }

    impl ObjectImpl for ExifCtlAppWindow {
        fn constructed(&self) {
            self.parent_constructed();
            self.obj().setup_settings();
        }
    }

    impl WidgetImpl for ExifCtlAppWindow {}

    impl WindowImpl for ExifCtlAppWindow {
        // Save window state right before the window will be closed
        fn close_request(&self) -> glib::Propagation {
            // Save window size
            // self.obj()
            //     .save_window_size()
            //     .expect("Failed to save window state");
            // Allow to invoke other event handlers
            glib::Propagation::Proceed
        }
    }

    impl ApplicationWindowImpl for ExifCtlAppWindow {}
}

glib::wrapper! {
    pub struct ExifCtlAppWindow(ObjectSubclass<exifctl_window_imp::ExifCtlAppWindow>)
        @extends gtk::ApplicationWindow, gtk::Window, gtk::Widget,
        @implements gio::ActionGroup, gio::ActionMap, gtk::Accessible, gtk::Buildable,
                    gtk::ConstraintTarget, gtk::Native, gtk::Root, gtk::ShortcutManager;
}

impl ExifCtlAppWindow {
    pub fn new(app: &Application) -> Self {
        // Create new window
        Object::builder().property("application", app).build()
    }

    fn setup_settings(&self) {
        self.imp()
            .settings
            .set(Settings::new(APP_ID))
            .expect("Settings should not have been set");
        self.imp()
            .settings
            .get()
            .expect("Settings should be set")
            .bind("window-width", self, "default-width")
            .build();
        self.imp()
            .settings
            .get()
            .expect("Settings should be set")
            .bind("window-height", self, "default-height")
            .build();
        self.imp()
            .settings
            .get()
            .expect("Settings should be set")
            .bind("is-maximized", self, "maximized")
            .build();
    }
}

glib::wrapper! {
  pub struct IntegerObject(ObjectSubclass<int_obj_imp::IntegerObject>);
}

impl IntegerObject {
    pub fn new(number: i32) -> Self {
        Object::builder().property("number", number).build()
    }

    pub fn increment(self) {
        self.set_number(self.number() + 1);
    }
}

mod int_obj_imp {
    use std::cell::Cell;

    use gtk::glib::object::ObjectExt;
    use gtk::glib::subclass::types::ObjectSubclass;
    use gtk::glib::{self, Properties};
    use gtk::subclass::prelude::*;

    #[derive(Properties, Default)]
    #[properties(wrapper_type = super::IntegerObject)]
    pub struct IntegerObject {
        #[property(get, set)]
        number: Cell<i32>,
    }

    #[glib::object_subclass]
    impl ObjectSubclass for IntegerObject {
        const NAME: &'static str = "MyGtkAppIntegerObject";
        type Type = super::IntegerObject;
    }

    #[glib::derived_properties]
    impl ObjectImpl for IntegerObject {}
}

const APP_ID: &str = "com.fredmorcos.exifctl";

fn main() -> glib::ExitCode {
    let app = Application::builder().application_id(APP_ID).build();
    app.connect_activate(build_ui);
    app.run()
}

fn build_ui(app: &Application) {
    let number = Rc::new(RefCell::new(0));
    let number_label = Rc::new(RefCell::new(
        Label::builder()
            .label((*number).borrow().to_string())
            .vexpand(false)
            .hexpand(true)
            .build(),
    ));

    let button_increase = Button::builder().label("+").build();
    button_increase.connect_clicked(clone!(
        #[weak]
        number,
        #[weak]
        number_label,
        move |_| {
            *number.borrow_mut() += 1;
            (*number_label.borrow_mut()).set_label(&(*number).borrow().to_string());
        }
    ));

    let button_decrease = Button::builder().label("-").build();
    button_decrease.connect_clicked(clone!(
        #[strong]
        number_label,
        move |_| {
            *number.borrow_mut() -= 1;
            (*number_label.borrow_mut()).set_label(&(*number).borrow().to_string());
        }
    ));

    let counter_button_1 = CustomButton::default();
    let counter_button_2 = CustomButton::default();

    counter_button_1
        .bind_property("number", &counter_button_2, "number")
        .transform_to(|_, number: i32| Some((number + 1).to_value()))
        .transform_from(|_, number: i32| Some((number - 1).to_value()))
        .bidirectional()
        .sync_create()
        .build();

    counter_button_1.connect_number_notify(|button| {
        println!("Number of counter_button_1 is {}", button.number());
    });

    counter_button_1.connect_closure(
        "max-number-reached",
        false,
        closure_local!(move |_button: CustomButton, number: i32| {
            println!("Counter Button 1 reached the maximum number {}", number);
        }),
    );

    counter_button_2.connect_number_notify(|button| {
        println!("Number of counter_button_2 is {}", button.number());
    });

    counter_button_2.connect_closure(
        "max-number-reached",
        false,
        closure_local!(move |_button: CustomButton, number: i32| {
            println!("Counter Button 2 reached the maximum number {}", number);
        }),
    );

    let switch1 = Switch::builder().valign(Align::Center).build();
    let switch2 = Switch::builder().valign(Align::Center).build();

    switch1
        .bind_property("active", &switch2, "active")
        .bidirectional()
        .build();

    let settings = Settings::new(APP_ID);
    // switch1.set_state(settings.boolean("is-switch-enabled"));

    // switch1.connect_state_set(move |_, is_enabled| {
    //     settings
    //         .set_boolean("is-switch-enabled", is_enabled)
    //         .expect("Could not write settings");
    //     glib::Propagation::Proceed // Invoke other event handlers
    // });

    settings
        .bind("is-switch-enabled", &switch1, "active")
        .build();

    let work_button = Button::with_label("Do some work!");

    // work_button.connect_clicked(move |button| {
    //     use gtk::gio;
    //     use std::{thread, time::Duration};
    //     gio::spawn_blocking(move || {
    //         let five_secs = Duration::from_secs(5);
    //         thread::sleep(five_secs);
    //         // button.set_label("Finished work");
    //     });
    //     button.set_label("Working...");
    // });

    // let (sender, receiver) = async_channel::bounded(1);
    // work_button.connect_clicked(move |_| {
    //     use gtk::gio;
    //     use std::{thread, time::Duration};
    //     let sender = sender.clone();
    //     gio::spawn_blocking(move || {
    //         sender.send_blocking(true).expect("Channel must be open");
    //         let five_secs = Duration::from_secs(5);
    //         thread::sleep(five_secs);
    //         sender.send_blocking(false).expect("Channel must be open");
    //     });
    // });

    // let (sender, receiver) = async_channel::bounded(1);
    // work_button.connect_clicked(move |_| {
    //     glib::spawn_future_local(clone!(
    //         #[strong]
    //         sender,
    //         async move {
    //             sender.send(true).await.expect("Channel must be open");
    //             glib::timeout_future_seconds(5).await;
    //             sender.send(false).await.expect("Channel must be open");
    //         }
    //     ));
    // });

    // glib::spawn_future_local(clone!(
    //     #[weak]
    //     work_button,
    //     async move {
    //         while let Ok(working) = receiver.recv().await {
    //             work_button.set_sensitive(!working);
    //             work_button.set_label(if working {
    //                 "Working..."
    //             } else {
    //                 "Do some work!"
    //             });
    //         }
    //     }
    // ));

    // work_button.connect_clicked(move |button| {
    //     glib::spawn_future_local(clone!(
    //         #[weak]
    //         button,
    //         async move {
    //             button.set_sensitive(false);
    //             button.set_label("Working...");
    //             glib::timeout_future_seconds(5).await;
    //             button.set_label("Do some work!");
    //             button.set_sensitive(true);
    //         }
    //     ));
    // });

    work_button.connect_clicked(move |button| {
        glib::spawn_future_local(clone!(
            #[weak]
            button,
            async move {
                use gtk::gio;
                use std::{thread, time::Duration};
                button.set_sensitive(false);
                button.set_label("Working...");
                let enable_button = gio::spawn_blocking(move || {
                    let five_secs = Duration::from_secs(5);
                    thread::sleep(five_secs);
                    true
                })
                .await
                .expect("Task must finish successfully");
                if enable_button {
                    button.set_sensitive(true);
                    button.set_label("Do some work!");
                }
            }
        ));
    });

    let userinfo_button = Button::with_label("Get user info (async)");
    userinfo_button.connect_clicked(move |button| {
        glib::spawn_future_local(clone!(
            #[weak]
            button,
            async move { fetch_user_information(button).await }
        ));
    });

    let request_button = Button::with_label("Send request (async)");
    let (sender, receiver) = async_channel::bounded(1);
    request_button.connect_clicked(move |_| {
        runtime().spawn(clone!(
            #[strong]
            sender,
            async move {
                let response = reqwest::get("https://www.gtk-rs.org").await;
                sender.send(response).await.expect("Channel must be open");
            }
        ));
    });

    glib::spawn_future_local(async move {
        while let Ok(response) = receiver.recv().await {
            if let Ok(response) = response {
                println!("Status: {}", response.status());
            } else {
                println!("Could not make a GET request");
            }
        }
    });

    let vector: Vec<IntegerObject> = (0..=10).map(IntegerObject::new).collect();
    let model = gio::ListStore::new::<IntegerObject>();
    model.extend_from_slice(&vector);

    let filter = CustomFilter::new(move |obj| {
        // Get `IntegerObject` from `glib::Object`
        let integer_object = obj
            .downcast_ref::<IntegerObject>()
            .expect("The object needs to be of type `IntegerObject`.");

        // Only allow even numbers
        integer_object.number() % 2 == 0
    });
    let filter_model = FilterListModel::new(Some(model), Some(filter.clone()));

    let sorter = CustomSorter::new(move |obj1, obj2| {
        // Get `IntegerObject` from `glib::Object`
        let integer_object_1 = obj1
            .downcast_ref::<IntegerObject>()
            .expect("The object needs to be of type `IntegerObject`.");
        let integer_object_2 = obj2
            .downcast_ref::<IntegerObject>()
            .expect("The object needs to be of type `IntegerObject`.");

        // Get property "number" from `IntegerObject`
        let number_1 = integer_object_1.number();
        let number_2 = integer_object_2.number();

        // Reverse sorting order -> large numbers come first
        number_2.cmp(&number_1).into()
    });
    let sort_model = SortListModel::new(Some(filter_model), Some(sorter.clone()));

    let factory = SignalListItemFactory::new();
    factory.connect_setup(move |_, list_item| {
        let list_item = list_item
            .downcast_ref::<ListItem>()
            .expect("Object must be a ListItem");

        let label = Label::new(None);
        list_item.set_child(Some(&label));

        // Bind `list_item->item->number` to `label->label`
        list_item
            .property_expression("item")
            .chain_property::<IntegerObject>("number")
            .bind(&label, "label", gtk::Widget::NONE);
    });

    factory.connect_bind(move |_, list_item| {
        let integer_object = list_item
            .downcast_ref::<ListItem>()
            .expect("Object must be a ListItem")
            .item()
            .and_downcast::<IntegerObject>()
            .expect("Item must be an IntegerObject");

        let label = list_item
            .downcast_ref::<ListItem>()
            .expect("Object must be a ListItem")
            .child()
            .and_downcast::<Label>()
            .expect("Child must be a Label");

        label.set_label(&integer_object.number().to_string());
    });

    let selection_model = SingleSelection::new(Some(sort_model));
    let list_view = ListView::new(Some(selection_model), Some(factory));

    list_view.connect_activate(move |list_view, position| {
        let model = list_view.model().expect("Model must exist");
        let integer_object = model
            .item(position)
            .and_downcast::<IntegerObject>()
            .expect("Item must be IntegerObject");

        integer_object.increment();

        filter.changed(FilterChange::Different);
        sorter.changed(SorterChange::Different);
    });

    let scrolled_window = ScrolledWindow::builder()
        .hscrollbar_policy(PolicyType::Never)
        // .min_content_width(360)
        .vexpand(true)
        .child(&list_view)
        .build();

    let str_model: StringList = (0..=10).map(|number| number.to_string()).collect();

    let str_factory = SignalListItemFactory::new();
    str_factory.connect_setup(move |_, list_item| {
        let list_item = list_item
            .downcast_ref::<ListItem>()
            .expect("Needs to be ListItem");

        let label = Label::new(None);
        list_item.set_child(Some(&label));

        // Bind `list_item->item->string` to `label->label`
        list_item
            .property_expression("item")
            .chain_property::<StringObject>("string")
            .bind(&label, "label", gtk::Widget::NONE);
    });

    let str_selection_model = SingleSelection::new(Some(str_model));
    let str_list_view = ListView::new(Some(str_selection_model), Some(str_factory));

    let str_scrolled_window = ScrolledWindow::builder()
        .hscrollbar_policy(PolicyType::Never)
        // .min_content_width(360)
        .vexpand(true)
        .child(&str_list_view)
        .build();

    let hlayout = gtk::Box::builder()
        .margin_top(12)
        .margin_start(12)
        .margin_end(12)
        .spacing(12)
        .build();

    hlayout.append(&button_increase);
    hlayout.append(&*(*number_label).borrow());
    hlayout.append(&button_decrease);
    hlayout.append(&counter_button_1);
    hlayout.append(&counter_button_2);
    hlayout.append(&switch1);
    hlayout.append(&switch2);
    hlayout.append(&work_button);
    hlayout.append(&userinfo_button);
    hlayout.append(&request_button);

    let vlayout = gtk::Box::builder()
        .orientation(gtk::Orientation::Vertical)
        .spacing(12)
        .build();

    vlayout.append(&hlayout);
    vlayout.append(&scrolled_window);
    vlayout.append(&str_scrolled_window);

    let window = ExifCtlAppWindow::new(app);
    window.set_title(Some("Exif Control"));
    window.set_child(Some(&vlayout));

    window.present();
}
