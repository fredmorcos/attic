#[derive(Default)]
struct Counter {
    value: i64,
}

#[derive(Debug, Clone, Copy)]
enum Message {
    Increment,
    Decrement,
    Quit,
}

impl Counter {
    fn update(&mut self, message: Message) -> iced::Task<Message> {
        match message {
            Message::Increment => {
                self.value += 1;
                iced::Task::none()
            }
            Message::Decrement => {
                self.value -= 1;
                iced::Task::none()
            }
            Message::Quit => iced::exit(),
        }
    }

    fn view(&self) -> iced::Element<Message> {
        // The buttons
        let increment_button = iced::widget::button("+")
            .on_press(Message::Increment)
            .padding(10);
        let decrement_button = iced::widget::button("-")
            .on_press(Message::Decrement)
            .padding(10);

        // The number
        let number_text = iced::widget::text(self.value);

        // The layout
        iced::widget::container(
            iced::widget::row![decrement_button, number_text, increment_button].spacing(10),
        )
        .padding(10)
        .center_x(iced::Length::Fill)
        .center_y(iced::Length::Fill)
        .into()
    }

    fn subscription(&self) -> iced::Subscription<Message> {
        iced::Subscription::batch([
            iced::keyboard::on_key_release(|key, mods| {
                if mods.command() && key == iced::keyboard::Key::Character("q".into()) {
                    Some(Message::Quit)
                } else {
                    None
                }
            }),
            iced::keyboard::on_key_press(|key, _mods| {
                if key == iced::keyboard::Key::Character("+".into())
                    || key == iced::keyboard::Key::Character("=".into())
                {
                    Some(Message::Increment)
                } else if key == iced::keyboard::Key::Character("-".into()) {
                    Some(Message::Decrement)
                } else {
                    None
                }
            }),
        ])
    }
}

fn main() -> iced::Result {
    iced::application("A cool counter", Counter::update, Counter::view)
        .subscription(Counter::subscription)
        .theme(|_| iced::Theme::Light)
        .centered()
        .run()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_counts_properly() {
        let mut counter = Counter::default();

        let _ = counter.update(Message::Increment);
        let _ = counter.update(Message::Increment);
        let _ = counter.update(Message::Decrement);

        assert_eq!(counter.value, 1);
    }
}
