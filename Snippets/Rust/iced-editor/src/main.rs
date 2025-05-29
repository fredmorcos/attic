use std::{
    io,
    path::{Path, PathBuf},
    sync::Arc,
};

use iced::{
    Element, Font,
    Length::Fill,
    Settings, Subscription, Task, Theme,
    highlighter::{self, Highlighter},
    keyboard,
    widget::{
        button, column, container, horizontal_space, pick_list, row, text, text_editor, tooltip,
    },
};

struct Editor {
    path: Option<PathBuf>,
    content: text_editor::Content,
    error: Option<Error>,
    theme: highlighter::Theme,
    is_dirty: bool,
}

#[derive(Debug, Clone)]
enum Message {
    New,
    Open,
    Save,
    Edit(text_editor::Action),
    FileOpened(Result<(PathBuf, Arc<String>), Error>),
    FileSaved(Result<PathBuf, Error>),
    ThemeSelected(highlighter::Theme),
}

impl Default for Editor {
    fn default() -> Self {
        Editor {
            content: text_editor::Content::new(),
            error: None,
            path: None,
            theme: highlighter::Theme::SolarizedDark,
            is_dirty: true,
        }
    }
}

impl Editor {
    fn update(&mut self, message: Message) -> Task<Message> {
        match message {
            Message::Edit(action) => {
                self.error = None;
                self.is_dirty = self.is_dirty || action.is_edit();
                self.content.perform(action);
            }
            Message::FileOpened(Ok((path, content))) => {
                self.path = Some(path);
                self.content = text_editor::Content::with_text(&content);
                self.is_dirty = false;
            }
            Message::FileOpened(Err(err)) => {
                self.error = Some(err);
            }
            Message::Open => return Task::perform(pick_file(), Message::FileOpened),
            Message::New => {
                self.path = None;
                self.content = text_editor::Content::new();
                self.is_dirty = true;
            }
            Message::Save => {
                let text = self.content.text();
                return Task::perform(save_file(self.path.clone(), text), Message::FileSaved);
            }
            Message::FileSaved(Ok(path)) => {
                self.path = Some(path);
                self.is_dirty = false;
            }
            Message::FileSaved(Err(err)) => self.error = Some(err),
            Message::ThemeSelected(theme) => self.theme = theme,
        }

        Task::none()
    }

    fn view(&self) -> Element<Message> {
        let controls = row![
            action(new_icon(), "New File", Some(Message::New)),
            action(open_icon(), "Open File", Some(Message::Open)),
            action(
                save_icon(),
                "Save File",
                self.is_dirty.then_some(Message::Save)
            ),
            horizontal_space(),
            pick_list(
                highlighter::Theme::ALL,
                Some(self.theme),
                Message::ThemeSelected
            )
        ]
        .spacing(10);

        let editor = text_editor(&self.content)
            .on_action(Message::Edit)
            .height(Fill)
            .highlight_with::<Highlighter>(
                highlighter::Settings {
                    theme: self.theme,
                    token: self
                        .path
                        .as_ref()
                        .and_then(|path| path.extension()?.to_str())
                        .unwrap_or("rs")
                        .to_string(),
                },
                |highlight, _theme| highlight.to_format(),
            );

        let status_bar = {
            let status = if let Some(Error::IOFailed(err)) = self.error.as_ref() {
                text(err.to_string())
            } else {
                match self.path.as_deref().and_then(Path::to_str) {
                    Some(path) => text(path).size(14),
                    None => text("New file"),
                }
            };

            let position = {
                let (line, col) = self.content.cursor_position();
                text(format!("{}:{}", line + 1, col + 1))
            };

            row![status, horizontal_space(), position]
        };

        container(column![controls, editor, status_bar].spacing(10))
            .padding(10)
            .into()
    }

    fn subscription(&self) -> Subscription<Message> {
        keyboard::on_key_press(|keycode, modifiers| match keycode.as_ref() {
            keyboard::Key::Character("s") if modifiers.command() => Some(Message::Save),
            keyboard::Key::Character("o") if modifiers.command() => Some(Message::Open),
            keyboard::Key::Character("n") if modifiers.command() => Some(Message::New),
            _ => None,
        })
    }

    fn theme(&self) -> Theme {
        if self.theme.is_dark() {
            Theme::Dark
        } else {
            Theme::Light
        }
    }
}

#[derive(Debug, Clone)]
enum Error {
    DialogClosed,
    IOFailed(io::ErrorKind),
}

fn default_file() -> PathBuf {
    PathBuf::from(format!("{}/src/main.rs", env!("CARGO_MANIFEST_DIR")))
}

async fn save_file(path: Option<PathBuf>, text: String) -> Result<PathBuf, Error> {
    let path = if let Some(path) = path {
        path
    } else {
        rfd::AsyncFileDialog::new()
            .set_title("Choose a file name...")
            .save_file()
            .await
            .ok_or(Error::DialogClosed)
            .map(|handle| handle.path().to_owned())?
    };

    tokio::fs::write(&path, text)
        .await
        .map_err(|err| Error::IOFailed(err.kind()))?;

    Ok(path)
}

async fn pick_file() -> Result<(PathBuf, Arc<String>), Error> {
    let handle = rfd::AsyncFileDialog::new()
        .set_title("Choose a text file...")
        .pick_file()
        .await
        .ok_or(Error::DialogClosed)?;
    load_file(handle.path().to_owned()).await
}

async fn load_file(path: PathBuf) -> Result<(PathBuf, Arc<String>), Error> {
    let contents = tokio::fs::read_to_string(&path)
        .await
        .map(Arc::new)
        .map_err(|err| err.kind())
        .map_err(Error::IOFailed)?;
    Ok((path, contents))
}

fn new_icon<'a>() -> Element<'a, Message> {
    icon('\u{E800}')
}

fn save_icon<'a>() -> Element<'a, Message> {
    icon('\u{E802}')
}

fn open_icon<'a>() -> Element<'a, Message> {
    icon('\u{E801}')
}

fn icon<'a>(codepoint: char) -> Element<'a, Message> {
    const ICON_FONT: Font = Font::with_name("iced-editor-icons");
    text(codepoint).font(ICON_FONT).into()
}

fn action<'a>(
    content: Element<'a, Message>,
    label: &'a str,
    on_press: Option<Message>,
) -> Element<'a, Message> {
    tooltip(
        button(container(content).center_x(30))
            .on_press_maybe(on_press)
            .padding([5, 10]),
        label,
        tooltip::Position::Bottom,
    )
    .gap(10)
    .style(container::rounded_box)
    .into()
}

fn main() -> iced::Result {
    iced::application("Iced Editor", Editor::update, Editor::view)
        .settings(Settings {
            default_font: Font::MONOSPACE,
            fonts: vec![
                include_bytes!("../fonts/iced-editor-icons.ttf")
                    .as_slice()
                    .into(),
            ],
            ..Settings::default()
        })
        .theme(Editor::theme)
        .subscription(Editor::subscription)
        .centered()
        .run_with(|| {
            (
                Editor::default(),
                Task::perform(load_file(default_file()), Message::FileOpened),
            )
        })
}
