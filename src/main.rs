use bot::DiscordBot;

mod bot;
mod dice;
mod nom_support;

fn main() {
    let bot =
        DiscordBot::new("OTY5NjcyOTk1MTg4NjY2NDI5.Ymw0WQ.GOu_-F_963B-6vwaD6g43x4tDqo").unwrap();

    bot.runloop();
}
