use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct Config {
    pub bot_token: String,
    pub bot_application_id: u64,
    pub webhook_name: String,
}
