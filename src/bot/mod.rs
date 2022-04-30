use std::collections::HashMap;

use anyhow::Result;
use discord::{
    model::{ChannelId, Event, Message, WebhookId},
    Discord,
};
use nom::Finish;
use regex::{Captures, Regex};
use tracing::{error, info, warn};

use crate::dice::DiceExpr;

const WEBHOOK_NAME: &'static str = "Lady Luck Webhook";
const BOT_APPLICATION_ID: u64 = 969672995188666429;

fn superscript(n: usize) -> String {
    let n = n.to_string();

    n.chars()
        .map(|c| match c {
            '0' => '⁰',
            '1' => '¹',
            '2' => '²',
            '3' => '³',
            '4' => '⁴',
            '5' => '⁵',
            '6' => '⁶',
            '7' => '⁷',
            '8' => '⁸',
            '9' => '⁹',
            _ => unreachable!(),
        })
        .collect()
}

pub struct DiscordBot {
    discord: Discord,
    webhook_map: HashMap<ChannelId, (WebhookId, String)>,
}

impl DiscordBot {
    pub fn new(token: &str) -> Result<Self> {
        Ok(DiscordBot {
            discord: Discord::from_bot_token(token)?,
            webhook_map: HashMap::new(),
        })
    }

    fn lookup_webhook(&self, channel_id: ChannelId) -> Option<(WebhookId, String)> {
        let webhooks = match self.discord.get_channel_webhooks(channel_id) {
            Ok(webhooks) => webhooks,
            Err(e) => {
                error!("Failed to get webhooks: {:?}", e);
                return None;
            }
        };

        for webhook in webhooks {
            if let Some(app_id) = webhook.application_id {
                if app_id.0 == BOT_APPLICATION_ID {
                    match webhook.token {
                        Some(token) => return Some((webhook.id, token)),
                        None => {
                            error!("Webhook was found, but no token was found");
                            return None;
                        }
                    }
                }
            }
        }

        None
    }

    fn get_webhook(&'_ mut self, channel_id: ChannelId) -> Result<(WebhookId, String)> {
        if !self.webhook_map.contains_key(&channel_id) {
            if let Some((webhook_id, token)) = self.lookup_webhook(channel_id) {
                self.webhook_map
                    .insert(channel_id, (webhook_id, token.clone()));
            } else {
                let webhook = self.discord.create_webhook(channel_id, WEBHOOK_NAME)?;
                let webhook_id = webhook.id;
                let webhook_token = webhook
                    .token
                    .ok_or_else(|| anyhow::anyhow!("No token was created"))?;
                self.webhook_map
                    .insert(channel_id, (webhook_id, webhook_token));
            }
        }
        Ok(self.webhook_map[&channel_id].clone())
    }

    fn replace_message(&mut self, message: Message, new_message: String) -> Result<()> {
        let (webhook_id, token) = self.get_webhook(message.channel_id)?;

        self.discord
            .delete_message(message.channel_id, message.id)?;

        self.discord
            .execute_webhook(webhook_id.clone(), &token, |m| {
                let m = m.content(&new_message).username(&message.author.name);
                if let Some(avatar) = message.author.avatar_url() {
                    m.avatar_url(&avatar)
                } else {
                    m
                }
            })?;

        Ok(())
    }

    fn process_message(&mut self, message: Message) -> Result<()> {
        if message.author.bot {
            return Ok(());
        }
        info!("Message {}: {}", message.author.name, message.content);
        let matcher = Regex::new(r"\[([^\]]+)\]")?;
        let mut explanation = Vec::new();

        let processed = matcher.replace_all(&message.content, |captures: &Captures| {
            if let Some(group) = captures.get(1) {
                let n = superscript(explanation.len() + 1);

                let expr = match DiceExpr::parse_input(group.as_str()) {
                    Ok(expr) => expr,
                    Err(e) => {
                        explanation.push(format!("{}Failed to parse: {}", n, e));
                        return format!("[{}]{}", group.as_str(), n);
                    }
                };
                let rolled = expr.roll();

                if let Some(result) = rolled.evaluate() {
                    explanation.push(format!("{}{} = {}", n, rolled.to_string(), result));
                    format!("_{}_{}", result, n)
                } else {
                    explanation.push(format!("{}Evaluation didn't finish", n));
                    format!("[{}]{}", group.as_str(), n)
                }
            } else {
                warn!("Expected to capture two groups, but got 1: {:?}", captures);
                "[error]".to_owned()
            }
        });

        if explanation.len() == 0 {
            return Ok(());
        }

        let new_message = format!("{}\n```{}```", processed, explanation.join("\n"));
        self.replace_message(message, new_message)?;

        Ok(())
    }

    pub fn runloop(mut self) -> () {
        let (mut connection, _) = self.discord.connect().expect("connect failed");

        loop {
            match connection.recv_event() {
                Ok(Event::MessageCreate(message)) => {
                    if let Err(e) = self.process_message(message) {
                        warn!("Failed to process message: {:?}", e);
                    }
                }
                Ok(_) => {}
                Err(discord::Error::Closed(code, body)) => {
                    error!("Gateway closed on us with code {:?}: {}", code, body);
                    break;
                }
                Err(err) => error!("Receive error: {:?}", err),
            }
        }
    }
}
