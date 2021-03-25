import os
from slackclient import SlackClient

client_token = os.environ.get('ETLMON_SLACKBOT_TOKEN')

sc = SlackClient(client_token)

sc.api_call(
  "chat.postMessage",
  channel="#quasar-notifications",
  text="I'M A BOT BEEP BOOP BEEP BOOP"
)