select id, createdAt, text, topic, userId, broadcastId, "match"
from messages
where createdAt >= date('2018-06-20')
and direction = 'inbound'
and topic = 'askSubscriptionStatus'