/*https://trello.com/c/N7pphlvw/1169-ride-seek-signups-in-a-post-moco-world*/
db.runCommand({
    "distinct": "messages",
    "query": {
        "topic": "seatbelt_level1"
    },
    "key": "conversationId"
}).values.length;

db.runCommand({
    "distinct": "messages",
    "query": {
        "topic": "seatbelt_completed"
    },
    "key": "conversationId"
}).values.length;