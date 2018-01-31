/*https://trello.com/c/N7pphlvw/1169-ride-seek-signups-in-a-post-moco-world*/

/*
SELECT 
    COUNT(DISTINCT conversation_id)
FROM messages 
WHERE topic = 'seatbelt_level1' 
*/

db.runCommand({
    "distinct": "messages",
    "query": {
        "topic": "seatbelt_level1"
    },
    "key": "conversationId"
}).values.length;

/*
SELECT 
    COUNT(DISTINCT conversation_id)
FROM messages 
WHERE topic = 'seatbelt_completed' 
*/

db.runCommand({
    "distinct": "messages",
    "query": {
        "topic": "seatbelt_completed"
    },
    "key": "conversationId"
}).values.length;