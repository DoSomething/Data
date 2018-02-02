/*https://trello.com/c/OUfjyD6T/1230-data-request-text-in-loud*/
records = [];
var cursor = db.getCollection('messages').aggregate(
    {
        $match: {
            $and: [
                {$text: {$search: "\'loud\'"}},
                {direction: 'inbound'},
                {createdAt: {$gte: new Date("2018-01-25T00:00:00.0Z")}},
            ]
        }
    },
    {
        '$lookup': {
            localField: 'conversationId',
            from: 'conversations',
            foreignField: '_id',
            as: 'conversation'
        }
    },
    {
        '$unwind': '$conversation'
    },
    {
        '$project': {
            'text':1,
            'createdAt':1,
            'conversation.platformUserId': 1,
            'score' : { $meta: "textScore" }
        }
    },
    { $match: { score: { $gt: 1.0 } } }
);
    
while(cursor.hasNext()) {
    records.push(cursor.next())
}
print(tojson(records));