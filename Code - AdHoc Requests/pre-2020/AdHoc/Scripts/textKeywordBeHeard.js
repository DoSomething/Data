/*https://trello.com/c/OUfjyD6T/1230-data-request-text-in-loud*/
records = [];
var cursor = db.getCollection('messages').aggregate(
    {
        $match: {
            $and: [
                {$text: {$search: "\'beheard\'"}},
                {direction: 'inbound'},
                {createdAt: {$gte: new Date("2018-02-14T00:00:00.0Z")}},
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
    { $match: { score: { $gt: .9 } } }
);
    
while(cursor.hasNext()) {
    records.push(cursor.next())
}
print(tojson(records));