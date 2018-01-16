records = [];
var cursor = db.getCollection('messages').aggregate([
    {
        $match: {
            $and: [
                {$text: {$search: 'truth'}},
                {direction: 'inbound'},
                {createdAt: {$gte: new Date("2017-10-15T00:00:00.0Z")}},
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
            'conversation._id': 1,
            'conversation.platformUserId': 1,
            'score' : { $meta: "textScore" }
        }
    },  
    { $match: { score: { $gt: 1.0 } } }
]);
/*while (cursor.hasNext()) {
   print(cursor.next());
};*/
while(cursor.hasNext()) {
    records.push(cursor.next())
}
print(tojson(records));