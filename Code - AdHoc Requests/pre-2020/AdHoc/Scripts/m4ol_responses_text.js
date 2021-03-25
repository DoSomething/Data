records = [];
var cursor = db.getCollection('messages').aggregate(
    {
        $match: {
            $and: [
                {broadcastId: { $in: ["7EGLM4kIrSsCuSKe8M0ygw","4w1YT4PkYoOoSUmImgW8C8"]}},
                {direction: 'inbound'},
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
            '_id':0,
            'text':1,
            'broadcastId':1,
            'conversation.topic':1,
            'conversation.userId': 1
        }
    },
    { $match: { 'conversation.topic': { $in: ["m4ol_broadcast","m4ol_supporting","m4ol_moreinfo","m4ol_declined","m4ol_attending"]} } }
);
    
while(cursor.hasNext()) {
    records.push(cursor.next())
}
print(tojson(records));