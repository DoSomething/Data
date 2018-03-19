/*https://trello.com/c/G0rMrSRR/1248-data-request-sms-week-of-action-march-for-our-lives-rivescript-query*/
records = [];
var cursor = db.getCollection('conversations').find({
    direction: 'inbound', 
    topic: { $in: [ "m4ol_attending", "m4ol_supporting", "m4ol_moreinfo", "m4ol_declined"] },
    },
    {_id: 0, userId: 1, topic: 1});
while(cursor.hasNext()) {
    records.push(cursor.next())
}
print(tojson(records));