/*https://trello.com/c/G0rMrSRR/1248-data-request-sms-week-of-action-march-for-our-lives-rivescript-query*/
db.getCollection('conversations').find({
    createdAt: {$gte: new Date("2018-03-013T00:00:00.0Z")},
    topic: { $in: [ "marchforourlives_attending", "marchforourlives_supporting", "marchforourlives_moreinfo", "marchforourlives_decline"] }
    },
    {_id: 1, userId: 1, topic: 1, createdAt: 1})