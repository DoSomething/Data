// Requires official MongoShell 3.6+
// https://trello.com/c/OVxJ4KvR/1297-data-request-dss-newsletter-text-sign-ups
use conversations-api;
db.getCollection("messages").find(
    { 
        "direction" : "inbound", 
        "createdAt" : {
            "$gte" : ISODate("2018-05-01T04:00:00.000+0000")
        }, 
        "$or" : [
            {
                "text" : /^.*tmi.*$/i
            }, 
            {
                "text" : /^.*tnt.*$/i
            }, 
            {
                "text" : /^.*dss.*$/i
            }
        ]
    }
);
