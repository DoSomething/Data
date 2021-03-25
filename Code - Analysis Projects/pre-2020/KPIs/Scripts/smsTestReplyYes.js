// Requires official MongoShell 3.6+
use conversations-api;
db.messages.find(
    { 
        "createdAt" : {
            "$gte" : ISODate("2018-05-15T04:00:00.000+0000")
        }, 
        "direction" : "inbound", 
        "text": /.*yes.*/,
        "topic" : {
            "$in" : [
                "20180521_under18", 
                "20180521_over18"
            ]
        },
    }, 
    { 
        "id" : "$id", 
        "userId" : "$userId", 
        "createdAt" : "$createdAt", 
        "text" : "$text", 
        "topic" : "topic"
    }
);

