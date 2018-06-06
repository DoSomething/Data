// Requires official MongoShell 3.6+
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
