db.getCollection('messages').find({
	"text" : {$in : [/fomo/i,/hero/i,/trend/i,/usa/i,/rear/i,/ride/i]}, 
	direction: 'inbound',
        createdAt: {$gte: ISODate("2017-11-01T00:00:00.0Z")}
        },
        {text: 1, createdAt: 1}
	);
        
db.messages.group({
    "initial": {
        "countstar": 0
    },
    "reduce": function(obj, prev) {
        if (true != null) if (true instanceof Array) prev.countstar += true.length;
        else prev.countstar++;
    },
    "cond": {
        "text" : {$in : [/fomo/i,/hero/i,/trend/i,/usa/i,/rear/i,/ride/i]}, 
	direction: 'inbound',
        createdAt: {$gte: ISODate("2017-11-01T00:00:00.0Z")}
    }
});