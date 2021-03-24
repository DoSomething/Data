var convos = db.getCollection('messages').find({
	"text" : {$in : [/truth/i]}, 
	direction: 'inbound',
        createdAt: {$gte: ISODate("2017-11-01T00:00:00.0Z")}
        },
        {conversationId: 1, _id: 0}
	);
;
db.getCollection('conversations').find( {"_id" : {$in : [convos] }});
convos;
convos.;
{
   $lookup:
     {
       from: ,
       localField: ,
       foreignField: ,
       as: 
     }
}