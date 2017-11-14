/*https://trello.com/c/pAyKF9ir/1175-tmi-keyword-signups*/
db.getCollection('messages').find({
	"text" : {$regex : "@"}, 
	topic: { $in: [ "tmi_level1", "tmi_completed" ] }, 
	direction: 'inbound'}, 
	{text: 1, _id: 0}
	)