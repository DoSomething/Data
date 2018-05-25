select id, userId, createdAt, text, broadcastId
from messages
where createdAt >= date('2018-05-15')
and direction = 'outbound-api-send'
and topic in ('survey_response','20180521_under18','20180521_over18')
and broadcastId in 
	('4PquTwIQl2i4qQYsKqKQAa','5vyK4K1AfCiKoO4eSqOWcG','3cX33cUhbaIIAaOoi2QCom',
	'qlWhZeya5MsUo4GgkmaEq','66EGbeyPYWOUqQeKGOE8o8','61xugfjgqIwKyo2oyeM6ia',
	'1AWgluG2UMOi00aUCOO42M','5LzV8fu88wKwIeCamQUEU2','67N6vJNwU88UaQo48wAKkO',
	'5gU2anzZs4QeIuEQIEu2wA','4WkcJ18nBSGq0QswIQE6QQ','6t4VjB7KY8OKIMUcogkqY0',
	'7Kx6vTVoQMKcOQoyEsKIY8','5o1htHPVv2U06uKEME2GG4','357paQN8jSiGiccAgAwGas',
	'5OEc1BTvnGqmIcUYU6ys0e','3ORkFOPltY46i8gy2YmWuK','5worKrNP20C2IyKqcKME8I',
	'1W1JpSxudCWoIs06oMU0Gy','5HJRlEvpJuw8EwMKEqWIi4','5vYcOK31HUKe6aA8SKS8Ow',
	'5t3dCcSFskEq0E2OYieUCy','2W8L0PszwkCIGS0Q2Ma4Co','4pP4o3FAJaeegeumey6QKU',
	'17C5wWDuEsmiik2mmqS2mI');
