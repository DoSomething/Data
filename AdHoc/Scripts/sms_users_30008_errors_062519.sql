SELECT	g1.broadcast_id,
	g1.user_id,
	u.created_at
FROM (
	SELECT	g.broadcast_id,
		CASE WHEN (g.metadata #> '{delivery}') ->> 'failureData' IS NOT NULL
		THEN (g.metadata #> '{delivery, failureData}') ->> 'code'
		ELSE '0' END AS failure_code,
		g.user_id
	FROM ft_gambit_conversations_api.messages g
	WHERE g.broadcast_id IN (
	    'szmNCZiT9hiV1dJUSbZpj',
	    '3M0f7AKBT1X2AaniIDZZfS',
	    '49lVtxVym9IvezhErRMbFi',
	    '5HcKgfxhSCuy86NtRfYqrk'
	)
) g1
LEFT JOIN users u
ON u.northstar_id = g1.user_id
WHERE g1.failure_code = '30008'
