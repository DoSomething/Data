-- returns count of users by broadcast and error code where an error code of 0 means no error --
SELECT 	g1.broadcast_id,
	g1.failure_code,
	count(DISTINCT g1.user_id) AS num_users
FROM (
	SELECT  g.broadcast_id,
		CASE WHEN (g.metadata ->> 'delivery')::json ->> 'failureData' IS NOT NULL
		THEN (g.metadata #> '{delivery, failureData}')::json ->> 'code'
		ELSE '0' END AS failure_code,
		g.user_id
	FROM (
		SELECT records ->> 'broadcastId' AS broadcast_id, cast(records ->> 'metadata' AS json) AS metadata, records ->> 'userId' AS user_id
		FROM gambit.messages_json
	) g
	WHERE g.broadcast_id IN () -- add broadcast ids --
) g1
WHERE g1.failure_code IN ('30003', '21610','21614','21606','30008','21612','30004','21211', '30006', '0')
GROUP BY g1.broadcast_id, g1.failure_code
